#' Run the R-based RFA simulations
#'
#' @param BestFitParams matrix of parameters exported from BestFit
#' @param LP3 boolean indicating whether BestFitParams are for LP3 or GEV distributions (TRUE = LP3; FALSE = GEV)
#' @param StageTS data frame/matrix time series of daily stage (reservoir pool elevation): col 1 = MM/DD/YYYY named 'date' ; col 2 = stage named 'stage'
#' @param Seasonality vector or data frame with twelve values; from the count column from RFA; first value = jan, second value = feb, etc.
#' @param Hydrographs matrix of historic/synthetic hydrographs with hourly time step: nrow = critical duration; ncol = number of hydrographs
#' @param ResModel elevation stage discharge rating curve: matrix/data frame with three columns: "stage_ft" , "stor_acft" , "discharge_cfs"
#' @param CritialDur integer indicating critical duration in hours
#' @param RoutingDur integer indicating routing duration in hours
#' @param ExpectedOnly boolean indicating whether to run expected only or full uncertainty. default is expected only
#' @return Vector of bin boundaries (length n_bins + 1)

rfaR <- function(BestFitParams, LP3=TRUE, StageTS, Seasonality, Hydrographs, ResModel, CriticalDur, RoutingDur, ExpectedOnly=TRUE){
  # LOAD DEPENDENCIES
  require(evd)
  require(lubridate)
  require(zoo)

  # FLOW FREQUENCY SAMPLER
  Q.samp = flowFrequencySampler(bestfit=BestFitParams, ExpectedOnly=FALSE)

  # SEASONALITY/STARTING POOL SAMPLER
  Nsims = nrow(Q.samp$flow) * ncol(Q.samp$flow)
  InitMonths = sample(1:12, size=Nsims, replace=TRUE, prob=Seasonality)
  InitStages = numeric(Nsims)
  StageTS$months = month(mdy(StageTS$date))
  UniqMonths = unique(InitMonths)

  for(i in 1:length(UniqMonths)){
    sampleID = which(InitMonths == UniqMonths[i])
    InitStages[sampleID] = sample(StageTS$stage[StageTS$months %in% UniqMonths[i]], size=sum(InitMonths == UniqMonths[i]), replace=TRUE)
  }


  # HYDROGRAPH SCALER/SAMPLER
  hydroSamps = sample(1:ncol(Hydrographs), size=Nsims, replace=TRUE)
  peakStage = peakFlow = matrix(NA, nrow=nrow(Q.samp$flow), ncol=ncol(Q.samp$flow))

  pb = txtProgressBar(min=0,max=nrow(Q.samp$flow),style=3)
  cnt=0
  for(i in 1:nrow(Q.samp$flow)){
    setTxtProgressBar(pb,i)
    for(j in 1:ncol(Q.samp$flow)){
      cnt=cnt+1
      hydroSamp  = as.vector(t(Hydrographs[,hydroSamps[cnt]]))
      hydroRatio = as.numeric(Q.samp$flow[i,j]) / max(rollmean(hydroSamp, k=CriticalDur))
      scaledHydrograph = hydroSamp * hydroRatio
      scaledHydrograph[is.na(scaledHydrograph)] = 0
      hydroInput = data.frame(time=1:length(scaledHydrograph), flow_cfs=scaledHydrograph)

      tmpResults = mod_puls_routing(resmodel_df = ResModel, inflow_df = hydroInput, initial_elev = InitStages[cnt], full_results = FALSE)
      peakStage[i,j] = tmpResults[1]
      peakFlow[i,j]   = tmpResults[2]
    }
  }

  # POST PROCESS
  Sbins = Q.samp$n.bins
  peakStages = seq(from=min(peakStage), to=max(peakStage), length.out=Sbins)

  # calculate weighted aep for peakStages
  aepStages = matrix(0,nrow=Sbins, ncol=ncol(peakStage))

  for(m in 1:ncol(peakStage)){
    tmpStage = matrix((peakStage[,m]), nrow=Mevents, ncol=Nbins)

    for (i in 1:Sbins){
      for (j in 1:Nbins){
        n = 0
        for (k in 1:Mevents){
          if (tmpStage[k, j] > peakStages[i]){
            n = n + 1
          }
        }
        aepStages[i,m] = aepStages[i] + n/Mevents*Weights[j]
      }
    }
  }


  rp.lab = c(0.99, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 1E-3, 1E-4, 1E-5, 1E-6, 1E-7, 1E-8, 1E-9)
  rp.tic = qnorm(1 - rp.lab)
  z = qnorm(1-aepStages)
  plot(z, peakStages,type = "l", xlab = "AEP", ylab = "Stage (ft)", main = "John Martin Dam Peak Stage Frequency", axes=F, ylim=c(3840, 3895))
  axis(2);box()
  axis(1, at=rp.tic, labels=rp.lab, las=2)
  abline(h=seq(3840, 3895, by=5), col='lightgrey')
  abline(v=qnorm(1 - c(0.5, 0.1, 0.01, 0.001, 1E-4, 1E-5, 1E-6, 1E-7)), col='lightgrey')
  abline(h=c(3871.8, 3877.3, 3881.8), lwd=2, lty=c(2:4))
  #axis(1, at=z[seq(1, length(z), by=10)], labels=round(aepStages[seq(1, length(z), by=10)], digits=8), las=2)

}


###############################################################
### supporting functions ######################################
###############################################################
flowFrequencySampler <- function(bestfit, ExpectedOnly=TRUE){
  if(ExpectedOnly){
    ords = stratifiedSampler(Nbins=20, Mevents=ceiling(nrow(BestFitParams)/20))
    Q = numeric(nrow(BestFitParams))
    if(LP3){
      for(i in 1:nrow(BestFitParams)){
        Q[i] = 10^qp3(pnorm(ords$normOrd[i]), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
      }
    }else{
      for(i in 1:nrow(BestFitParams)){
        Q[i] = qgev(pnorm(ords$normOrd[i]), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
      }
    }
    Q = matrix(sort(Q), ncol=1)
    #Q = matrix(Q, nrow=ords$Mevents, ncol=ords$Nbins)#ncol=1)
  }else{
    ords = stratifiedSampler()
    Q = matrix(NA,nrow=length(ords$normOrd), ncol=nrow(BestFitParams))

    if(LP3){
      for(i in 1:nrow(BestFitParams)){
        # for(j in 1:length(ords$normOrd)){
        #   Q[j,i] = 10^qp3(pnorm(ords$normOrd[j]), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
        # }
        Q[,i] = qp3(pnorm(ords$normOrd), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
      }
    }else{
      for(i in 1:nrow(BestFitParams)){
        # for(j in 1:length(ords$normOrd)){
        #  Q[j,i] = qgev(pnorm(ords$normOrd[j]), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
        # }
        Q[,i] = qgev(pnorm(ords$normOrd), BestFitParams[i,1], BestFitParams[i,2], BestFitParams[i,3])
      }
    }
  }

  Nbins = ords$Nbins
  Mevents = ords$Mevents
  Weights = ords$Weights

  return(list(flow=Q,
              n.bins=Nbins,
              events.per.bin=Mevents,
              weights=Weights))
}

stratifiedSampler <- function(minAEP = 1E-8, maxAEP = 0.99, Nbins=NULL, Mevents=NULL){
  if(is.null(Nbins)){
    Nbins = 20
  }
  if(is.null(Mevents)){
    Mevents = 50
  }

  maxZ = qnorm(1-minAEP)
  minZ = qnorm(1-maxAEP)
  zlower = numeric(Nbins)
  zupper = numeric(Nbins)
  weights = numeric(Nbins)

  # create stratified bins
  delta = (maxZ - minZ)/Nbins
  zlower[1] = minZ
  zupper[1] = zlower[1] + delta
  weights[1] = pnorm(zupper[1])
  for (i in 2:Nbins){
    zlower[i] = zupper[i-1]
    zupper[i] = zlower[i] + delta
    weights[i] = pnorm(zupper[i]) -  pnorm(zlower[i])
  }
  weights[Nbins] = 1 -  pnorm(zlower[Nbins])

  z = seq(from=min(zlower), to=max(zupper), length.out=Nbins*Mevents)

  return(list(normOrd = z, Weights = weights, Zlower=zlower, Zupper=zupper, Nbins=Nbins, Mevents=Mevents))
}


# Pearson Type III - Inverse CDF
qp3 = function(p, mu, sigma, gamma)
{
  # convert to parameters
  xi = mu-2*sigma/gamma #location
  beta = 0.5*sigma*gamma #scale
  alpha = 4/gamma^2 #shape
  # get min and max support
  min = -.Machine$double.xmax
  if(beta > 0) min = xi
  max = .Machine$double.xmax
  if(beta < 0) max = xi
  # check support
  if (any(p == 0)) break# return(min)
  if (any(p == 1)) break# return(max)
  # check if normal
  if (abs(gamma) < 1E-3)
  {
    return(qnorm(p=p, mean=mu, sd=sigma))
  }
  if (beta > 0)
  {
    return(xi + qgamma(p=p, shape=alpha, scale=abs(beta)))
  }
  else
  {
    return(xi - qgamma(p=1-p, shape=alpha, scale=abs(beta)))
  }
}



#' Modified Puls Reservoir Routing
#'
#' Performs Modified Puls (level pool) routing of inflow hydrograph given
#' a defined reservoir geometry (Stage (ft), Storage (ac-ft), Discharge (cfs))
#'
#' @param resmodel_df Data frame with three columns: elevation/stage (ft),
#'   storage (acre-feet), and discharge (cfs). Must be in that order.
#' @param inflow_df Data frame with two columns: time (hours - for now) and
#'   inflow (cfs). Must be in that order.
#' @param initial_elev Starting water surface elevation in feet.
#' @param full_results Logical. If `FALSE` (default), returns only peak
#'   stage and discharge. If `TRUE`, returns the complete routing result.
#'
#' @return If `full_results = FALSE`, a named numeric vector with
#'   `peak_stage_ft` and `peak_discharge_cfs`.
#'
#'   If `full_results = TRUE`,
#'   a data frame with columns: `time_hr`, `inflow_cfs`, `elevation_ft`,
#'   `storage_acft`, and `outflow_cfs`.
#'
#' @export
#'
#' @examples
#' # Peak values only
#' mod_puls_routing(resmodel, inflow, initial_elev = 5565)
#'
#' # Full routing table
#' mod_puls_routing(resmodel, inflow, initial_elev = 5565, full_results = TRUE)
#' @references
#' Chow, V.T. (1959). Open-Channel Hydraulics. McGraw-Hill.
mod_puls_routing <- function(resmodel_df,inflow_df, initial_elev, full_results = FALSE){
  # ============================================================================
  # CONVERT TO DF (TIBBLE HANDLING)
  # ============================================================================
  resmodel_df <- as.data.frame(resmodel_df)
  inflow_df <- as.data.frame(inflow_df)

  # ============================================================================
  # ENUSRE NUMERIC DATA (NOT INTERGER)
  # ============================================================================
  resmodel_df[] <- lapply(resmodel_df, as.numeric)
  inflow_df[] <- lapply(inflow_df, as.numeric)

  # ============================================================================
  # CONSTANTS
  # ============================================================================
  # Time step in seconds
  dt <- 3600L

  # Ac-ft to cubic feet
  SQFT_PER_ACRE <- 43560L

  # Starting elevation in feet
  INITIAL_ELEV <- initial_elev

  # =============================================================================
  # PREPARE RESERVOIR MODEL - STAGE, STOR, DISCHARGE
  # =============================================================================
  # Convert storage to cubic feet and calculate storage indicator lookup
  res_elev <- resmodel_df[,1]
  res_stor_cuft <- resmodel_df[,2] * SQFT_PER_ACRE
  res_outflow <- resmodel_df[,3]

  # Storage indicator
  res_SI <- (2 * res_stor_cuft / dt) + res_outflow

  # =============================================================================
  # CREATE INTERPOLATION FUNCTIONS
  # =============================================================================
  # approxfun() creates a function that can be called repeatedly.
  # rule 2: extrapolate using the nearest value if outside range

  # Initiation (t1): starting elevation -> starting storage, starting outflow
  interp_elev_to_stor <- approxfun(res_elev, res_stor_cuft, rule = 2)
  interp_elev_to_outflow <- approxfun(res_elev, res_outflow, rule = 2)

  # Routing (t2 - tINF): storage indicator -> outflow, storage indicator -> storage
  interp_SI_to_outflow <- approxfun(res_SI, res_outflow, rule = 2)
  interp_SI_to_stor <- approxfun(res_SI, res_stor_cuft, rule = 2)

  # Elevation from storage (output)
  interp_stor_to_elev <- approxfun(res_stor_cuft, res_elev, rule = 2)

  # =============================================================================
  # PREPARE INFLOW DATA
  # =============================================================================
  # Length of timeseries
  n <- nrow(inflow_df)

  # Extract as vector (faster than data frame column access in loop)
  time_vec <-inflow_df[,1]
  I_vec <- inflow_df[,2]

  # Calculate I[t-1] + I[t] for all timesteps at once
  # For t=1, this will be NA (no previous value)
  I_sum <- c(NA_real_, I_vec[-n] + I_vec[-1])

  # =============================================================================
  # PRE-ALLOCATE OUTPUT VECTORS
  # =============================================================================
  O_vec <- numeric(n)      # Outflow at each timestep
  S_vec <- numeric(n)      # Storage at each timestep
  elev_vec <- numeric(n)   # Elevation at each timestep

  # =============================================================================
  # INITIALIZE FIRST TIMESTEP
  # =============================================================================

  # Set initial elevation
  elev_vec[1] <- INITIAL_ELEV

  # Look up initial storage and outflow from elevation
  S_vec[1] <- interp_elev_to_stor(INITIAL_ELEV)
  O_vec[1] <- interp_elev_to_outflow(INITIAL_ELEV)

  # =============================================================================
  # ROUTING LOOP
  # =============================================================================
  # Store dt_factor to avoid repeated division (multiplication is faster)
  two_over_dt <- 2 / dt

  # Max Stage & Max Discharge
  peak_stage <- 0
  peak_dis <- 0

  # Routing
  for (t in 2:n) {
    # Calculate carry-forward term: (2*S[t-1]/dt) - O[t-1]
    # Then add inflow sum to get new storage indicator
    SI_new <- (S_vec[t - 1] * two_over_dt) - O_vec[t - 1] + I_sum[t]

    # Look up outflow and storage from storage indicator
    O_vec[t] <- interp_SI_to_outflow(SI_new)
    S_vec[t] <- interp_SI_to_stor(SI_new)

    # Look up elevation from storage
    elev_vec[t] <- interp_stor_to_elev(S_vec[t])

    # Record Peak Stage & Discharge
    if (elev_vec[t] > peak_stage) peak_stage <- elev_vec[t]
    if (O_vec[t] > peak_dis) peak_dis <- O_vec[t]
  }

  # =============================================================================
  # FULL ROUTING RESULTS
  # =============================================================================
  # If full routing results desired
  if (full_results) {
    # Return the full data frame
    results <- data.frame(
      time_hr = time_vec,
      inflow_cfs = I_vec,
      elevation_ft = elev_vec,
      storage_acft = S_vec/SQFT_PER_ACRE,
      outflow_cfs = O_vec
    )
    return(results)
  } else {
    # Return just the peak values
    return(c(
      peak_stage_ft = peak_stage,
      peak_discharge_cfs = peak_dis
    ))
  }
}
