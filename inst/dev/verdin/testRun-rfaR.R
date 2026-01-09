setwd("C:/Users/q0rmcapv/OneDrive - US Army Corps of Engineers/Desktop/Projects/Drop-In/RFAr/verdin/")
source("rfaR-verdin-take1.R")

bfParams = read.csv("data/parameter_sets.csv")
lp3 = TRUE
stage.ts = read.csv("data/jmd_por_stage.csv")
seasonal = read.csv("data/seasonality_full_por.csv")
seasonality = seasonal$frequency
hydrographs = read.csv("data/hydrographs.csv")
res.model = read.csv("data/jmd_resmodel_best_est.csv")
critDur = 48
routDur = 240
expected = TRUE

rfaR(BestFitParams=bfParams,
     LP3 = lp3,
     StageTS = stage.ts,
     Seasonality = seasonality,
     Hydrographs = hydrographs,
     ResModel = res.model,
     CriticalDur = critDur,
     RoutingDur = routDur,
     ExpectedOnly = expected)


