### Distribution Functions ###########################################################

# Pearson Type III - PDF
dp3 = function(x, mu, sigma, gamma)
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
  if (x < min || x > max) return(0)
  # check if normal
  if (abs(gamma) < 1E-3)
  {
    return(dnorm(x=x, mean=mu, sd=sigma))
  }
  if (beta > 0)
  {
    return(dgamma(x=x-xi, shape=alpha, scale=abs(beta)))
  }
  else
  {
    return(dgamma(x=xi-x, shape=alpha, scale=abs(beta)))
  }
}

# Pearson Type III - CDF
pp3 = function(x, mu, sigma, gamma)
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
  if (x <= min) return(0)
  if (x >= max) return(1)
  # check if normal
  if (abs(gamma) < 1E-3)
  {
    return(pnorm(q=x, mean=mu, sd=sigma))
  }
  if (beta > 0)
  {
    return(pgamma(q=x-xi, shape=alpha, scale=abs(beta)))
  }
  else
  {
    return(1-pgamma(q=xi-x, shape=alpha, scale=abs(beta)))
  }
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
  if (p == 0) return(min)
  if (p == 1) return(max)
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

### Rating Curve ####

flows = c(3.3E-43,	1.15791364,	7.35229332,	21.67701688,	46.6841526,	84.64400055,	137.6404776,	207.6208397,	296.4258918,	405.8101092,	537.4559021,	692.9841958,	873.9625556,	1081.911597,	1318.310157,	1584.599546,	1882.18709,	2212.449128,	2576.733577,	2976.362145,	3412.632257,	3886.81875,	4400.175366,	4953.936086,	5549.31632,	6187.513976,	6869.710428,	7597.071391,	8370.747723,	9191.876152,	10061.57995,	10980.96954,	11951.14307,	12973.18694,	14048.17629,	15177.17545,	16361.23836,	17601.40897,	18898.7216,	20254.2013,	21668.86414,	23273.91799,	24878.97183,	26484.02567,	28089.07952,	29694.13336,	31299.18721,	32904.24105,	34509.29489,	36114.34874,	37719.40258,	39324.45643,	40929.51027,	42534.56411,	44139.61796,	45744.6718,	47349.72565,	48954.77949,	50559.83334,	52164.88718,	53769.94102,	55374.99487,	56980.04871,	58585.10256,	60190.1564,	61795.21024,	63400.26409,	65005.31793,	66610.37178,	68215.42562,	69820.47946,	161658.6175,	168135.6761,	174712.7095,	181388.5675,	188162.1432,	195032.37,	201998.2191,	209058.6969,	216212.8431,	223459.7288,	230798.4543,	238228.1477,	245747.9634,	253357.0804,	261054.7014,	268840.0513,	276712.3763,	284670.9424,	292715.0352,	300843.9582,	309057.0324,	317353.5956,	325733.0012,	334194.618,	342737.8293,	351362.0323,	360066.6376,	368851.0687,	377714.7613,	386657.1629)
stages = c(0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10,	11,	12,	13,	14,	15,	16,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37,	38,	39,	40,	41,	42,	43,	44,	45,	46,	47,	48,	49,	50,	51,	52,	53,	54,	55,	56,	57,	58,	59,	60,	61,	62,	63,	64,	65,	66,	67,	68,	69,	70,	71,	72,	73,	74,	75,	76,	77,	78,	79,	80,	81,	82,	83,	84,	85,	86,	87,	88,	89,	90,	91,	92,	93,	94,	95,	96,	97,	98,	99,	100)
logflows = log10(flows)


### Crude Monte Carlo ####
R = 1000
peakFlows = numeric(R)
peakStages = numeric(R)
pp = numeric(R)
z = numeric(R)
set.seed(12345)
for (i in 1:R)
{
  peakFlows[i] = qp3(runif(1), 4.232, 0.153, 0.401)
  peakStages[i] = approx(x=logflows, y=stages, xout=peakFlows[i])$y
  pp[i] = i / (R+1)
  z[i] = qnorm(1-pp[i])
}
# Sort results in descending order
peakStages = sort(peakStages, decreasing = TRUE)
# create stage-frequency curve plot
plot(z, peakStages, xlab = "Z-Variate", ylab = "Stage (ft)", main = "River Stage-Frequency")


### Stratified Sampling ####

minAEP = 1E-8
maxAEP = 0.99
maxZ = qnorm(1-minAEP)
minZ = qnorm(1-maxAEP)

# 1000 events
Nbins = 20
Mevents = 50
stratResults = matrix(nrow = Mevents, ncol = Nbins)

zlower = numeric(Nbins)
zupper = numeric(Nbins)
weights = numeric(Nbins)

# stratResults = matrix(nrow = Mevents, ncol = Nbins)

# create stratified bins
delta = (maxZ - minZ)/Nbins
zlower[1] = minZ
zupper[1] = zlower[1] + delta
weights[1] = pnorm(zupper[1])
for (i in 2:Nbins)
{
  zlower[i] = zupper[i-1]
  zupper[i] = zlower[i] + delta
  weights[i] = pnorm(zupper[i]) -  pnorm(zlower[i])
}
weights[Nbins] = 1 -  pnorm(zlower[Nbins])
# 1 -  pnorm(zlower[Nbins]) == pnorm(zupper[Nbins])

# Perform simulation
for (i in 1:Nbins){
  for (j in 1:Mevents){
    z = zlower[i]+runif(1)*(zupper[i]-zlower[i])
    Q = qp3(pnorm(z), 4.232, 0.153, 0.401)
    stratResults[j,i] = approx(x=logflows, y=stages, xout=Q)$y
  }
}

# Post-process
minS = min(stratResults)
maxS = max(stratResults)
Sbins = 50
peakStages = numeric(Sbins)
aepStages = numeric(Sbins)
peakStages[1] = minS
for (i in 2:Sbins){
  peakStages[i] = peakStages[i-1] + (maxS - minS)/(Sbins-1)
}

for (i in 1:Sbins){
  for (j in 1:Nbins){
    n = 0
    for (k in 1:Mevents){
      if (stratResults[k, j] > peakStages[i]){
        n = n + 1
      }
    }
    aepStages[i] = aepStages[i] + n/Mevents*weights[j]
  }
}
z = qnorm(1-aepStages)
plot(z, peakStages,type = "l", xlab = "Z-Variate", ylab = "Stage (ft)", main = "River Stage-Frequency")
