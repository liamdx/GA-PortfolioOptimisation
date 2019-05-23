install.packages("quantmod")
install.packages("GA")
install.packages("SpatialEpi")
install.packages("ggplot2")
library(GA)
library(quantmod)
library(SpatialEpi)
library(ggplot2)

# GA Params
popSize<-2000
generations<-1000
mutation<-0.1
crossover<-0.8
# 2.13 19:50 removed aker from set

# Load data
myStocks<-c("QCOM", "INTC", "NVDA", "GOOG", 
            "TTWO", "JOUT", "CMG", "AKER",
            "KBSF", "COST", "SBUX", "CROX",
            "URBN", "MGLN", "VRTX")

getSymbols(myStocks, src="yahoo", from="2014-01-01", to="2018-01-01")
# Load stocks from yahoo into dataframe
myRetData <- data.frame(as.xts(merge(weeklyReturn(QCOM), weeklyReturn(INTC), weeklyReturn(NVDA), weeklyReturn(GOOG),
                                     weeklyReturn(TTWO), weeklyReturn(JOUT), weeklyReturn(CMG), weeklyReturn(AKER),
                                     weeklyReturn(KBSF), weeklyReturn(COST), weeklyReturn(SBUX), weeklyReturn(CROX),
                                     weeklyReturn(URBN), weeklyReturn(MGLN), weeklyReturn(VRTX))))



names(myRetData) <- paste(myStocks)
myRetData<-na.omit(myRetData)
dimensions<-as.integer(length(myRetData))

matplot(myRetData * 100, type = c("b"),pch=-5,col = 1:dimensions, ylim=c(-30,30), lwd = 1.5, cex=0.1, ylab="Returns * 100", xlab="Time") #plot
title("Portfolio Weekly Returns 2014-2018")
legend("bottomleft", inset=0.01, legend = colnames(myRetData)[1:dimensions], col = 1:dimensions, lty = 1:4)

for(col in myRetData)
  plot(col * 100)

# get mean returns in advance
meanReturns<-list()
for(i in 1:dimensions){
  meanReturns[i] <- mean(myRetData[myStocks[i]][[1]])
}

# get base covariance matrix
baseCovMatrix<-matrix(data=NA, nrow=dimensions, ncol=dimensions)
for(i in 1:dimensions){
  # j = column
  for(j in 1:dimensions){
    # take covariance of two assets, multiply this value by asset1's weight and asset2's weight
    baseCovMatrix[j,i]=cov(myRetData[myStocks[j]], myRetData[myStocks[i]])
  }
}

# create a vector of objective weights
objectiveWeight<-0.1
objectiveWeights<-seq(0.1,0.9, 0.1)

getRisk<-function(x){
  
  COV_MATRIX<-matrix(data=NA, nrow=dimensions, ncol=dimensions)
  
  # Create covariance matrix
  # i = row
  for(i in 1:dimensions){
    # j = column
    for(j in 1:dimensions){
      # take covariance of two assets, multiply this value by asset1's weight and asset2's weight
      COV_MATRIX[j,i]=baseCovMatrix[j,i] * x[j] * x[i]
    }
  }
  
  final_return<-sum(COV_MATRIX)
  return(final_return)
  
}

getReturns<-function(x){
  # Find total return
  returns<-0
  
  for(i in 1:dimensions){
    returns <- returns + meanReturns[[i]] * x[i]
  }
  return(returns)
  
}



fitness<-function(x){
  
  # normalize vector such that is sums to 1.
  weights<-normalize(x)
  
  # risk = sum(COV_MATRIX)
  risk = getRisk(weights)
  # returns = sum of all weekly returns
  returns<-getReturns(weights)
  
  # Single objective forumla
  # f(x) = w . f1(x) + (1 - w).f2(x)
  final<-objectiveWeight %*% returns + (1 - objectiveWeight) %*% (1-risk)
  return(final)
  
}

# Collect the re-normalized best individuals
bestSolutions<-list(NA)

for(i in 1:length(objectiveWeights))
{
  print(cat("Running GA with Objective Weight Splitting: ", objectiveWeights[i]))
  objectiveWeight<-objectiveWeights[i]
  # run the GA with this weighting
  GA<-ga(type='real-valued', fitness=fitness, lower=rep(0,dimensions), upper=rep(1,dimensions),popSize = popSize, maxiter=generations,keepBest=TRUE)
  # get the best solution from this GA run
  solutions<-GA@bestSol
  # renormalize the solution and add to best solutions 
  bestSolutions[[i]]<-normalize(solutions[generations][[1]])
}

# Get the risk and return values for each GA run
bestRisks<-list()
bestReturns<-list()

for(i in 1:length(objectiveWeights)){
  print(bestSolutions[[i]])
  bestRisks[i]<-getRisk(bestSolutions[[i]]) * 100
  bestReturns[i]<-getReturns(bestSolutions[[i]]) * 100
}

# Get random results
randomRisks<-list()
randomReturns<-list()

for(i in 1:500){
  randomWeightVector<-normalize(runif(dimensions, 0.0, 1.0))
  randomRisks[i]<-getRisk(randomWeightVector) * 100
  randomReturns[i]<-getReturns(randomWeightVector) * 100
}


# Get even weighted results
evenWeightVector<-normalize(rep(1,15))
evenRisk<-getRisk(evenWeightVector) * 100
evenReturn<-getReturns(evenWeightVector) * 100

bestGAResults<-do.call(rbind, Map(data.frame, RISK=bestRisks, RETURN=bestReturns))
randomResults<-do.call(rbind, Map(data.frame, RISK=randomRisks, RETURN=randomReturns))
evenResults<-do.call(rbind, Map(data.frame, RISK=evenRisk, RETURN=evenReturn))

ggplot(bestGAResults, aes(x=RISK, y=RETURN)) + geom_point(data = bestGAResults,  size=2.5, color='red') + geom_point(data = randomResults,  size=1.5, color='green') + geom_point(data = evenResults,  size=3, color='blue') + theme_light() + scale_x_reverse(lim=c(0.4, 0.0)) + scale_y_continuous(lim=c(0,1.3)) 


# Future results
getSymbols(myStocks, src="yahoo", from="2018-01-01", to="2019-01-01")
# Load 'future' stocks from yahoo into dataframe
myRetData <- data.frame(as.xts(merge(weeklyReturn(QCOM), weeklyReturn(INTC), weeklyReturn(NVDA), weeklyReturn(GOOG),
                                     weeklyReturn(TTWO), weeklyReturn(JOUT), weeklyReturn(CMG), weeklyReturn(AKER),
                                     weeklyReturn(KBSF), weeklyReturn(COST), weeklyReturn(SBUX), weeklyReturn(CROX),
                                     weeklyReturn(URBN), weeklyReturn(MGLN), weeklyReturn(VRTX))))
names(myRetData) <- paste(myStocks)

matplot(myRetData * 100, type = c("b"),pch=-5,col = 1:dimensions, ylim=c(-30,30), lwd = 1.5, cex=0.1, ylab="Returns * 100", xlab="Time") #plot
title("Portfolio Weekly Returns 2018-2019")
legend("bottomleft", inset=0.01, legend = colnames(myRetData)[1:dimensions], col = 1:dimensions, lty = 1:4)


# get mean returns in advance
meanReturns<-list()
for(i in 1:dimensions){
  meanReturns[i] <- mean(myRetData[myStocks[i]][[1]])
}

# get base covariance matrix
baseCovMatrix<-matrix(data=NA, nrow=dimensions, ncol=dimensions)

for(i in 1:dimensions){
  # j = column
  for(j in 1:dimensions){
    # take covariance of two assets, multiply this value by asset1's weight and asset2's weight
    baseCovMatrix[j,i]=cov(myRetData[myStocks[j]], myRetData[myStocks[i]])
  }
}

# Get the risk and return values for each GA run
bestRisks<-list()
bestReturns<-list()

for(i in 1:length(objectiveWeights)){
  print(bestSolutions[[i]])
  bestRisks[i]<-getRisk(bestSolutions[[i]]) * 100
  bestReturns[i]<-getReturns(bestSolutions[[i]]) * 100
}

# Get random results
randomRisks<-list()
randomReturns<-list()

for(i in 1:100){
  randomWeightVector<-normalize(runif(dimensions, 0.0, 1.0))
  randomRisks[i]<-getRisk(randomWeightVector) * 100
  randomReturns[i]<-getReturns(randomWeightVector) * 100
}


# Get even weighted results
evenWeightVector<-normalize(rep(1,15))
evenRisk<-getRisk(evenWeightVector) * 100
evenReturn<-getReturns(evenWeightVector) * 100

bestGAResults<-do.call(rbind, Map(data.frame, RISK=bestRisks, RETURN=bestReturns))
randomResults<-do.call(rbind, Map(data.frame, RISK=randomRisks, RETURN=randomReturns))
evenResults<-do.call(rbind, Map(data.frame, RISK=evenRisk, RETURN=evenReturn))

ggplot(bestGAResults, aes(x=RISK, y=RETURN)) + geom_point(data = bestGAResults,  size=2.5, color='red') + geom_point(data = randomResults,  size=1.5, color='green') + geom_point(data = evenResults,  size=3, color='blue') + theme_light() + scale_x_reverse(lim=c(0.4, 0.0)) + scale_y_continuous(lim=c(-1, 1)) 
