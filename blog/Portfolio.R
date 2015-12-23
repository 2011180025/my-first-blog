
setwd("C:/Users/admin/Desktop/Data Analytics/QuantitativeFinance/QFLabs")
getwd()
##baseData <- read.csv("portfolio.csv")
baseData <- read.csv("Kosdaq.csv")
N0 <- ncol(baseData)
Close <- baseData[, 3:N0]
logRet <- log(head(Close, -1) / tail(Close, -1))
Retn <- colMeans(logRet)
Risk <- diag(var(logRet))
RiskReturn <- as.data.frame(t(rbind(Retn,Risk)))

library(ggplot2)

plot1 <- ggplot(data = RiskReturn,aes(x = Risk, y = Retn) )
plot1 <- plot1 + geom_point()
plot1 <- plot1 + xlab("Risk / Variance") + ylab("Daily Returns") + ggtitle("Risk/Returns")
plot1

library(tseries)
w0 <- portfolio.optim(as.matrix(logRet),pm = 0.005,shorts = TRUE,riskless= FALSE)
w0$pw
sum(w0$pw)

library(quadprog)

rOptPort10 <- function(hRets,pRet){
  Dmat <- 2*cov(hRets)
  dvec <- rep(0,ncol(hRets))
  Amat <- cbind(rep(1,ncol(hRets)),colMeans(hRets))
  bvec <- c(1,pRet)
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq =2)
  wP <- result$solution
  varP <- result$value
  retList <- list(wP,varP)
  names(retList) <- c("wP","varP")
  return(retList)
}

# testing out the function with expected return

z <- rOptPort10(logRet,0.005)

z$wP
z$varP
sum(z$wP)

# here we create the Efficient Frontier for a given range of returns

EFMinVar10 <- function(hRets, minRet, maxRet){
  smuP <- seq(minRet,maxRet,length=50)
  svarP <- sapply(smuP,function(x) rOptPort10(hRets,x)$varP)
  EffF <- as.data.frame(cbind(smuP,svarP))  
  minVar <- min(EffF$svarP)
  L <- EffF$svarP == minVar
  minRet <- EffF[L,]$smuP
  minPoint <- as.data.frame(cbind(minRet,minVar))
  minVarwP <- rOptPort10(hRets,minRet)$wP
  rList <-list(EffF,minPoint,minVarwP)
  names(rList) <- c("EFF","minPoint","wP")
  return(rList)
}


z10 <- EFMinVar10(logRet,-0.005,.005)
z10$wp
cminRet <- (z10$minPoint)$minRet
z11 <- rOptPort10(logRet,cminRet)
z11$wP

# 이 부분이 분산이 가장 적은 "효율적 경계"를 생성하는 곳입니다!
EFMinVar10Plot <- function(list1){
  
  plot2 <- ggplot(data = list1$EFF,aes(x = svarP, y = smuP) )
  plot2 <- plot2 + geom_point()
  plot2 <- plot2 + geom_point(data = list1$minPoint, aes(x = minVar,y = minRet),color = "red", size=3)
  plot2 <- plot2 + xlab("Variance") + ylab("Returns") + ggtitle("Efficient Frontier - MinVar")
  plot2
}

EFMinVar10Plot(z10)
cminRet <- (z10$minPoint)$minRet 

z10a <- EFMinVar10(logRet,min(0,cminRet),0.001)
EFMinVar10Plot(z10a)


# This function calculates the Max Sharpe Ratio
# and the Tangency Portfolio weights

EFSharpe10 <- function(hRets, minRet, maxRet,RF){
  smuP <- seq(minRet,maxRet,length=50)
  svarP <- sapply(smuP,function(x) rOptPort10(hRets,x)$varP)
  sharpe <- (smuP-RF)/svarP
  EFF <- as.data.frame(cbind(smuP,svarP,sharpe,RF))
  L <- EFF$sharpe == max(EFF$sharpe)
  maxSharpe <- EFF[L,]
  wTP <- rOptPort10(hRets,maxSharpe$smuP)$wP
  rList <-list(EFF,maxSharpe,wTP)
  names(rList) <- c("EFF","maxSharpe","wTP")
  return(rList)
}

z11 <- EFSharpe10(logRet,min(0,cminRet),0.001,0.0001)
z11$wTP
sum(z11$wTP)
(z11$maxSharpe)$smuP
(z11$maxSharpe)$svarP
maxSharpeRet <- (z11$maxSharpe)$smuP

# 접점 포트폴리오를 생성하는 지점!!

EFSharpe10Plot <- function(list1){
 
  
  plot2 <- ggplot(data = list1$EFF,aes(x = svarP, y = smuP) )
  plot2 <- plot2 + geom_point()
  plot2 <- plot2 + geom_point(data = list1$maxSharpe, aes(x = svarP,y = smuP),colour = "red", pch =24, size=3)
  plot2 <- plot2 + geom_point(data = list1$maxSharpe, aes(x = 0,y = RF),color = "red", pch =24, size=3)
  plot2 <- plot2 + xlab("Variance") + ylab("Returns") + ggtitle("Efficient Frontier - Sharpe")
  plot2 <- plot2 + geom_abline(intercept = (list1$maxSharpe)$RF, slope = (list1$maxSharpe)$sharpe, colour = "red")
  plot2
}

EFSharpe10Plot(z11)