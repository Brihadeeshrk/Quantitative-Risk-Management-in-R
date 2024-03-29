library(xts) 
library(qrmdata) 
library(qrmtools)

## standardization function
standardize <- function(data)
  apply(data,2,function(v){(v-mean(v))/sd(v)})


# Data preparation
data(SP500_const)
SP500.const <- SP500_const['1995-01-01/',]
dim(SP500.const)
SP500.const <- SP500.const[, colSums(is.na(SP500.const)) <= 0.1 * nrow(SP500.const)] # omit columns with more than 10% NA
SP500.const <- na.fill(SP500.const, fill="extend") # fill the remaining NAs
dim(SP500.const)

## plotting all data
par(ask=TRUE)
nplots <- ceiling(dim(SP500.const)[2]/20)
for (i in 1:nplots){
  r1 <- 20*(i-1)+1
  r2 <- min(20*i,dim(SP500.const)[2])
  plot.zoo(SP500.const[,r1:r2], xlab="Time t", main="SP500 Constituents")
}
par(ask=FALSE)

## Removing certain stocks that were affected badly by the crisis
## AIG, C, ETFC (badly affected by financial crisis)
## T, CTL, FTR, VZ (only 4 telecommunications companies so this sector is thin)
ignore <- list("AIG","T","CTL","C","ETFC","FTR","VZ")
which(names(SP500.const) %in% ignore)
SP500.const <- SP500.const[,-which(names(SP500.const) %in% ignore)]
dim(SP500.const)

## Build and plot log-returns
X.const <- diff(log(SP500.const))[-1,] # compute -log-returns

## using monthly data as basis for analysis
X <- apply.monthly(X.const, FUN=colSums) # 252 by 367 matrix
plot.zoo(X[,1:20], type="h", xlab="Time t", main="Monthly risk-factor changes (log-returns) of SP500 constituents")

summary(SP500_const_info)

firms <- as.character(SP500_const_info$Ticker)
## make two firm names consistent
firms[firms=="BRK-B"] <- "BRK.B"
firms[firms=="BF-B"]  <- "BF.B"
sectors <- as.character(SP500_const_info$Sector)

## Construct a X.sectors and tabulate levels
X.sectors <- sectors[which(firms %in% colnames(X))]
X.sectors <- as.factor(X.sectors)
table(X.sectors)
levels(X.sectors) <- c("Con-Disc.","Con-Stap.","Energy",
                       "Financials","Health","Industrials","IT",
                       "Materials","Utilities")

X <- xts(standardize(X),time(X))


# Model fitting

## Fit a cross-sectional regression model X_t = B*F_t + eps
## A regression is fitted at each time point
## No intercept is required (-1)

mod <- lm(t(X) ~ X.sectors -1)

## Factors can be constructed from coefficient matrix
coef.mat <- t(coef(mod))
dimnames(coef.mat)[[2]] <- levels(X.sectors)
F <- xts(coef.mat,time(X))
plot.zoo(F,type="h")

## Calculate error variances for each stock
eps <- t(residuals(mod))
eps.variances <- diag(var(eps))
## Displaying from smallest -> largest
sort(eps.variances)

## Use generalized least squares to obtain better estimates
mod2 <- lm(t(X) ~ X.sectors -1, weights = 1/eps.variances)
coef.mat <- t(coef(mod2))
dimnames(coef.mat)[[2]] <- levels(X.sectors)
## This time we standardize the factors
F <- xts(standardize(coef.mat),time(X))
var(F)
plot.zoo(F,type="h")
help("glm")

## Construct the systematic terms for each sector
A <- model.matrix(mod2)
F.tilde <- t(A %*% t(F))
# Check for unit variances
apply(F.tilde,2,var)

## Estimate beta_i terms by regression
n.stocks <- dim(X)[2]
beta <- rep(NA,n.stocks)
names(beta) <- names(X)
for (i in 1:n.stocks){
  tmp <- lm(X[,i] ~ F.tilde[,i] - 1)
  beta[i] <- coef(tmp)^2
}
tmp
summary(beta)

## plotting
npa <- par(mfrow=c(3,5),cex=0.5,mar=c(3,3,2,1),mgp=c(2,1,0))
## most systematic
beta.top <- order(-beta)[1:15]
for (i in beta.top){
  plot(F.tilde[,i],X[,i],xlab="F.tilde",ylab="X",main=names(X)[i])
  abline(0,sqrt(beta[i]))
}

## least systematic
beta.bottom <- order(beta)[1:15]
for (i in beta.bottom){
  plot(F.tilde[,i],X[,i],xlab="F.tilde",ylab="X",main=names(X)[i])
  abline(0,sqrt(beta[i]))
}

par(npa)
