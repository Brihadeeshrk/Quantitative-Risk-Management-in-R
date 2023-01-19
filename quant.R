library(qrmdata)
install.packages('QRM')
library(qrmtools)
library(xts)
library(QRM)
data("FTSE")
data("SP500")
data("SMI")
data("USD_GBP")
data("CHF_GBP")
X.FTSE <- FTSE["2008/2009"]
X.GSPC <- SP500["2008/2009"]
X.SSMI <- SMI["2008/2009"]
USD.GBP <- USD_GBP["2008/2009"]
CHF.GBP <- CHF_GBP["2008/2009"]
riskfactors <- merge(X.FTSE,  X.GSPC, X.SSMI, USD.GBP, CHF.GBP , all = TRUE)
riskfactors <- na.omit(riskfactors)
# Plot the risk-factor data
plot.zoo(riskfactors)

# Calculate the log-returns, assign to returns, and plot
returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns)

# Make a Q-Q plot against normal for the 5th return series and add a reference line
qqnorm(returns[, 5])
qqline(returns[, 5])

# Make a picture of the sample acfs for returns and their absolute values
acf(returns)

acf(abs(returns))

lossop <- function (xseries, wts = c(0.3, 0.4, 0.3)){
  if (is.xts(xseries)) 
    x <- coredata(xseries)
  else if (is.matrix(xseries)) 
    x <- xseries
  else x <- matrix(xseries, nrow = 1)
  ll <- apply(x, 1, function(x, wts) {
    1 - (wts[1] * exp(x[1]) + wts[2] * exp(x[2] + x[4]) + 
           wts[3] * exp(x[3] + x[5]))
  }, wts = wts)
  if (is.xts(xseries)) 
    ll <- xts(ll, time(xseries))
  ll
}
# Calculate the loss from a log-return of -0.1 for all risk factors
lossop(rep(-0.1, 5))

# Apply lossop() to returns and plot hslosses
hslosses <- lossop(returns)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)

# Plot the sample acf of hslosses and their absolute values
acf(hslosses)

acf(abs(hslosses))

quantile(hslosses, 0.99)

# Estimate the 99% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.99)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99% quantile of a normal distribution
qnorm(0.99, mean = mu, sd = sigma)

# Compute the 99% ES of a normal distribution
ESnorm(0.99, mu = mu, sd = sigma)

lossop <- function (xseries, r = 0.01, K = 100, T = 1, sigma = 0.2, S = 100){
  if (is.xts(xseries)) 
    x <- coredata(xseries)
  else if (is.matrix(xseries)) 
    x <- xseries
  else x <- matrix(xseries, nrow = 1)
  ll <- apply(x, 1, function(x, r, K, T, sigma, S) {
    deltat <- 1/250
    V_t0 <- Black_Scholes(0, S, r, sigma, K, T, "call")
    V_t1 = Black_Scholes(deltat, exp(log(S) + x[1]), r, exp(log(sigma) + 
                                                              x[2]), K, T, "call")
    -(V_t1 - V_t0)/V_t0
  }, r = r, K = K, T = T, sigma = sigma, S = S)
  if (is.xts(xseries)) 
    ll <- xts(ll, time(xseries))
  ll
}

quantile(hslosses, 0.999)
return_w <- apply.weekly(returns, colSums)
hslosses <- lossop(return_w, S = 120, sigma = 0.25)
quantile(hslosses, 0.99)
