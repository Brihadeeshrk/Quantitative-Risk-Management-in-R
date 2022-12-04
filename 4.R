djx <- diff(log(dj))[-1,]["2008/2009"]
sigma <- sd(djx)
mu <- mean(djx)

# Make a sequence of 100 x-values going from -4*sigma to 4*sigma
xvals <- seq(from = -4*sigma, to = 4*sigma, length.out = 100)

# Compute the density of a N(mu, sigma^2) distribution at xvals
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot ndens against xvals
plot(xvals, ndens, type = "l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(0.99, mean = mu, sd = sigma)
VaR99

ES99 <- ESnorm(0.99, mu = mu, sd = sigma)
ES99

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")

abline(v = ES99, col = "green")

data("USD_GBP")
data("CHF_GBP")
X.FTSE <- FTSE["2000/2012"]
X.GSPC <- SP500["2000/2012"]
X.SSMI <- SMI["2000/2012"]
USD.GBP <- USD_GBP["2000/2012"]
CHF.GBP <- CHF_GBP["2000/2012"]
riskfactors <- merge(X.FTSE,  X.GSPC, X.SSMI, USD.GBP, CHF.GBP , all = TRUE)
riskfactors <- na.omit(riskfactors)

# Plot the risk-factor data
plot.zoo(riskfactors)

# Calculate the log-returns, assign to returns, and plot
returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns)

# Use apply() to carry out the Jarque-Bera test for all 5 series
apply(returns, 2, jarque.test)

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

# Estimate the 99th sample percentile of the distribution of hslosses
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

# Set the interest rate r to be 0.01, the volatility sigma to be 0.2 and the strike K to be 100
r <- 0.01
sigma <- 0.2
K <- 100
library(qrmtools)

# Look at the arguments of the Black_Scholes function
args(Black_Scholes)

# Price a European call option that matures in one year if the current stock price is 80
Black_Scholes(0, 80, r, sigma, K, 1, "call")

# Price a European call option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1, "call")

# Price a European put option that matures in one year if the current stock price is 80
Black_Scholes(0, 80, r, sigma, K, 1,"put")

# Price a European put option that matures in one year if the current stock price is 120
Black_Scholes(0, 120, r, sigma, K, 1,"put")

data("VIX")
X.GSPC <- SP500["1990/2010"]
X.VIX <- VIX["1990/2010"]
riskfactors <- merge(X.GSPC, X.VIX, all = TRUE)
riskfactors <- na.omit(riskfactors)
returns <- diff(log(riskfactors))[-1, ]

names(returns)

# Plot the risk factors and the log-returns
plot.zoo(riskfactors)

plot.zoo(returns)

# Make a scatterplot of the two return series
plot(as.matrix(returns))

# Apply the Jarque-Bera test to the returns and make a Q-Q plot of the volatility log-returns
apply(returns, 2, jarque.test)

qqnorm(returns[, 2])

# Create the sample acf of the returns and absolute returns
acf(returns)

acf(abs(returns))

# Calculate the correlation between the log-returns
cor(returns)

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

# Calculate the first loss
lossop(c(-0.1, -0.1), S = 80, sigma = 0.2)

# Calculate the second loss
lossop(c(-0.1, 0.1), S = 100, sigma = 0.2)

# Create and plot hslosses
hslosses <- lossop(returns, S = 100, sigma = 0.2)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)

# Plot the sample acf of raw data and absolute values in hslosses
acf(hslosses)

acf(abs(hslosses))

# Estimate the 99.5% percentile of the distribution
quantile(hslosses, 0.995)

# Estimate the 99.5% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.995)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99.5% quantile of a normal distribution
qnorm(0.995, mean = mu, sd = sigma)

# Compute the 99.5% ES of a normal distribution
ESnorm(0.995, mu = mu, sd = sigma)

lossop <- function (xseries, r = 0.01, K = 100, T = 1, sigma = 0.2, S = 100){
  if (is.xts(xseries)) 
    x <- coredata(xseries)
  else if (is.matrix(xseries)) 
    x <- xseries
  else x <- matrix(xseries, nrow = 1)
  ll <- apply(x, 1, function(x, r, K, T, sigma, S) {
    deltat <- 5/250
    V_t0 <- Black_Scholes(0, S, r, sigma, K, T, "call")
    V_t1 = Black_Scholes(deltat, exp(log(S) + x[1]), r, exp(log(sigma) + 
                                                              x[2]), K, T, "call")
    -(V_t1 - V_t0)/V_t0
  }, r = r, K = K, T = T, sigma = sigma, S = S)
  if (is.xts(xseries)) 
    ll <- xts(ll, time(xseries))
  ll
}

return_w <- apply.weekly(returns, colSums)
hslosses <- lossop(return_w, S = 120, sigma = 0.25)
quantile(hslosses, 0.995)