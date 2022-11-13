install.packages('qrmdata')

# Load DJ index
library(qrmdata)
data("DJ")

# Show head() and tail() of DJ index
head(DJ)
tail(DJ)

# Plot DJ index
plot(DJ)

# Extract 2008-2009 and assign to dj0809
dj0809 <- DJ["2008/2009"]

# Plot dj0809
plot(dj0809)

# Load DJ constituents data
data(DJ_const)

# Apply names() and head() to DJ_const
names(DJ_const)
head(DJ_const)

# Checking for NULL values
which(is.na(DJ_const))
# Counting the no: of NULL Values
sum(is.na(DJ_const))

plot(DJ_const)

# Extract AAPL and GS in 2008-09 and assign to stocks
stocks <- DJ_const["2008/2009",c("AAPL","GS")]

library(zoo)
plot.zoo(stocks)

# Load exchange rate data
data(GBP_USD)
data(EUR_USD)

# Plot the two exchange rates
plot(GBP_USD)
plot(EUR_USD)

# Plot a USD_GBP exchange rate
plot(1/GBP_USD)

# Merge the two exchange rates GBP_USD and EUR_USD
fx <- merge(GBP_USD, EUR_USD, all = TRUE)

# Extract 2010-15 data from fx and assign to fx0015
fx0015 <- fx["2010/2015"]

# Plot the exchange rates in fx0015
plot.zoo(fx0015)

# Compute the log-returns of dj0809 and assign to dj0809_x
dj0809_x <- diff(log(dj0809))

# Plot the log-returns
plot(dj0809_x)

djstocks <- DJ_const["2008/2009",c("AAPL","GS")]
# Compute the log-returns of djstocks and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot the two share returns
plot.zoo(djstocks_x)

# Compute the log-returns of GBP_USD and assign to erate_x
erate_x <- diff(log(GBP_USD))

# Plot the log-returns
plot(erate_x)

djstocks <- DJ_const["2008/2009", c("AAPL", "AXP", "BA", "CAT")]
# Plot djstocks in four separate plots
plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks, plot.type = "single", col=c(1,2,3,4))
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x, type = "h")

dj <- DJ["2000/2015"]
djx <- diff(log(dj))
djstocks <- DJ_const["2000/2015",c("AAPL", "AXP", "BA", "CAT")]
djreturns <- diff(log(djstocks))
# Plot djx
plot(djx)

library(xts)
# Plot weekly log-returns of djx
plot(apply.weekly(djx, sum), type = "h")

# Plot monthly log-returns of djx
plot(apply.monthly(djx, sum), type = "h")

# Plot djreturns
plot.zoo(djreturns)

# Plot monthly log-returns of djreturns
plot.zoo(apply.monthly(djreturns, colSums), type = "h")

data("SP500")
sp <- SP500["1990/2010"]
sp <- diff(log(sp))[-1]
mean(apply.quarterly(sp, sum))

data("OIL_Brent")
oil <- OIL_Brent["1990/2015"]
data("GOLD")
gold <- GOLD["1990/2015"]

# Plot gold and oil prices
plot(gold)
plot(oil)

# Calculate daily log-returns
goldx <- diff(log(gold))
oilx <- diff(log(oil))

# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx, sum)
oilx_m <- apply.monthly(oilx, sum)

# Merge goldx_m and oilx_m into coms
coms <- merge(goldx_m, oilx_m)

# Plot coms with vertical bars
plot.zoo(coms, type = "h")

# Make a pairwise scatterplot of coms
pairs(as.zoo(coms))

data("ZCB_CAD")
zcb <- ZCB_CAD["2006/2015"]
# Compute log-returns as zcb_x and simple returns as zcb_x2
zcb_x <- diff(log(zcb))
zcb_x2 <- diff(zcb)

yield_cols <- c("1.00y", "5.00y", "10.00y")
# Plot zcb_x for 1, 5 and 10-year maturities
plot.zoo(zcb_x[, yield_cols])

# Plot zcb_x2 for 1, 5 and 10-year maturities
plot.zoo(zcb_x2[, yield_cols])

maturity <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 
              3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6, 6.25, 
              6.5, 6.75, 7, 7.25, 7.5, 7.75, 8, 8.25, 8.5, 8.75, 9, 9.25, 9.5, 
              9.75, 10, 10.25, 10.5, 10.75, 11, 11.25, 11.5, 11.75, 12, 12.25, 
              12.5, 12.75, 13, 13.25, 13.5, 13.75, 14, 14.25, 14.5, 14.75, 
              15, 15.25, 15.5, 15.75, 16, 16.25, 16.5, 16.75, 17, 17.25, 17.5, 
              17.75, 18, 18.25, 18.5, 18.75, 19, 19.25, 19.5, 19.75, 20, 20.25, 
              20.5, 20.75, 21, 21.25, 21.5, 21.75, 22, 22.25, 22.5, 22.75, 
              23, 23.25, 23.5, 23.75, 24, 24.25, 24.5, 24.75, 25, 25.25, 25.5, 
              25.75, 26, 26.25, 26.5, 26.75, 27, 27.25, 27.5, 27.75, 28, 28.25, 
              28.5, 28.75, 29, 29.25, 29.5, 29.75, 30)

# Plot the yield curve for the first day of zcb
plot(maturity, zcb[1, ], ylim = range(zcb), type = "l", ylab = "yield (%)", col = "red")
# Add a line for the last day of zcb
lines(maturity, zcb[nrow(zcb), ])