djx <- DJ["2008/2009"]
djx <- diff(log(djx))
djx <- as.numeric(djx)[-1]
djx <- sort(djx)
# Calculate average and standard deviation of djx
mu <- mean(djx)
sigma <- sd(djx)

# Plot histogram of djx
hist(djx, nclass = 20, probability = TRUE)
# Add the normal density as a red line to histogram
lines(djx, dnorm(djx, mean = mu, sd = sigma), col = "red")

# Plot non-parametric KDE of djx
plot(density(djx))
# Add the normal density as red line to KDE
lines(djx, dnorm(djx, mean = mu, sd = sigma), col = "red")

# Make a Q-Q plot of djx and add a red line
qqnorm(djx)
qqline(djx, col = "red")

# Calculate the length of djx as n
n <- length(djx)