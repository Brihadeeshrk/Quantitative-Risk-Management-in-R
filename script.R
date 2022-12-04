library(qrmdata)
data("DJ")
data("FTSE")

# showing head() and tail() for DJ
head(DJ)
tail(DJ)
# plotting Dow Jones Index
plot(DJ)

summary(DJ)

# Checking for NULL values
which(is.na(DJ))
# Counting the no: of NULL Values
sum(is.na(DJ))

# showing head() and tail() for FTSE
head(FTSE)
tail(FTSE)
# plotting Financial Times SE
plot(FTSE)

summary(FTSE)

# Checking for NULL values
which(is.na(FTSE))
# Counting the no: of NULL Values
sum(is.na(FTSE))

# Extracting the data from 2008-2009 and storing it in another var
dj0809 <- DJ["2008/2009"]
ftse0809 <- FTSE["2008/2009"]
plot(dj0809)
plot(ftse0809)

data("DJ_const")

head(DJ_const)
which(is.na(DJ_const))
sum(which(is.na(DJ_const)))
plot.zoo((DJ_const), plot.type = "single", col=c(1,2,3,4))
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)
