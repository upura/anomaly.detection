library("car")
data("Davis")
Davis
hist(Davis$weight, xlim = c(35, 105), breaks = 14)

mu = mean(Davis$weight)
s2 = mean((Davis$weight - mu)^2)
c(mu, s2)

a = (Davis$weight - mu)^2/s2
th = qchisq(0.99, 1)
plot(a, xlab = "index", ylab = "anomaly score")
lines(0:200, rep(th, length(0:200)), col = "red", lty = 2)
