library(CondIndTests)
?KCI()
data <- non_lin_fork(1000)
KCI(Y = data$X3, E = data$X2, X = data$X1, gammaApprox = F)
