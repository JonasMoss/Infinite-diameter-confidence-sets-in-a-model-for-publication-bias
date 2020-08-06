## This package is used for the cuddy data
## The data is from the publipha package, which is required for.
## calculating the density as well.

cuddy2018 = publipha::dat.cuddy2018
cuddy2018 = cuddy2018[cuddy2018$yi < 1.5, ] # removes outlier.
cuddy2018 = cuddy2018

yi = cuddy2018$yi
vi = cuddy2018$vi

eta = c(1, 0.6, 0.1)
alpha = c(0, 0.025, 0.05, 1)

objective = function(p) {
  theta0 = p[1]
  tau_sq = p[2]^2
  sum(-publipha::dmpsnorm(
    x = sqrt(yi),
    theta0 = theta0, 
    tau = sqrt(tau_sq),
    sigma = sqrt(vi),
    eta = eta,
    alpha = alpha,
    log = TRUE))
}

x = seq(-3, 1, length.out = 100)
y = seq(10^(-7), 1, length.out = 100)
FUN = Vectorize(function(theta0, tau) objective(c(theta0, tau)))
z = outer(X = x, Y = y, FUN = FUN)

solution = abs(nlm(objective, p = c(0.2, 0.01))$estimate) # 5.53-01 -4.99-07

pdf("chunks/cuddy.pdf")
contour(
  x = x, 
  y = y, 
  z = -z, 
  levels = c(12, 10, 5, 0, -10, -100, -200, -500),
  xlab = expression(theta[0]), ylab = expression(tau),
  cex.lab = sqrt(2), main = "Likelihood of Hedges' publication bias model"
  )
points(x = solution[1], y = solution[2], pch = 20)
dev.off()  


nlm(objective, p = c(0.2, 0.01))