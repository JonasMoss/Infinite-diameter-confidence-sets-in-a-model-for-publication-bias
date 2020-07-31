## This package is used for the cuddy data.
#install.packages("publipha")

cuddy2018 = publipha::dat.cuddy2018
cuddy2018 = cuddy2018[cuddy2018$yi < 1.5, ] # removes outlier.

eta = c(1, 0.6, 0.1)

objective = function(p) {
  theta0 = p[1]
  tau_sq = p[2]^2
  sum(-publipha::dmpsnorm(
    x = sqrt(yi),
    theta0 = theta0, 
    tau = sqrt(tau_sq),
    sigma = sqrt(vi),
    eta = eta,
    log = TRUE))
}

x = seq(-10, 0, by = 0.1)
y = seq(0.01, 0.8, by = 0.1)
FUN = Vectorize(function(theta0, tau) objective(c(theta0, tau)))
z = outer(X = x, Y = y, FUN = FUN)

pdf("chunks/cuddy.pdf")
contour(
  x = x, 
  y = y, 
  z = -z, 
  xlab = expression(theta[0]), ylab = expression(tau),
  cex.lab = sqrt(2), main = "Likelihood of Hedges' publication bias model"
  )
dev.off()  