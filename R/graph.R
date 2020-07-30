### ============================================================================
### Sampling
### ============================================================================

set.seed(20)
n = 5
theta0 = 0
tau = 0.1
sigma = 1/sqrt(runif(n, 10, 80))
eta = c(1, 0.5, 0.1)

samples = publipha::rmpsnorm(
  n = n, 
  theta0 = theta0, 
  tau = tau, 
  sigma = sigma, 
  eta = eta)


### ============================================================================
### Estimating
### ============================================================================

effects = samples
v = sigma^2
weightr::weightfunct(effects, v, steps = c(0.025, 0.05, 1))

### ============================================================================
### Plotting
### ============================================================================

x = seq(10, 80)

plot(
  x = 1/sigma^2, 
  y = samples, 
  log = "x", 
  xlab = expression(1/sigma),
  ylab = "Effect size estimate"
  )
lines(x, 1.96/sqrt(x))

### ============================================================================
### Maximum likelihood estimation
### ============================================================================

objective = function(p) {
  theta0 = p[1]
  tau_sq = p[2]^2
  sum(-publipha::dmpsnorm(
    x = samples,
    theta0 = theta0, 
    tau = sqrt(tau_sq),
    sigma = sigma,
    eta = eta,
    log = TRUE))
}

nlm(objective, p = c(1, 1))

# objective2 = function(p) {
#   theta0 = p[1]
#   tau_sq = p[2]^2
#   sum(-log(truncnorm::dtruncnorm(
#     x = samples/sigma,
#     a = qnorm(1 - 0.025),
#     mean = theta0, 
#     sd = sqrt(tau_sq + sigma^2))))
# }

#nlm(objective2, p = c(1, 1))

### ============================================================================
### Contour plot
### ============================================================================

X = seq(-50, 0, by = 0.1)
Y = seq(0.01, 10, by = 0.1)
FUN = Vectorize(function(theta0, tau) objective(c(theta0, tau)))
Z = outer(X = X, Y = Y, FUN = FUN)


filled.contour(X, Y, -Z, xlab = expression(theta[0]), ylab = expression(tau),
               cex.lab = sqrt(2), main = "Likelihood of publication bias model")
#x = seq(from = -50, to = 0, by = 0.1)
#lines(x, sqrt(abs(x)), lty = 2, lwd = 2)

library("rgl")
plot3d(X, Y, Z)


# NOT RUN {

require(grDevices) # for colours
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1,
        xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple",
        main = "20 levels; \"simple\" labelling method")
par(op)

## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
opar <- par(mfrow = c(2, 2), mar = rep(0, 4))
for(f in pi^(0:3))
  contour(cos(r^2)*exp(-r/f),
          drawlabels = FALSE, axes = FALSE, frame = TRUE)

rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1, 1) * (diff(rx) - diff(ry))/2
tcol <- terrain.colors(12)
par(opar); opar <- par(pty = "s", bg = "lightcyan")
plot(x = 0, y = 0, type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE,
        vfont = c("sans serif", "plain"))
title("A Topographic Map of Maunga Whau", font = 4)
abline(h = 200*0:4, v = 200*0:4, col = "lightgray", lty = 2, lwd = 0.1)

## contourLines produces the same contour lines as contour
plot(x = 0, y = 0, type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[1], lty = "solid", add = TRUE,
        vfont = c("sans serif", "plain"))
line.list <- contourLines(x, y, volcano)
invisible(lapply(line.list, lines, lwd=3, col=adjustcolor(2, .3)))
par(opar)
# }
