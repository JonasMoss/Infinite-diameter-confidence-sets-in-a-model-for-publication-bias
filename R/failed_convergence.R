data1 <- cbind(yi = c(rep(1.96, 5), rep(1.65, 5), -1), vi = rep(1, 11))
res <- rma(yi, vi, data = data, method = "ML", digits = 3)
sel1 <- selmodel(res,
  type = "stepfun", 
  alternative = "greater",
  steps = c(.025, .05, 1), 
  verbose = TRUE
)
sel1

data2 <- cbind(yi = c(rep(1.96, 5), rep(1.65, 5), 0), vi = rep(1, 11))
res <- rma(yi, vi, data = data, method = "ML", digits = 3)
sel2 <- selmodel(res,
  type = "stepfun", 
  alternative = "greater",
  steps = c(.025, .05, 1), 
  verbose = TRUE
)
sel2
