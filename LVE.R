

LVE <- function(Time, X, parms, ...) {
  #mu =  inherent growth per-capita
  mu <- parms$mu
  #A = ratio of mu and carrying capacity
  A <- parms$A
  dX <- X * (mu - (A * X))
  list(dX)
}
