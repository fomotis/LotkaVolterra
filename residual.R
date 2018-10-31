###Residual function

SSR <- function(parms) {
  #retrieving things needed
  x0 <- get("x0", e1)
  ddat <- get("ddat", e1)

  #parameters
  A <- parms[2]
  mu <- parms[1]
  # 
  ini_fit <- deSolve::ode(y = x0, times = ddat$Time, func = LVE, 
                          parms = list(A = A, mu = mu))
  ini_fit_df <- as.data.frame(ini_fit)
  names(ini_fit_df) <- c("Time", "X")
  #residual
  ini_fit_df$X - ddat$X
}