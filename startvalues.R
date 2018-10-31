

startvalues <- function(ddat) {
  start_values <- sapply(1:nrow(ddat), function(i, x, y) {
    if(i == NROW(y) | x[i] == 0) return(NA)
    pcgr <- ((x[i + 1] - x[i]) / x[i]) / (y[i + 1] - y[i])
    return(pcgr)
  }, x = ddat$X, y = ddat$Time)
  parms_start <- c(mu = mean(start_values, na.rm = T), 
                   A = mean(start_values, na.rm = T) / max(ddat$X))
  return(parms_start)
}

