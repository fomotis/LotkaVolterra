source("startvalues.R")
source("LVE.R")
source("residual.R")

LotkaVolterra <- function(X, Time) {
  e1 <<- environment()
  
   ddat <- data.frame(X = X, Time = Time)
   x0 <- ddat$X[1]
   
   #get the starting values
   parms_start <- startvalues(ddat)
   # initial fit with the starting values above
   ini_fit <- ode(y = x0, times = ddat$Time, func = LVE, parms = 
                    list(A = parms_start[2], mu = parms_start[1]))
   ini_fit_df <- as.data.frame(ini_fit)
   names(ini_fit_df) <- c("Time", "X")
   final_fit <- minpack.lm::nls.lm(par = parms_start, fn = SSR, 
                                   lower = c(-Inf, -1.0E-9))
   #extracting the final estimates
   final_estimates <- coef(final_fit)
   #summary 
   final_fit_summary <- summary(final_fit)
   #extract sigma
   sigma <- final_fit_summary$sigma
   #extract coefficients
   final_fit_summary$coefficients
   #predicted values for the final fits
   final_fit2 <- ode(y = x0, times = ddat$Time, func = LVE, parms = 
                       list(A = final_estimates[2], mu = final_estimates[1]))
   final_fit_df <- as.data.frame(final_fit2)
   names(final_fit_df) <- c("Time", "X_pred")
   #part of the output
   fit_results <- data.frame(Time = Time, X = X, X_pred = final_fit_df$X_pred)
   #Loglikelihood
   loglike <- sum(dnorm(x = ddat$X, mean = final_fit_df$X, 
             sd = final_fit_summary$sigma, log = T))
   return(list(fitresults = fit_results, loglike = loglike, 
               mu = final_estimates[1], A = final_estimates[2]))
   
}
