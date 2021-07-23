#' Tuning of our randomForest population density regression
#' @rdname popfit_init_tuning
#' @param x matrix or data frame of predictor variables
#' @param y response vector (factor for classification, numeric for regression)
#' @param proximity Should proximity measures be computed?
#' @param verbose logical. Should report extra information on progress?
#' @param log logical. Should report on progress be saved in log file?
#' @importFrom randomForest tuneRF
#' @importFrom stats na.omit
#' @return it returns a matrix whose first column contains the mtry values searched, 
#' and the second column the corresponding OOB error
#' @noRd 
popfit_init_tuning <- function(x, 
                               y, 
                               proximity=TRUE, 
                               verbose=FALSE, 
                               log=FALSE) {

  #  x_data = x
  #  y_data = y  
  
  log_info("MSG", paste0("Start tuning of our randomForest population density regression."), verbose=verbose, log=log)  
  
  start_time <- Sys.time()
  
  init_popfit = tuneRF(x=x, 
                       y=y, 
                       plot=TRUE, 
                       mtryStart=length(x)/3, 
                       ntreeTry=length(y)/20, 
                       improve=0.0001, 
                       stepFactor=1.20, 
                       trace=verbose, 
                       doBest=TRUE, 
                       nodesize=length(y)/1000, 
                       na.action=na.omit, 
                       importance=TRUE, 
                       proximity=proximity, 
                       sampsize=min(c(length(y), 1000)), 
                       replace=TRUE) 
  
  
  end_time <- Sys.time()
  log_info("MSG", paste("End tuning RF. Elapsed Fitting Time:", tmDiff(start_time,end_time)), verbose=verbose, log=log)  
  
  return(init_popfit)
}
