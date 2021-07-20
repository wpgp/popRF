#' get_popfit optimize the model
#' 
#' @rdname get_popfit
#' @param x_data matrix or data frame of predictor variables. 
#'        See \code{\link[randomForest]{tuneRF}} for more details.
#' @param y_data response vector.
#' @param proximity is logical. TRUE or FALSE: flag indicating whether proximity
#'        measures among the rows be computed? Default is \code{proximity} = TRUE. 
#'        See \code{\link[randomForest]{randomForest}} for more details.4
#' @param set_seed Integer, set the seed. Default is \code{set_seed} = 2010
#' @param init_popfit randomForest object with the optimal mtry.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE
#' @importFrom randomForest tuneRF importance
#' @importFrom stats na.omit
#' @return it returns the randomForest object produced with the optimal mtry. 
#'         See \code{\link[randomForest]{tuneRF}} for more details.
#' @seealso \code{\link{randomForest}}
#' @noRd         
get_popfit <- function(x_data, 
                       y_data, 
                       init_popfit=NULL, 
                       proximity=TRUE,
                       set_seed=2010, 
                       verbose=FALSE, 
                       log=FALSE) {
 
  
  start_time <- Sys.time()
  # set.seed(set_seed)
  ##	Now we will optimize the model by iteratively removing any 
  ##		covariates with negative increases in node purity:
  
  ##	Get list of covariates that have an importance score greater than 0:
  importance_scores <- importance(init_popfit)[order(importance(init_popfit)[,1], decreasing=TRUE),]
  pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
  
  if (length(pos_importance) == length(importance_scores[,1])) {
    
    x_data <- x_data[pos_importance]
    
    popfit = tuneRF(x=x_data, 
                    y=y_data, 
                    plot=TRUE, 
                    mtryStart=length(x_data)/3, 
                    ntreeTry=length(y_data)/20, 
                    improve=0.0001, 
                    stepFactor=1.20, 
                    trace=TRUE, 
                    doBest=TRUE, 
                    nodesize=length(y_data)/1000, 
                    na.action=na.omit, 
                    importance=TRUE, 
                    proximity=proximity, 
                    sampsize=min(c(length(y_data), 1000)), 
                    replace=TRUE) 
    
  }else{
    
    while (length(pos_importance) < length(importance_scores[,1])) {
      
      log_info("MSG", 
               paste(" Jumping into the [while (length(pos_importance) < length(importance_scores[,1])) ] ... "),
               verbose=verbose, 
               log=log
               ) 
      ##	Subset our x_data to just those columns having positive scores:
      x_data <- x_data[pos_importance]
      
      popfit = tuneRF(x=x_data, 
                      y=y_data, 
                      plot=TRUE, 
                      mtryStart=length(x_data)/3, 
                      ntreeTry=length(y_data)/20, 
                      improve=0.0001, 
                      stepFactor=1.20, 
                      trace=TRUE, 
                      doBest=TRUE, 
                      nodesize=length(y_data)/1000, 
                      na.action=na.omit, 
                      importance=TRUE, 
                      proximity=proximity, 
                      sampsize=min(c(length(y_data), 1000)), 
                      replace=TRUE) 
      
      ##	Re-check importance scores:
      importance_scores <- importance(popfit)[order(importance(popfit)[,1], decreasing=TRUE),]
      pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
      
      if (verbose) print(popfit)
      
    } ## End of while loop
  }
  
  end_time <- Sys.time()
  log_info("MSG", paste0("Elapsed Fitting Time: ", tmDiff(start_time,end_time)), verbose=verbose, log=log)  
  
  return(popfit)
  
}
