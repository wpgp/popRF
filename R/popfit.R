#' get_popfit optimize the model
#' 
#' @rdname get_popfit
#' @param x_data data for optimize
#' @param y_data data for optimize
#' @param proximity proximity
#' @param init_popfit init_popfit
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom randomForest tuneRF importance
#' @importFrom stats na.omit
#' @return randomForest objects
get_popfit <- function(x_data, 
                       y_data, 
                       init_popfit=NULL, 
                       proximity=TRUE, 
                       verbose=FALSE, 
                       log=FALSE) {
  
  set.seed(2002)
  
  start_time <- Sys.time()

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