#' get_popfit_final Another alternative is to use Quantile Regression Forests to generate
#' prediction intervals.  We'll fit a quantile regression using
#' the tuning parameters pulled from the popfit object.
#' 
#' @rdname get_popfit_final
#' @param x_data matrix or data frame of predictor variables
#' @param y_data response vector (factor for classification, numeric for 
#'        regression)
#' @param nodesize Minimum size of terminal nodes. Setting this number larger 
#'        causes smaller trees to be grown (and thus take less time). See 
#'        \code{\link[randomForest]{randomForest}} for more details. Default 
#'        is \code{nodesize} = NULL and will be calculated 
#'        as \code{length(y_data)/1000}.
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have. 
#'        If not given, trees are grown to the maximum possible (subject to 
#'        limits by nodesize). If set larger than maximum possible, a warning is 
#'        issued. See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{maxnodes} = NULL.
#' @param ntree Number of variables randomly sampled as candidates at each split. 
#'        See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{ntree} = NULL and \code{ntree} will be used 
#'        \code{popfit$ntree}
#' @param mtry Number of trees to grow. This should not be set to too small a 
#'        number, to ensure that every input row gets predicted at least a few 
#'        times. See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{ntree} = NULL and \code{ntree} will be used 
#'        \code{popfit$mtry}
#' @param set_seed Integer, set the seed. Default is \code{set_seed} = 2010
#' @param popfit the randomForest object produced with the optimal mtry. 
#'         See \code{\link[randomForest]{tuneRF}} for more details.
#' @param popfit_fln path to save \code{popfit} objects
#' @param proximity is logical. TRUE or FALSE: flag indicating whether proximity
#'        measures among the rows be computed? Default is \code{proximity} = TRUE. 
#'        See \code{\link[randomForest]{randomForest}} for more details.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE
#' @importFrom randomForest randomForest
#' @return constructed n object of class randomForest, 
#'         see \code{\link[randomForest]{randomForest}} for more details
#' @noRd 
get_popfit_final <- function(x_data, 
                             y_data,
                             nodesize=NULL, 
                             maxnodes=NULL, 
                             ntree=NULL,
                             mtry=NULL, 
                             set_seed=2010, 
                             popfit,
                             popfit_fln,
                             proximity=TRUE, 
                             verbose=FALSE, 
                             log=FALSE) {
  
  if (file.exists(popfit_fln)) {
    
    log_info("MSG", paste0("Loading popfit object from ",popfit_fln),
             verbose=verbose, log=log) 
    load(file=popfit_fln)
    
  }else{
    
    set.seed(set_seed)
    
    rf_nodesize <- ifelse(is.null(nodesize), length(y_data)/1000, nodesize)
    
    if (is.null(maxnodes)){
      rf_maxnodes <- NULL
    }else{
      rf_maxnodes <- maxnodes
    }
    
    rf_ntree <- ifelse(is.null(ntree), popfit$ntree, ntree)
    rf_mtry <- ifelse(is.null(mtry), popfit$mtry, mtry)    
    
    # popfit_final <- randomForest(x=x_data, 
    #                              y=y_data, 
    #                              mtry=popfit$mtry, 
    #                              ntree=popfit$ntree, 
    #                              nodesize=length(y_data)/1000, 
    #                              importance=TRUE, 
    #                              proximity=proximity,
    #                              do.trace=F)
    
    popfit_final <- randomForest(x=x_data, 
                                 y=y_data,
                                 ntree=rf_ntree,
                                 mtry=rf_mtry, 
                                 nodesize=rf_nodesize,
                                 maxnodes=rf_maxnodes,
                                 importance=TRUE, 
                                 proximity=proximity,
                                 do.trace=F)    
    
    log_info("MSG", paste0("Saving popfit_final object ",popfit_fln),
             verbose=verbose, log=log) 
    save(popfit_final, file=popfit_fln)
    
  } 
  
  return(popfit_final)
  
}




#' get_popfit_final_old exit program without error
#' 
#' @rdname get_popfit_final_old
#' @param fset list of the path to popfit.RData objects
#' @param only.names if true return only names
#' @param proximity Should proximity measures be computed?
#' @param verbose logical. Should report extra information on progress?
#' @param log logical. Should report on progress be saved in log file?
#' @importFrom randomForest importance combine
#' @return constructed n object of class randomForest, 
#'         see \code{\link[randomForest]{randomForest}} for more details
#' @noRd 
get_popfit_final_old <- function(fset, 
                                 only.names=FALSE, 
                                 proximity=TRUE, 
                                 verbose=FALSE, 
                                 log=FALSE) {
  ##  Function which retrieves previously constructed popfit.RData objects.
  err_mess <- ""
  err_bool <- FALSE
  
  list.of.old.popfits.final <- list.files(fset$final,
                                          pattern=paste0("\\.Rdata$"),
                                          full.names=TRUE) 
  
  

  log_info("MSG", 
           paste("Loading old popfit final from: ", fset$final),
           verbose=verbose, log=log)   

  if ( length(list.of.old.popfits.final) == 0 ){
    err_mess <- paste0('There is no old popfit Please check the folder : ',
                       fset$final)
    stop(err_mess)
  }
  
  ##  Load it:

  log_info("MSG", 
           paste("Loading", basename(list.of.old.popfits.final[[1]]) ),
           verbose=verbose, log=log)   
 
  local_env.Popfit_final = local({load(file=list.of.old.popfits.final[[1]]);environment()})
  
  popfit.final.old <- local_env.Popfit_final$popfit_final
  popfit.final.old$proximity <- NULL
  popfit.final.old$predicted <- 0  
  
  if (only.names){
    
    fixed.predictors <- row.names(importance(popfit.final.old))    
    return(fixed.predictors)  
  }
  
  for ( i in 1:length(list.of.old.popfits.final) ) {
    
    if (i==1) next()
    
    local_env.Popfit_final = local({load(file=list.of.old.popfits.final[[i]]);environment()})
    
    local_env.Popfit_final$popfit_final$proximity <- NULL
    local_env.Popfit_final$popfit_final$predicted <- 0
    
    ##  Combine it with the other popfit finals:
    log_info("MSG", paste("'Combine popfit ",
                          basename(list.of.old.popfits.final[[i]]) ),
             verbose=verbose, log=log) 
    
    popfit.final.old <- combine( popfit.final.old, local_env.Popfit_final$popfit_final )    
  } 
  
  ##  Return it:
  return(popfit.final.old)  
  
}