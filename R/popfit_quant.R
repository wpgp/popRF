######################################################################################
#
#
#' get_popfit_quant Another alternative is to use Quantile Regression Forests to generate
#' prediction intervals.  We'll fit a quantile regression using
#' the tuning parameters pulled from the popfit object above:
#' 
#' @rdname get_popfit_quant
#' @param x_data x data for randomForest
#' @param y_data y data for randomForest
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
#' @param popfit popfit objects
#' @param rfg.popfit.quant.RData path to load/save popfit objects
#' @param proximity proximity
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom quantregForest quantregForest
#' @return constructed popfit objects
#' @noRd 
get_popfit_quant <- function(x_data, 
                             y_data,
                             nodesize=NULL, 
                             maxnodes=NULL, 
                             ntree=NULL,
                             mtry=NULL, 
                             set_seed=2010, 
                             popfit,
                             rfg.popfit.quant.RData,
                             proximity=TRUE, 
                             verbose=FALSE, 
                             log=FALSE) {
  
  if (file.exists(rfg.popfit.quant.RData)) {
    
    log_info("MSG", paste0("Loading popfit object from ",rfg.popfit.quant.RData), verbose=verbose, log=log) 
    load(file=rfg.popfit.quant.RData)
    
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
    
    # popfit_quant <- quantregForest(x=x_data, 
    #                                y=y_data, 
    #                                mtry=popfit$mtry, 
    #                                ntree=popfit$ntree, 
    #                                nodesize=length(y_data)/1000)
    
    popfit_quant <- quantregForest(x=x_data, 
                                   y=y_data, 
                                   mtry=rf_mtry, 
                                   ntree=rf_ntree, 
                                   nodesize=rf_nodesize,
                                   maxnodes=rf_maxnodes)    
    
    log_info("MSG", paste0("Saving popfit_quant object ",rfg.popfit.quant.RData), verbose=verbose, log=log) 
    save(popfit_quant, file=rfg.popfit.quant.RData)
    
  } 
  
  return(popfit_quant)
  
}





#' get_popfit_quant_old exit program without error
#' 
#' @rdname get_popfit_quant_old
#' @param fset list of the path to popfit.RData objects
#' @param only.names if true return only names
#' @param proximity proximity
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom randomForest importance combine
#' @return previously constructed popfit.RData objects
#' @noRd 
get_popfit_quant_old <- function(fset, 
                                 only.names=FALSE, 
                                 proximity=TRUE, 
                                 verbose=FALSE, 
                                 log=FALSE) {
  ##  Function which retrieves previously constructed popfit.RData objects.
  err_mess <- ""
  err_bool <- FALSE
  
  list.of.old.popfits.quant <- list.files(fset$quant,
                                          pattern=paste0("\\.Rdata$"),
                                          full.names=TRUE) 
  
  
  log_info("MSG", paste("Loading old popfit quant from: ", fset$quant), verbose=verbose, log=log) 
  
  
  if ( length(list.of.old.popfits.quant) == 0 ){
    err_mess <- paste0('There is no old popfit Please check the folder : ', fset$quant)
    stop(err_mess)
  }
  
  ##  Load it:
  log_info("MSG", paste("Loading ", basename(list.of.old.popfits.quant[[1]]) ), verbose=verbose, log=log) 
  local_env.Popfit_quant = local({load(file=list.of.old.popfits.quant[[1]]);environment()})
  
  popfit.quant.old <- local_env.Popfit_quant$popfit_quant
  popfit.quant.old$proximity <- NULL
  popfit.quant.old$predicted <- 0  
  
  if (only.names){
    
    fixed.predictors <- row.names(importance(popfit.quant.old))  
    
    return(fixed.predictors)  
  }
  
  for ( i in 1:length(list.of.old.popfits.quant) ) {
    
    if (i==1) next()
    
    local_env.Popfit_quantl = local({load(file=list.of.old.popfits.quant[[i]]);environment()})
    
    local_env.Popfit_quant$popfit_quant$proximity <- NULL
    local_env.Popfit_quant$popfit_quant$predicted <- 0
    
    ##  Combine it with the other popfit quant:
    log_info("MSG", paste("Combine popfit ", basename(list.of.old.popfits.quant[[i]]) ), verbose=verbose, log=log) 
    popfit.quant.old <- combine( popfit.quant.old, local_env.Popfit_quant$popfit_quant )    
  } 
  
  ##  Return it:
  return(popfit.quant.old)  
  
}