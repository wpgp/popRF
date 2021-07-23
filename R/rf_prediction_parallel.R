#' rf_prediction_parallel Predict for gridded covariates
#' 
#' @rdname rf_prediction_parallel
#' @param covariates popfit quant objects
#' @param census_mask census_mask
#' @param water_raster water_raster
#' @param popfit_final covariates list
#' @param popfit_quant popfit final objects
#' @param outdir path to load/save popfit objects
#' @param nrpoc path to load/save popfit objects
#' @param tag proximity
#' @param quant proximity
#' @param blocks number of blocks sugesting for processing raster file.
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom raster getValues writeRaster writeStart writeStop
#' compareRaster hasValues writeValues blockSize  getCluster returnCluster
#' @importFrom stats complete.cases predict sd aggregate
#' @importFrom utils stack getFromNamespace
#' @importFrom doParallel registerDoParallel  
#' @importFrom parallel detectCores clusterEvalQ clusterExport
#' @importFrom foreach '%dopar%' foreach
#' @return raster objects
#' @noRd 
rf_prediction_parallel <- function(covariates,
                                   census_mask,
                                   water_raster,
                                   popfit_final, 
                                   popfit_quant, 
                                   outdir, 
                                   nrpoc, 
                                   tag, 
                                   quant = TRUE, 
                                   blocks=NULL, 
                                   verbose=TRUE, 
                                   log=FALSE) {
  

  prediction_raster <- census_mask
    
  log_info("MSG", paste0("Start predict for gridded covariates"), verbose=verbose, log=log) 
  
  rfg.predict.density.rf.pred <- file.path(outdir, 
                                           paste0("predict_density_rf_pred_", tag, ".tif")) 
  rfg.predict.density.rf.sd <- file.path(outdir, 
                                         paste0("predict_density_rf_sd_", tag, ".tif")) 
  
  
  rfg.predict.density.rf.pred_05 <- file.path(outdir, 
                                              paste0("predict_density_rf_pred_05_", tag , ".tif")) 
  rfg.predict.density.rf.pred_50 <- file.path(outdir, 
                                              paste0("predict_density_rf_pred_50_", tag , ".tif")) 
  rfg.predict.density.rf.pred_95 <- file.path(outdir, 
                                              paste0("predict_density_rf_pred_90_", tag , ".tif")) 
  
  
  
  recvOneData <- getFromNamespace("recvOneData", "parallel")
  sendCall <- getFromNamespace("sendCall", "parallel")
  
  
  tStart <- Sys.time()
  
  
  
  # Stack all of our covariates and masks together:
  #
  for (i in 1:length(names(popfit_final$forest$xlevels))){
    
    var_name <- names(popfit_final$forest$xlevels)[i]
    r <- raster( covariates[[var_name]]$path )
    names(r) <- var_name    
    
    if (i == 1) {
      covariate_stack <- r  
    }else{
      covariate_stack <- raster::addLayer(covariate_stack, r)
    }
    
  } 
  
  ##  Append the census mask and the water mask to that list:
  names(census_mask) <- "census_mask"
  covariate_stack <- raster::addLayer(covariate_stack, census_mask)
  names(water_raster) <- "water_raster"
  covariate_stack <- raster::addLayer(covariate_stack, water_raster)
  
  rm(r)
  #
  #

  
  # # set additional param for geting min blocks for cluster
  # if (quant) nmb=60 else nmb=30
  # 
  # ##  Stack all of our covariates and masks together:
  # #covariate_stack <- creat_raster_stack(covariates, popfit_final)
  # 
  # if (is.null(minblocks)) {
  #   minblocks <- get_blocks_need(covariate_stack, cores=nrpoc, n=nmb)  
  # }
  
  
  
  cl <- getCluster()
  on.exit( returnCluster() )  
  
  nodes <- length(cl)
  # blocks <- blockSize(prediction_raster,minblocks=minblocks)
  
  log_info("MSG", paste0("covariate_stack will be divided to ",blocks$n," blocks"), verbose=verbose, log=log) 
  
  clusterEvalQ(cl, {
    require(raster)
    require(randomForest)
  })  
  
  if (quant) {
    clusterExport(cl, c("popfit_final", "popfit_quant", "covariate_stack", "transY"), envir=environment())
  } else {
    clusterExport(cl, c("popfit_final", "covariate_stack", "transY"), envir=environment())
  }
  
  clusterExport(cl, "blocks", envir=environment())
  clusterExport(cl, "quant", envir=environment())
  clusterExport(cl, "verbose", envir=environment())
  clusterExport(cl, c("recvOneData", "sendCall"), envir=environment())
  
  #################################################################################
  #################################################################################
  ##	Define the function that will be run on each cluster to do the predictions:
  #
  call_predictions <- function (i) {
    
    row_data <- data.frame( getValues(covariate_stack, 
                                      row=blocks$row[i], 
                                      nrows=blocks$nrows[i]) )
    
    ## Convert field names to something more manageable and 
    ## that matches our popfit variable list:
    ## Full covariate stack:
    #
    names(row_data) <- c(names(popfit_final$forest$xlevels), "census_mask", "water_raster")
    
    ##	Detect if we have any NA or Inf values, and that the values are 
    ##		covered by our census administrative units:
    #
    na_present <- apply(is.na(row_data), 1, any)
    inf_present <- apply(row_data == -Inf | row_data == Inf, 1, any)
    census_mask <- (is.na(row_data$census_mask))
    water_mask <- (row_data$water_raster == 1)
    
    ## Use the first if you want to mask out water pixels, this can greatly
    ## speed up predictions over areas with a lot of water, however, you
    ## run the risk of having no predictions in the resulting dataset
    ## if you have a census block small enough that it might only have
    ## water cover (GeoCover/GlobCover is what determines the water mask):
    #
    roi_subset <- (!na_present & !inf_present & !census_mask & !water_mask)
    
    
    ##	Create a set of predictions based on our covariates:
    #
    predictions <- numeric(length=length(row_data[,1]))
    predictions[] <- NA
    
    if (quant) {
      predictions <- data.frame("rf_pred"=predictions, 
                                "rf_sd"=predictions, 
                                "rf_05"=predictions, 
                                "rf_50"=predictions, 
                                "rf_95"=predictions)              
    }else{
      predictions <- data.frame("rf_pred"=predictions, 
                                "rf_sd"=predictions)              
    }
    
    
    ## I f we have data where NAs or Inf values are not present then we 
    ## predict for those cells (where we subset our data according to the 
    ## roi_subset and remove the census zone and water mask columns (length(row_data) - 2):
    #
    if (sum(roi_subset) > 0) {
      
      prediction_set <- predict(popfit_final, 
                                newdata=row_data[roi_subset,1:(length(row_data)-2)], 
                                predict.all=TRUE)
      
      predictions$rf_pred[roi_subset] <- transY(apply(prediction_set$individual, MARGIN=1, mean), inverse=TRUE)
      
      predictions$rf_sd[roi_subset] <- apply(prediction_set$individual, MARGIN=1, sd)
      
      if (quant) {
        
        prediction_set <- predict(popfit_quant, 
                                  newdata=row_data[roi_subset,1:(length(row_data)-2)], 
                                  quantiles=c(0.05, 0.50, 0.95))
        
        predictions$rf_05[roi_subset] <- transY(prediction_set[,1], inverse=TRUE)
        predictions$rf_50[roi_subset] <- transY(prediction_set[,2], inverse=TRUE)
        predictions$rf_95[roi_subset] <- transY(prediction_set[,3], inverse=TRUE)
      }
    }
    
    return(predictions)
  } 
  #
  ##
  #################################################################################
  #################################################################################
  
  
  
  ##	Start all nodes on a prediction:
  for (i in 1:nodes) {
    sendCall(cl[[i]], call_predictions, i, tag=i)
  }  
  
  ## Start the raster writer object so we can store our results as they
  ## come back from our cluster:  
  #
  prediction_raster <- writeStart(prediction_raster, 
                                  filename=rfg.predict.density.rf.pred, 
                                  format="GTiff", 
                                  datatype="FLT4S", 
                                  overwrite=TRUE, 
                                  options=c("COMPRESS=LZW"))
  
  
  sd_raster <- prediction_raster
  sd_raster <- writeStart(sd_raster, 
                          filename=rfg.predict.density.rf.sd, 
                          format="GTiff", 
                          datatype="FLT4S", 
                          overwrite=TRUE, 
                          options=c("COMPRESS=LZW"))  
  
  
  if (quant) {
    prediction_raster_05 <- prediction_raster
    prediction_raster_05 <- writeStart(prediction_raster_05, 
                                       filename=rfg.predict.density.rf.pred_05, 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
    
    prediction_raster_50 <- prediction_raster
    prediction_raster_50 <- writeStart(prediction_raster_50, 
                                       filename=rfg.predict.density.rf.pred_50, 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
    
    prediction_raster_95 <- prediction_raster
    prediction_raster_95 <- writeStart(prediction_raster_95, 
                                       filename=rfg.predict.density.rf.pred_95, 
                                       format="GTiff", 
                                       datatype="FLT4S", 
                                       overwrite=TRUE, 
                                       options=c("COMPRESS=LZW"))
  }
  
  
  
  ########################################################################
  ##
  ## Create our primary cluster processing loop, recalling that we already
  ## have clusters running:
  #
  
  
  for (i in 1:blocks$n) {
    
    ##	Receive results from a node:
    predictions <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!predictions$value$success) {
      stop("ERROR: Cluster barfed...\n\n", predictions)
    }
    
    ##	Which block are we processing:
    block <- predictions$value$tag
    
    
    prediction_raster <- writeValues(prediction_raster, 
                                     predictions$value$value$rf_pred, 
                                     blocks$row[block])
    sd_raster <- writeValues(sd_raster, 
                             predictions$value$value$rf_sd, 
                             blocks$row[block])
    
    
    if (quant) {
      
      prediction_raster_05 <- writeValues(prediction_raster_05, 
                                          predictions$value$value$rf_05, 
                                          blocks$row[block])
      
      prediction_raster_50 <- writeValues(prediction_raster_50, 
                                          predictions$value$value$rf_50, 
                                          blocks$row[block])
      
      prediction_raster_95 <- writeValues(prediction_raster_95, 
                                          predictions$value$value$rf_95, 
                                          blocks$row[block])
    }
    
    
    ##	Check to see if we are at the end of our block list:
    ni <- nodes + i
    if (ni <= blocks$n) {
      sendCall(cl[[predictions$node]], call_predictions, ni, tag=ni)
    }
    tEnd <-  Sys.time()
    
    if (verbose){
      progress_message(x=i, 
                       max=blocks$n, 
                       label=paste0("received block ",ni, " Processing Time: ", tmDiff(tStart,tEnd))
      ) 
    }
  }
  
  prediction_raster <- writeStop(prediction_raster)
  sd_raster <- writeStop(sd_raster)
  
  if (quant) {
    prediction_raster_05 <- writeStop(prediction_raster_05)
    prediction_raster_50 <- writeStop(prediction_raster_50)
    prediction_raster_95 <- writeStop(prediction_raster_95)
  }
  
  
  return(prediction_raster)
}  
