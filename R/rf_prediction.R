#' Predict for gridded covariates
#' 
#' @rdname rf_prediction
#' @param covariates covariates list
#' @param census_mask census_mask 
#' @param water_raster water_raster   
#' @param popfit_final popfit final objects
#' @param popfit_quant popfit quant objects
#' @param outdir path to load/save popfit objects
#' @param tag proximity
#' @param quant proximity
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom raster getValues writeRaster
#' @importFrom stats complete.cases predict sd
#' @importFrom utils stack
#' @return raster objects
#' @noRd 
rf_prediction <- function(covariates,
                          census_mask, 
                          water_raster,
                          popfit_final, 
                          popfit_quant, 
                          outdir, 
                          tag, 
                          quant=TRUE, 
                          verbose=FALSE, 
                          log=FALSE) {

  
  tStart <- Sys.time()
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
  
  row_data <- data.frame(getValues(covariate_stack))
  
  
  
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
  predictions <- numeric(length = length(row_data[, 1]))
  predictions[] <- NA
  
  
  
  if (quant) {
    predictions <- data.frame(
      "rf_pred" = predictions,
      "rf_sd" = predictions,
      "rf_05" = predictions,
      "rf_50" = predictions,
      "rf_95" = predictions
    )
    #prinitialising rasters
    predictions_rf_pred <- covariate_stack$census_mask
    predictions_rf_sd <- covariate_stack$census_mask    
    predictions_rf_05 <- covariate_stack$census_mask
    predictions_rf_50 <- covariate_stack$census_mask
    predictions_rf_95 <- covariate_stack$census_mask
    
  } else{
    predictions <- data.frame("rf_pred" = predictions,
                              "rf_sd" = predictions)
    
    #prinitialising rasters
    predictions_rf_pred <- covariate_stack$census_mask
    predictions_rf_sd <- covariate_stack$census_mask
  }
  
  rm(covariate_stack)
  
  ## I f we have data where NAs or Inf values are not present then we
  ## predict for those cells (where we subset our data according to the
  ## roi_subset and remove the census zone and water mask columns (length(row_data) - 2):
  #
  if (sum(roi_subset) > 0) {
    prediction_set <- predict(popfit_final,
                              newdata = row_data[roi_subset, 1:(length(row_data) - 2)],
                              predict.all = TRUE)
    
    
    raster::values(predictions_rf_pred) <- transY(apply(prediction_set$individual, 
                                                        MARGIN = 1, 
                                                        mean), 
                                                  inverse = TRUE)
    
    raster::values(predictions_rf_sd) <- apply(prediction_set$individual, 
                                               MARGIN = 1, 
                                               sd)
    
    # predictions$rf_pred[roi_subset] <-
    #   transY(apply(prediction_set$individual, MARGIN = 1, mean),
    #          inverse = TRUE)
    # 
    # predictions$rf_sd[roi_subset] <-
    #   apply(prediction_set$individual, MARGIN = 1, sd)
    
    if (quant) {
      
      prediction_set <- predict(popfit_quant,
                                newdata = row_data[roi_subset, 1:(length(row_data) - 2)],
                                quantiles = c(0.05, 0.50, 0.95))
      
      raster::values(predictions_rf_05) <- transY(prediction_set[, 1], inverse = TRUE)
      raster::values(predictions_rf_50) <- transY(prediction_set[, 2], inverse = TRUE)
      raster::values(predictions_rf_95) <- transY(prediction_set[, 3], inverse = TRUE)
      
      # 
      # predictions$rf_05[roi_subset] <-
      #   transY(prediction_set[, 1], inverse = TRUE)
      # predictions$rf_50[roi_subset] <-
      #   transY(prediction_set[, 2], inverse = TRUE)
      # predictions$rf_95[roi_subset] <-
      #   transY(prediction_set[, 3], inverse = TRUE)
      
    }
  }
  
  
  prediction_raster <- writeRaster(predictions_rf_pred, 
                                   filename=rfg.predict.density.rf.pred, 
                                   format="GTiff", 
                                   datatype="FLT4S", 
                                   overwrite=TRUE, 
                                   options=c("COMPRESS=LZW"))
  
  
   writeRaster(predictions_rf_sd, 
               filename=rfg.predict.density.rf.sd, 
               format="GTiff", 
               datatype="FLT4S", 
               overwrite=TRUE, 
               options=c("COMPRESS=LZW"))  
  
  
  if (quant) {
    
    writeRaster(predictions_rf_05,
                filename=rfg.predict.density.rf.pred_05,
                format="GTiff",
                datatype="FLT4S",
                overwrite=TRUE,
                options=c("COMPRESS=LZW"))
    
    
    writeRaster(predictions_rf_50,
                filename=rfg.predict.density.rf.pred_50,
                format="GTiff",
                datatype="FLT4S",
                overwrite=TRUE,
                options=c("COMPRESS=LZW"))
    
    
    writeRaster(predictions_rf_95,
                filename=rfg.predict.density.rf.pred_95, 
                format="GTiff",
                datatype="FLT4S",
                overwrite=TRUE,
                options=c("COMPRESS=LZW"))
  }  
  
  
  tEnd <-  Sys.time()
  log_info("MSG", paste0("Processing Time:  ",tmDiff(tStart,tEnd)), verbose=verbose, log=log) 
  
  return(prediction_raster)
}