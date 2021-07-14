#' Apply population density. 
#' 
#' @details masking out the prediction layer and L1 using mask.
#' 
#' @rdname apply_constrained
#' @param pop the name of the file which the administrative ID and the population 
#'        values are to be read from. The file should contain two columns 
#'        comma-separated with the value of administrative ID and population 
#'        without columns names. If it does not contain an absolute path, the 
#'        file name is relative to the current working directory.
#' @param mastergrid_filename census mask Path FileName
#' @param const Path to the mask to constrained population layer. 
#'        Mask file should be a raster file with 0 and Nodata values.
#' @param output_dir Path to the folder to save the outputs. 
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow. 
#' @param rfg.countries.tag character of tag
#' @param quant logical. If FALSE then quant will not be calculated
#' @param blocks number of blocks sugesting for processing raster file.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom raster getValues writeRaster values calc
#' @importFrom plyr join
#' @importFrom utils read.csv
#' @return raster objects
#' @noRd 
apply_constrained <- function(pop,
                              mastergrid_filename,
                              const,
                              output_dir,
                              cores=NULL,
                              rfg.countries.tag, 
                              quant = TRUE, 
                              blocks=NULL,
                              verbose=TRUE, 
                              log=FALSE) {
  
  log_info("MSG", paste0("Start creating a constrained population layer"), 
           verbose=verbose, 
           log=log)  
  

  rfg.predict.density.rf.pred <- file.path(output_dir, 
                                           paste0("predict_density_rf_pred_", 
                                                  rfg.countries.tag, ".tif"))
  
  rfg.predict.density.rf.pred.const <- file.path(output_dir, 
                                                 paste0("predict_density_rf_pred_", 
                                                        rfg.countries.tag, "_const.tif"))
  
  mastergrid.basename = basename(mastergrid_filename)
  
  mastergrid.fl.name = substr(basename(mastergrid.basename), 
                              1, 
                              nchar(basename(mastergrid.basename)) - 4)  
  
  mastergrid.const <- file.path(output_dir, 
                                paste0(mastergrid.fl.name, "_const.tif"))
  
  
  fun_const = function(x) {
    if (any(!is.na(x))) {
      if (is.na(x[1]) & !is.na(x[2])) {
        return(NA)
      } else if (x[1]==0 & !is.na(x[2])) {
        return(x[2])
      }else{
        return(NA)
      }
    } else{
      return(NA)
    }
  } 
  
  log_info("MSG", paste0("Start prepering a data to constraine"), 
           verbose=verbose, 
           log=log)
  
  # if we have multi cores then do masking in parallel
  if ( cores > 1 ){
  
    mask_ppd_stack <- raster::stack(raster(const), 
                                    raster(rfg.predict.density.rf.pred))

    if (is.null(blocks)) {
      
      blocks <- get_blocks_size(mask_ppd_stack, 
                                cores,
                                nl=2,
                                nt=1,
                                verbose = verbose)        

    }

    npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores)

    log_info("MSG", paste0("Constraining prediction layer"), verbose=verbose, log=log)
    
    density_mask <- masking_out(mask_ppd_stack,
                                fun = fun_const,
                                filename = rfg.predict.density.rf.pred.const,
                                NAflag=-99999,
                                datatype="FLT4S",
                                overwrite = TRUE,
                                cores = npoc_blocks,
                                blocks = blocks,
                                cblk = 2,
                                silent = ifelse(verbose, FALSE, TRUE))

    
    mastergrid_stack <- raster::stack(raster(const), 
                                      raster(mastergrid_filename))
    

    log_info("MSG", paste0("constraining mastegrid"), verbose=verbose, log=log)
    
    mastergrid_mask <- masking_out(mastergrid_stack,
                                   fun = fun_const,
                                   filename = mastergrid.const,
                                   NAflag=-99999,
                                   datatype="FLT4S",
                                   overwrite = TRUE,
                                   cores = npoc_blocks,
                                   blocks = blocks,
                                   cblk = 2,
                                   silent = ifelse(verbose, FALSE, TRUE))
    
    
  }else{
    
    density_mask <- calc(mask_ppd_stack, fun_const)
    
    
    writeRaster(density_mask, 
                filename=rfg.predict.density.rf.pred.const,
                format="GTiff", 
                overwrite=TRUE, 
                NAflag=-99999, 
                datatype='FLT4S', 
                options=c("COMPRESS=LZW")
    )
    
    rm(density_mask)
    
    mastergrid_mask <- calc(mastergrid_stack, fun_const)
    
    
    writeRaster(mastergrid_mask, 
                filename=mastergrid.const,
                format="GTiff", 
                overwrite=TRUE, 
                NAflag=-99999, 
                datatype='FLT4S', 
                options=c("COMPRESS=LZW")
    )
    
    rm(mastergrid_mask)
    
  } # cores > 1
  
  
  
  
  apply_pop_density_constrained(pop, 
                                mastergrid.const, 
                                output_dir, 
                                cores=npoc_blocks, 
                                rfg.countries.tag, 
                                blocks=blocks, 
                                verbose=verbose, 
                                log=log)
  
}