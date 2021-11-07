#' Constrain raster using mask. 
#' 
#' @details  Constrain raster using mask.
#' 
#' @rdname constrain_rst
#' @param f the name of the file which will be constraned.
#' @param f.const the name of the output file.
#' @param const Path to the mask to constrained population layer. 
#'        Mask file should be a raster file with 0 and Nodata values.
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow. 
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
constrain_rst <- function(f,
                          f.const,
                          const, 
                          cores=NULL, 
                          blocks=NULL, 
                          verbose=TRUE, 
                          log=FALSE) {
  
  log_info("MSG", paste0("Start constraining pixel area covariate"), 
           verbose=verbose, 
           log=log)  
  
  # output_dir = dirname(normalizePath(f))
  # 
  # f.basename = basename(f)
  # 
  # f.fl.name = substr(basename(f.basename), 
  #                             1, 
  #                             nchar(basename(f.basename)) - 4)  
  
  # f.const <- file.path(output_dir, paste0(f.fl.name, "_const.tif"))
  
  
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
  
  
  # if we have multi cores then do masking in parallel
  if ( cores > 1 ){
    
    mask_ppd_stack <- raster::stack(raster(const), 
                                    raster(f))
    
    if (is.null(blocks)) {
      
      blocks <- get_blocks_size(mask_ppd_stack, 
                                cores,
                                verbose = verbose)        
      
    }
    
    npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores)
    
    pixel_mask <- masking_out(mask_ppd_stack, 
                              fun = fun_const,
                              filename = f.const,
                              NAflag=-99999,
                              datatype="FLT4S",
                              overwrite = TRUE,
                              cores = npoc_blocks,
                              blocks = blocks,
                              cblk = 2,
                              silent = ifelse(verbose, FALSE, TRUE))
    
    
  }else{
    
    pixel_mask <- calc(mask_ppd_stack, fun_const)
    
    
    writeRaster(pixel_mask, 
                filename=f.const,
                format="GTiff", 
                overwrite=TRUE, 
                NAflag=-99999, 
                datatype='FLT4S', 
                options=c("COMPRESS=LZW")
    )
    
    rm(pixel_mask)
    
    
  } # cores > 1
  
  
}






#' Apply Constrain raster using mask. 
#' 
#' @details  Apply Constrain raster using mask.
#' 
#' @rdname apply_constrained_pixel
#' @param px_area A named list where each element of the list defines the path 
#'        to the input raster(s) containing the pixel area. The name corresponds
#'        to the 3-letter ISO code of a specified country. Each corresponding 
#'        element defines the path to the raster whose values indicate the area 
#'        of each unprojected (WGS84) pixel. If the path is local and not a full
#'        path, it is assumed to be relative to the current working directory.
#'        Example:
#'```{r}      
#'list(
#'     "NPL" = "npl_px_area.tif"
#'    )
#'``` 
#' @param const Path to the mask to constrained population layer. 
#'        Mask file should be a raster file with 0 and Nodata values.
#' @param output_dir Character vector containing the path to the directory for 
#'        writing output files.       
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow.
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
#' @return named list
#' @noRd 
apply_constrained_pixel <- function(px_area, 
                                    const,
                                    output_dir,
                                    cores=NULL, 
                                    blocks=NULL, 
                                    verbose=TRUE, 
                                    log=FALSE) {
  
  px_area_new <- px_area
  
  for ( i  in  names(px_area) ) {
    
    f <- px_area[[i]]
    
    #output_dir = dirname(normalizePath(f))
    
    f.basename = basename(f)
    
    f.fl.name = substr(basename(f.basename), 
                       1, 
                       nchar(basename(f.basename)) - 4)  
    
    f.const <- file.path(output_dir, i , "tmp",  paste0(f.fl.name, "_const.tif"))
    const_mask <- const[[i]]
    
    if (!file.exists(f.const)){
      constrain_rst(f, 
                    f.const, 
                    const_mask, 
                    cores=cores, 
                    blocks=blocks, 
                    verbose=verbose, 
                    log=log)
    }
    
    px_area_new[[i]] <- f.const
  }  
  
  
  return(px_area_new)
  
}
