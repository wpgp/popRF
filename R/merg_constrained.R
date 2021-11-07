#' Mergin constraine
#'
#' @param const A named list where each element of the list defines the path 
#'        to the input country-specific raster mask to constrain population layer. 
#'        The mask file should have value \code{0} as a mask.
#' @param rfg.countries.tag tag for the project
#' @param rfg.countries.merged directory to save merged rasters
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom gdalUtils gdalwarp
#' @importFrom raster crs
#' @noRd 
#' @rdname merg_constrained
#' @return Path to merged raster file
#' @examples
#' \dontrun{
#' merg_constrained(const,  
#'                  rfg.countries.merged)
#' }
merg_constrained <- function(const,
                             rfg.countries.tag,
                             rfg.countries.merged,
                             verbose=FALSE,
                             log=FALSE){
  

  
  
  if (length(const) > 1){
    
    log_info("MSG", paste0("Start merging constrain"), verbose=verbose, log=log) 
    
    list_of_tiffs <- c()
    
    for ( ic in 1:length(const) ) {
      
      cst <- const[[ic]]
      list_of_tiffs <- append(list_of_tiffs, cst, 1)
      
      
      
    }    
    dstfile <- file.path(rfg.countries.merged, paste0(rfg.countries.tag, "_merged_const.tif"))
    
    if (file.exists(dstfile)){
      log_info("MSG", paste0("Merged constrined rst file already exists."), verbose=verbose, log=log) 
      return(dstfile)
    }else{
      
      gdalwarp(srcfile=list_of_tiffs,
               of = "GTiff",
               dstfile=dstfile,
               co=c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
               s_srs=crs(raster(const[[1]])),
               output_Raster=TRUE,
               overwrite=TRUE,
               verbose=FALSE) 
      
      return(dstfile)
    }

    
    
  }else{
    return(const[[1]])
  }
 
}
