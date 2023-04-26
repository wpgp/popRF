#' Merging covariates.
#'
#' @param input.countries list countries 
#' @param covariates.var.names list of covariates names 
#' @param covariates detailes of the covariates
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
#' @rdname merg_covariates
#' @return A data.frame merged covariates
#' @examples
#' \dontrun{
#' merge_covariates( covariates.var.names, 
#'                  covariates, 
#'                  rfg.countries.tag, 
#'                  rfg.countries.merged)
#' }
merge_covariates <- function(input.countries,
                             covariates.var.names, 
                             covariates, 
                             rfg.countries.tag, 
                             rfg.countries.merged, 
                             verbose=FALSE,
                             log=FALSE){
  
  
  rfg.input.countries <- input.countries
  
  log_info("MSG", paste0("Start merging covariates"), verbose=verbose, log=log) 
  
  covariates_merged <- list()
  
  
  for ( i in 1:length(covariates.var.names) ) {  
    
    cname <- covariates.var.names[[i]]
    
    list_of_tiffs <- c()
    for ( ic in 1:length(rfg.input.countries) ) {
      
      country <- rfg.input.countries[[ic]]
      rst <- paste0(covariates[[country]][[cname]][["dataset_path"]])
      list_of_tiffs <- append(list_of_tiffs, rst, 1)
      rtype <- covariates[[country]][[cname]][["dataset_summary"]]
      
    }
    
    dstfile <- file.path(rfg.countries.merged, paste0(rfg.countries.tag, "_",cname,".tif"))
    
    if (!file.exists(dstfile)){
      
      
      gdalwarp(srcfile=list_of_tiffs,
               of = "GTiff",
               dstfile=dstfile,
               co=c("COMPRESS=LZW", "BLOCKXSIZE=512", "BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
               s_srs=crs(raster(rst)),
               output_Raster=TRUE,
               overwrite=TRUE,
               verbose=FALSE) 
      
    }
    
    #covariates_merged[[rfg.countries.tag]][[cname]][["path"]] <- dstfile
    covariates_merged[[rfg.countries.tag]][[cname]] <- list(dataset_folder = dirname(dstfile), 
                                                            dataset_filename    = basename(dstfile),
                                                            dataset_description = cname,
                                                            dataset_summary     = rtype,
                                                            dataset_country     = cname,
                                                            dataset_class       = cname,
                                                            dataset_path        = dstfile 
    )
    
    
    if (verbose){
      if ( i != length(covariates.var.names)){
        
        progress_message(x=i, 
                         max=length(covariates.var.names), 
                         label=paste0("Merging ",cname))  
        
      }else{
        
        progress_message(x=i, 
                         max=length(covariates.var.names), 
                         label=paste0("Complited merging                         " ))
        
      }
    }      
    
    
    
  } 
  
  #log_info("MSG", paste0("Completed merging covariates"), verbose=verbose, log=log) 
  return(covariates_merged)
  
}
