#' Apply population density. 
#' 
#' @details Apply population density to a final RF prediction 
#' (RF_pred +L1_pop)/RF_pred_ZS_sum.
#' 
#' @rdname apply_pop_density
#' @param pop the name of the file which the administrative ID and the population 
#'        values are to be read from. The file should contain two columns 
#'        comma-separated with the value of administrative ID and population 
#'        without columns names. If it does not contain an absolute path, the 
#'        file name is relative to the current working directory.
#' @param mastergrid_filename census mask Path FileName
#' @param output_dir Path to the folder to save the outputs. 
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow. 
#' @param rfg.countries.tag character of tag
#' @param quant logical. If FALSE then quant will not be calculated
#' @param minblocks Integer. if \code{minblocks} is NULL then \code{minblocks} 
#'        for cluster prediction parallesation will be calculated based on 
#'        available memory.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom raster getValues writeRaster values
#' @importFrom plyr join
#' @importFrom utils stack
#' @return raster objects
#' @noRd 
apply_pop_density <- function(pop,
                              mastergrid_filename,
                              output_dir,
                              cores=NULL,
                              rfg.countries.tag, 
                              quant = TRUE, 
                              minblocks=NULL,
                              verbose=TRUE, 
                              log=FALSE) {


  silent <- if (verbose) FALSE else TRUE
  
  # rasterising pop table 
  rfg.rst.pop.census.tif <- file.path(output_dir,
                                      paste0("pop_census_mask_",rfg.countries.tag, ".tif"))
  
  rfg.rst.zonal.stats.rf.pred.tif<- file.path(output_dir,
                                              paste0("predict_density_rf_pred_",rfg.countries.tag, "_ZS_sum.tif")) 
  
  
  rfg.predict.density.rf.pred <- file.path(output_dir, 
                                           paste0("predict_density_rf_pred_", rfg.countries.tag, ".tif"))
  
  rst.predict.density.rf.pred <- raster(rfg.predict.density.rf.pred) 
  
  df <- get_pop_census_all(pop)
  zonal_raster <- raster(mastergrid_filename)
  
  if (is.null(cores) | cores < 2 ){
    
    v <- data.frame( raster::getValues(zonal_raster) )
    colnames(v) <- c("v1")
    colnames(df) <- c("v1","v2")
    out <- plyr::join(v, df, type="left",by = "v1")[-1]
    
    
    out.rst.pop.census.raster <- zonal_raster
    
    raster::values(out.rst.pop.census.raster) <- out[[1]]
    
    rst.pop.census <- raster::writeRaster(out.rst.pop.census.raster,
                                           filename=rfg.rst.pop.census.tif,
                                           format="GTiff",
                                           datatype='FLT4S',
                                           overwrite=TRUE,
                                           options=c("COMPRESS=LZW"),
                                           NAflag=-99999)
    
    rm(out.rst.pop.census.raster)
    
    
    zonal.stats.rf.pred.sum <- zonal(rst.predict.density.rf.pred, 
                                     zonal_raster, 
                                     fun="sum")
    
    
    ##  Adjust column names:
    colnames(zonal.stats.rf.pred.sum) <- c("ADMINID", "sum")
    ##  Sort the stats by Admin ID:
    zonal.stats.rf.pred.sum <-  zonal.stats.rf.pred.sum[sort.list(zonal.stats.rf.pred.sum[,1]), ]
    
    ##  Return the zonal stats excluding "Admin ID 0":
    zonal.stats.rf.pred.sum <- as.data.frame(zonal.stats.rf.pred.sum[zonal.stats.rf.pred.sum[,1] != 0, ])
    
    colnames(zonal.stats.rf.pred.sum) <- c("v1","v2")
    out.zonal.stats.rf.pred.sum <- plyr::join(v, zonal.stats.rf.pred.sum, type="left",by = "v1")[-1]
    
    out.zonal.stats.rf.pred.sum.raster <- zonal_raster
    
    raster::values(out.zonal.stats.rf.pred.sum.raster) <- out.zonal.stats.rf.pred.sum[[1]]
    
    rst.zonal.stats.rf.pred  <- raster::writeRaster(out.zonal.stats.rf.pred.sum.raster, 
                                                     filename=rfg.rst.zonal.stats.rf.pred.tif, 
                                                     format="GTiff", 
                                                     datatype='FLT4S', 
                                                     overwrite=TRUE, 
                                                     options=c("COMPRESS=LZW"), 
                                                     NAflag=-99999)
    
    rm(out.zonal.stats.rf.pred.sum.raster)
    
    
    
  }else{
    
    
    colnames(df) <- c("ADMINID", "ADMINPOP")
    minblks <- get_blocks_need(zonal_raster, cores, n=2)
    
    rst.pop.census <- rasterize_parallel(zonal_raster, 
                                         df, 
                                         cores=cores, 
                                         minblk=minblks, 
                                         NAflag=NULL, 
                                         datatype=NULL, 
                                         filename=rfg.rst.pop.census.tif, 
                                         overwrite=TRUE, 
                                         silent=silent)
    
    
    
    
    out.zonal.stats.rf.pred.sum <- calculate_zs_parallel(rst.predict.density.rf.pred,
                                                         zonal_raster, 
                                                         fun="sum", 
                                                         cores=cores,  
                                                         minblk=minblks,
                                                         silent=silent)
    
    
    ##  Adjust column names:
    colnames(out.zonal.stats.rf.pred.sum) <- c("ADMINID", "sum")
    ##  Sort the stats by Admin ID:
    out.zonal.stats.rf.pred.sum <- out.zonal.stats.rf.pred.sum[sort.list(out.zonal.stats.rf.pred.sum[,1]), ]
    
    ##  Return the zonal stats excluding "Admin ID 0":
    out.zonal.stats.rf.pred.sum <- as.data.frame(out.zonal.stats.rf.pred.sum[out.zonal.stats.rf.pred.sum[,1] != 0, ]) 
    
    rst.zonal.stats.rf.pred <- rasterize_parallel(zonal_raster, 
                                                  out.zonal.stats.rf.pred.sum, 
                                                  cores=cores, 
                                                  minblk=minblks, 
                                                  NAflag=NULL, 
                                                  datatype=NULL, 
                                                  filename=rfg.rst.zonal.stats.rf.pred.tif, 
                                                  overwrite=TRUE, 
                                                  silent=silent)
    rm(out.zonal.stats.rf.pred.sum)
    
  }  
  
  
  rfg.predict.density.rf.pred.final <- file.path(output_dir, 
                                                 paste0("ppp_",rfg.countries.tag, ".tif"))
  
  r_calc <- (rst.predict.density.rf.pred * rst.pop.census)/rst.zonal.stats.rf.pred
  
  writeRaster(r_calc, 
              filename=rfg.predict.density.rf.pred.final,
              format="GTiff", 
              overwrite=TRUE, 
              NAflag=-99999, 
              datatype='FLT4S', 
              options=c("COMPRESS=LZW")
  )
  
  return(r_calc)
}