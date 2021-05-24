#' Apply population densityto a final RF prediction 
#' (RF_pred +L1_pop)/RF_pred_ZS_sum We will need to change it 
#' by using gdal_calc
#' 
#' @rdname apply_pop_density
#' @param input_poptables input poptablest
#' @param censusmaskPathFileName census mask Path FileName
#' @param rfg.output.path.countries path to load/save popfit objects
#' @param nrpoc number of cores
#' @param rfg.countries.tag proximity
#' @param quant proximity
#' @param minblocks minblocks
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @importFrom raster getValues writeRaster values
#' @importFrom plyr join
#' @importFrom utils stack
#' @return raster objects
apply_pop_density <- function(input_poptables,
                              censusmaskPathFileName,
                              rfg.output.path.countries,
                              nrpoc=NULL,
                              rfg.countries.tag, 
                              quant = TRUE, 
                              minblocks=NULL,
                              verbose=TRUE, 
                              log=FALSE) {


  silent <- if (verbose) FALSE else TRUE
  
  # rasterising pop table 
  rfg.rst.pop.census.tif <- file.path(rfg.output.path.countries,
                                      paste0("pop_census_mask_",rfg.countries.tag, ".tif"))
  
  rfg.rst.zonal.stats.rf.pred.tif<- file.path(rfg.output.path.countries,
                                              paste0("predict_density_rf_pred_",rfg.countries.tag, "_ZS_sum.tif")) 
  
  
  rfg.predict.density.rf.pred <- file.path(rfg.output.path.countries, 
                                           paste0("predict_density_rf_pred_", rfg.countries.tag, ".tif"))
  
  rst.predict.density.rf.pred <- raster(rfg.predict.density.rf.pred) 
  
  df <- get_pop_census_all(input_poptables)
  zonal_raster <- raster(censusmaskPathFileName)
  
  if (is.null(nrpoc) | nrpoc < 2 ){
    
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
    minblks <- get_blocks_need(zonal_raster, nrpoc, n=2)
    
    rst.pop.census <- rasterize_parallel(zonal_raster, 
                                         df, 
                                         cores=nrpoc, 
                                         minblk=minblks, 
                                         NAflag=NULL, 
                                         datatype=NULL, 
                                         filename=rfg.rst.pop.census.tif, 
                                         overwrite=TRUE, 
                                         silent=silent)
    
    
    
    
    out.zonal.stats.rf.pred.sum <- calculate_zs_parallel(rst.predict.density.rf.pred,
                                                         zonal_raster, 
                                                         fun="sum", 
                                                         cores=nrpoc,  
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
                                                  cores=nrpoc, 
                                                  minblk=minblks, 
                                                  NAflag=NULL, 
                                                  datatype=NULL, 
                                                  filename=rfg.rst.zonal.stats.rf.pred.tif, 
                                                  overwrite=TRUE, 
                                                  silent=silent)
    rm(out.zonal.stats.rf.pred.sum)
    
  }  
  
  
  rfg.predict.density.rf.pred.final <- file.path(rfg.output.path.countries, 
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