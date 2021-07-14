
#' check_result comper input with output
#' 
#' @param input_poptables Input list of pop tables
#' @param censusmaskPathFileName path to census mask raster file
#' @param rfg.output.path.countries output dir
#' @param nrpoc number of processors
#' @param rfg.countries.tag tag of the project
#' @param blocks number of blocks sugesting for processing raster file.
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @rdname check_result
#' @return Number of mean difference
#' @noRd 
check_result <- function(input_poptables, 
                         censusmaskPathFileName, 
                         rfg.output.path.countries, 
                         nrpoc, 
                         rfg.countries.tag, 
                         blocks=NULL, 
                         verbose=TRUE, 
                         log=FALSE) {
  
  
  silent <- if (verbose) FALSE else TRUE
  
  census_data <- get_pop_census_all(input_poptables)
  census_data <-  census_data[sort.list(census_data[,1]), ]
  
  
  ##  Load final results
  rfg.predict.density.rf.pred.final <- file.path(rfg.output.path.countries, 
                                                 paste0("ppp_",rfg.countries.tag, ".tif"))
  
  dataset_raster <- raster(rfg.predict.density.rf.pred.final)
  
  
  ##  Load the zonal raster:
  zonal_raster <- raster(censusmaskPathFileName)   
  
  
  if (is.null(nrpoc) | nrpoc < 2 ){
    
    output_stats <- zonal(dataset_raster, zonal_raster, fun="sum")
    
    
  }else{
    
    if (is.null(blocks)) {
      
      blocks <- get_blocks_size(dataset_raster, 
                                nrpoc,
                                nl=2,
                                nt=1,
                                verbose = ifelse(silent, FALSE, TRUE))      
    } 
    
    npoc_blocks <- ifelse(blocks$n < nrpoc, blocks$n, nrpoc)  
    
    output_stats <- calculate_zs_parallel(dataset_raster, 
                                          zonal_raster, 
                                          fun="sum", 
                                          cores=nrpoc, 
                                          blocks=blocks, 
                                          silent=silent)
  }
  
  
  colnames(output_stats) <- c("ADMINID", "PPP_FINAL_RES")
  output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ] 
  output_stats.sorted <-  output_stats.sorted[output_stats.sorted[,1] != 0, ] 
  
  df <- merge( as.data.frame(census_data), 
               as.data.frame(output_stats.sorted), 
               by="ADMINID", 
               sort=FALSE)   
  
  
  
  df <- cbind( df, abs(df$ADMINPOP - df$PPP_FINAL_RES)) 
  
  colnames(df) <- c("ADMINID", "ADMINPOP", "PPP", "DIFFERENCE")  
  
  
  file.path.csv <- file.path(rfg.output.path.countries, 
                             paste0("/check_result_prj_",rfg.countries.tag,".csv"))
  
  write.csv( as.data.frame(df), file = file.path.csv, row.names=FALSE )
  
  return(mean(df[,4]))
}



#' check_result_constrained comper input with output
#' 
#' @param input_poptables Input list of pop tables
#' @param censusmaskPathFileName path to census mask raster file
#' @param rfg.output.path.countries output dir
#' @param nrpoc number of processors
#' @param rfg.countries.tag tag of the project
#' @param blocks number of blocks sugesting for processing raster file.
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @rdname check_result_constrained
#' @return Number of mean difference
#' @noRd 
check_result_constrained <- function(input_poptables, 
                                     censusmaskPathFileName, 
                                     rfg.output.path.countries, 
                                     nrpoc, 
                                     rfg.countries.tag, 
                                     blocks=NULL, 
                                     verbose=TRUE, 
                                     log=FALSE) {
  
  
  mastergrid.basename = basename(censusmaskPathFileName)
  
  mastergrid.fl.name = substr(basename(mastergrid.basename), 
                              1, 
                              nchar(basename(mastergrid.basename)) - 4)  
  
  mastergrid.const <- file.path(rfg.output.path.countries, 
                                paste0(mastergrid.fl.name, "_const.tif"))
  
  
  silent <- if (verbose) FALSE else TRUE
  
  census_data <- get_pop_census_all(input_poptables)
  census_data <-  census_data[sort.list(census_data[,1]), ]
  
  
  ##  Load final results
  rfg.predict.density.rf.pred.final <- file.path(rfg.output.path.countries, 
                                                 paste0("ppp_",rfg.countries.tag, "_const.tif"))
  
  dataset_raster <- raster(rfg.predict.density.rf.pred.final)
  
  
  ##  Load the zonal raster:
  zonal_raster <- raster(censusmaskPathFileName)   
  
  
  if (is.null(nrpoc) | nrpoc < 2 ){
    
    output_stats <- zonal(dataset_raster, zonal_raster, fun="sum")
    
    
  }else{
    
    if (is.null(blocks)) {
      
      blocks <- get_blocks_size(dataset_raster, 
                                nrpoc,
                                nl=2,
                                nt=1,
                                verbose = ifelse(silent, FALSE, TRUE))      
    } 
    
    npoc_blocks <- ifelse(blocks$n < nrpoc, blocks$n, nrpoc)  
    
    output_stats <- calculate_zs_parallel(dataset_raster, 
                                          zonal_raster, 
                                          fun="sum", 
                                          cores=nrpoc, 
                                          blocks=blocks, 
                                          silent=silent)
  }
  
  
  colnames(output_stats) <- c("ADMINID", "PPP_FINAL_RES")
  output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ] 
  output_stats.sorted <-  output_stats.sorted[output_stats.sorted[,1] != 0, ] 
  
  df <- merge( as.data.frame(census_data), 
               as.data.frame(output_stats.sorted), 
               by="ADMINID", 
               sort=FALSE)   
  
  
  
  df <- cbind( df, abs(df$ADMINPOP - df$PPP_FINAL_RES)) 
  
  colnames(df) <- c("ADMINID", "ADMINPOP", "PPP", "DIFFERENCE")  
  
  
  file.path.csv <- file.path(rfg.output.path.countries, 
                             paste0("check_result_prj_",rfg.countries.tag,"_const.csv"))
  
  write.csv( as.data.frame(df), file = file.path.csv, row.names=FALSE )
  
  return(mean(df[,4]))
}