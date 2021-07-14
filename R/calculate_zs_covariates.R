#' Calculation of covariates zonal statistics 
#'
#' @param x Input covariates list 
#' @param y path to save results
#' @param pop the name of the file which the administrative ID and the population 
#'        values are to be read from. The file should contain two columns 
#'        comma-separated with the value of administrative ID and population 
#'        without columns names. If it does not contain an absolute path, the 
#'        file name is relative to the current working directory.
#' @param save_zst is logical. TRUE or FALSE: flag indicating whether output of 
#'        zonal statistics will be sved into the file. 
#'        Default is \code{save_zst} = TRUE.
#' @param cores is a integer. Number of cores to use when executing the function.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom raster raster zonal
#' @importFrom utils write.csv read.csv
#' @rdname calculate_zonal_stats_covariates
#' @return A data.frame compiled census_data
#' @examples
#' \dontrun{
#' calculate_zonal_stats_covariates(x, y, pop)
#' }
#' @noRd 
calculate_zonal_stats_covariates <- function(x, 
                                             y,
                                             pop,
                                             save_zst=TRUE,
                                             cores=NULL,
                                             verbose=FALSE,
                                             log=FALSE){

  
  log_info("MSG", paste0("Start calculating zonal-statistics for all covariates"), verbose=verbose, log=log)  
  
  tag <- paste(names(x), collapse="_")
  
  ##  Pre allocate the matrix frameworks we'll use:
  census_data <- matrix(nrow=0, ncol = 2) 
  POP_TABLE <- matrix(nrow=0, ncol = 2)
  colnames(POP_TABLE) <- c("ADMINID", "ADMINPOP")
  
  for ( icountry  in names(x) ) {
    # icountry <- "BTN"
    ##  Declare the path to the raster containing the zonal information:
    zonal_raster_path <- x[[icountry]][["mastergrid"]][["dataset_path"]]
    ##  Bring in the zonal raster:
    zonal_raster <- raster(zonal_raster_path)
    
    ##  Set up the matrix to hold the census data for that country:
    census_data.country <- matrix(nrow=0, ncol = 2)
    
    for ( icvr in 1:length(x[[icountry]]) ){
      
      # icvr <- 1
      ## Skip water mask and L1 in covariates based upon names of covariates:
      if( names(x[[icountry]][icvr]) %in%
          c("watermask","mastergrid") ){next}
      
      ##  Retrieve the name of the covariate:
      var_name <- names(x[[icountry]][icvr])
      ##  Retrieve the corresponding attributes of the covariate:
      covariate <- x[[icountry]][[icvr]]
      ##  Retrieve the dataset summary:
      dataset_summary <- covariate$dataset_summary
      ##  Retrieve the corresponding dataset class:
      var_name_class <- covariate$dataset_class
      ##  Retrieve the raster path of the covariate:
      raster_path <- covariate$dataset_path
      
      
      ##  Load the actual covariate raster:
      dataset_raster <- raster(raster_path)
      
      ##  Explicitly retrieve what the covariate is meant to represent:
      covariates.var.names <- covariate$dataset_class
      
      fname <- paste0(tolower(icountry),"_",var_name_class,"_ZS_",dataset_summary,".csv")
      file.path.csv <- file.path(y, fname)
      
      if(!file.exists(file.path.csv )){

        
        if (!is.null(cores)){
          
          ##  Determine the minimum number of blocks needed for processing:
          blocks <- get_blocks_size(dataset_raster, 
                                    cores,
                                    nl=2,
                                    nt=1,
                                    verbose = verbose)      
          
          npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores)
          
          ##  Calculate the stats in parallel:
          output_stats <- calculate_zs_parallel(dataset_raster, 
                                                zonal_raster, 
                                                fun=dataset_summary, 
                                                cores=npoc_blocks, 
                                                blocks=blocks)  
        }else{
          
          output_stats <- zonal(dataset_raster, zonal_raster, fun=dataset_summary)
          
        }
        
        
        ##  Adjust the column names:
        colnames(output_stats) <- c("ADMINID", var_name_class)
        ##  Sort the stats:
        output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ]   
        ##Return the stats which do not correspond to "admin ID 0":
        output_stats.sorted <-  output_stats.sorted[output_stats.sorted[,1] != 0, ]        
        ## Saving zonal statiscs per country for each covariate:
        if (save_zst){
          write.csv( as.data.frame(output_stats.sorted), file = file.path.csv, row.names=FALSE )
        }        
        
      }else{
        ##  If the zonal stats already exist locally:
        
        # log_info("MSG", paste0("Working on ", 
        #                        var_name_class, 
        #                        " for ", 
        #                        icountry,
        #                        " ", 
        #                        dataset_summary ), 
        #          verbose=verbose, log=log)  
        # 
        # log_info("MSG", paste0("Zonal stats has been calculated before for a covariat. Loading..."), 
        #          verbose=verbose, log=log)
        
        ##  Read in the file:
        output_stats <- read.csv( file.path.csv ) 
        ##  Adjust column names:
        colnames(output_stats) <- c("ADMINID", var_name_class)
        ##  Sort the stats:
        output_stats.sorted <-  output_stats[sort.list(output_stats[,1]), ] 
      }
      
      ##  If this is the first iteration:
      if (icvr == 1 ) { 
        census_data.country <- output_stats.sorted
      } else {
        ##  Merge with the previous iterations:
        census_data.country <- merge( as.data.frame(census_data.country), 
                                      as.data.frame(output_stats.sorted), 
                                      by="ADMINID", 
                                      sort=FALSE)
      }
      
      if (verbose){
        if ( icvr != length(x[[icountry]])){
          
          progress_message(x=icvr, 
                           max=length(x[[icountry]]), 
                           label=paste0(" ",
                                        icountry,
                                        ": ",
                                        var_name_class
                                        )
                           )  
          
        }else{
          
          progress_message(x=icvr, 
                           max=length(x[[icountry]]), 
                           label=paste0(" ",
                                        icountry,
                                        "                                   "
                                        )
                           )
          
        }
      }
      
      
    }
    
    census_data.country <- merge(as.data.frame(census_data.country), 
                                 as.data.frame(load_pop(icountry,
                                                        pop
                                                        )), 
                                 by="ADMINID",
                                 sort=FALSE)       
    
    #   Merging census_data all countries:
    census_data <- rbind(census_data, census_data.country)   
    
    
  }
  
  
  ##	Convert our calculated admin unit areas into hectares and add them to the 
  ##  census data:
  census_data$AREA_HA <- census_data$px_area / 10000
  
  ##	Finally calculate our population density in people per hectare for use as 
  ##  our model's outcome of interest:
  census_data$POPD_PPHA <- census_data$ADMINPOP / census_data$AREA_HA
  
  ##  Save the compiled census data as a new file in the temporary output 
  ##  folder:
  fln.csv <- paste0(tag,"_census_data.csv")
  file.path.csv <- file.path(y, fln.csv)
  write.csv(as.data.frame(census_data), file = file.path.csv, row.names=FALSE )    
  
  ##  Convert that data to a dataframe for continuted use:
  census_data <- as.data.frame(census_data)
  
  log_info("MSG", paste0("Complited calculation zonal-statistics for all covariates"), 
           verbose=verbose, log=log) 
  
  return(census_data)
}  
