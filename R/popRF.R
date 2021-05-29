## Authors & Maintainer of the script
## Maksym Bondarenko <mb4@soton.ac.uk> 
## Jeremiah J. Nieves <jeremiah.j.nieves@outlook.com>
## David Kerr <dk2n16@soton.ac.uk>
## Alessandro Sorichetta <as1v13@soton.ac.uk>
#' 
#' @title popRF Random Forests population modelling scripts
#' 
#' @details Random Forest (RF)-based dasymetric mapping approach developed
#' by Stevens et al. (2015)*
#' 
#' @note * Stevens, F. R., Gaughan, A. E., Linard, C. & Tatem, A. J.
#'       Disaggregating Census Data for Population Mapping Using Random Forests
#'       with Remotely-Sensed and Ancillary Data. PLoS ONE 10, e0107042 (2015).
#'       <http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0107042> 
#'       for more details.
#' 
#' @usage
#' popRF(pop, cov, mastergrid, watermask, px_area, output_dir, cores=NULL, 
#' minblocks=NULL, quant=TRUE, proximity=TRUE, fset=NULL, fset_incl=FALSE, 
#' fset_cutoff=20, fix_cov=FALSE, check_result=FALSE, verbose=TRUE, log=FALSE)
#' @param pop the name of the file which the administrative ID and the population 
#'        values are to be read from. The file should contain two columns 
#'        comma-separated with the value of administrative ID and population 
#'        without columns names. If it does not contain an absolute path, the 
#'        file name is relative to the current working directory.
#' @param cov List contains a list of elements. Each element of a list is another 
#'       list object with a given name of the country, the element of this list 
#'       is the input covariates with the name of the covariates and the path to 
#'       them. 
#'       Example:
#'```{r}      
#'list(
#'     "NPL"=list(
#'                "covariate1" = "covariate1.tif",
#'                "covariate2" = "covariate2.tif"
#'               )  
#'    )
#'```
#' @param mastergrid List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input mastergrid with the path to the raster file. 
#'       Example:
#'```      
#'list(
#'     "NPL" = "npl_mastergrid.tif"
#'    )
#'```
#' @param watermask List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input watermask with the path to the raster file. 
#'       Example:
#'```      
#'list(
#'     "NPL" = "npl_watermask.tif"
#'    )
#'``` 
#' @param px_area List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input px_area with the path to the raster file. 
#'       Example:
#'```{r}      
#'list(
#'     "NPL" = "npl_px_area.tif"
#'    )
#'``` 
#' @param output_dir Path to the folder to save the outputs. 
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow.
#' @param minblocks Integer. if \code{minblocks} is NULL then \code{minblocks} 
#'        for cluster prediction parallesation will be calculated based on 
#'        available memory.
#' @param quant If FALSE then quant will not be calculated
#' @param proximity is logical. TRUE or FALSE: flag indicating whether proximity
#'        measures among the rows be computed? Default is \code{proximity} = TRUE. 
#'        See \code{\link[randomForest]{randomForest}} for more details.
#' @param fset Declare if we are using a fixed set in this modeling, i.e. are we 
#'        parameterizing, in part or in full, this RF model run upon another 
#'        country's(ies') RF model object.   
#' @param fset_incl is logical. TRUE or FALSE: flag indicating whether RF model object 
#'        will or will not be combined with RF model run upon another 
#'        country's(ies') RF model object.
#' @param fset_cutoff Default 20
#' @param fix_cov is logical. TRUE or FALSE: flag indicating whether the raster 
#'        extend of the covariates will be fixed if the extend does not match 
#'        mastergrid.
#' @param check_result is logical. TRUE or FALSE: flag indicating whether the 
#'        results will be compare with input data. 
#'        Default is \code{check_result} = TRUE.
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @references Stevens, F. R., Gaughan, A. E., Linard, C. & Tatem, 
#'        A. J. Disaggregating Census Data for Population Mapping Using Random 
#'        Forests with Remotely-Sensed and Ancillary Data. PLoS ONE 10, e0107042 
#'        (2015). <https://doi.org/10.1371/journal.pone.0107042>        
#' @importFrom randomForest varImpPlot
#' @rdname popRF
#' @return raster object of population
#' @export
#' @examples
#' \dontrun{
#' 
#' pop_table <- list("NPL"="/user/npl_population.csv")
#' 
#' input_cov <- list(
#'                  "NPL"=list(
#'                             "covariate1" = "covariate1.tif",
#'                             "covariate2" = "covariate2.tif"
#'                             )  
#'                  )
#' 
#' input_mastergrid <- list("NPL"= "npl_mastergrid.tif")
#' input_watermask <- list("NPL" ="npl_watermask.tif")
#' input_px_area <- list("NPL" = "npl_px_area.tif")
#' 
#' popRF(pop=pop_table, 
#'       cov=input_cov, 
#'       mastergrid=input_mastergrid, 
#'       watermask=input_watermask, 
#'       px_area=input_px_area, 
#'       output_dir="/user/output", 
#'       cores=4 
#'       )
#' }
popRF <- function(pop, 
                  cov, 
                  mastergrid, 
                  watermask,
                  px_area,
                  output_dir, 
                  cores = NULL, 
                  minblocks=NULL, 
                  quant = TRUE,
                  proximity = TRUE,
                  fset = NULL,
                  fset_incl = FALSE,
                  fset_cutoff = 20,
                  fix_cov=FALSE,
                  check_result=FALSE,
                  verbose = TRUE, 
                  log = FALSE){
  
  
  
  # input_covariates <- cov
  # input_mastergrid <- mastergrid
  # input_watermask <- watermask
  # input_px_area <- px_area
  # input_poptables <- pop
  # pkg.env <- new.env()
  # pkg.env$cur.val
  
  rfg.initial.checks <- initial_check(cov,
                                      mastergrid,
                                      watermask,
                                      px_area,
                                      pop,
                                      output_dir, cores, minblocks)
  
  
  if (rfg.initial.checks != TRUE ){
    
    message(rfg.initial.checks)
    stop_quietly()
    
  }

  #set a global variabal getOption("pj.output.dir")
  options("pj.output.dir"=output_dir)
  
  fixed_set <- ifelse(is.null(fset), FALSE, TRUE)
  
  rfg.output.dir.covariates <- file.path(output_dir,"covariates")
  
  
  create_dir(output_dir, verbose)
  
  
  rfg.input.countries <- c()
  
  for ( i in names(cov) ) {
    rfg.input.countries <- append(rfg.input.countries, i, 1)
  } 
  
  
  glPaths <- create_dirs_for_prj(rfg.input.countries, output_dir)  
  
  
  ##  Declare where we are outputting things:
  rfg.output.path.countries <- glPaths$output
  ##  Declare where we are outputting things:
  rfg.output.path.countries.cvr <- glPaths$data_cvr
  ##  Declare where our temporary path is:
  rfg.output.path.countries.tmp <- glPaths$tmp
  ##  Retrieve the country tag:
  rfg.countries.tag <- glPaths$countries_tag 
  rfg.countries.merged <- glPaths$data_merged
  
  #rfg.data.old.popfits.final  <- popfits$final
  #rfg.data.old.popfits.quant <- popfits$quant
  
  
  rfg.init.popfit.RData <- file.path(rfg.output.path.countries.tmp, 
                                     paste0("init_popfit_",rfg.countries.tag, ".Rdata"))
  
  rfg.popfit.RData <- file.path(rfg.output.path.countries.tmp, 
                                paste0("popfit_",rfg.countries.tag, ".Rdata"))
  
  rfg.popfit.final.RData <- file.path(rfg.output.path.countries.tmp, 
                                      paste0("popfit_final_",rfg.countries.tag, ".Rdata"))
  
  rfg.popfit.quant.RData <- file.path(rfg.output.path.countries.tmp, 
                                      paste0("popfit_quant_",rfg.countries.tag, ".Rdata"))  
  
  
  ##  Pre allocate a list to hold all possible covariate names we will be dealing 
  ##  with:
  covariates <- list()
  
  
  covariates <- create_covariates_list(cov,
                                       mastergrid,
                                       watermask,
                                       px_area)
  
  rfg.initial.cov.check <-  check_cov(covariates, 
                                      fix_cov, 
                                      verbose=verbose, 
                                      log=log)
  
  if (rfg.initial.cov.check){
    stop_quietly()
  }    
  
  
  census_data <- calculate_zonal_stats_covariates(covariates, 
                                                  rfg.output.path.countries.cvr, 
                                                  pop, 
                                                  save_zst=TRUE, 
                                                  cores=cores, 
                                                  verbose=TRUE, 
                                                  log=FALSE)
  
  
  covariates.var.names <- list()
  
  for ( i in names(covariates[[1]]) ) {
    
    covariates.var.names <- append(covariates.var.names, i, 1)   
    
  }   
  
  
  if (length(rfg.input.countries) > 1){
    
    merged_covariates <- merg_covariates(rfg.input.countries,
                                         covariates.var.names, 
                                         covariates, 
                                         rfg.countries.tag, 
                                         rfg.countries.merged, 
                                         verbose=TRUE)
    
    covariates <- merged_covariates
    
  }  
  
  
  covariates <- create_covariates_list_for_RF(covariates)
  
  
  ##  Retrieve the paths for the watermask and the census mask:
  watermaskPathFileName <- covariates[["watermask"]]$path
  censusmaskPathFileName <- covariates[["mastergrid"]]$path
  
  
  ##  Remove AdminId, Watermask and px_area info from prepared covariates list:
  if ("watermask" %in% names(covariates)){
    covariates <- covariates[ - which(names(covariates) == "watermask")]
  }
  if ("mastergrid" %in% names(covariates)){
    covariates <- covariates[ - which(names(covariates) == "mastergrid")]
  }
  if ('px_area' %in% names(covariates)) {
    covariates <- covariates[ - which(names(covariates) == "px_area")]
  } 
  
  
  
  ##	Set up response and covariate dataframes for the random forest modeling.
  ##    Retrieve the population density data:
  y_data <- census_data$POPD_PPHA
  
  ## Get a list of all covariates. Function called from a rf_functions.R file
  
  ## getting a list of all covariates
  if (!is.null(fset)) {
    fixed_predictors <- get_popfit_final_old(fset,  
                                             only.names=TRUE)
  }else{
    fixed_predictors <- get_covariates_var_names(covariates)
  }  
  
  log_info("MSG", paste0("Remove unnecessary covariates from x_data for RF..."), verbose=verbose, log=log)  
  
  ##  Full covariate set:
  x_data <- census_data[,fixed_predictors]
  
  ##	Subset x_data to remove NAs:
  indexX <- complete.cases(x_data)
  
  ##	Subset to remove zero population densities or lower:
  indexY <- y_data > 0
  
  ##	Subset data according to indices to make sure we maintain alignment:
  y_data <- y_data[indexX & indexY]
  x_data <- x_data[indexX & indexY,]
  
  ##	Transform (i.e. log) y_data as defined in the transY() function:
  y_data <- transY(y_data)
  
  
  
  #####
  ##  BEGIN:  FITTING RANDOM FOREST
  #####
  ## Fit the RF, removing any covariates which are not important to the model:
  
  if (file.exists(rfg.popfit.RData)) {
    
    log_info("MSG", paste0("Loading popfit object from ",rfg.popfit.RData), verbose=verbose, log=log)  
    
    load(file=rfg.popfit.RData)
    
    if (verbose){
      varImpPlot(popfit)   
    }
    
  }else{
    
    if (is.null(fset)) {
      
      init_popfit <- NULL
      
      log_info("MSG", paste0("fset is NULL Will optimize the model..."), verbose=verbose, log=log)
      
      if (file.exists(rfg.init.popfit.RData)) {
        
        log_info("MSG", paste0("Tuning of our randomForest population density regression was done before."), verbose=verbose, log=log)
        log_info("MSG", paste0("Loading ", rfg.init.popfit.RData), verbose=verbose, log=log)  
        load(file=rfg.init.popfit.RData) 
        
      }else{
        
        ## tryCatch Tuning
        tryCatch(                      
          {                     
            init_popfit <- popfit_init_tuning(x_data, y_data, proximity, verbose, log)
            ##	Save off our init_popfit object for this set of data:
            save(init_popfit, file=rfg.init.popfit.RData)
          },
          error = function(e){          
            message( paste0("Error in tuning RF. ",e))
            if (nrow(x_data) < 15){
              message( paste0("Number of admin units is ", 
                              length(x_data), 
                              ". It could be one of the reason RF can not tune the model"))  
            }
            stop_quietly()
          },
          warning = function(w){        
            message( paste0("There was a warning message. ",w))
          },
          finally = {                   
            log_info("MSG", paste0("Completed tuning"), verbose=verbose, log=log) 
          }
        )
        ## end tryCatch Tuning
      }
      
      popfit <- get_popfit(x_data, y_data, init_popfit, proximity, verbose, log)
      
    }else{
      
      popfit_final_old <- get_popfit_final_old(fset = fset, 
                                               only.names=FALSE, 
                                               proximity = proximity, 
                                               verbose = verbose, 
                                               log = log)
      
      popfit = randomForest(x=x_data, 
                            y=y_data, 
                            mtry=popfit_final_old$mtry, 
                            ntree=popfit_final_old$ntree, 
                            nodesize=length(y_data)/1000, 
                            importance=TRUE, 
                            proximity=proximity)
      
      rm(popfit_final_old)
    }  
    
    
    log_info("MSG", paste0("Save off our popfit object for this set of data ", rfg.popfit.RData), verbose=verbose, log=log)  
    ##	Save off our popfit object for this set of data:
    save(popfit, file=rfg.popfit.RData) 
    
    if (verbose){  
      ##	For continuous regression, plot observed vs. predicted:
      plot(x=y_data, y=predict(popfit), ylim=c(min(y_data),max(y_data)), xlim=c(min(y_data),max(y_data)))
      #abline(a=0, b=1, lty=2)
      
      ###	For continuous regression, plot residuals vs. observed:
      #plot(x=y_data, y=(y_data - predict(popfit)), xlim=c(0,max(y_data)))
      #abline(a=0, b=0, lty=2)
      #
      ###	For continuous regression, plot residuals vs. fitted:
      #plot(x=predict(popfit), y=(y_data - predict(popfit)), xlim=c(0,max(y_data)))
      #abline(a=0, b=0, lty=2)
      
      varImpPlot(popfit)  
    }
    
  }
  
  
  ##  Fit the final RF and the quant RF:
  
  if (is.null(fset)) {
    
    popfit_final <- get_popfit_final(x_data, 
                                     y_data, 
                                     popfit,
                                     rfg.popfit.final.RData,
                                     proximity=proximity, 
                                     verbose=verbose, 
                                     log=log)
    
    popfit_quant <- get_popfit_quant(x_data, 
                                     y_data, 
                                     popfit,
                                     rfg.popfit.quant.RData,
                                     proximity=proximity, 
                                     verbose=verbose, 
                                     log=log)
    
  }else{
    
    if (fset_incl==TRUE & (nrow(census_data) > fset_cutoff)) {
      
      popfit_final <- get_popfit_final(x_data, 
                                       y_data, 
                                       popfit,
                                       rfg.popfit.final.RData,
                                       proximity=proximity, 
                                       verbose=verbose, 
                                       log=log)
      
      popfit_quant <- get_popfit_quant(x_data, 
                                       y_data, 
                                       popfit,
                                       rfg.popfit.quant.RData,
                                       proximity=proximity, 
                                       verbose=verbose, 
                                       log=log)
      
    }else{
      if (fset_incl){
        log_info("MSG", paste0("Number of ADMIN units is less then threshold ", fset_cutoff), verbose=verbose, log=log) 
        log_info("MSG", paste0("popfit_final and popfit_quant will not be calculated "), verbose=verbose, log=log) 
      }  
    }
  }
  
  
  ##  Remove objects unnecessary for future operations:
  rm(popfit)
  
  ## Set the fixed_set to existing countries if you are using an existing
  ##    set of randomForest objects to predict from:
  
  if (!is.null(fset)) {
    
    popfit.final.old <- get_popfit_final_old(fset, 
                                             only.names=FALSE, 
                                             proximity=TRUE, 
                                             verbose=verbose, 
                                             log=log)
    
    popfit.quant.old <- get_popfit_quant_old(fset, 
                                             only.names=FALSE, 
                                             proximity=TRUE, 
                                             verbose=verbose, 
                                             log=log) 
    
    if (fset_incl==TRUE &  (nrow(census_data) > fset_cutoff)) {
      
      popfit.final.tmp <- popfit_final
      
      popfit.final.tmp$proximity <- NULL
      popfit.final.tmp$predicted <- 0
      popfit_final <- combine(popfit.final.tmp, popfit.final.old)
      
      popfit.quant.tmp <- popfit_quant
      popfit.quant.tmp$predicted <- 0
      popfit_quant <- combine(popfit.quant.tmp, popfit.quant.old) 
      
      rm(popfit.final.tmp, popfit.final.old)
      rm(popfit.quant.tmp, popfit.quant.old)      
      
    }else{
      
      popfit_final <- popfit.final.old
      popfit_quant <- popfit.quant.old
      
      rm(popfit.final.old)
      rm(popfit.quant.old)      
      
    }    
    
  }
  
  
  ##	Last, to save on memory we don't have any need for the proximity 
  ##		matrix for prediction purposes, and for census data with many, many
  ##		units this proximity matrix can be extremely large. We remove it
  ##		here from the popfit_final object since this object will be 
  ##		duplicated across nodes of the cluster. If you need it, it is saved
  ##		with the object and can be load() from disk:
  popfit_final$proximity <- NULL
  
  #####
  ##	END:	FITTING RANDOM FOREST
  #####  
  
  
  #####
  ##  BEGIN:  RF PREDICTION PREP
  #####
  ##	Create a raster stack of our cropped covariates and the zonal_raster 
  ##  file which will allow us to restrict processing to just the areas within the 
  ##  boundaries of our census data area (NOTE: This should be changed here to 
  ##  match the covariates used in the estimation of the model, as well as the 
  ##  renaming applied in the cluster predict function. This will speed processing
  ##  up slightly, especially if used on a subset of the predictors:
  census_mask <- raster(censusmaskPathFileName)
  water_raster <- raster(watermaskPathFileName)
  
  
  invisible(gc())
  

  log_info("MSG", paste0("Starting prediction"), verbose=verbose, log=log)   
  
  
  if (!file.exists( file.path(rfg.output.path.countries,
                              paste0("predict_density_rf_pred_", rfg.countries.tag, ".tif")) )){
    
    
    ##  Stack all of our covariates and masks together:
    # covariate_stack <- creat_raster_stack(covariates, 
    #                                       popfit_final, 
    #                                       census_mask, 
    #                                       water_raster)
    
    if ( cores < 2 ){
      
      tryCatch(                      
        {                     
          prediction_raster <- rf_prediction(covariates,
                                             census_mask,
                                             water_raster,
                                             popfit_final,
                                             popfit_quant,
                                             rfg.output.path.countries, 
                                             rfg.countries.tag, 
                                             quant = TRUE)
        },
        error = function(e){          
          message( paste0("There was an error. ",e))
          stop_quietly()
        },
        warning = function(w){        
          message( paste0("There was an error message. ",w))
        },
        finally = {                   
          log_info("MSG", paste0("Completed prediction"), verbose=verbose, log=log) 
        }
      )
      
      
    }else{

      
      # teamporally Stack all of our covariates and masks together:
      #
      for (i in 1:length(names(popfit_final$forest$xlevels))){
        
        var_name <- names(popfit_final$forest$xlevels)[i]
        r <- raster( covariates[[var_name]]$path )
        names(r) <- var_name    
        
        if (i == 1) {
          covariate_stack_tmp <- r  
        }else{
          covariate_stack_tmp <- raster::addLayer(covariate_stack_tmp, r)
        }
        
      } 
      
      ##  Append the census mask and the water mask to that list:
      names(census_mask) <- "census_mask"
      covariate_stack_tmp <- raster::addLayer(covariate_stack_tmp, census_mask)
      names(water_raster) <- "water_raster"
      covariate_stack_tmp <- raster::addLayer(covariate_stack_tmp, water_raster)
      #
      #
      
      if (is.null(minblocks)) {
        if (quant) nmb=60 else nmb=30
        minblocks <- get_blocks_need(covariate_stack_tmp, cores=cores, n=nmb)
      }
      blocks <- blockSize(covariate_stack_tmp, minblocks=minblocks)
      npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores)
      
      rm(covariate_stack_tmp)
      rm(r)
      invisible(gc())
      
      ##  Start up the cluster:
      beginCluster(n=npoc_blocks)
      
      ##  Create the population density weighting layer:
      prediction_raster <- rf_prediction_parallel(covariates,
                                                  census_mask,
                                                  water_raster, 
                                                  popfit_final, 
                                                  popfit_quant, 
                                                  rfg.output.path.countries, 
                                                  npoc_blocks, 
                                                  rfg.countries.tag, 
                                                  quant = quant, 
                                                  minblocks=NULL)
      
      ##  Terminate the cluster:
      endCluster()
      
    }
    
    
  }else{
    
    prediction_raster <- raster(file.path(rfg.output.path.countries,
                                          paste0("predict_density_rf_pred_", rfg.countries.tag, ".tif")))
    
  }
  
  
  #####
  ##  END:  RF PREDICTION
  #####

  
  p_raster <- apply_pop_density(pop, 
                                censusmaskPathFileName, 
                                rfg.output.path.countries, 
                                cores=cores, 
                                rfg.countries.tag, 
                                quant = quant, 
                                minblocks=minblocks, 
                                verbose=verbose, 
                                log=log)
  


  if (check_result){
    
    c_result <- check_result(pop, 
                             censusmaskPathFileName, 
                             rfg.output.path.countries, 
                             cores, 
                             rfg.countries.tag,  
                             minblocks=NULL, 
                             verbose=verbose, 
                             log=log)
    
    return_results <- list(pop=p_raster, popfit=popfit_final, error= c_result)  
    
  }else{
    return_results <- list(pop=p_raster, popfit=popfit_final)
  }
  

  return(return_results)
  
}
