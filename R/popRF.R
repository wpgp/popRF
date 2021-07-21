#' @title Disaggregating Census Data for Population Mapping Using Random Forests
#'        with Remotely-Sensed and Ancillary Data.
#' 
#' @author Maksym Bondarenko <mb4@soton.ac.uk>, 
#'         Jeremiah J. Nieves <J.J.Nieves@liverpool.ac.uk>,
#'         Forrest R. Stevens <forrest.stevens@louisville.edu>,
#'         Andrea E. Gaughan <ae.gaughan@louisville.edu>,
#'         David Kerr <dk2n16@soton.ac.uk>, 
#'         Chris Jochem <W.C.Jochem@soton.ac.uk> and 
#'         Alessandro Sorichetta <as1v13@soton.ac.uk>
#'          
#'         
#' @details This function produces gridded population density estimates using 
#'          a Random Forest model as described in _Stevens, et al. (2015)_ 
#'          \doi{10.1371/journal.pone.0107042}.
#'          The unit-average log-transformed population density and covariate 
#'          summary values for each census unit are then used to train a 
#'          Random Forest model (\doi{10.1023/A:1010933404324})
#'          to predict log population density. Random Forest models are an 
#'          ensemble, nonparametric modeling approach that grows a "forest" of 
#'          individual classification or regression trees and improves upon 
#'          bagging by using the best f a random selection of predictors at 
#'          each node in each tree. The Random Forest is used to produced grid, 
#'          i.e. pixel, level population density estimates that are used as 
#'          unit-relative weights to dasymetrically redistribute the census 
#'          based areal population counts. This function also allows for 
#'          modelling based upon a 
#'          regional parameterisation (\doi{10.1080/17538947.2014.965761})
#'          of other previously run models as well as the creation of models based
#'          upon multiple countries at once (\doi{10.1016/j.compenvurbsys.2019.01.006}).
#'          This function assumes that all data is unprojected and is in the 
#'          WGS84 coordinate system.
#' 
#'  
#' @usage
#' popRF(pop, cov, mastergrid, watermask, px_area, output_dir, cores=0, 
#' quant=FALSE, set_seed=2010, fset=NULL, fset_incl=FALSE, 
#' fset_cutoff=20, fix_cov=FALSE, check_result=TRUE, verbose=TRUE, 
#' log=FALSE, ...)
#' 
#' @param pop Character vector containing the name of the file from which the 
#'        unique area ID and corresponding population values are to be read 
#'        from. The file should contain two columns comma-separated with the 
#'        value of administrative ID and population without columns names. 
#'        If it does not contain an absolute path, the file name is relative to
#'        the current working directory.
#' @param cov A nested list of named list(s), i.e. where each element of the 
#'        first list is a named list object with atomic elements. The name of 
#'        each named list corresponds to the 3-letter ISO code of a specified 
#'        country. The elements within each named list define the specified 
#'        input covariates to be used in the random forest model, i.e. the name 
#'        of the covariates and the corresponding, if applicable and local, path
#'        to them. If the path is not a full path, it is assumed to be relative 
#'        to the current working directory.
#'        Example for Nepal (NPL):
#'```{r}      
#'list(
#'     "NPL"=list(
#'                "covariate1" = "covariate1.tif",
#'                "covariate2" = "covariate2.tif"
#'               )  
#'    )
#'```
#' @param mastergrid A named list where each element of the list defines the 
#'        path to the input mastergrid(s), i.e. the template gridded raster(s) 
#'        that contains the unique area IDs as their value. The name(s) 
#'        corresponds to the 3-letter ISO code(s) of a specified country(ies). 
#'        Each corresponding element defines the path to the mastergrid(s). If
#'        the path is local and not a full path, it is assumed to be relative to
#'        the current working directory.
#'        Example:
#'```      
#'list(
#'     "NPL" = "npl_mastergrid.tif"
#'    )
#'```
#' @param watermask A named list where each element of the list defines the path
#'        to the input country-specific watermask. The name corresponds to the 
#'        3-letter ISO code of a specified country. Each corresponding element 
#'        defines the path to the watermask, i.e. the binary raster that 
#'        delineates the presence of water (1) and non-water (0), that is used 
#'        to mask out areas from modelling. If the path is local and not a full 
#'        path, it is assumed to be relative to the current working directory.
#'        Example:
#'```      
#'list(
#'     "NPL" = "npl_watermask.tif"
#'    )
#'``` 
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
#' @param output_dir Character vector containing the path to the directory for 
#'        writing output files. Default is the temp directory.
#' @param cores Integer vector containing an integer. Indicates the number of 
#'        cores to use in parallel when executing the function. If set to 0 
#'        \code{(max_number_of_cores - 1)}  will be used based on as many
#'        processors as the hardware and RAM allow. Default is \code{cores} = 0.
#' @param quant Logical vector indicating whether to produce the quantile 
#'        regression forests (TRUE) to generate prediction intervals.
#'        Default is \code{quant} = TRUE.
#' @param set_seed Integer, set the seed. Default is \code{set_seed} = 2010
#' @param fset Named list containing character vector elements that give the 
#'        path to the directory(ies) containing the random forest model objects 
#'        (.RData) with which we are using as a "fixed set" in this modeling, 
#'        i.e. are we parameterizing, in part or in full, this RF model run upon
#'        another country's(ies') RF model object. The list should have two 
#'        named character vectors, "final" and "quant", with the character 
#'        vectors corresponding to the directory paths of the corresponding 
#'        folders that hold the random forest model objects and the quantile 
#'        regression random forest model objects, respectively.
#'        Numerous model objects can be in each folder "./final/" and "./quant/"
#'        representing numerous countries with the understanding that the model 
#'        being run will incorporate all model objects in the folder, e.g. if 
#'        a model object for Mexico and 
#' @param fset_incl Logical vector indicating whether the RF model object 
#'        will or will not be combined with another RF model run upon another 
#'        country's(ies') RF model object. Default is \code{fset_incl} = FALSE
#' @param fset_cutoff Numeric vector containing an integer. This parameter is 
#'        only used if \code{fset_incl} is TRUE. If the country has less than 
#'        \code{fset_cutoff} admin units, then RF popfit will not be combined 
#'        with the RF model run upon another country's(ies') RF model object. 
#'        Default is \code{fset_cutoff} = 20.
#' @param fix_cov Logical vector indicating whether the raster extent of the 
#'        covariates will be corrected if the extent does not match mastergrid. 
#'        Default is \code{fix_cov} = FALSE.
#' @param check_result Logical vector indicating whether the results will be 
#'        compared with input data. Default is \code{check_result} = TRUE.
#' @param verbose Logical vector indicating whether to print 
#'        intermediate output from the function to the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log Logical vector indicating whether to print intermediate 
#'        output from the function to the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @param ...	Additional arguments:\cr 
#'        \code{binc}: Numeric. Increase number of blocks sugesting for 
#'        processing raster file.\cr 
#'        \code{boptimise}: Logical. Optimize total memory requires to 
#'        processing raster file by reducing the memory need to 35%.\cr
#'        \code{bsoft}: Numeric. If raster can be processed on less 
#'        then \code{cores} it will be foresed to use less number 
#'        of \code{cores}.\cr
#'        \code{nodesize}: Minimum size of terminal nodes. Setting this number larger 
#'        causes smaller trees to be grown (and thus take less time). See 
#'        \code{\link[randomForest]{randomForest}} for more details. Default 
#'        is \code{nodesize} = NULL and will be calculated 
#'        as \code{length(y_data)/1000}.\cr
#'        \code{maxnodes}: Maximum number of terminal nodes trees in the forest can have. 
#'        If not given, trees are grown to the maximum possible (subject to 
#'        limits by nodesize). If set larger than maximum possible, a warning is 
#'        issued. See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{maxnodes} = NULL.\cr 
#'        \code{ntree}: Number of variables randomly sampled as candidates at each split. 
#'        See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{ntree} = NULL and \code{ntree} will be used 
#'        \code{popfit$ntree}\cr
#'        \code{mtry}: Number of trees to grow. This should not be set to too small a 
#'        number, to ensure that every input row gets predicted at least a few 
#'        times. See \code{\link[randomForest]{randomForest}} for more details. 
#'        Default is \code{ntree} = NULL and \code{ntree} will be used 
#'        \code{popfit$mtry}.\cr
#'        \code{proximity}: Logical vector indicating whether proximity measures among 
#'        the rows should be computed. Default is \code{proximity} = TRUE. 
#'        See \code{\link[randomForest]{randomForest}} for more details.\cr
#'        \code{const}: Character vector containing the name of the file from which the 
#'        mask will be used to constraine population layer. The mask file should
#'         have value \code{0} as a mask. If it does not contain an absolute path, 
#'        the file name is relative to the current working directory.
#' @references      
#' \itemize{
#' \item Stevens, F. R., Gaughan, A. E., Linard, C. & A. J. Tatem. 2015. 
#'       Disaggregating Census Data for Population Mapping Using Random Forests 
#'       with Remotely-Sensed and Ancillary Data. PLoS ONE 10, e0107042  
#'       \doi{10.1371/journal.pone.0107042}
#' \item L. Breiman. 2001. Random Forests. Machine Learning, 45: 5-32. 
#'       \doi{10.1023/A:1010933404324}
#' \item Gaughan, A. E., Stevens, F. R., Linard, C., Patel, N. N., & A. J. Tatem. 
#'       2015. Exploring Nationally and Regionally Defined Models for Large Area 
#'       Population Mapping. International Journal of Digital Earth, 12(8): 
#'       989-1006. \doi{10.1080/17538947.2014.965761}
#' \item Sinha, P., Gaughan, A. E, Stevens, F. R., Nieves, J. J., Sorichetta, A., 
#'       & A. J. Tatem. 2019. Assessing the Spatial Sensitivity of a Random 
#'       Forest Model: Application in Gridded Population Modeling. Computers, 
#'       Environment and Urban Systems, 75: 132-145. 
#'       \doi{10.1016/j.compenvurbsys.2019.01.006}
#' }        
#' @importFrom randomForest varImpPlot
#' @importFrom raster nlayers
#' @rdname popRF
#' @return Raster* object of gridded population.
#' @export
#' @examples
#' \dontrun{
#' 
#' library("popRF")
#' 
#' pop_table <- list("NPL"="/user/npl_population.csv")
#' 
#' input_cov <- list(
#'                  "NPL"=list(
#'                             "cov1" = "covariate1.tif",
#'                             "cov2" = "covariate2.tif"))
#'                             
#'                  
#' input_mastergrid <- list("NPL" = "npl_mastergrid.tif")
#' input_watermask  <- list("NPL" = "npl_watermask.tif")
#' input_px_area    <- list("NPL" = "npl_px_area.tif")
#' 
#' res <- popRF(pop=pop_table, 
#'              cov=input_cov, 
#'              mastergrid=input_mastergrid, 
#'              watermask=input_watermask, 
#'              px_area=input_px_area, 
#'              output_dir="/user/output", 
#'              cores=4) 
#'  
#' # Plot populataion raster 
#' plot(res$pop) 
#' 
#' # Plot Error via Trees     
#' plot(res$popfit)
#'             
#' }
popRF <- function(pop, 
                  cov, 
                  mastergrid, 
                  watermask,
                  px_area,
                  output_dir=tempdir(), 
                  cores = 0, 
                  quant = FALSE,
                  set_seed=2010,
                  fset = NULL,
                  fset_incl = FALSE,
                  fset_cutoff = 20,
                  fix_cov=FALSE,
                  check_result=TRUE,
                  verbose = TRUE, 
                  log = FALSE, ...){
  
  timeStart <- Sys.time()

  rfg.initial.checks <- initial_check(cov,
                                      mastergrid,
                                      watermask,
                                      px_area,
                                      pop,
                                      output_dir, cores)
  
  
  options("pj.output.dir"=output_dir)
  
  if (rfg.initial.checks != TRUE ){
    
    message(rfg.initial.checks)
    stop_quietly()
    
  }
  

  log_info("MSG", paste(""), verbose=verbose, log=log)
  log_info("MSG", paste("Path for the location of the output is ", output_dir), 
           verbose=verbose, 
           log=log)
  log_info("MSG", paste(""), verbose=verbose, log=log)
  
  # get real physical cores
  if ( cores == 0 ){
    
    cores <- parallel::detectCores(logical = TRUE) - 1
    
    log_info("MSG", paste(""), verbose=verbose, log=log)
    #log_info("MSG", paste(replicate(48, "="), collapse = ""), verbose=verbose, log=log)
    log_info("MSG", paste0("Parameter cores is set to 0"), verbose=verbose, log=log)  
    log_info("MSG", 
             paste0("popRF will use maximum avalible cores on the PC which is ", 
                    cores), 
             verbose=verbose, 
             log=log)
    
    log_info("MSG", paste(""), verbose=verbose, log=log)
  } 


  #set a global variabal getOption("pj.output.dir")

  
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
  
  
  
  #get blokcs for parallel calculation of zonal stats
  
  # blocks_zs <- get_blocks_size(raster(covariates[[1]]$mastergrid$dataset_path), 
  #                              cores,
  #                              verbose=verbose, ...)
  
  
  census_data <- calculate_zonal_stats_covariates(covariates, 
                                                  rfg.output.path.countries.cvr, 
                                                  pop, 
                                                  save_zst=TRUE, 
                                                  cores=cores,
                                                  blocks = NULL,
                                                  verbose=verbose, 
                                                  log=log, ...)
  
  
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
                                         verbose=verbose)
    
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
  
  
  
  ### checking  arguments for RF
  ###
  args <- list(...);
  
  if ("nodesize" %in% names(args)){
    nodesize <- args[["nodesize"]]
  }else{
    nodesize <- NULL
  }
  
  if ("maxnodes" %in% names(args)){
    maxnodes <- args[["maxnodes"]]
  }else{
    maxnodes <- NULL
  }
  
  if ("ntree" %in% names(args)){
    ntree <- args[["ntree"]]
  }else{
    ntree <- NULL
  }  
  
  if ("mtry" %in% names(args)){
    mtry <- args[["mtry"]]
  }else{
    mtry <- NULL
  }    
  
  if ("proximity" %in% names(args)){
    proximity <- args[["proximity"]]
  }else{
    proximity <- TRUE
  }    

  
    
  
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
            set.seed(set_seed)
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
      
      set.seed(set_seed)
      popfit <- get_popfit(x_data, 
                           y_data, 
                           init_popfit, 
                           proximity, 
                           set_seed, 
                           verbose, 
                           log)
      
    }else{
      
      popfit_final_old <- get_popfit_final_old(fset = fset, 
                                               only.names=FALSE, 
                                               proximity = proximity, 
                                               verbose = verbose, 
                                               log = log)
      
      set.seed(set_seed)
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
    
    # popfit_final <- get_popfit_final(x_data, 
    #                                  y_data, 
    #                                  popfit,
    #                                  rfg.popfit.final.RData,
    #                                  proximity=proximity, 
    #                                  verbose=verbose, 
    #                                  log=log)
    # 
    # popfit_quant <- get_popfit_quant(x_data, 
    #                                  y_data, 
    #                                  popfit,
    #                                  rfg.popfit.quant.RData,
    #                                  proximity=proximity, 
    #                                  verbose=verbose, 
    #                                  log=log)
    
    set.seed(set_seed)
    popfit_final <- get_popfit_final(x_data=x_data, 
                                     y_data=y_data,
                                     nodesize=nodesize, 
                                     maxnodes=maxnodes,
                                     ntree=ntree,
                                     mtry=mtry, 
                                     set_seed=set_seed, 
                                     popfit,
                                     rfg.popfit.final.RData,
                                     proximity=proximity, 
                                     verbose=verbose, 
                                     log=log)
    
    set.seed(set_seed)
    popfit_quant <- get_popfit_quant(x_data=x_data, 
                                     y_data=y_data,
                                     nodesize=nodesize, 
                                     maxnodes=maxnodes,
                                     ntree=ntree,
                                     mtry=mtry, 
                                     set_seed=set_seed,                                     
                                     popfit,
                                     rfg.popfit.quant.RData,
                                     proximity=proximity, 
                                     verbose=verbose, 
                                     log=log)    
    

    log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
    
    importance_scores.log <- importance(popfit_final)[order(importance(popfit_final)[,1], decreasing=TRUE),]
    importance_scores.log <- as.data.frame(importance_scores.log)
    importance_scores.log <- cbind(rownames(importance_scores.log), importance_scores.log)
    rownames(importance_scores.log) <- NULL
    colnames(importance_scores.log) <- c("Covariate", "%IncMSE", "IncNodePurity")    
    
    write.csv(importance_scores.log, file.path(rfg.output.path.countries.tmp, 
                                           paste0("IncMSE_IncNodePurity_",rfg.countries.tag,".csv") ))
    
    # 
    # write.table((importance_scores.log), file.path(rfg.output.path.countries.tmp, 
    #                                                paste0("IncMSE_IncNodePurity_",rfg.countries.tag,".csv")), sep="," )
    
    if (verbose) print(importance_scores.log)
    
    log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
    
    log_info("MSG", paste0("Mean of squared residuals: ", round(popfit_final$mse[length(popfit_final$mse)], 3) ), 
             verbose=verbose, 
             log=log) 
    
    log_info("MSG", paste0("% Var explained: ", round(popfit_final$rsq[length(popfit_final$rsq)], 3) ), 
             verbose=verbose, 
             log=log)    
    
    log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
    log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
    
  }else{
    
    if (fset_incl==TRUE & (nrow(census_data) > fset_cutoff)) {
      
      # popfit_final <- get_popfit_final(x_data, 
      #                                  y_data, 
      #                                  popfit,
      #                                  rfg.popfit.final.RData,
      #                                  proximity=proximity, 
      #                                  verbose=verbose, 
      #                                  log=log)
      # 
      # popfit_quant <- get_popfit_quant(x_data, 
      #                                  y_data, 
      #                                  popfit,
      #                                  rfg.popfit.quant.RData,
      #                                  proximity=proximity, 
      #                                  verbose=verbose, 
      #                                  log=log)
      
      popfit_final <- get_popfit_final(x_data=x_data, 
                                       y_data=y_data,
                                       nodesize=nodesize, 
                                       maxnodes=maxnodes,
                                       ntree=ntree,
                                       mtry=mtry, 
                                       set_seed=set_seed,
                                       popfit,
                                       rfg.popfit.final.RData,
                                       proximity=proximity, 
                                       verbose=verbose, 
                                       log=log)
      
      popfit_quant <- get_popfit_quant(x_data=x_data, 
                                       y_data=y_data,
                                       nodesize=nodesize, 
                                       maxnodes=maxnodes,
                                       ntree=ntree,
                                       mtry=mtry, 
                                       set_seed=set_seed,                                       
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
                                             proximity=proximity, 
                                             verbose=verbose, 
                                             log=log)
    
    popfit.quant.old <- get_popfit_quant_old(fset, 
                                             only.names=FALSE, 
                                             proximity=proximity, 
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
  

  log_info("MSG", paste0("Creating the population density weighting layer."), verbose=verbose, log=log)   
  
  
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
                                             quant = quant)
        },
        error = function(e){          
          message( paste0("There was an error. ",e))
          stop_quietly()
        },
        warning = function(w){        
          message( paste0("There was an error message. ",w))
        },
        finally = {                   
          log_info("MSG", paste0("Completed prediction."), verbose=verbose, log=log) 
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
      
      # blocks <- get_blocks_size(covariate_stack_tmp, 
      #                           cores,
      #                           nl=nlayers(covariate_stack_tmp),
      #                           nt=popfit_final$ntree,
      #                           verbose = verbose, ...)
      
      #get blokcs for parallel calculation of zonal stats
      
      blocks_prediction <- get_blocks_size(covariate_stack_tmp,
                                           cores,
                                           nt=popfit_final$ntree,
                                           n=4,
                                           verbose=verbose, ...)      
      
      npoc_blocks <- ifelse(blocks_prediction$n < cores, blocks_prediction$n, cores)
      
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
                                                  blocks=blocks_prediction,
                                                  verbose=verbose, 
                                                  log=log)
      
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

  
  
  log_info("MSG", paste0("Start dasymetrically distribute census-level population counts"), 
           verbose=verbose, 
           log=log)
  
  
  # blocks <- get_blocks_size(census_mask, 
  #                           cores,
  #                           nl=2,
  #                           nt=1,
  #                           verbose = verbose, ...)   
  
  #get blokcs for parallel calculation of zonal stats
  
  blocks <- get_blocks_size(census_mask,
                            cores, 
                            verbose=verbose, ...) 
  
  npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores) 
  
  
  p_raster <- apply_pop_density(pop, 
                                censusmaskPathFileName, 
                                rfg.output.path.countries, 
                                cores=npoc_blocks, 
                                rfg.countries.tag, 
                                quant = quant, 
                                blocks=blocks, 
                                verbose=verbose, 
                                log=log)
  
  
  
  if ("const" %in% names(args)){
    const <- args[["const"]]
  }else{
    const <- NULL
  }    
  
  if (!is.null(const)) {
    
    p_raster_const <- apply_constrained(pop, 
                                        mastergrid_filename=censusmaskPathFileName,
                                        const=const,
                                        output_dir=rfg.output.path.countries, 
                                        cores=npoc_blocks, 
                                        rfg.countries.tag=rfg.countries.tag, 
                                        quant = quant, 
                                        blocks=blocks, 
                                        verbose=verbose, 
                                        log=log)
    
    c_result_const <- check_result_constrained(pop, 
                                               censusmaskPathFileName, 
                                               rfg.output.path.countries, 
                                               npoc_blocks, 
                                               rfg.countries.tag,  
                                               blocks=blocks, 
                                               verbose=verbose, 
                                               log=log)
    
  }  
  
  
  if (check_result){
    
    log_info("MSG", paste0("Checking results."), verbose=verbose, log=log)
    
    c_result <- check_result(pop, 
                             censusmaskPathFileName, 
                             rfg.output.path.countries, 
                             npoc_blocks, 
                             rfg.countries.tag,  
                             blocks=blocks, 
                             verbose=verbose, 
                             log=log)
    
    if (!is.null(const)) {
      
      return_results <- list(pop=p_raster, 
                             pop_const=p_raster_const, 
                             popfit=popfit_final, 
                             error= c_result,
                             error_const= c_result_const)  
    }else{
      
      return_results <- list(pop=p_raster, 
                             popfit=popfit_final, 
                             error= c_result)  
    }  
    
  }else{
    
    if (!is.null(const)) {
      
      return_results <- list(pop=p_raster,
                             pop_const=p_raster_const,
                             popfit=popfit_final)  
      
    }else{
      
      return_results <- list(pop=p_raster, 
                             popfit=popfit_final)  
      
    }  
    
  }
  
  
  
  timeEnd <-  Sys.time()
  
  log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
  log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
  
  log_info("MSG", paste0("popRF completed. "), verbose=verbose, log=log)
  log_info("MSG", paste0("Total processing Time: ", 
                         tmDiff(timeStart,timeEnd)), verbose=verbose, log=log)
  
  log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
  
  
  return(return_results)
  
}
