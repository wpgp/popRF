#' create_covariates_list
#' 
#' @param input_covariates Input list of covariates
#' @param input_mastergrid Input list to mastergrid
#' @param input_watermask Input list to watermask
#' @param input_px_area Input list to population
#' @rdname create_covariates_list
#' @return A list of covariates
#' @examples
#' \dontrun{
#' create_covariates_list( input_covariates, 
#'                         input_covariates, 
#'                         input_watermask, 
#'                         input_px_area  )
#' }
create_covariates_list <- function(input_covariates,
                                   input_mastergrid,
                                   input_watermask,
                                   input_px_area){
  
  
  covariates <- list()
  
  # i = "GHA" 
  # j = "gha_dist_cities" 
  for ( i in names(input_covariates) ) {
    
    for (j in names(input_covariates[[i]]) ){
      
      fc <- input_covariates[[i]][[j]]
      
      
      covariates[[i]][[j]][["path"]] <- fc
      
      covariates[[i]][[j]] <- list(
        dataset_folder      =  dirname(fc),
        dataset_filename    =  basename(fc),
        dataset_description =  j,
        dataset_summary     =  "mean",
        dataset_country     = i,
        dataset_class       = j,
        dataset_path        = fc 
      )
      
      
      
    }
  }
  
  
  # input_mastergrid
  #
  for ( i in names(input_mastergrid) ) {
    
    for (j in 1:length(input_mastergrid[[i]]) ){
      
      fc <- input_mastergrid[[i]][[j]]
      
      
      covariates[[i]][["mastergrid"]][["path"]] <- fc
      
      covariates[[i]][["mastergrid"]] <- list(
        dataset_folder      =  dirname(fc),
        dataset_filename    =  basename(fc),
        dataset_description =  "mastergrid",
        dataset_summary     =  "sum",
        dataset_country     = i,
        dataset_class       = "mastergrid",
        dataset_path        = fc 
      )
    }
  }  
  
  
  
  # rfg.water.mask
  #
  for ( i in names(input_watermask) ) {
    
    for (j in 1:length(input_watermask[[i]]) ){
      
      fc <- input_watermask[[i]][[j]]
      
      
      covariates[[i]][["watermask"]][["path"]] <- fc
      
      covariates[[i]][["watermask"]] <- list(
        dataset_folder      =  dirname(fc),
        dataset_filename    =  basename(fc),
        dataset_description =  "watermask",
        dataset_summary     =  "sum",
        dataset_country     = i,
        dataset_class       = "watermask",
        dataset_path        = fc 
      )
    }
  }
  
  # input_px_area
  #
  for ( i in names(input_px_area) ) {
    
    for (j in 1:length(input_px_area[[i]]) ){
      
      fc <- input_px_area[[i]][[j]]
      
      
      covariates[[i]][["px_area"]][["path"]] <- fc
      
      covariates[[i]][["px_area"]] <- list(
        dataset_folder      =  dirname(fc),
        dataset_filename    =  basename(fc),
        dataset_description =  "px_area",
        dataset_summary     =  "sum",
        dataset_country     = i,
        dataset_class       = "px_area",
        dataset_path        = fc 
      )
    }
  }  
  
  
  return(covariates)
  
}


#' create_covariates_list_for_RF
#' 
#' @param covariates Input list of covariates
#' @return A list of covariates
#' @examples
#' \dontrun{
#' create_covariates_list_for_RF( covariates )
#' }
create_covariates_list_for_RF <- function(covariates){
  
  covariates.new <- list()
  
  cname <- names(covariates[[1]])
  
  for (i in cname) {
    
    dstfile <- covariates[[1]][[i]]$dataset_path
    rtype <- covariates[[1]][[i]]$dataset_summary
    
    covariates.new[[ i ]] <- list(
      dataset_folder      = dirname(dstfile),
      dataset_name        = i,
      dataset_description = i,
      dataset_summary     = rtype,
      path     = dstfile
    )       
    
    
  }  
  
  
  return(covariates.new)
  
}

#' get_covariates_var_names This function retrieves variable names 
#' from the given covariates. Create a vector to hold the covariate 
#' variable names:
#' 
#' @param x Input list of covariates
#' @return A list of covariates
#' @examples
#' \dontrun{
#' get_covariates_var_names( covariates )
#' }
get_covariates_var_names <- function(x){
  
  covariates.var.names <- c()
  
  ##  For every covariate in the covariates object:
  for ( icvritm in 1:length(x) ) { 
    ##  Retrieve that covariate object:
    covariate <- x[[icvritm]]
    ##  Retrieve the dataset_name attribute:
    var_name_class <- covariate[['dataset_name']]
    ##  Append that variable name to the covariates.var.names vector:
    covariates.var.names <- c(covariates.var.names, var_name_class)
  }    
  ##  Sort those names:
  sort(covariates.var.names)
  ##  Return the names vector:
  return(covariates.var.names)
}
