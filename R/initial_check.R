# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  June 2021
# Version 0.1
#
#' stop_quietly exit program without error
#' @importFrom methods is
#' @rdname initial_check
#' @param input_covariates list of covariates
#' @param input_mastergrid list of mastergrid
#' @param input_watermask list of watermask 
#' @param input_px_area list of px_area 
#' @param input_poptables list of population table
#' @param output_dir name of the folder
#' @param nrpoc Number of process to use in script
#' @param minblocks min number of blocks for cluster
#' @return stop execution of package
initial_check <- function(input_covariates, 
                          input_mastergrid, 
                          input_watermask,
                          input_px_area,
                          input_poptables,
                          output_dir, nrpoc, minblocks){
  
   
  if ( !is(nrpoc,  "numeric") & !is.null(nrpoc) ){
    
    msg <- paste0("Error :: nrpoc " , nrpoc , " should be integer or NULL value")
    return(msg)

  } 
  
  if ( is(nrpoc,  "numeric")){
    if ( nrpoc < 1 ){
      msg <- paste0("Error :: nrpoc " , nrpoc , " should be integer equal 1 or 2.. value")
      return(msg)
    }
  } 

  
  
  if ( !is(minblocks,  "numeric") & !is.null(minblocks)){
    
    msg <- paste0("Error :: minblocks ", minblocks , " should be integer or NULL value")
    return(msg)
    
  } 
  
  if ( is(minblocks,  "numeric")){
    if ( minblocks < 1 ){
      msg <- paste0("Error :: minblocks " , minblocks , " should be integer equal 1 or 2.. value")
      return(msg)
    }
  } 
  
  
  if (is.null( output_dir ) | !file.exists( output_dir )) {
    
    msg <- paste0("Error :: Output directory ",output_dir," does not exsit. Please choose a different directory.")
    return(msg)
    
  }    
  
  if(!is.list( input_covariates )) {
    
    msg <- paste0("Error :: Input parameter 'input_covariates' should be a list.")
    return(msg)
    
  }
  
  if(!is.list( input_mastergrid )) {
    
    msg <- paste0("Error :: Input parameter 'input_mastergrid' should be a list.")
    return(msg)
    
  } 
  
  if(!is.list( input_watermask )) {
    
    msg <- paste0("Error :: Input parameter 'input_watermask' should be a list.")
    return(msg)
    
  } 
  
  if(!is.list( input_px_area )) {
    
    msg <- paste0("Error :: Input parameter 'input_px_area' should be a list.")
    return(msg)
    
  }   
  
  
  for ( i in names(input_covariates) ) {
    
    for (ii in 1:length(input_covariates[[i]]) ){
      fc <- input_covariates[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Covariates file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(input_mastergrid) ) {
    
    for (ii in 1:length(input_mastergrid[[i]]) ){
      fc <- input_mastergrid[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Mastergrid file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(input_watermask) ) {
    
    for (ii in 1:length(input_watermask[[i]]) ){
      fc <- input_watermask[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Watermask file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(input_px_area) ) {
    
    for (ii in 1:length(input_px_area[[i]]) ){
      fc <- input_px_area[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Watermask file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }  
  
  
  for ( i in names(input_poptables) ) {
    
    fc <- input_watermask[[i]]
    if(!file.exists( fc )) {
      
      msg <- paste0("Error :: Population data file ", fc ," does not exist.")
      return(msg)
    }       
    
  }  
  

  
  return(TRUE)
  
}
