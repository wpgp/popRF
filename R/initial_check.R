#' Function to check the input arguments of popRF.It will be checked that 
#' input raster files exists.    
#' @importFrom methods is
#' @rdname initial_check
#' @param cov List contains a list of elements. Each element of a 
#'        list is another list object with a given name of the country, the 
#'        element of this list is the input covariates with the name of the 
#'        covariates and the path to them.
#' @param mastergrid List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input mastergrid with the path to the raster file. 
#' @param watermask List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input watermask with the path to the raster file.
#' @param px_area List with each element of a list is 
#'       another object with a given name of the country, the element of 
#'       this list is the input px_area with the path to the raster file.
#' @param pop the name of the file which the administrative ID and the population 
#'        values are to be read from. The file should contain two columns 
#'        comma-separated with the value of administrative ID and population 
#'        without columns names. If it does not contain an absolute path, the 
#'        file name is relative to the current working directory.
#' @param output_dir Path to the folder to save the outputs. 
#' @param cores is a integer. Number of cores to use when executing the function.
#' @param minblocks Integer. if \code{minblocks} is NULL then \code{minblocks} 
#'        for cluster prediction parallesation will be calculated based on 
#'        available memory.
#' @return TRUE if no errors found otherwsie error message will be returned.
#' @noRd 
initial_check <- function(cov, 
                          mastergrid, 
                          watermask,
                          px_area,
                          pop,
                          output_dir, 
                          cores, 
                          minblocks){
  
   
  # if ( !is(cores,  "numeric") & !is.null(cores) ){
  #   
  #   msg <- paste0("Error :: cores " , cores , " should be integer or NULL value")
  #   return(msg)
  # 
  # }
  
  if ( !is(cores,  "numeric") ){
    
    msg <- paste0("Error :: cores value ", cores ," should be integer value.")
    return(msg)
    
  }   
  
  # if ( is(cores,  "numeric")){
  #   if ( cores < 1 ){
  #     msg <- paste0("Error :: cores " , cores , " should be integer equal 1 or 2.. value")
  #     return(msg)
  #   }
  # } 

  
  
  # if ( !is(minblocks,  "numeric") & !is.null(minblocks)){
  #   
  #   msg <- paste0("Error :: minblocks ", minblocks , " should be integer or NULL value")
  #   return(msg)
  #   
  # } 
  # 
  # if ( is(minblocks,  "numeric")){
  #   if ( minblocks < 1 ){
  #     msg <- paste0("Error :: minblocks " , minblocks , " should be integer equal 1 or 2.. value")
  #     return(msg)
  #   }
  # } 
  
  
  if (is.null(output_dir) | !dir.exists(output_dir)) {
    
    msg <- paste0("Error :: Output directory ",output_dir," does not exsit. Please choose a different directory.")
    return(msg)
    
  }    
  
  if(!is.list( cov )) {
    
    msg <- paste0("Error :: Input parameter 'cov' should be a list.")
    return(msg)
    
  }
  
  if(!is.list( mastergrid )) {
    
    msg <- paste0("Error :: Input parameter 'mastergrid' should be a list.")
    return(msg)
    
  } 
  
  if(!is.list( watermask )) {
    
    msg <- paste0("Error :: Input parameter 'watermask' should be a list.")
    return(msg)
    
  } 
  
  if(!is.list( px_area )) {
    
    msg <- paste0("Error :: Input parameter 'px_area' should be a list.")
    return(msg)
    
  }   
  
  
  for ( i in names(cov) ) {
    
    for (ii in 1:length(cov[[i]]) ){
      fc <- cov[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Covariates file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(mastergrid) ) {
    
    for (ii in 1:length(mastergrid[[i]]) ){
      fc <- mastergrid[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Mastergrid file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(watermask) ) {
    
    for (ii in 1:length(watermask[[i]]) ){
      fc <- watermask[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Watermask file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }
  
  for ( i in names(px_area) ) {
    
    for (ii in 1:length(px_area[[i]]) ){
      fc <- px_area[[i]][[ii]]
      if(!file.exists( fc )) {
        
        msg <- paste0("Error :: Watermask file ", fc ," does not exist.")
        return(msg)
      }       
    }
  }  
  
  
  for ( i in names(pop) ) {
    
    fc <- watermask[[i]]
    if(!file.exists( fc )) {
      
      msg <- paste0("Error :: Population data file ", fc ," does not exist.")
      return(msg)
    }       
    
  }  
  

  
  return(TRUE)
  
}
