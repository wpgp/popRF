#
#' Load population data from input
#'
#' @param icountry ISO of the country
#' @param input_poptables Path to the csv file with pop data
#' @rdname load_pop
#' @return A data.frame with the populations for each zone
#' @noRd 
load_pop <- function(icountry,input_poptables){
  
  file_local <- input_poptables[[icountry]]
  df <- utils::read.csv(file_local, stringsAsFactors=FALSE, header = FALSE)
  colnames(df) <-  c("ADMINID", "ADMINPOP") 
  
  return(df)
  
}

#
#' get_pop_census_all load population data from input
#'
#' @param x input poptables
#' @rdname get_pop_census_all
#' @return A data.frame with the populations
#' @noRd 
get_pop_census_all  <- function(x) {
  
  c <- names(x)
  
  census_data <- load_pop(c[[1]], x)
  
  for ( i in length(c) ) {
    if (i==1) next()
    census_data <- rbind(census_data, load_pop(c[[i]], x)) 
  }
  
  return(census_data)  
}