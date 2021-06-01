#' Function will return a number of blocks
#' sugesting for processing raster file. It will take into consideration
#' number of layers, cells, cores and avalible memory on computer
#' (not maximum memory but avalible)
#' @param x raster
#' @param cores number of cores
#' @param n parameter to increase requrement of the raster
#' @param number_type Will be used to estimate requred memory
#' @importFrom raster nlayers ncell
#' @rdname get_blocks_need
#' @return integer
#' @examples
#' \dontrun{
#' get_blocks_need(x, cores=2, n=1)
#' }
#' @noRd 
get_blocks_need <- function( x, cores, n=1, number_type = "numeric"){
  
  #stopifnot(hasValues(x))
  
  n <- n + nlayers(x) - 1
  cells <- round( 1.1 * ncell(x) ) * n
  #memneed <- cells * 8 * n / (1024 * 1024)
  
  if(number_type == "integer") {
    
    byte_per_number = 4
    
  } else if(number_type == "numeric") {
    
    byte_per_number = 8
    
  } else {
    
    #byte_per_number = .Machine$sizeof.pointer
    stop(sprintf("Unknown number_type: %s", number_type))
  }
  
  blocks <- 1
  
  memneed <- (cells * byte_per_number * n / (1024 * 1024 * 1024))/blocks
  
  memavail <- get_aval_memory()/cores
  
  while ((memneed > memavail)) {
    
    memneed <- (cells * byte_per_number * n / (1024 * 1024 * 1024))/blocks
    blocks <- blocks + 1
  }
  
  if ( blocks < cores) blocks <- cores
  
  return(blocks)
  
}