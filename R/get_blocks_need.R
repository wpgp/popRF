#' Function will return true or false based on block can be run in memory
#' @param x raster object or raster stack
#' @param nc number of cells
#' @param nl number of  laeyrs
#' @param nt number of tree used in RF training
#' @param rs number size of raster object in byte
#' @param verbose Logical vector indicating whether to print 
#'        intermediate output from the function to the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @rdname canRunInMemory
#' @return logical
#' @examples
#' \dontrun{
#' canRunInMemory(cores=2, nc=100, nl=1, nt=1, rs=100, verbose=T)
#' }
#' @noRd 
canRunInMemory <- function(cores, nc, nl=4, nt=1, rs, verbose=T) {
  
  if (nt > 1) nl <- 1
  memneed <- (nc * nl * nt * 8)
  
  # maxmemory: the maximum amount of memory (in bytes) to 
  # use for a given operation, defaults to 5 billion bytes (4.66 GB)
  #maxmem <- .maxmemory()
  
  #memavail <- (Rcpp::.availableRAM(maxmem) - rs)/cores
  
  # get aval memory in bytes
  memavail <- (get_aval_memory() * 1073741824 - rs) /cores
  
  if (nc > (2^31 -1)) return(FALSE)
  
  # can't use all of it; default is 60%
  #memavail <- .memfrac() * memavail
  
  # the below allows you to safely set a high maxmem
  # but still limit total mem use
  #memavail <- min(memavail, maxmem)
  
  if (memneed > memavail) {
    return(FALSE)
  } else {
    
    if (verbose) {
      
      log <- TRUE
      
      mb <- 1048576
      
      log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
      
      log_info("MSG", paste0("Total avalible memory per core: ", 
                             round(memavail / mb, 2), " Mb" ), 
               verbose=verbose, 
               log=log)
      log_info("MSG", paste0("Memory needs per block of data: ", 
                             round(memneed / mb, 2), " Mb" ), 
               verbose=verbose, 
               log=log)
      
      log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
      
    }
    
    return(TRUE)
  }
  
}

#' Function will return a number of blocks
#' sugesting for processing raster file. Indicates the minimum number of blocks 
#' to break the processing extent into for parallel processing. 
#' It will take into consideration number of layers, cells, cores and avalible 
#' memory on computer
#' binc number parameter to increase requrement of the raster
#' 
#' @param x raster
#' @param cores number of cores
#' @param nl number of  laeyrs
#' @param nt number of trees used in RF training
#' @param number_type Will be used to estimate requred memory
#' @param verbose Logical vector indicating whether to print 
#'        intermediate output from the function to the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @importFrom utils object.size
#' @importFrom raster nlayers
#' @rdname get_blocks_size
#' @return integer
#' @examples
#' \dontrun{
#' get_blocks_size(x, cores=2, n=1)
#' }
#' @noRd 
get_blocks_size <- function(x, 
                            cores,
                            nl=1,
                            nt=1,
                            verbose=T, ...){
  
  args <- list(...);
  
  if ("binc" %in% names(args)){
    binc <- args[["binc"]]
  }else{
    binc <- 1
  }
  
  canProcess <- FALSE
  
  minblocks <- raster::blockSize(x)$n
  
  while (!canProcess) {
    
    blocks <- raster::blockSize(x, minblocks=minblocks)
    
    #get number of cells in block
    nc <- blocks$nrows[[1]] * ncol(x)
    
    #get number of layers +  optional increment
    if (nl > nlayers(x)){
      nl <- nl * binc
    }else{
      nl <- nlayers(x) * binc  
    }
    
    
    #get the size of the raster object multiply
    rs <- as.integer(object.size(x))
    
    canProcess <- canRunInMemory(cores,
                                 nc, 
                                 nl,
                                 nt,
                                 rs, 
                                 verbose = T)
    minblocks <- blocks$n + 1
    
    
  }
  
  if (verbose){
    log <- TRUE
    log_info("MSG", paste0("Total number of blocks: ", blocks$n), verbose=verbose, log=log)
    log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
  }
  
  return(blocks)
  
}
