#' @param nrows number in raster object
#' @param n number of  chancks
#' @rdname get_blocks
#' @return list
#' @examples
#' \dontrun{
#' get_blocks(nrows, n=2)
#' }
#' @noRd
#' 
get_blocks <- function(nrows, n){
  
  chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
  
  mblocks <- list()
  
  if (n == 1) {
    
    mblocks$row[1] <- 1 
    mblocks$nrows[1] <- nrows
    mblocks$n <- 1
    
    return(mblocks)
    
  }
  
  ch <- chunk2(1:nrows,n)
  
  
  for (i in 1:n){
    
    if (i == 1){
      mblocks$row[i] <- 1 
    }else{
      mblocks$row[i] <- mblocks$row[[i-1]] + length(ch[[i-1]])  
    }
    
    mblocks$nrows[i] <- length(ch[[i]])
    
  }
  
  mblocks$n <- n
  
  return(mblocks)
  
}


#' @param memavail total memory avalible
#' @param totalmem_need total memory need
#' @param blks number of  blocks
#' @param maxmem.mg integer. The maximum amount of memory (in Mb) to use for a 
#'        given operation, defaults to 5 billion bytes (4.66 GB)
#' @param verbose Logical vector indicating whether to print 
#'        intermediate output from the function to the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE
#' @rdname log_mem_info
#' @examples
#' \dontrun{
#' log_mem_info(nrows, n=2)
#' }
#' @noRd
#' 
log_mem_info <- function(memavail, maxmem.mg, totalmem_need, blks, verbose){
  
  log <- TRUE
  
  log_info("MSG", paste(replicate(48, "-"), collapse = ""), verbose=verbose, log=log)
  
  if (blks==1){
    
    log_info("MSG", paste0("Total avalible memory: ", 
                           round(memavail, 2), " Mb" ), 
             verbose=verbose, 
             log=log)
    
    log_info("MSG", paste0("Max memory to use for operation: ", 
                           round(maxmem.mg, 2), " Mb" ), 
             verbose=verbose, 
             log=log)    
    
    log_info("MSG", paste0("Memory needs to proccess raster: ", 
                           round(totalmem_need, 2), " Mb" ), 
             verbose=verbose, 
             log=log)    
    
    
  }else{
    
    log_info("MSG", paste0("Total avalible memory per core: ", 
                           round(memavail, 2), " Mb" ), 
             verbose=verbose, 
             log=log)
    
    log_info("MSG", paste0("Max memory to use for operation: ", 
                           round(maxmem.mg, 2), " Mb" ), 
             verbose=verbose, 
             log=log)      
    
    log_info("MSG", paste0("Memory needs per block of data: ", 
                           round(totalmem_need, 2), " Mb" ), 
             verbose=verbose, 
             log=log)    
    
  }
  

  
  log_info("MSG", paste(replicate(48, "-"), collapse = ""),
           verbose=verbose, log=log)
  log_info("MSG", paste0("Total number of blocks: ", blks),
           verbose=verbose, log=log)
  log_info("MSG", paste(replicate(48, "-"), collapse = ""),
           verbose=verbose, log=log)
  
}




#' Function will return a number of blocks suggested for processing raster file.
#' Indicates the minimum number of blocks to break the processing extent into 
#' for parallel processing. It will take into consideration number of layers, 
#' cells, cores and avalible memory on computer
#' binc number parameter to increase requrement of the raster
#' @param x raster
#' @param cores number of cores
#' @param nt number of trees used in RF training
#' @param n number of  layers
#' @param verbose Logical vector indicating whether to print 
#'        intermediate output from the function to the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @importFrom utils object.size getFromNamespace
#' @importFrom raster nlayers ncell
#' @rdname get_blocks_size
#' @return integer
#' @examples
#' \dontrun{
#' get_blocks_size(x, cores=2)
#' }
#' @noRd 
get_blocks_size <- function(x, 
                            cores,
                            nt=1,
                            n=1,
                            verbose=T,
                            ...){
  
  args <- list(...);
  
  if ("binc" %in% names(args)){
    binc <- args[["binc"]]
  }else{
    binc <- 1
  }
  
  if ("boptimise" %in% names(args)){
    boptimise <- args[["boptimise"]]
  }else{
    boptimise <- FALSE
  }
  
  if ("bsoft" %in% names(args)){
    bsoft <- args[["bsoft"]]
  }else{
    bsoft <- FALSE
  }
  
  # importing .availableRAM and .maxmemory function from raster package
  #
  maxmemory.rf <- getFromNamespace(".maxmemory", "raster")
  availableRAM.rf <- getFromNamespace(".availableRAM", "raster")
  
  
  # maxmem.mg: integer. The maximum amount of memory (in bytes) to use for a 
  # given operation, defaults to 5 billion bytes (4.66 GB)

  if ("maxmem.mg" %in% names(args)){
    maxmem.mg <- args[["maxmem.mg"]]
  }else{
    #maxmem.mg <- 4768.372
    #maxmem.mg <- raster:::.maxmemory() /(1024 * 1024 )
    maxmem.mg <- maxmemory.rf() /(1024 * 1024 )
  }
  
  
  blocks <- list()
  # get aval memory in bytes
  rs <- as.integer(object.size(x))
  
  
  #.availableRAM(maxmem)
  #memavail.bytes <- (get_aval_memory() * 1073741824 ) - (rs*cores)
  
  memavail.bytes <- (availableRAM.rf(maxmemory.rf())) - (rs*cores)
  
  memavail.mg <- memavail.bytes /(1024 * 1024 )
  
  # to be on safe side 65% is applied to a total avalible memory on PC
  if (boptimise){
    memavail.mg <- (memavail.mg * 65 )/100
  }
  
  # memory aval per core
  memavail_core.mg <- ceiling(memavail.mg /(cores))
  
  #nl = n * nlayers(x) 
  
  nl = n 
  
  totalmem_need.mg <- ((ncell(x) * 8 * nl * nt * binc)/(1024 * 1024 ))
  
  
  if (bsoft  &  totalmem_need.mg < maxmem.mg & totalmem_need.mg < memavail_core.mg){
    
    blocks$row[1] <- 1 
    blocks$nrows[1] <- nrow(x)
    blocks$n <- 1
    
    if (verbose){
      log_mem_info(memavail_core.mg, 
                   maxmem.mg, 
                   totalmem_need.mg, 
                   1, 
                   verbose=verbose)
    }    
    
    return(blocks)
    
  }
  
  can_be_run <- FALSE
  
  if (bsoft){
    bl <- 2  
  }else{
    bl <- cores
  }
  

  while(!can_be_run){

    blocks_nrows <- nrow(x)/bl
    
    nc <- ceiling(blocks_nrows) * ncol(x) 
    
    memneed <- ( (nc  * 8 * nl * nt * binc) / (1024 * 1024 ) )
    
    memavail <- min(memavail_core.mg, maxmem.mg)
    
    if (memneed < memavail) {
      can_be_run <- TRUE
    }else{
      bl <- bl + 1  
    }  
    
    if (bl == nrow(x)) can_be_run <- TRUE
    
  }
  
  blocks <- get_blocks(nrow(x), bl)
  
  if (verbose){

    log_mem_info(memavail_core.mg, 
                 maxmem.mg, 
                 memneed, 
                 blocks$n, 
                 verbose=verbose)
    
   }  
  
  
  return(blocks)
}




