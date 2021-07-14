#' @title rasterize_parallel_start function will return a string with OS
#' @param x Raster* object
#' @param df data.frame of points
#' @param blocks number of blocks sugesting for processing raster file.
#' @param NAflag NO data value will be used for a new raster
#' @param datatype Type of raster. Available are 
#'        INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S
#' @param filename File of a new raster file.
#' @param overwrite Overwrite existing file
#' @param silent If FALSE then the progress will be shown
#' @rdname rasterize_parallel_start
#' @return Raster* object
#' @noRd 
rasterize_parallel_start <- function(x, 
                                     df, 
                                     blocks, 
                                     NAflag, 
                                     datatype, 
                                     filename, 
                                     overwrite=TRUE, 
                                     silent=TRUE) {
  
  tStart <- Sys.time()
  
  layernames <- names(x)
  
  cat('\n')
  cat('Rasterising')
  # To let user know how many blocks will be used
  cat(paste0('\nTotal blocks ',blocks$n))
  cat('\n')
  
  cl <- raster::getCluster()
  
  #on.exit( returnCluster() )
  nodes <- length(cl)

  
  clusterExport(cl, c("blocks", "x","df", "silent"), envir=environment())
  
  
  clRasteriseFun <- function(i) {
    # tryCatch({
    v <- data.frame( raster::getValues(x, row=blocks$row[i], nrows=blocks$nrows[i]) )
    colnames(v) <- c("v1")
    colnames(df) <- c("v1","v2")
    v <- plyr::join(v,df,type="left",by = "v1")[-1]
    
    return(v[[1]])
  }
  
  # get all nodes going
  for (i in 1:nodes) {
    snow::sendCall(cl[[i]], clRasteriseFun, i, tag=i)
  }
  
  #out <- raster:::setValues(x, 0)
  out <- x
  
  out <- raster::writeStart(out, 
                            filename=filename, 
                            format="GTiff", 
                            datatype=datatype, 
                            overwrite=overwrite, 
                            options=c("COMPRESS=LZW"), 
                            NAflag=NAflag)
  
  for (i in 1:blocks$n) {
    
    d <- snow::recvOneData(cl)
    
    if (!d$value$success) {
      stop('cluster error')
    }
    
    tEnd <-  Sys.time()
    
    b <- d$value$tag
    
    if ((silent == FALSE) & (i%%10 == 0) )   {
      
      progress_message(x=i, 
                       max=blocks$n, 
                       label=paste0("received block ", 
                                    i, 
                                    " Processing Time: ", 
                                    tmDiff(tStart,tEnd)
                       )
      )     
      
    }  
    
    out <- raster::writeValues(out, d$value$value, blocks$row[b])
    
    # need to send more data
    #
    ni <- nodes + i
    if (ni <= blocks$n) {
      snow::sendCall(cl[[d$node]], clRasteriseFun, ni, tag=ni)
    }
  }
  
  out <- raster::writeStop(out)
  
  return(out)
}


#' @title Function will transfer values associated with 'object' type spatial 
#' data (data.frame) to raster cells. Function is using parallel library to work 
#' with a big raster data. The raster file will be split to blocks and 
#' processed per block.
#'
#' @author Maksym Bondarenko <mb4@soton.ac.uk> and 
#'        Chris Jochem <W.C.Jochem@soton.ac.uk>
#' @usage rasterize_parallel(x, df, cores=NULL, blocks=NULL, NAflag=NULL, 
#'                           datatype=NULL, filename=rasterTmpFile(), 
#'                           overwrite=TRUE, silent=TRUE)
#' @param x Raster* object.
#' @param df data.frame of points.
#' @param cores is a integer. Number of cores to use when executing the 
#'        function in paralle.
#' @param blocks number of blocks sugesting for processing raster file.
#' @param NAflag NO data value will be used for a new raster.
#' @param datatype Type of raster. Available are 
#'        INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S.
#' @param filename the name of the raster file.
#' @param overwrite Overwrite existing file.
#' @param silent is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. If FALSE then the progress will be shown. 
#'        Default is \code{silent} = TRUE.
#' @importFrom raster getValues writeRaster writeStart writeStop compareRaster 
#'             hasValues writeValues blockSize  getCluster returnCluster 
#'             endCluster beginCluster rasterTmpFile
#' @importFrom stats complete.cases predict sd aggregate
#' @importFrom utils stack
#' @importFrom doParallel registerDoParallel 
#' @importFrom parallel detectCores 
#' @importFrom snow sendCall sendCall recvOneData clusterEvalQ clusterExport
#' @importFrom foreach '%dopar%' foreach
#' @rdname rasterize_parallel
#' @return Raster* object
#' @examples
#' \dontrun{
#' rasterize_parallel(x=rasterObj, df=df, cores=2,NAflag=-99999,datatype='INT1U' )
#' }
#' @noRd 
rasterize_parallel <- function(x, 
                               df, 
                               cores=NULL, 
                               blocks=NULL, 
                               NAflag=NULL, 
                               datatype=NULL, 
                               filename=rasterTmpFile(), 
                               overwrite=TRUE, 
                               silent=TRUE) {
  
  stopifnot(hasValues(x))
  
  # get real physical cores
  max.cores <- detectCores(logical = TRUE)
  
  if (is.null(NAflag)) NAflag=-99999
  if (is.null(datatype)) datatype='FLT4S'
  
  # if user did not tell how many cores to use then max-1 will be taken
  #
  if (is.null(cores)) {
    cores <- max.cores - 1
  }
  

  if (cores > max.cores) {
    stop(paste0("Number of cores ",cores," more then real physical cores in PC ",max.cores ))
  }
  
  if (!is.data.frame(df)) stop(paste0("df should be a data.frame"))
  if (!is(NAflag, "numeric")) stop(paste0("NAflag should be  numeric"))
  if (!is(cores,  "integer")) stop(paste0("cores should be integer value"))
  if (!is(overwrite, "logical")) stop(paste0("overwrite should be  logical (e.g., TRUE, FALSE)"))
  if (!is(silent, "logical")) stop(paste0("silent should be logical (e.g., TRUE, FALSE)"))
  
  datatype <- toupper(datatype)
  
  if (!(datatype %in% c('INT1S', 'INT2S', 'INT4S', 'FLT4S', 'LOG1S', 'INT1U', 'INT2U', 'INT4U', 'FLT8S'))) {
    stop('not a valid data type. Avalible are INT1S/INT2S/INT4S/FLT4S/LOG1S/INT1U/INT2U/INT4U/FLT8S')
  }
  
  if (!file.exists(dirname(filename))){
    stop(paste0("Directory  ",dirname(filename)," for file ", basename(filename) ," does not exist"))
  }
  
  if ( file.exists(filename) & overwrite==FALSE) {
    stop(paste0("File ",filename," exist. Use option overwrite=TRUE"))
  } else{
    if ( file.exists(filename) ) file.remove(filename)
  }
  
  if (cores > max.cores) {
    stop(paste0("Number of cores ",cores," more then real physical cores in PC ",max.cores ))
  }
  
  # if user did not tell how many blocks to use then blocks will
  # calculated by get_blocks_need() function
  #
  if (is.null(blocks)) {
    
    blocks <- get_blocks_size(x, 
                              cores,
                              nl=2,
                              nt=1,
                              verbose = ifelse(silent, FALSE, TRUE))      
  }  

  npoc_blocks <- ifelse(blocks$n < cores, blocks$n, cores)  
  
  beginCluster(n=npoc_blocks)
  
  out <- rasterize_parallel_start(x, df, blocks, NAflag, datatype, filename, overwrite, silent)
  
  endCluster()
  
  return(out)
}
