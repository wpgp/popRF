#' This function to apply to a datafram.
#' @param x dataframe 
#' @param y number
#' @param z number
#' @rdname dis_delta
#' @return dataframe
#' @noRd 
dis_delta <- function(x, y , z){
  # x <- df
  # y <- t_05
  
  while (y>1.1) {
    
    for (i in 1:nrow(x)) {
      
      fnb <- x[i,"pop"]
      
      if (fnb < z ) next
      
      dec <- fnb - trunc(fnb)
      
      decs <- 1 - dec  
      
      if (y > 0 & decs <= y ){
        
        x[i,"pop"] <- x[i,"pop"] + decs
        
        y <- y - decs
        
      }
      
    }  
    
  }
  return(x)
}

#' This function to apply to a datafram.
#' @param df dataframe 
#' @return dataframe
#' @noRd 
process_admin <- function(df){
  
  df$prv <- 0
  df$inb <- 0
  df$delta <- 0
  df$ac <- 0 
  
  delta <- 0
  
  for (i in 1:nrow(df)) {
    
    fnb <- df[i,"pop"]
    
    if (fnb == 0 ) next
    
    delta <- delta * (-1)
    prv <- fnb + delta
    
    dec <- prv - trunc(prv)
    
    if (dec <= 0.5){
      round_prv <- round(prv)
    }else{
      round_prv <- ceiling(prv)
    }
    
    df[i,"prv"] <- prv
    df[i,"inb"] <- round_prv
    
    delta <- round_prv - prv
    df[i,"delta"] <- delta
    
  }  
  return(df)
}


#' Applying integer type to population raster using single core.
#'
#' @param x RasterLayer object with codes representing zones 
#' @param y RasterLayer population
#' @param df_pop dataframe containing the name of the file from which the 
#'        unique area ID and corresponding population
#' @param fn Character file name output
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @noRd 
#' @rdname apply_int_type
#' @return Raster* object
#' @examples
#' \dontrun{
#' apply_int_type(x, 
#'                y,
#'                df_pop, 
#'                fn)
#' }
apply_int_type <- function(x, 
                           y, 
                           df_pop, 
                           fn,
                           verbose=FALSE, 
                           log=FALSE){ 
  
  
  
  
  tStart <- Sys.time()
  
  rst_tmp <- y
  rst_tmp[] <- NA
  
  for (i in 1:nrow(df_pop)){
    
    ADMINID <- df_pop[i,"ADMINID"]
    ADMINPOP <- df_pop[i,"ADMINPOP"]
    
    df <- data.frame(which(x[]==ADMINID))
    
    names(df) <- c("idx")
    df$pop <- y[df$idx] 
    df <- na.omit(df)
    
    if (nrow(df)==0){
      warning(paste0("AdminID ", ADMINID, " has no urban pixels. Will be skipped"))
      next
    }
    
    #df <- df[ with(df, order(pop,decreasing = T)),]
    df <- df[order(df$pop, decreasing = T),]
    total  <- sum(df$pop)
    eps <- 0.5
    
    t_05 <- sum(df[df$pop < eps,"pop"])
    eps_l <- length(df[df$pop > eps,"pop"])
    
    eps_d <- 0.01
    while (eps_l < t_05) {
      
      t_05 <- sum(df[df$pop < eps,"pop"])
      eps_l <- length(df[df$pop > eps,"pop"])
      eps <- eps - eps_d
      
      if (eps < 0){
        eps <- 0.1
        t_05 <- sum(df[df$pop < eps,"pop"])
        eps_l <- length(df[df$pop > eps,"pop"])   
        eps_d <- 0.001
      }
      
    }
    
    t_05 <- sum(df[df$pop < eps,"pop"])
    
    df[df$pop < eps,"pop"] <- 0  
    
    df <- dis_delta(df, t_05 , eps)
    
    out_df <- process_admin(df)
    
    raster::values(rst_tmp)[out_df$idx] = out_df$inb
    
    if ((verbose) & (i%%10 == 0) )   {
      
      tEnd <-  Sys.time()
      
      progress_message(x=i, 
                       max=nrow(df_pop), 
                       label=paste0("Working on ", 
                                    i, " out of ", nrow(df_pop), 
                                    " Processing Time: ", 
                                    tmDiff(tStart,tEnd)
                       )
      )     
      
    }  
    
  }
  
  
  writeRaster(rst_tmp, 
              filename=fn, 
              format="GTiff", 
              datatype="INT4S", 
              overwrite=TRUE, 
              options=c("COMPRESS=LZW"))
  
  return(invisible(NULL))
  
}


#' Applying integer type to pop raster in parallel.
#'
#' @param x RasterLayer object with codes representing zones 
#' @param y RasterLayer population
#' @param df_pop dataframe containing the name of the file from which the 
#'        unique area ID and corresponding population
#' @param fn Character file name output
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom doParallel registerDoParallel 
#' @importFrom parallel detectCores 
#' @noRd 
#' @rdname apply_int_type_paralle
#' @return Raster* object
#' @examples
#' \dontrun{
#' apply_int_type_paralle(x, 
#'                        y,
#'                        df_pop, 
#'                        fn)
#' }
apply_int_type_paralle <- function(x, 
                                   y, 
                                   df_pop, 
                                   fn,
                                   cores,
                                   verbose=FALSE, 
                                   log=FALSE){ 
  
  
  
  rst_tmp <- y
  rst_tmp[] <- NA
  
  
  blocks <- nrow(df_pop) 
  
  tStart <- Sys.time()   
  
  beginCluster(n=cores)
  
  cl <- getCluster()
  #on.exit( returnCluster() )  
  
  nodes <- length(cl)
  
  clusterEvalQ(cl, {
    require(raster)
  })  
  clusterExport(cl, c("x","y","df_pop","fn"), envir=environment())
  clusterExport(cl, c("process_admin"), envir=environment())
  clusterExport(cl, c("dis_delta"), envir=environment())
  
  recvOneData <- getFromNamespace("recvOneData", "parallel")
  sendCall <- getFromNamespace("sendCall", "parallel")  
  
  ####################
  call_a_int <- function (i) {
    
    ADMINID <- df_pop[i,"ADMINID"]
    ADMINPOP <- df_pop[i,"ADMINPOP"]
    
    df <- data.frame(which(x[]==ADMINID))
    
    names(df) <- c("idx")
    df$pop <- y[df$idx] 
    df <- na.omit(df)
    
    if (nrow(df)==0){
      warning(paste0("AdminID ", ADMINID, " has no urban pixels. Will be skipped"))
      return(FALSE)
    }
    
    #df <- df[ with(df, order(pop,decreasing = T)),]
    df <- df[order(df$pop, decreasing = T),]
    total  <- sum(df$pop)
    eps <- 0.5
    
    t_05 <- sum(df[df$pop < eps,"pop"])
    eps_l <- length(df[df$pop > eps,"pop"])
    
    eps_d <- 0.01
    while (eps_l < t_05) {
      
      t_05 <- sum(df[df$pop < eps,"pop"])
      eps_l <- length(df[df$pop > eps,"pop"])
      eps <- eps - eps_d
      
      if (eps < 0){
        eps <- 0.1
        t_05 <- sum(df[df$pop < eps,"pop"])
        eps_l <- length(df[df$pop > eps,"pop"])   
        eps_d <- 0.001
      }
      
    }
    
    t_05 <- sum(df[df$pop < eps,"pop"])
    
    df[df$pop < eps,"pop"] <- 0  
    
    df <- dis_delta(df, t_05 , eps)
    
    out_df <- process_admin(df)
    return(list(idx=out_df$idx,pint=out_df$inb))
    
  }
  ######################
  
  for (i in 1:cores) {
    sendCall(cl[[i]], call_a_int, i, tag=i)
  }  
  
  progress_message(x=0, 
                   max=blocks, 
                   label="")
  
  
  
  for (i in 1:blocks) {
    
    ##	Receive results from a node:
    pd <- recvOneData(cl)
    
    ##	Check if there was an error:
    if (!pd$value$success) {
      stop("ERROR: Cluster barfed...\n\n", pd)
    }
    
    ##	Which block are we processing:
    block <- pd$value$tag
    
    tEnd <-  Sys.time()
    
    if (verbose & (i%%10 == 0) & (i < blocks))   {
      
      progress_message(x=i, 
                       max=blocks, 
                       label=paste0("received block ", 
                                    i,"/",blocks, 
                                    " Processing Time: ", 
                                    tmDiff(tStart,tEnd)
                       )
      )     
      
    }else if(verbose & (i == blocks)){
      
      progress_message(x=i, 
                       max=blocks, 
                       label=paste0(" Processing Time: ", 
                                    tmDiff(tStart,tEnd), 
                                    "                     "
                       )
      )  
      
    }   
    

    
#    raster::values(rst_tmp)[pd$value$value$idx] = pd$value$value$pint
    
    if(is.list( pd$value$value )) {
      
      raster::values(rst_tmp)[pd$value$value$idx] = pd$value$value$pint
      
    }
    
    ##	Check to see if we are at the end of our block list:
    ni <- cores + i
    if (ni <= blocks) {
      sendCall(cl[[pd$node]], call_a_int, ni, tag=ni)
    }
    
  }
  
  writeRaster(rst_tmp, 
              filename=fn, 
              format="GTiff", 
              datatype="INT4S", 
              overwrite=TRUE, 
              NAflag=-99999,
              options=c("COMPRESS=LZW"))
  
  endCluster()
  
  return(invisible(NULL))
  
}




#' Applying integer type to population raster.
#'
#' @param x RasterLayer object with codes representing zones 
#' @param y RasterLayer population
#' @param df_pop dataframe containing the name of the file from which the 
#'        unique area ID and corresponding population
#' @param fn Character file name output
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom doParallel registerDoParallel 
#' @importFrom parallel detectCores 
#' @noRd 
#' @rdname apply_integer_type
#' @return Raster* object
#' @examples
#' \dontrun{
#' apply_integer_type(x, 
#'                        y,
#'                        df_pop, 
#'                        fn)
#' }
apply_integer_type <- function(x,
                               y,
                               df_pop,
                               fn,
                               cores=1,
                               verbose=FALSE,
                               log=FALSE){ 
  
  
  if (cores > 1){
    
    apply_int_type_paralle(x=x,
                           y=y, 
                           df_pop=df_pop, 
                           fn=fn, 
                           cores=cores, 
                           verbose=verbose, 
                           log=log)
    
  }else{
    
    apply_int_type(x=x,
                   y=y, 
                   df_pop=df_pop, 
                   fn=fn,
                   verbose=verbose,
                   log=log)
    
  }
  
  
  
  
  return(invisible(NULL))
  
}
