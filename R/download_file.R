#' Function to download file from ftp server
#'
#' @param file_remote is a url to a remoute file
#' @param dest_file is a path where downloaded file will be stored
#' @param quiet If TRUE, suppress status messages (if any), and the progress bar.
#' @param method Method to be used for downloading files.
#'  Current download methods are "internal", "wininet" (Windows only) "libcurl",
#' "wget" and "curl", and there is a value "auto"
#' @rdname download_file
#' @importFrom utils read.csv
#' @noRd 
download_file <- function(file_remote, dest_file, quiet, method="auto") {
  
  tmStartDw  <- Sys.time()
  
  checkStatus <- tryCatch(
    {
      utils::download.file(file_remote, destfile=dest_file,mode="wb",
                           quiet=quiet, method=method)
    },
    error=function(cond){
      message(paste("URL does not seem to exist:", file_remote))
      message("Here's the original error message:")
      message(cond)
    },
    warning=function(cond){
      message(paste("URL caused a warning:", file_remote))
      message("Here's the original warning message:")
      message(cond)
    },
    finally={
      if (!quiet){
        tmEndDw  <- Sys.time()
        #message(paste("Processed URL:", file_remote))
        message(paste("It took ", tmDiff(tmStartDw ,tmEndDw,frm="hms"), "to download" ))
      }
    }
  )
  
  if(inherits(checkStatus, "error") | inherits(checkStatus, "warning")){
    return(NULL)
  } else{
    return(1)
  }
}
