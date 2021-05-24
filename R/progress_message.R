# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  October 2017
# Version 0.1
#
#' progress_message function will return progress
#'
#' Tested on Windows 10
#' @param x integer
#' @param max maximum for progress bvar
#' @param label additional text for progress  bar
#' @rdname progress_message
#' @return character
#' @export
#' @examples
#' \dontrun{
#' progress_message( x=10, max = 200, label="Progress message" )
#' }
progress_message <- function (x, max = 100, label=NULL) {
  
  if (is.null(label)) label=''
  if (x != max) ar = '>' else ar=''
  
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d%% %s',
              paste(paste(rep('=', percent / 2), collapse = ''),'',sep = ar),
              floor(percent),
              label))
  if (x == max)
    cat('\n')
}
