#' Function will return progress plotting a progress bar
#'
#' @param x integer, current level
#' @param max maximum for progress bar
#' @param label additional text for progress  bar
#' @rdname progress_message
#' @return character
#' @examples
#' \dontrun{
#' progress_message( x=10, max = 200, label="Progress message" )
#' }
#' @noRd 
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
