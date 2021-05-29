# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  October 2017
# Version 0.1
#
#' get_aval_memory function will return avalible
#' of the system memory in GB
#' Tested on Windows 10
#'
#' @rdname get_aval_memory
#' @return numeric
#' @noRd 
get_aval_memory <- function(){
  
  OS = tolower(get_OS_system())
  
  if(OS == 'windows'){
    memavail = shell('wmic OS get FreePhysicalMemory /Value',intern=T)
    memavail = memavail[grep('FreePhysicalMemory', memavail)]
    memavail = as.numeric(gsub('FreePhysicalMemory=','',memavail))
  }else if (OS == 'osx'){
    memavail = as.numeric(unlist(strsplit(system("sysctl hw.memsize", intern = T), split = ' '))[2])/1e3
  }else{
    memavail = as.numeric(system(" awk '/MemTotal/ {print $2}' /proc/meminfo", intern=T))
  }
  
  return(memavail/ (1024 * 1024))
}