#' Function to demo the popFR package using WorldPop input data 
#'
#' @usage
#' popRFdemo(project_dir, country="NPL", cores=4, quant=TRUE, ftp=TRUE, 
#' verbose=TRUE, log=TRUE)
#' @param project_dir Path to the folder to save the outputs
#' @param country ISO of the country. Default one is NPL (Nepal)
#' @param cores is a integer. Number of cores to use when executing the function, 
#'        which defaults to 4. If set to 0 or NULL max number of cores will be 
#'        used based on as many processors as the hardware and RAM allow.
#' @param quant If FALSE then quant will not be calculated
#' @param ftp is logical. TRUE or FALSE: flag indicating whether 
#'        [FTP](ftp://ftp.worldpop.org) or [HTTPS](https://data.worldpop.org) 
#'        WorldPop data hub server will be used. Default is \code{ftp} = TRUE. 
#' @param verbose is logical. TRUE or FALSE: flag indicating whether to print 
#'        intermediate output from the function on the console, which might be 
#'        helpful for model debugging. Default is \code{verbose} = TRUE.
#' @param log is logical. TRUE or FALSE: flag indicating whether to print intermediate 
#'        output from the function on the log.txt file. 
#'        Default is \code{log} = FALSE.
#' @importFrom utils write.table read.csv
#' @rdname popRFdemo
#' @return A data.frame with a value for each zone (unique value in zones)
#' @export
#' @examples
#' \dontrun{
#' popRFdemo( project_dir="/home/user/demo",
#'            country="NPL", 
#'            cores=4
#'          )
#' } 
popRFdemo <- function(project_dir, 
                      country="NPL", 
                      cores=4,
                      quant=TRUE,
                      ftp=TRUE, 
                      verbose=TRUE, 
                      log=TRUE){
  
  iso.list <- c('ABW','AFG','AGO','AIA','ALA','ALB','AND','ARE','ARG',
                'ARM','ASM','ATG','AUS','AUT','AZE','BDI','BEL','BEN','BES','BFA','BGD','BGR','BHR','BHS',
                'BIH','BLM','BLR','BLZ','BMU','BOL','BRA','BRB','BRN','BTN','BWA','CAF','CAN','CHE','CHL',
                'CIV','CMR','COD','COG','COK','COL','COM','CPV','CRI','CUB','CUW','CYM','CZE','DEU','DJI',
                'DMA','DNK','DOM','DZA','ECU','EGY','ERI','ESH','ESP','EST','ETH','FIN','FJI','FLK','FRA',
                'FRO','FSM','GAB','GBR','GEO','GGY','GHA','GIB','GIN','GLP','GMB','GNB','GNQ','GRC','GRD',
                'GRL','GTM','GUF','GUM','GUY','HKG','HND','HRV','HTI','IDN','IMN','IRL','IRN','IRQ','ISL',
                'ITA','JAM','JOR','JPN','KAZ','KGZ','KHM','KIR','KNA','KOR','KOS','KWT','LAO','LBN','LBR',
                'LBY','LCA','LIE','LKA','LSO','LTU','LUX','LVA','MAC','MAF','MAR','MCO','MDA','MDG','MDV',
                'MHL','MKD','MLI','MLT','MMR','MNE','MNG','MNP','MOZ','MRT','MSR','MUS','MYS','MYT','NAM',
                'NCL','NER','NFK','NGA','NIC','NIU','NLD','NOR','NPL','NRU','NZL','OMN','PAK','PAN','PCN',
                'PER','PHL','PLW','PNG','PRI','PRK','PRT','PRY','PSE','PYF','QAT','REU','ROU','RWA',
                'SAU','SDN','SEN','SGP','SHN','SJM','SLB','SLE','SLV','SMR','SOM','SPM','SPR','SSD','STP',
                'SUR','SVN','SWE','SWZ','SXM','SYC','SYR','TCA','TCD','TGO','THA','TJK','TKL','TKM','TLS',
                'TON','TTO','TUN','TUR','TUV','TWN','TZA','UGA','UKR','URY', 'UZB','VAT','VCT','VEN',
                'VGB','VIR','VNM','VUT','WLF','WSM','YEM','ZAF','ZMB','ZWE')
  
  is.populated <- function(x, xlist) x %in% xlist
  
  iso.s <- tolower(country)
  country <- toupper(country)
  
  if (!is.populated(country, iso.list)) {
    cat(paste0("***********************************************\n"))
    cat(paste0("Error: ",country," does not exist in this demo.\n"))
    cat(paste0("***********************************************\n"))
    cat(paste0("Please use the follwoing ISO \n"))
    cat(paste0("----------------------------------------\n"))
    cat(iso.list)
    cat(paste0("\n----------------------------------------\n"))
    stop() 
  }
  
  quiet <- ifelse(verbose, FALSE, TRUE)
  
  output_dir <- file.path(project_dir, country, "covariates")
  
  if(!file.exists(output_dir)){ 
    message("Info :: Creating dir ", output_dir)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) 
  }
  
  
  url_prefix <- "https://data.worldpop.org"
  if (ftp){
    url_prefix <- "ftp://ftp.worldpop.org"
  }
  ptcov <- paste0(url_prefix,"/GIS/Covariates/Global_2000_2020/",toupper(country))
  
  input_covariates <- list(
    country = list(
      "esaccilc_dst011_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst011_100m_2015.tif"),
      "esaccilc_dst040_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst040_100m_2015.tif"),
      "esaccilc_dst130_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst130_100m_2015.tif"),
      "esaccilc_dst140_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst140_100m_2015.tif"),
      "esaccilc_dst140_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst140_100m_2015.tif"),
      "esaccilc_dst160_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst160_100m_2015.tif"),
      "esaccilc_dst190_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst190_100m_2015.tif"),
      "esaccilc_dst200_100m_2015"=paste0(ptcov,"/ESA_CCI_Annual/2015/",iso.s,"_esaccilc_dst200_100m_2015.tif"),
      "esaccilc_dst_water_100m_2000_2012"= paste0(ptcov,"/ESA_CCI_Water/DST/",iso.s,"_esaccilc_dst_water_100m_2000_2012.tif"),
      "dst_bsgme_100m_2020"= paste0(ptcov,"/BSGM/2020/DTE/",iso.s,"_dst_bsgme_100m_2020.tif"),
      "dst_ghslesaccilc_100m_2000"= paste0(ptcov,"/BuiltSettlement/2000/DTE/",iso.s,"_dst_ghslesaccilc_100m_2000.tif"),
      "osm_dst_roadintersec_100m_2016"= paste0(ptcov,"/OSM/DST/",iso.s,"_osm_dst_roadintersec_100m_2016.tif"),
      "osm_dst_waterway_100m_2016"=paste0(ptcov,"/OSM/DST/",iso.s,"_osm_dst_waterway_100m_2016.tif"),
      "osm_dst_road_100m_2016"=paste0(ptcov,"/OSM/DST/",iso.s,"_osm_dst_road_100m_2016.tif"),
      "srtm_slope_100m"=paste0(ptcov,"/Slope/",iso.s,"_srtm_slope_100m.tif"),
      "srtm_topo_100m"=paste0(ptcov,"/Topo/",iso.s,"_srtm_topo_100m.tif"),
      "dst_coastline_100m_2000_2020"=paste0(ptcov,"/Coastline/DST/",iso.s,"_dst_coastline_100m_2000_2020.tif"),
      "viirs_100m_2016"=paste0(ptcov,"/VIIRS/",iso.s,"_viirs_100m_2016.tif"),
      "wdpa_dst_cat1_100m_2017"=paste0(ptcov,"/WDPA/WDPA_1/",iso.s,"_wdpa_dst_cat1_100m_2017.tif")
    )  
  )
  names(input_covariates) <- c(country)
  
  ptcov <- paste0(url_prefix,"/GIS/Mastergrid/Global_2000_2020/",toupper(country))
  
  input_mastergrid <- list(
    country = paste0(ptcov,"/Subnational/",iso.s,"_subnational_admin_2000_2020.tif")
  )
  names(input_mastergrid) <- c(country)
  
  ptcov <- paste0(url_prefix,"/GIS/Covariates/Global_2000_2020/",toupper(country))
  input_watermask <- list(
    country = paste0(ptcov,"/ESA_CCI_Water/Binary/",iso.s,"_esaccilc_water_100m_2000_2012.tif")
  )
  names(input_watermask) <- c(country)
  
  ptcov <- paste0(url_prefix,"/GIS/Pixel_area/Global_2000_2020/",toupper(country))
  input_px_area <- list(
    country = paste0(ptcov,"/",iso.s,"_px_area_100m.tif")
  )
  names(input_px_area) <- c(country)
  
  
  countries <- c()
  
  for ( i in names(input_covariates) ) {
    countries <- append(countries, i, 1)
  } 
  
  for( i in countries){
    
    covariates <- names(input_covariates[[i]])
    
    cat("\n===============================================================\n")
    cat("===============================================================\n")
    cat(paste0("Follwoing covariaes will be downloaded to \n",output_dir,"\n"))
    cat("===============================================================\n")
    cat(paste0("",covariates,"\n"))
    cat("===============================================================\n")
    
    for (c in covariates){
      file_remote <- input_covariates[[i]][[c]]
      
      output_file <- file.path(output_dir, paste0(c,".tif"))
      if (!file.exists(output_file)){
        cat(paste0("Downloading... ", c ,"\n"))
        download_file(file_remote, output_file, quiet, method="auto")
        
      }
    }
    
  }
  
  cat(paste0("\n"))
  output_px_area <- file.path(output_dir, paste0("px_area_100m.tif"))
  file_remote_px_area <- input_px_area[[country]]
  if (!file.exists(output_px_area)){
    cat(paste0("Downloading... px_area px_area_100m\n"))
    download_file(file_remote_px_area, output_px_area, quiet, method="auto")  
  }  
  
  
  output_watermask <- file.path(output_dir, paste0("esaccilc_water_100m_2000_2012.tif"))
  file_remote_watermask <- input_watermask[[country]]
  if (!file.exists(output_watermask)){
    cat(paste0("Downloading... watermask esaccilc_water_100m_2000_2012\n"))
    download_file(file_remote_watermask, output_watermask, quiet, method="auto") 
  }   
  
  
  output_mastergrid <- file.path(output_dir, paste0("subnational_admin_2000_2020.tif"))
  file_remote_mastergrid <- input_mastergrid[[country]]
  if (!file.exists(output_mastergrid)){
    cat(paste0("Downloading... mastergrid subnational_admin_2000_2020\n"))
    download_file(file_remote_mastergrid, output_mastergrid, quiet, method="auto")
  }   
  
  ####
  
  
  input_covariates <- list(
    country = list(
      "esaccilc_dst011_100m_2015"=file.path(output_dir,"esaccilc_dst011_100m_2015.tif"),
      "esaccilc_dst040_100m_2015"=file.path(output_dir,"esaccilc_dst040_100m_2015.tif"),
      "esaccilc_dst130_100m_2015"=file.path(output_dir,"esaccilc_dst130_100m_2015.tif"),
      "esaccilc_dst140_100m_2015"=file.path(output_dir,"esaccilc_dst140_100m_2015.tif"),
      "esaccilc_dst140_100m_2015"=file.path(output_dir,"esaccilc_dst140_100m_2015.tif"),
      "esaccilc_dst160_100m_2015"=file.path(output_dir,"esaccilc_dst160_100m_2015.tif"),
      "esaccilc_dst190_100m_2015"=file.path(output_dir,"esaccilc_dst190_100m_2015.tif"),
      "esaccilc_dst200_100m_2015"=file.path(output_dir,"esaccilc_dst200_100m_2015.tif"),
      "esaccilc_dst_water_100m_2000_2012"= file.path(output_dir,"esaccilc_dst_water_100m_2000_2012.tif"),
      "dst_bsgme_100m_2020"= file.path(output_dir,"dst_bsgme_100m_2020.tif"),
      "dst_ghslesaccilc_100m_2000"= file.path(output_dir,"dst_ghslesaccilc_100m_2000.tif"),
      "osm_dst_roadintersec_100m_2016"= file.path(output_dir,"osm_dst_roadintersec_100m_2016.tif"),
      "osm_dst_waterway_100m_2016"=file.path(output_dir,"osm_dst_waterway_100m_2016.tif"),
      "osm_dst_road_100m_2016"=file.path(output_dir,"osm_dst_road_100m_2016.tif"),
      "srtm_slope_100m"=file.path(output_dir,"srtm_slope_100m.tif"),
      "srtm_topo_100m"=file.path(output_dir,"srtm_topo_100m.tif"),
      "dst_coastline_100m_2000_2020"=file.path(output_dir,"dst_coastline_100m_2000_2020.tif"),
      "viirs_100m_2016"=file.path(output_dir,"viirs_100m_2016.tif"),
      "wdpa_dst_cat1_100m_2017"=file.path(output_dir,"wdpa_dst_cat1_100m_2017.tif")
    )  
  )
  names(input_covariates) <- c(country)
  
  
  
  
  input_mastergrid <- list(
    country = file.path(output_dir,"subnational_admin_2000_2020.tif")
  )
  names(input_mastergrid) <- c(country)
  
  input_watermask <- list(
    country = file.path(output_dir,"esaccilc_water_100m_2000_2012.tif")
  )
  names(input_watermask) <- c(country)
  
  
  input_px_area <- list(
    country = file.path(output_dir,"px_area_100m.tif")
  )
  names(input_px_area) <- c(country)  
  cat( paste0("Saving input covariates, watermask, px_area and mastergrid") )
  cat( paste0("\nas R objects RData in :",output_dir,"\n") )
  save(input_covariates, file=file.path(output_dir,"input_covariates.RData"))
  save(input_mastergrid, file=file.path(output_dir,"input_mastergrid.RData"))
  save(input_watermask, file=file.path(output_dir,"input_watermask.RData"))
  save(input_px_area, file=file.path(output_dir,"input_px_area.RData"))
  
  
  output_mastergrid <- file.path(output_dir, paste0("subnational_admin_2000_2020.tif"))
  
  dpop_file <- file.path(output_dir, paste0(iso.s, "_population.csv"))
  
  if (!file.exists(dpop_file)){
    cat( paste0("\nDownloading and saving population table for ",country) )
    cat( paste0(" in ", paste0(iso.s, "_population.csv"), "\n" ,dpop_file,"\n") )
    
    dpop <- read.csv(file.path(url_prefix,
                               "GIS/Population/Global_2000_2020/CensusTables",
                               paste0(iso.s,"_population_2000_2020.csv")
    )
    )
    
    dpop <- dpop[,c("GID","P_2020")]
    
    write.table(dpop, dpop_file, sep=",",  col.names=FALSE, row.names=FALSE)
    
  }
  
  pop_tmp <- read.csv(dpop_file)
  
  if ( nrow(pop_tmp) < 20 ){
    
    cat("\n===============================================================\n")
    cat("===============================================================\n\n")
    cat( paste0("Country ",country," has only ",nrow(pop_tmp)," admin units.\n") )
    cat( paste0("This amount of admin units will not be enought to train the model\n") )
    cat( paste0("For the purpers of the demo please choose another country\n\n") )
    cat("===============================================================\n")
    cat("===============================================================\n")
    
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  } 
  
  input_poptables <- list(
    country=dpop_file
  )
  names(input_poptables) <- c(country)
  
  
  fset <- NULL
  fset_incl <- FALSE
  fset_cutoff <- 20
  
  pop <- popRF(input_poptables, 
               input_covariates, 
               input_mastergrid, 
               input_watermask, 
               input_px_area, 
               project_dir, 
               cores=cores,
               fset=fset,
               fset_incl=fset_incl,
               fset_cutoff=fset_cutoff,
               check_result=TRUE, 
               verbose =verbose, 
               log =log)
  
  
  return(pop)
}