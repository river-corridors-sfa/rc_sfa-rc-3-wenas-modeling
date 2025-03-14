# create swat-cup files: get and format calibration data for calibration with swatcup 
  #written by Katie A. Wampler on 2024-03-25 

#### READ ME ###### 
  #in addition to running these you need to: 
    #update the file.cio 
    #update calibration parameters
    #paste correct swat.exe file 
    #update carbon defaults and set cswat to 2 (paste files to backup)

#start here: set defaults 
  #set calibration period 
    cal_start <- as.Date("2005-01-01")
    cal_end <- as.Date("2017-12-31") #end of year of fire, because we don't want to calibrate on burned data
    
    timestep <- "monthly" #monthly or daily
    
  #set basin which will set other defaults 
    model <- "american"
    
  if(model == "american"){
    #american
    sub_sites <- data.frame(sites=c("12488500"), subbasin=c(1)) #name and subbasin number of rch sites
    et_sites <- data.frame(hru=c(112,48), landuse=c(52,42), name=c("RNGB", "FRSE")) #hru number, and landuse of et sites
    nsub <- 21 #number of subbasins
    nhru <- 198 #number of hrus
    wd <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/american river simp2.Sufi2.SwatCup/" #location of swatcup project
    ETwd <- "Z:/1_Research/4_Wenas_Thresholds/data/American River/" #location of et data
    sub_shp <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/subs1.shp" #location of subbasin shape
    hru_shp <- "C:/SWAT/American River Simp2/American River Simp2/Watershed/shapes/hru2.shp" #location of hru shape
    
    #levers to adjust flow data to be representative of DOC/nitrate (will need to adjust manually based on expert opinion, see 0-swat-cup-utilities for testing)
      #doc a/b 
      doc_a <- 0.1
      doc_b <- 1
      #nitrate a/b 
      nit_a <- 0.1 
      nit_b <- 0.6
  }else if(model == "tule"){
    #tule
    sub_sites <- data.frame(sites=c("11204100", "11203580"), subbasin=c(1,9))
    lai_sites <- data.frame(hru=c(103,48,83,144), landuse=c(71,52,42,41), name=c("RNGE","RNGB", "FRSE", "FRSD"))
    nsub <- 21
    nhru <- 224
    wd <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/tule river simp2.Sufi2.SwatCup/"
    ETwd <- "Z:/1_Research/4_Wenas_Thresholds/data/Tule_River/"
    sub_shp <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Watershed/shapes/subs1.shp"
    hru_shp <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Watershed/shapes/hru1.shp"
    
    #levers to adjust flow data to be representative of DOC/nitrate (will need to adjust manually based on expert opinion, see 0-swat-cup-utilities for testing)
      doc_a <- 0.05 
      doc_b <- 1.2 
      #nitrate a/b 
      nit_a <- 0.01 
      nit_b <- 1.2
  }
  
  
  #create folders for saving calibration data 
    setwd(wd)
    dir.create("Raw Cal Files", showWarnings = F)

#section 0: load libraries and functions ------
  library(waterData) #get observed data
  library(dataRetrieval) #get chemical data 
  library(stringr) #for dealing with strings
  library(readr) #for reading and writing files
  library(lubridate) #for dealing with dates 
  library(dplyr) #for dealing with data
  library(sf) #for geospatial
  library(raster) #for working with rasters
  library(ggplot2) #for plotting
  library(gridExtra)
  library(data.table) #for reading data
  library(readr)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(tidyr)
  sf_use_s2(FALSE)
    
  #used to merge multiple dataframes together
    comb_files <- function(file_list){
      pb <- txtProgressBar(max = length(file_list), style = 3)
      
      for(x in 1:length(file_list)){
        setTxtProgressBar(pb, x)
        df <- data.table::fread(file_list[x])
        if(x == 1){
          full_df <- df
        }else{full_df <- rbind(full_df, df)}}
      close(pb)
      return(full_df)
    }
    
    
  
#' Write flow data for swat cup
#'
#' @param sites a dataframe, with the columns "site" and "subbasin" to link subbain number and USGS gagues
#' @param cal_start a date object, the starting date of the calibration
#' @param cal_end a date object, the ending date of the calibration

  write_flow_raw <- function(sites,  cal_start, cal_end,timestep){
    #get flow data 
    formatflowdata_out <- function(site_no, cal_start, cal_end, timestep) {
      #download full flow file from USGS
      data <- importDVs(site_no, code = "00060", sdate = cal_start, edate = cal_end) 
      if (nrow(data)==0){  ##check to see if there is data for that range
        data
      }else{data <- fillMiss(data, block=15, pmiss = 10, model = "trend")  #fill in daily gaps
      data$val <- data$val * 0.0283168 #convert to from cfs to m3/s 
      
      if(timestep == "daily"){
        #check when data starts 
        dates <- data.frame(date=seq(cal_start, cal_end, "day"), index=1:length(seq(cal_start, cal_end, "day")))
        data$label <- paste("FLOW_OUT", strftime(data$dates, "%j"), year(data$dates), sep="_")
        data$ID <- dates$index[dates$date %in% data$dates]
        data <- data %>% dplyr::select(c(ID, label,val))
        data <- subset(data, is.na(data$val)==F)
        
      }else if(timestep == "monthly"){
        #summarise by month 
        data <- data %>% mutate(month_date = floor_date(dates, unit="month")) %>% 
          group_by(month_date) %>% summarise(val=mean(val))
        
        dates <- data.frame(date=seq(cal_start, cal_end, "month"), index=1:length(seq(cal_start, cal_end, "month")))
        data$label <- paste("FLOW_OUT", strftime(data$month_date, "%m"), year(data$month_date), sep="_")
        data$ID <- dates$index[dates$date %in% data$month_date]
        data <- data %>% dplyr::select(c(ID, label,val))
        data <- subset(data, is.na(data$val)==F)
        
      }
      
      data}}
    
    #set wd and clear folders  
      setwd(paste(wd, "Raw Cal Files/", sep=""))
      unlink(list.files(), recursive = T)
      dir.create("observed_rch")
      dir.create("observed")
      
    # write observed_rch.txt file 
    for (n in 1:nrow(sites)){
      site <- sites$site[n]
      basin <- sites$subbasin[n]
      if(str_detect(site, "sub")==F){
        data <- formatflowdata_out(site, cal_start, cal_end,timestep)
      }else{data <- as.data.frame(matrix(nrow=0, ncol=1))}
      if (nrow(data) != 0){
        setwd(paste(wd, "Raw Cal Files/observed_rch", sep=""))
        var_name <- paste("FLOW_OUT_", basin, sep="")
        file_name <- paste(var_name, ".txt", sep="")    
        obs <- nrow(data)
        line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
        line3 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
        line4 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
        head <- paste(line2, line3, line4, "", sep="\n")
        write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
        write_tsv(data, file = file_name, append=T, escape="none", col_names = F)
        write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
        write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
        
        setwd(paste(wd, "Raw Cal Files/observed", sep=""))
        weight <- 1    
        obs <- nrow(data)
        line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
        line3 <- paste(weight,"     : weight of the variable in the objective function", sep="")
        line4 <- paste(-1, "    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations", sep="")
        line5 <- paste(-1,"    : constant flow separation, threshold value. (not considered if -1)", sep="")
        line6 <- paste(1, "     : if separation of signal is considered, this is weight of the smaller values in the objective function", sep="")
        line7 <- paste(1, "     : if separation of signal is considered, this is weight of the larger values in the objective function", sep="")
        line8 <- paste(10, "    : percentage of measurement error", sep="")
        line9 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
        line10 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
        head <- paste(line2, line3, line4, line5, line6, line7, line8, line9, line10, "", sep="\n")
        write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
        write_tsv(data, file =  file_name, append=T, escape="none", col_names = F)
        write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
        write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
      }
    }}  
  

#' Write flow data for swat cup
#'
#' @param sites a dataframe, with the columns "site" and "subbasin" to link subbain number and USGS gagues
#' @param cal_start a date object, the starting date of the calibration
#' @param cal_end a date object, the ending date of the calibration
#' @param wd character with path to the raw ET data downloaded using MODISTools package
 
  write_ET_raw <- function(et_loc, et_sites, ETwd,  cal_start, cal_end, timestep){
    
    fill_ET <- function(dataset, block = 30, pmiss = 90, model = "level", smooth = TRUE){
      pck <- is.na(dataset$value)
      percent <- 0
      max.mis <- 0
      if (sum(pck) > 0) {
        percent <- round((sum(pck)/length(dataset$value)) * 100, 
                         digits = 2)
        my.message <- paste("There are ", sum(pck), "missing values for", 
                            dataset$HRU_ID[1], "or", percent, "percent of the data.", 
                            sep = " ")
        message(my.message)
        rles <- rle(is.na(dataset$value))
        max.mis <- max(rles$lengths[rles$values])
        my.message2 <- paste("The maximum block of missing", 
                             dataset$HRU_ID[1], "data is", max.mis, "days long.", 
                             sep = " ")
        message(my.message2)
      }
      else {
        noMis <- paste("No missing values for", dataset$HRU_ID[1], 
                       sep = " ")
        message(noMis)
      }
      if (percent >= pmiss | max.mis >= block) {
        tooMuch <- paste("Too much missing data for", dataset$HRU_ID[1], 
                         "Cannot fill in missing values.", sep = " ")
        message(tooMuch)
      }
      else {
        dataset$fill_val <- zoo::na.approx(dataset$value, na.rm=F)
        
        ggplot() + geom_line(data=na.omit(dataset), aes(x=date, y=value),color="black", linewidth=1) +
          geom_line(data=dataset, aes(x=date, y=fill_val), color="green", linewidth=0.25) + 
          labs(x="Observation",y= "Observed and estimated times series") +
          scale_x_date(limits=c(as.Date("2001-01-01"), as.Date("2001-12-31")))
        
        fillMess <- paste("Filled in", sum(pck), "values for", 
                          dataset$HRU_ID[1], sep = " ")
        message(fillMess)
      }
      dataset
    }
    
    #set wd and clear folders  
    dir.create(paste(wd, "Raw Cal Files/observed_hru", sep=""), showWarnings = F)
    setwd(paste(wd, "Raw Cal Files/observed_hru", sep=""))
    #unlink(list.files(), recursive = T)
    
    #load ET data 
    files <- list.files(paste(ETwd, "MODIS", sep=""))
    ET_file <- paste(ETwd, "MODIS/", files[str_detect(files, "MOD16A2GF_ET_500m")] , sep="")
    
    ET_dat <- comb_files(ET_file)
    
    #get just data for sites of interest 
    ET_dat <- merge(lai_loc, ET_dat, by.x="name", by.y="site")
    
    #average by across subbasin 
    #convert ET and PET from 0.1 kg/m2 /8days to mm/day
    ET_dat$doy <- yday(ET_dat$date)
    ET_dat$leap <- leap_year(ET_dat$date)
    ET_dat$conv <- 0.1/8 #normally
    ET_dat$conv[ET_dat$doy == 361 & ET_dat$leap==T] <- 0.1/6 #for leap years 
    ET_dat$conv[ET_dat$doy == 361 & ET_dat$leap==F] <- 0.1/5 #for nonleap years 
    ET_dat$value <- ET_dat$ET_500m * ET_dat$conv  #1kg/m2 is 1 mm, for some reason the it's stored as 0.1mm/8days
    
    ET_dat <- ET_dat %>% st_drop_geometry() %>% group_by(HRU_ID, date) %>% 
      summarise(value=mean(value))
    
    # write observed_sub.txt file 
    for (n in 1:nrow(lai_sites)){
      site <- lai_sites$hru[n]
      basin <- site
      ETdata <- subset(ET_dat,ET_dat$HRU_ID == site)
      ETdata$analyte <- "ET"
      data <- ETdata
      
      #clip to calibration period 
      data <- subset(data, data$date >= cal_start & data$date<=cal_end)
      
      setwd(paste(wd, "Raw Cal Files/observed_hru", sep=""))
      
      var_name <- paste("ET", basin, sep="_")
      file_name <- paste(var_name, ".txt", sep="")
      
      #format
      if(timestep == "daily"){
        subdata <- data
        dates <- data.frame(date=seq(cal_start, cal_end, "day"), index=1:length(seq(cal_start, cal_end, "day")))
        subdata$label <- paste("ET", strftime(subdata$date, "%j"), year(subdata$date), sep="_")
        subdata$ID <- dates$index[dates$date %in% subdata$date]
        subdata <- subdata %>% ungroup() %>% dplyr::select(c(ID, label,value))
        
      }else if(timestep == "monthly"){
        #fill in daily gaps 
        dates <- data.frame(date = seq(from=cal_start, to=cal_end, by="day"))
        data <- merge(data, dates, by="date", all.y=T)
        data_fill <- fill_ET(data)
        
        subdata <- data_fill %>% mutate(month_date = floor_date(as.Date(date), unit="month")) %>% 
          group_by(month_date) %>% summarise(value=sum(fill_val))
        
        dates <- data.frame(date=seq(cal_start, cal_end, "month"), index=1:length(seq(cal_start, cal_end, "month")))
        subdata$label <- paste("ET", strftime(subdata$month_date, "%m"), year(subdata$month_date), sep="_")
        subdata$ID <- dates$index[dates$date %in% subdata$month_date]
        subdata <- subdata %>% ungroup() %>% dplyr::select(c(ID, label,value))
        subdata <- na.omit(subdata)
        
      }
      
      obs <- nrow(subdata)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line4 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(subdata, file = file_name, append=T, escape="none", col_names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      
      setwd(paste(wd, "Raw Cal Files/observed", sep=""))
      weight <- 1    
      obs <- nrow(subdata)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(weight,"     : weight of the variable in the objective function", sep="")
      line4 <- paste(-1, "    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations", sep="")
      line5 <- paste(-1,"    : constant flow separation, threshold value. (not considered if -1)", sep="")
      line6 <- paste(1, "     : if separation of signal is considered, this is weight of the smaller values in the objective function", sep="")
      line7 <- paste(1, "     : if separation of signal is considered, this is weight of the larger values in the objective function", sep="")
      line8 <- paste(10, "    : percentage of measurement error", sep="")
      line9 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line10 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, line5, line6, line7, line8, line9, line10, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(subdata, file =  file_name, append=T, escape="none", col_names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
    }
  }  
  
  #generate dummy DOC/nitrate records by transforming the streamflow records
  write_DOC_raw <- function(sites, cal_start, cal_end, timestep, doc_a, doc_b){
    formatdoc_out <- function(site_no, cal_start, cal_end, a, b) {
      #download full flow file from USGS
      data <- importDVs(site_no, code = "00060", sdate = cal_start, edate = cal_end) 
      if (nrow(data)==0){  ##check to see if there is data for that range
        data
      }else{data <- fillMiss(data, block=15, pmiss = 10, model = "trend")  #fill in daily gaps
      data$val <- data$val * 0.0283168 #convert to from cfs to m3/s 
      
      #convert flow in to dummy DOC 
      data$doc <- a * data$val^b
      summary(data)
      ggplot(data, aes(x=doc)) + geom_histogram()
      ggplot(data, aes(x=dates, y=doc)) + geom_line()
      
      #check when data starts 
      dates <- data.frame(date=seq(cal_start, cal_end, "day"), index=1:length(seq(cal_start, cal_end, "day")))
      data$label <- paste("SORPST_OUT", strftime(data$dates, "%j"), year(data$dates), sep="_")
      data$ID <- dates$index[dates$date %in% data$dates]
      data <- data %>% dplyr::select(c(ID, label,doc))
      
      data <- subset(data, is.na(data$doc)==F)
      data}}
    site <- sites$site[1]
    basin <- sites$subbasin[1]
    if(str_detect(site, "sub")==F){
      data <- formatdoc_out(site, cal_start, cal_end, doc_a, doc_b)
    }else{data <- as.data.frame(matrix(nrow=0, ncol=1))}
    if (nrow(data) != 0){
      setwd(paste(wd, "Raw Cal Files/observed_rch", sep=""))
      var_name <- paste("SORPST_OUT_", basin, sep="")
      file_name <- paste(var_name, ".txt", sep="")    
      obs <- nrow(data)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line4 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(data, file = file_name, append=T, escape="none", col_names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      
      setwd(paste(wd, "Raw Cal Files/observed", sep=""))
      weight <- 1    
      obs <- nrow(data)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(weight,"     : weight of the variable in the objective function", sep="")
      line4 <- paste(-1, "    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations", sep="")
      line5 <- paste(-1,"    : constant flow separation, threshold value. (not considered if -1)", sep="")
      line6 <- paste(1, "     : if separation of signal is considered, this is weight of the smaller values in the objective function", sep="")
      line7 <- paste(1, "     : if separation of signal is considered, this is weight of the larger values in the objective function", sep="")
      line8 <- paste(10, "    : percentage of measurement error", sep="")
      line9 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line10 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, line5, line6, line7, line8, line9, line10, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(data, file =  file_name, append=T, escape="none", col_names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
    }
  }
  write_nitrate_raw <- function(sites, cal_start, cal_end, timestep, nit_a, nit_b){
    formatnitrate_out <- function(site_no, cal_start, cal_end, a, b) {
      #download full flow file from USGS
      data <- importDVs(site_no, code = "00060", sdate = cal_start, edate = cal_end) 
      if (nrow(data)==0){  ##check to see if there is data for that range
        data
      }else{data <- fillMiss(data, block=15, pmiss = 10, model = "trend")  #fill in daily gaps
      data$val <- data$val * 0.0283168 #convert to from cfs to m3/s 
      
      #convert flow in to dummy nitrate 
      data$nitrate <- a * data$val^b    
      data$nitrate <- round(data$nitrate, 3)
      summary(data)
      ggplot(data, aes(x=nitrate)) + geom_histogram()
      ggplot(data, aes(x=dates, y=nitrate)) + geom_line()
      
      #check when data starts 
      dates <- data.frame(date=seq(cal_start, cal_end, "day"), index=1:length(seq(cal_start, cal_end, "day")))
      data$label <- paste("NO3CONC", strftime(data$dates, "%j"), year(data$dates), sep="_")
      data$ID <- dates$index[dates$date %in% data$dates]
      data <- data %>% dplyr::select(c(ID, label,nitrate))
      
      data <- subset(data, is.na(data$nitrate)==F)
      data}}
    
    site <- sites$site[1]
    basin <- sites$subbasin[1]
    if(str_detect(site, "sub")==F){
      data <- formatnitrate_out(site, cal_start, cal_end,nit_a,nit_b)
    }else{data <- as.data.frame(matrix(nrow=0, ncol=1))}
    if (nrow(data) != 0){
      setwd(paste(wd, "Raw Cal Files/observed_rch", sep=""))
      var_name <- paste("NO3CONC_", basin, sep="")
      file_name <- paste(var_name, ".txt", sep="")    
      obs <- nrow(data)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line4 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(data, file = file_name, append=T, escape="none", col_names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file= file_name, append=T, quote=F, col.names = F, row.names = F)
      
      setwd(paste(wd, "Raw Cal Files/observed", sep=""))
      weight <- 1    
      obs <- nrow(data)
      line2 <- paste(var_name,"   : this is the name of the variable and the hru number to be included in the objective function", sep="")
      line3 <- paste(weight,"     : weight of the variable in the objective function", sep="")
      line4 <- paste(-1, "    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations", sep="")
      line5 <- paste(-1,"    : constant flow separation, threshold value. (not considered if -1)", sep="")
      line6 <- paste(1, "     : if separation of signal is considered, this is weight of the smaller values in the objective function", sep="")
      line7 <- paste(1, "     : if separation of signal is considered, this is weight of the larger values in the objective function", sep="")
      line8 <- paste(10, "    : percentage of measurement error", sep="")
      line9 <- paste(obs, "    : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
      line10 <- "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
      head <- paste(line2, line3, line4, line5, line6, line7, line8, line9, line10, "", sep="\n")
      write.table(head, file= file_name, append=F, quote=F, col.names = F, row.names = F)
      write_tsv(data, file =  file_name, append=T, escape="none", col_names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
      write.table("", file=  file_name, append=T, quote=F, col.names = F, row.names = F)
    }
    
    
  }
#' Write files for swat cup calibration
#'
#' @param nsub the number of subbasins in the model
#' @param cal_start a date object, the starting date of the calibration
#' @param cal_end a date object, the ending date of the calibration
#' @param rch_analytes character vector of the analytes for .rch file, if none put NULL 
#' @param sub_analytes character vector of the analytes for .sub file, if none put NULL
#' @param function_type number of the function used to assess model fit 
#' @param min_value value for minimum value of objective function to do behavioral solutions 
#' @param warm_up a number indicating the number of warm up years

  write_cal_files <- function(nsub, cal_start, cal_end, timestep, rch_analytes=c("FLOW", "SEDCONC"),
                              sub_analytes=c("ET","PET"), hru_analytes=c("LAI"), 
                              function_type=5, min_value=0.5,
                              warm_up = 5){
    setwd(paste(wd, "SUFI2.IN", sep="/"))
  
  #pull the files together
    #clear  files
    clear <- vector()
    write.table(clear, "observed.txt", append=F, quote=F, row.names = F, col.names = F)
    if(is.null(rch_analytes) ==F){write.table(clear, "observed_rch.txt", append=F, quote=F, row.names = F, col.names = F)
      write.table(clear, "var_file_rch.txt", append=F, quote=F, row.names = F, col.names = F)
    }

    if(is.null(sub_analytes) ==F){write.table(clear, "observed_sub.txt", append=F, quote=F, row.names = F, col.names = F)
      write.table(clear, "var_file_sub.txt", append=F, quote=F, row.names = F, col.names = F)
    }
    
    if(is.null(hru_analytes) ==F){write.table(clear, "observed_hru.txt", append=F, quote=F, row.names = F, col.names = F)
      write.table(clear, "var_file_hru.txt", append=F, quote=F, row.names = F, col.names = F)
    }
    write.table(clear, "var_file_name.txt", append=F, quote=F, row.names = F, col.names = F)
    
    ### write observed_rch.txt and observed_sub.txt file 
    line1 <- paste(0, "      : number of observed variables", sep="")
    if(is.null(rch_analytes)==F){
      write.table(line1, file="observed_rch.txt", append=F, quote=F, col.names = F, row.names = F)
      write.table("", file="observed_rch.txt", append=T, quote=F, col.names = F, row.names = F) 
    }
    if(is.null(sub_analytes)==F){
      write.table(line1, file="observed_sub.txt", append=F, quote=F, col.names = F, row.names = F)
      write.table("", file="observed_sub.txt", append=T, quote=F, col.names = F, row.names = F) 
    }
    if(is.null(hru_analytes)==F){
      write.table(line1, file="observed_hru.txt", append=F, quote=F, col.names = F, row.names = F)
      write.table("", file="observed_hru.txt", append=T, quote=F, col.names = F, row.names = F) 
    }
    
    ## write observed.txt file 
    line1_a <- paste(0, "     : number of observed variables", sep="")
    line1_b <- paste(function_type, "     : Objective function type, 1=mult,2=sum,3=r2,4=chi2,5=NS,6=br2,7=ssqr,8=PBIAS,9=KGE,10=RSR,11=MNS", sep="")
    line1_c <- paste(min_value, "   : min value of objective function threshold for the behavioral solutions", sep="")              
    line1_d <- paste(1, "     : if objective function is 11=MNS (modified NS),indicate the power, p.", sep="")
    line1 <- paste(line1_a, line1_b, line1_c, line1_d, sep="\n")
    write.table(line1, file= "observed.txt", append=F, quote=F, col.names = F, row.names = F)
    write.table(" ", file= "observed.txt", append=T, quote=F, col.names = F, row.names = F) 
    
    ## write SUFI2_extract_rch.def file 
    if(is.null(rch_analytes)==F){
      rch_out <- data.frame(analyte=c("FLOW_OUT", "SED_OUT", "SEDCONC","ORGN", "ORGP", "NO3", "NH4", "MINP", "SORPST_OUT", "NO3CONC"), line=c(7,11,12,14,16,18,20,24, 34,50))
      line1 <- "output.rch     : swat output file name"
      line2 <- paste( length(rch_analytes)    , "              : number of variables to get")
      
      files_rch <- data.frame(file=list.files(paste(wd,"Raw Cal Files/observed_rch", sep="")))
      files_rch$analyte <-sub("_\\d{1,3}\\.txt", "", files_rch$file)
      files_rch$sub <- str_split2(files_rch$file, paste(files_rch$analyte, "_", sep=""), piece = 2)
      files_rch$sub <- as.numeric(str_split2(files_rch$sub, ".txt", piece=1))
      files_rch <- files_rch[order(files_rch$analyte, files_rch$sub ),]
      files_rch$index <- NA
      for(x in 1:nrow(files_rch)){
        files_rch$index[x] <- rch_out$line[which(rch_out$analyte == files_rch$analyte[x])]
      }
      line3 <- paste(paste(unique(files_rch$index), collapse=" "), "            : variable column number(s) in the swat output file (as many as the above number)") 
      line4 <- " "
      line5 <- paste(paste(nsub, collapse = " "), "              : total number of reaches (subbasins) in the project")
      
      line <- paste(line1, line2, line3, line4, line5, sep="\n")
      write.table(line, file= paste(wd, "SUFI2_extract_rch.def", sep=""), append=F, quote=F, col.names = F, row.names = F)
      write.table(" ", file= paste(wd, "SUFI2_extract_rch.def", sep=""), append=T, quote=F, col.names = F, row.names = F) 
    }
    
    if(is.null(sub_analytes)==F){
      ## write SUFI2_extract_sub.def file 
      sub_out <- data.frame(analyte=c("ET", "PET"), line=c(8, 7))
      line1 <- "output.sub     : swat output file name"
      line2 <- paste(length(sub_analytes), "              : number of variables to get")
      files_sub <- data.frame(file=list.files(paste(wd,"Raw Cal Files/observed_sub", sep="")))
      files_sub$analyte <-sub("_\\d{1,3}\\.txt", "", files_sub$file)
      files_sub$sub <- str_split2(files_sub$file, paste(files_sub$analyte, "_", sep=""), piece = 2)
      files_sub$sub <- as.numeric(str_split2(files_sub$sub, ".txt", piece=1))
      files_sub <- files_sub[order(files_sub$analyte, files_sub$sub ),]
      
      files_sub$index <- NA
      for(x in 1:nrow(files_sub)){
        files_sub$index[x] <- sub_out$line[which(sub_out$analyte == files_sub$analyte[x])]
      }
      line3 <- paste(paste(unique(files_sub$index), collapse=" "), "            : variable column number(s) in the swat output file (as many as the above number)") 
      line4 <- " "
      line5 <- paste(paste(nsub, collapse = " "), "              : total number of subbasins in the project")
      
      line <- paste(line1, line2, line3, line4, line5, sep="\n")
      write.table(line, file= paste(wd, "SUFI2_extract_sub.def", sep=""), append=F, quote=F, col.names = F, row.names = F)
      write.table(" ", file= paste(wd, "SUFI2_extract_sub.def", sep=""), append=T, quote=F, col.names = F, row.names = F) 
    }
   
    if(is.null(hru_analytes)==F){
      ## write SUFI2_extract_hru.def file 
      hru_out <- data.frame(analyte=c("ET", "LAI"), line=c(12, 71))
      line1 <- "output.hru    : swat output file name"
      line2 <- paste(length(hru_analytes), "              : number of variables to get")
      files_hru <- data.frame(file=list.files(paste(wd,"Raw Cal Files/observed_hru", sep="")))
      files_hru$analyte <-sub("_\\d{1,3}\\.txt", "", files_hru$file)
      files_hru$sub <- str_split2(files_hru$file, paste(files_hru$analyte, "_", sep=""), piece = 2)
      files_hru$sub <- as.numeric(str_split2(files_hru$sub, ".txt", piece=1))
      files_hru <- files_hru[order(files_hru$analyte, files_hru$sub ),]
      files_hru$index <- NA
      for(x in 1:nrow(files_hru)){
        files_hru$index[x] <- hru_out $line[which(hru_out $analyte == files_hru$analyte[x])]
      }
      line3 <- paste(paste(unique(files_hru$index), collapse=" "), "            : variable column number(s) in the swat output file (as many as the above number)") 
      line4 <- " "
      line5 <- paste(paste(nhru, collapse = " "), "         : total number of HRUs in the project")
      
      line <- paste(line1, line2, line3, line4, line5, sep="\n")
      write.table(line, file= paste(wd, "SUFI2_extract_hru.def", sep=""), append=F, quote=F, col.names = F, row.names = F)
      write.table(" ", file= paste(wd, "SUFI2_extract_hru.def", sep=""), append=T, quote=F, col.names = F, row.names = F) 
    }
    
    #get all raw cal files 
    files <- list.files(paste(wd, "Raw Cal Files/observed", sep=""))
      #remove files we don't want added 
        analytes <- c(rch_analytes, sub_analytes, hru_analytes)
        
        #make sure in number order
        files <- data.frame(file=files[str_detect(files, paste(analytes, collapse = "|"))])
        files$sub <- as.numeric(str_replace_all( files$file, paste(c(analytes, ".txt", "_"), collapse = "|"), ""))
        files$analyte <- gsub("_[0-9]+.txt", "", files$file)
        files <- files[order(files$analyte, files$sub ),]
    #create observed.txt file 
      for(x in 1:nrow(files)){
        lines <- readLines(paste(wd, "Raw Cal Files/observed/",files$file[x], sep=""))
        cat(lines, file=paste(wd,"SUFI2.IN/observed.txt", sep=""), append = T, sep="\n")}
    
    if(is.null(rch_analytes)==F){
      #create observed_rch.txt file
      for(x in files_rch$file){
        lines <- readLines(paste(wd, "Raw Cal Files/observed_rch/",x, sep=""))
        cat(lines, file=paste(wd,"SUFI2.IN/observed_rch.txt", sep=""), append = T, sep="\n")}
    
    #write extract_def's
    
      rch_sum <- files_rch %>% group_by(analyte) %>% mutate(sub = as.numeric(sub)) %>% arrange(sub) %>%
        summarise(count=n(), subbasins=paste(sub, collapse=" "))
      for(x in 1:nrow(rch_sum)){
        marker <- c("first","second","third","fourth")
        line1 <- paste(rch_sum$count[x],"              : number of reaches (subbasins) to get for the ", marker[x], " variable", sep="")
        line2 <- paste(rch_sum$subbasins[x], "        : reach (subbasin) numbers for the ", marker[x], " variable", sep="")
        line <- paste(line1, line2, " ", sep="\n")
        write.table(line, file= paste(wd, "SUFI2_extract_rch.def", sep=""), append=T, quote=F, col.names = F, row.names = F)
      }}
    
     if(is.null(sub_analytes)==F){
       #create observed_sub.txt file
       for(x in files_sub$file){
         lines <- readLines(paste(wd, "Raw Cal Files/observed_sub/",x, sep=""))
         cat(lines, file=paste(wd,"SUFI2.IN/observed_sub.txt", sep=""), append = T, sep="\n")}
     
       sub_sum <- files_sub %>% group_by(analyte) %>% mutate(sub = as.numeric(sub)) %>% arrange(sub) %>%
         summarise(count=n(), subbasins=paste(sub, collapse=" "))
       for(x in 1:nrow(sub_sum)){
         marker <- c("first","second","third","fourth")
         line1 <- paste(sub_sum$count[x],"              : number of reaches (subbasins) to get for the ", marker[x], " variable", sep="")
         line2 <- paste(sub_sum$subbasins[x], "        : reach (subbasin) numbers for the ", marker[x], " variable", sep="")
         line <- paste(line1, line2, " ", sep="\n")
         write.table(line, file= paste(wd, "SUFI2_extract_sub.def", sep=""), append=T, quote=F, col.names = F, row.names = F)
       }}
       
    if(is.null(hru_analytes)==F){
      #create observed_hru.txt file
      for(x in files_hru$file){
        lines <- readLines(paste(wd, "Raw Cal Files/observed_hru/",x, sep=""))
        cat(lines, file=paste(wd,"SUFI2.IN/observed_hru.txt", sep=""), append = T, sep="\n")}
      
      hru_sum <- files_hru %>% group_by(analyte) %>% mutate(sub = as.numeric(sub)) %>% arrange(sub) %>%
        summarise(count=n(), subbasins=paste(sub, collapse=" "))
      for(x in 1:nrow(hru_sum)){
        marker <- c("first","second","third","fourth")
        line1 <- paste(hru_sum$count[x],"              : number of HRUs to get for the ", marker[x], " variable", sep="")
        line2 <- paste(hru_sum$subbasins[x], "        : HRU number(s) for the ", marker[x], " variable", sep="")
        line <- paste(line1, line2, " ", sep="\n")
        write.table(line, file= paste(wd, "SUFI2_extract_hru.def", sep=""), append=T, quote=F, col.names = F, row.names = F)
      }}
    ##write variable name files
        setwd(paste(wd,"SUFI2.IN", sep=""))
        names <- str_split2(files$file, ".txt", piece=1)
        write.table(names, file="var_file_name.txt", append=T, quote=F, col.names=F, row.names=F)
        if(is.null(rch_analytes)==F){write.table(names <- str_split2(files_rch$file, ".txt", piece=1), file="var_file_rch.txt", append=T, quote=F, col.names=F, row.names=F)}
        if(is.null(sub_analytes)==F){write.table(names <- str_split2(files_sub$file, ".txt", piece=1), file="var_file_sub.txt", append=T, quote=F, col.names=F, row.names=F)}
        if(is.null(hru_analytes)==F){write.table(names <- str_split2(files_hru$file, ".txt", piece=1), file="var_file_hru.txt", append=T, quote=F, col.names=F, row.names=F)}
        
    ##finish writing SUFI2_extract_rch.def file 
    setwd(wd)
    line1<- paste(year(cal_start), "           : beginning year of simulation not including the warm up period")
    line2 <- paste(year(cal_end), "           : end year of simulation")
    line3 <- " "
    if(timestep == "daily"){
      line4 <- "1              : time step (1=daily, 2=monthly, 3=yearly)"
      
    }else if(timestep == "monthly"){
      line4 <- "2              : time step (1=daily, 2=monthly, 3=yearly)"
    }
    line <- paste(line1, line2, line3, line4, sep="\n")
    if(is.null(rch_analytes)==F){write.table(line, file= paste(wd, "SUFI2_extract_rch.def", sep=""), append=T, quote=F, col.names = F, row.names = F)}
    if(is.null(sub_analytes)==F){write.table(line, file= paste(wd, "SUFI2_extract_sub.def", sep=""), append=T, quote=F, col.names = F, row.names = F)}
    if(is.null(hru_analytes)==F){write.table(line, file= paste(wd, "SUFI2_extract_hru.def", sep=""), append=T, quote=F, col.names = F, row.names = F)}
    
    #adjust file for sites without data
    setwd(paste(wd,"SUFI2.IN", sep=""))
    if(is.null(rch_analytes)==F){
      new_num_rch <- length(readLines("var_file_rch.txt"))
      new_line1 <-  paste(new_num_rch, "      : number of observed variables", sep="")
      x <- readLines("observed_rch.txt")
      x[1] <- new_line1
      x <- x[1:(length(x)-2)]
      cat(x, file="observed_rch.txt", sep="\n")
    }
   
    if(is.null(sub_analytes)==F){
      new_num_sub <- length(readLines("var_file_sub.txt"))
      new_line1 <- paste(new_num_sub, "      : number of observed variables", sep="")
      x <- readLines("observed_sub.txt")
      x[1] <- new_line1
      x <- x[1:(length(x)-2)]
      cat(x, file="observed_sub.txt", sep="\n")
    }
    
    if(is.null(hru_analytes)==F){
      new_num_hru <- length(readLines("var_file_hru.txt"))
      new_line1 <- paste(new_num_hru, "      : number of observed variables", sep="")
      x <- readLines("observed_hru.txt")
      x[1] <- new_line1
      x <- x[1:(length(x)-2)]
      cat(x, file="observed_hru.txt", sep="\n")
    }
    
    new_num <- length(readLines("var_file_name.txt"))
    x <- readLines("observed.txt")
    new_line1 <- paste(new_num, "      : number of observed variables", sep="")
    x[1] <- new_line1
    x <- x[1:(length(x)-2)]
    cat(x, file="observed.txt", sep="\n")
    
    
    #print years 
    cat("NBYR", ((year(cal_end)-year(cal_start))+warm_up+1), sep="\n")
    cat("IYR", year(cal_start)-warm_up, sep="\n") 
  }

  
#section 1: load ET data and determine sites needed -------
  #if you haven't run 2-create-pet-file-txt.R yet, run that to download/clean the MODIS ET data
  
  #download cleaned ET files
  files <- list.files(paste(ETwd, "MODIS", sep=""))
  ET_file <- paste(ETwd, "MODIS/", files[grep("site_[0-9]+_MOD16A2GF_ET_500m", files)] , sep="")
  ET_dat <- comb_files(ET_file)
  
  sites <- unique(ET_dat %>% dplyr::select(x, y, site))
  
  #determine the sites I need for each HRU 
  #get ET site locations for different landuses 
  shp <- read_sf(hru_shp)
  shp <- shp[order(shp$HRUGIS, decreasing=F),] #need to give numbers to HRU's and they don't start out in order
  shp$HRU_ID <- 1:nrow(shp)
  landuse <- raster(paste(ETwd, "projected_spatial/landcover.tif", sep=""))
  modis_sites <- st_as_sf(x = sites, coords = c("x", "y"),crs = "EPSG:4326")
  plots <- list() #put maps on points across hru here for checking
  rm("et_loc")
  for(x in 1:nrow(et_sites)){
    print(x)
    subshp <- subset(shp, shp$HRU_ID == et_sites$hru[x])
    subshp <- st_transform(subshp, crs = "EPSG:4326")
    
    #figure out what sites are in HRU
    subsites <- st_intersection(subshp, modis_sites) 
    
    #check locations 
    p1<-ggplot() + geom_sf(data=subshp)  + geom_sf(data=subsites, color="red")
    plots[[x]] <- p1
    
    if(nrow(subsites) > 0){
      #check landuse 
      landuse <- projectRaster(landuse, crs="EPSG:4326", method="ngb")
      subsites$landuse <- terra::extract(landuse, subsites)
      
      #remove any incorrect landuses 
      subsites <- subset(subsites, subsites$landuse == et_sites$landuse[x])
      
      if(nrow(subsites) >0){
        #make site names unique 
        row.names(subsites) <- 1:nrow(subsites)
        
        if(exists("et_loc") == F){
          et_loc <- subsites }else{et_loc <- rbind(et_loc,subsites)}
      }else{cat(paste("hru", et_sites$hru[x], "had no ET spots"))}
      
    }
    
  }
  
#section 2: write calibration files -------- 
  #section 2.1: download data and format
      write_flow_raw(sub_sites, cal_start = cal_start, cal_end = cal_end, timestep=timestep)
      write_ET_raw(et_loc, et_sites, ETwd, cal_start = cal_start, cal_end = cal_end, 
                   timestep = timestep)
      write_DOC_raw(sub_sites, cal_start = cal_start, cal_end = cal_end, timestep=timestep,
                    doc_a =doc_a, doc_b = doc_b)
      write_nitrate_raw(sub_sites, cal_start = cal_start, cal_end = cal_end, timestep=timestep,
                    nit_a =nit_a, nit_b = nit_b)
      
  #section 2.2: write cal files
    write_cal_files(nsub,nhru, cal_start = cal_start, cal_end = cal_end, timestep=timestep, 
                    rch_analytes = c("FLOW_OUT", "SORPST_OUT", "NO3CONC"),
                    sub_analytes = NULL,
                    hru_analytes=c("ET"), 
                    function_type=5)
   

