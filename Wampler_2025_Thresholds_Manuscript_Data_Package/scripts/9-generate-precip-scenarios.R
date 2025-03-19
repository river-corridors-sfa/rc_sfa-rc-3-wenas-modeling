## script used to generate the different precipitation scenarios when running the wildfire scenarios 
  #written by Katie A. Wampler on 2024-10-24 

#section 0: load functions and libraries ----- 
  library(tidyr)
  library(lubridate)

#section 1: generate new precip scenarios for American ------ 
  #load precip file 
    n_stats <- 10 #number of precip stations
    setwd("C:/SWAT/American River Simp2/American River Simp2/Scenarios/American River Simp2.Sufi2.SwatCup/Backup/") #location of existing pcp file
    pcp <- read.table("pcp1.pcp", skip=4)
    header <- read_lines("pcp1.pcp", n_max = 4)
    col <- c(7, rep(5,n_stats))
    names(col) <- c("date", paste0("pcp", 1:n_stats))
    pcp <- pcp %>% separate_wider_position(V1, widths=col)
    pcp$date <- as.Date(pcp$date, format="%Y%j")
    
    dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/PRECIP", sep=""), showWarnings = F)
    
  #set_postfire year 
    fire_date <- as.Date("2017-08-11") #middle of the two real fire ignition dates 
    dates <- seq(fire_date, fire_date + years(1) - days(1), by="day") #this will be the year of post-fire simulation 
    
  #get previous 30 years of precip for those dates to get uncertainty in post-fire response 
    years <- seq( - 29, to=0, by =1)
    fireyear_rows <- which(pcp$date %in% dates) #rows with initial post-fire precip
    
    for(x in years){
      repl_rows <- which(pcp$date %in% seq(from=(fire_date + years(x)), to=(fire_date + years(x + 1) - days(1)), by="day"))
      #remove leap day
      if(length(repl_rows) == 366){repl_rows <- repl_rows[-203]}
      
      #replace precip 
      pcp_edit <- pcp
      pcp_edit[fireyear_rows, 2:(n_stats+1)] <- pcp_edit[repl_rows,  2:(n_stats+1)]
      
      #format for saving 
      pcp_edit$date <- strftime(pcp_edit$date, "%Y%j")
      pcp_edit$file <- do.call(paste, c(pcp_edit[1:(n_stats+1)], sep=""))
      
      save_loc <- paste("~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes/PRECIP/", year(fire_date + years(x)), sep="")
      dir.create(save_loc, showWarnings = F)
      write.table(header, file.path(save_loc,"/pcp1.pcp"), quote=F, row.names=F, col.names=F, sep="")
      write.table(pcp_edit$file, file.path(save_loc,"/pcp1.pcp"), quote=F, row.names=F, col.names=F, sep="", append = TRUE)
    }
    
    
#section 2: generate new precip scenarios for Tule ----- 
    #load precip file 
    n_stats <- 10 #number of precip stations
    setwd("C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/Tule River Simp2.Sufi2.SwatCup/Backup/")
    pcp <- read.table("pcp1.pcp", skip=4)
    header <- read_lines("pcp1.pcp", n_max = 4)
    col <- c(7, rep(5,n_stats)) 
    names(col) <- c("date", paste0("pcp", 1:n_stats))
    pcp <- pcp %>% separate_wider_position(V1, widths=col)
    pcp$date <- as.Date(pcp$date, format="%Y%j")
    
    dir.create(paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/PRECIP", sep=""), showWarnings = F)
    
    #set_postfire year 
    fire_date <- as.Date("2017-08-11") #middle of the two real fire ignition dates 
    dates <- seq(fire_date, fire_date + years(1) - days(1), by="day") #this will be the year of post-fire simulation 
    
    #get previous 30 years of precip for those dates to get uncertainty in post-fire response 
    years <- seq( - 29, to=0, by =1)
    fireyear_rows <- which(pcp$date %in% dates) #rows with initial post-fire precip
    
    for(x in years){
      repl_rows <- which(pcp$date %in% seq(from=(fire_date + years(x)), to=(fire_date + years(x + 1) - days(1)), by="day"))
      #remove leap day
      if(length(repl_rows) == 366){repl_rows <- repl_rows[-203]}
      
      #replace precip 
      pcp_edit <- pcp
      pcp_edit[fireyear_rows, 2:(n_stats+1)] <- pcp_edit[repl_rows,  2:(n_stats+1)]
      
      #format for saving 
      pcp_edit$date <- strftime(pcp_edit$date, "%Y%j")
      pcp_edit$file <- do.call(paste, c(pcp_edit[1:(n_stats+1)], sep=""))
      
      save_loc <- paste("~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes/PRECIP/", year(fire_date + years(x)), sep="")
      dir.create(save_loc, showWarnings = F)
      write.table(header, file.path(save_loc,"/pcp1.pcp"), quote=F, row.names=F, col.names=F, sep="")
      write.table(pcp_edit$file, file.path(save_loc,"/pcp1.pcp"), quote=F, row.names=F, col.names=F, sep="", append = TRUE)
    }
    