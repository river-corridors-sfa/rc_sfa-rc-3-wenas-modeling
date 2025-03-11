#take raw swat model outputs and extract data for further data analysis 
  #written by Katie A. Wampler 
  #NOTE: this code can take several hours to run

#set save path
  data_save_path <- "~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/cleaned-rch-files"
  summary_save_path <- "~/1_Research/0_Misc/rc_sfa-rc-3-wenas-modeling/data-package/outputs/summary-outputs"
  
#section 0: load libraries and functions -------
  library(tools)
  library(readr)
  library(stringr)
  library(data.table)
  library(lubridate)
  library(tidyr)
  library(doSNOW)
  library(parallelly)
  library(foreach)
  library(dplyr)
  
  
  #calculate richard baker flashiness index (Baker et al. 2004)
  rb_flashiness <- function(timeseries, solute){
    df <- timeseries %>% dplyr::select(dates, any_of(solute))
    
    #replace NA's with 0 
    #df[is.na(df[,2]) == T,2] <- 0
    vals <- unlist(as.vector(df[-nrow(df),2]))
    df$shift <- c(NA, vals)
    df$dif <- abs(df[,2]- df[,3])
    rb_index <- sum(df$dif, na.rm = T) / sum(df[,2])
    
    return(rb_index)
  } 
  
  #section 5.1: extract data of interest from output.rch file
  #' Clean flow data from SWAT 
  #' 
  #' function that reads the output.rch file from SWAT and extracts the columns of interest and
  #' converts them from loads to concentrations and adds dates
  #'
  #' @param file_name the name of the output.rch file
  #' @param start the starting date of the results
  #' @param end the ending date of the results
  #' @param n_sub the number of subbasins in the model
  #' @param outlet the subbasin number associated with the outlet of the basin 
  #' @param area the area of the entire basin in km2
  #' @param fire the date of the wildfire
  #'
  #'
  clean_flow <- function(file_name, file_loc, n_sub=21, start="2005-01-01", end="2018-12-31", outlet=1, area, fire){
    #calculate richard baker flashiness index (Baker et al. 2004)
    rb_flashiness <- function(timeseries, solute){
      df <- timeseries %>% dplyr::select(dates, any_of(solute))
      
      #replace NA's with 0 
      #df[is.na(df[,2]) == T,2] <- 0
      vals <- unlist(as.vector(df[-nrow(df),2]))
      df$shift <- c(NA, vals)
      df$dif <- abs(df[,2]- df[,3])
      rb_index <- sum(df$dif, na.rm = T) / sum(df[,2])
      
      return(rb_index)
    } 
    
    name <- gsub("_output.rch", "_clean_rch.csv", file_name)
    save_file <- file.path(paste0(data_save_path, "/rch_files_lvl1"), file_loc, name)
    if(file.exists(save_file) == F){
      #load data
      reach <- fread(file_name, skip = 9)
      #reach <- read_table(file_name, col_names = FALSE, skip = 9)
      reach <- reach[,c(2,7,11,12,14,16,18,24,34)]
      colnames(reach) <- c("subbasin", "flow", "sed", "sed_con", "org_n", "org_p", 
                           "nitrate","othop","doc")
      
      dates <- seq(from=as.Date(start), to=as.Date(end), by="day")
      dates <- rep(dates, n_sub)
      reach$dates <- dates[order(dates)]
      
      #get concentrations
      reach$flow_L <- reach$flow * 60*60*24* 1000 #flow in L per day  
      reach$org_n_mgL <- reach$org_n * 1e6 / reach$flow_L 
      reach$org_p_mgL <- reach$org_p * 1e6 / reach$flow_L 
      reach$nitrate_mgL <- reach$nitrate * 1e6 / reach$flow_L 
      reach$othop_mgL <- reach$othop * 1e6 / reach$flow_L
      reach$doc_mgL <- reach$doc * 1e6 / reach$flow_L 
      reach$sed_conc_check <- reach$sed  * 1e6 / reach$flow_L  *1000 
      
      #add info about scenario 
      if(grepl("UNBURN", file_name)){
        reach$scenario <- str_split_i(file_name, "_", i=1)
        reach$sev <- str_split_i(file_name, "_", i=1) 
        reach$year <- str_split_i(file_name, "_", i=2) 
      }else{
        reach$scenario <- paste("PER", str_split_i(file_name, "_", i=2), sep="_")
        reach$sev <- str_split_i(file_name, "_", i=3) 
        reach$year <- str_split_i(file_name, "_", i=4) 
      }
      
      #write level 1 dataset (daily values)
      fwrite(reach, save_file)
      #write_csv(reach, file.path("rch_files_lvl1/", paste(reach$scenario[1], reach$sev[1], reach$year[1],  "clean_rch.csv", sep="_"))) 
      
    }else{
      reach <- fread(save_file)
    }
    
    #get annual values 
    #subset to post-fire year at outlet
    reach_fire <- reach[reach$dates >= as.Date(fire) & reach$dates < (as.Date(fire) + years(1)) & reach$subbasin == outlet,]
    
    #convert flow to mm/day 
    #seconds to day (m3/day)
    reach_fire$flow_mm_d <- reach_fire$flow  * 60*60*24 
    
    #divide by area (m/day)
    area_m2 <- area * 1000000
    reach_fire$flow_mm_d   <- reach_fire$flow_mm_d   / area_m2 
    
    #convert to mm/day 
    reach_fire$flow_mm_d <- reach_fire$flow_mm_d * 1000
    
    #get richard baker flashiness 
    rb_doc <- rb_flashiness(reach_fire, "doc_mgL")
    rb_nit <- rb_flashiness(reach_fire, "nitrate_mgL")
    rb_doc_ld <- rb_flashiness(reach_fire, "doc")
    rb_nit_ld <- rb_flashiness(reach_fire, "nitrate")
    
    #sum to mm/yr and kg/yr 
    annual <- reach_fire %>% group_by(scenario, sev, year) %>% 
      summarise(flow_mm_yr = sum(flow_mm_d), 
                nitrate_kg_yr = sum(nitrate), 
                avg_nitrate_mgL = mean(nitrate_mgL), 
                sd_nitrate_mgL = sd(nitrate_mgL), 
                q_05_nitrate = quantile(nitrate_mgL, 0.05),
                q_25_nitrate = quantile(nitrate_mgL, 0.25),
                q_50_nitrate = quantile(nitrate_mgL, 0.5),
                q_75_nitrate = quantile(nitrate_mgL, 0.75),
                q_95_nitrate = quantile(nitrate_mgL, 0.95),
                doc_kg_yr = sum(doc), 
                avg_doc_mgL = mean(doc_mgL),
                sd_doc_mgL = sd(doc_mgL),
                q_05_doc = quantile(doc_mgL, 0.05),
                q_25_doc = quantile(doc_mgL, 0.25),
                q_50_doc = quantile(doc_mgL, 0.5),
                q_75_doc = quantile(doc_mgL, 0.75),
                q_95_doc = quantile(doc_mgL, 0.95),
                .groups = "drop") %>% 
      mutate(nitrate_rb = rb_nit, nitrate_rb_ld=rb_nit_ld,
             doc_rb = rb_doc, doc_rb_ld=rb_doc_ld 
      )
    
    return(annual) #output so we can merge together and save
  }
  
  
  #' to get the fire year from a set of dates
  #'
  #' @param date_col a vector or dataframe column with dates to transform
  #' @param fire_date the date of the wildfire
  #'
  #' @return a vector of "wildfire" years
  fire_year <- function(date_col, fire_date){
    year <- lubridate::year(date_col)
    fire_date <- as.Date(fire_date)
    month_day <- as.Date(paste("1900",lubridate::month(date_col), lubridate::day(date_col), sep="-"))
    fire_month_day <- as.Date(paste("1900",lubridate::month(fire_date), lubridate::day(fire_date), sep="-"))
    prev_year <- which(month_day < fire_month_day)
    year[prev_year] <- year[prev_year] - 1
    return(year)
  } 
  
 
  #read hru file, extract flow partitioning, sum over post-fire year per hru, average across hru
  get_hru_flows <- function(path, file, start, fire, nhru){
    dates <- seq(from=fire, length=365, by="day")
    nskip <- (length(seq(from=start, to=fire-days(1), by="day"))*nhru )+9
    
    if(grepl("UNBURN", file)){
      scenario <- "PER_0"
      sev <- "UNBURN"
      year <- str_split_i(file, "_", 2)
    }else{
      scenario <- paste("PER_", str_split_i(file, "_", 2), sep="")
      sev <- str_split_i(file, "_", 3)
      year <- str_split_i(file, "_", 4)
    }
    #load hru and get hru and dates
    hru <- data.table::fread(file.path(path, file), select= c(7, 12, 24,26,27,28), skip=nskip,nrows=(length(dates)*nhru))
    colnames(hru) <- c("PRECIP", "ET", "SURQ", "LATQ", "GWQ", "WYLD")
    hru$date <- rep(dates, each=nhru)
    hru$hru <- rep(1:nhru, times=length(dates))
    
    #get averages across basin 
    hru_sum <- hru %>% group_by(hru) %>% summarise_at(c("PRECIP","ET",
                                                        "SURQ","LATQ","GWQ","WYLD"), 
                                                      ~sum(.x, na.rm = T)) %>% 
      mutate(WYLD_check = SURQ+LATQ+GWQ,
             SURQ_per = SURQ / WYLD_check * 100,
             LATQ_per = LATQ / WYLD_check * 100,
             GWQ_per =  GWQ / WYLD_check * 100,
      ) %>% ungroup() %>% summarise(SURQ_PER = mean(SURQ_per),
                                    LATQ_PER = mean(LATQ_per),
                                    GWQ_PER = mean(GWQ_per),
                                    PRECIP = mean(PRECIP),
                                    ET = mean(ET),
                                    SURQ = mean(SURQ),
                                    LATQ = mean(LATQ), 
                                    GWQ = mean(GWQ)) %>%
      mutate(scenario = scenario, 
             sev = sev, 
             year= year)
    return(hru_sum)
  }
  
 
#section 1: load and tidy data and get annual loads / flow depth  ------- 
  #section 2.1: american  
    #specify data info for running clean_flow function
    setwd("~/1_Research/4_Wenas_Thresholds/data/american_scenario_outputs") #location of the data
    outlet <- 1 #subbasin number for the outlet
    area <- 206.834425 #km2
    fire <- "2017-08-11" #date of fire
    
    #get file names 
    files <- list.files()
    rch <- files[file_ext(files) == "rch"]
    
    #run cleaning script (this can take a bit)
    nCores <- parallelly::availableCores() - 1
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = length(rch), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    annuals <- foreach(i=1:length(rch), .options.snow = opts,
                       .packages=c("data.table", "stringr", "dplyr", "lubridate"), 
                       .combine=rbind) %dopar%{
                         df <- clean_flow(rch[i], n_sub=21, file_loc="American", outlet=outlet, area=area, fire=fire)
                         return(df)}
    stopCluster(cl)
    
    annuals <- annuals[annuals$year != "1987",] #ran an extra year for the american, remove that to get 30 years
    
    #load precip data 
    n_stats <- 10 #number of precip stations
    setwd("C:/SWAT/American River Simp2/American River Simp2/Scenarios/American River Simp2.Sufi2.SwatCup/Backup/")
    pcp <- read.table("pcp1.pcp", skip=4)
    header <- read_lines("pcp1.pcp", n_max = 4)
    col <- c(7, rep(5,n_stats)) #seven precip stations used
    names(col) <- c("date", paste0("pcp", 1:n_stats))
    pcp <- pcp %>% separate_wider_position(V1, widths=col)
    pcp$date <- as.Date(pcp$date, format="%Y%j") 
    pcp <-  pcp %>% mutate_if(is.character, as.numeric) #ensure values are numeric 
    pcp$avg <- rowMeans(pcp[,-1]) #get average across all sites
    pcp$fire_year <- fire_year(pcp$date, fire) #get fire year
    
    annual_precip <- pcp %>% group_by(fire_year) %>% summarise(precip_mm = sum(avg))
    
    annuals <- merge(annuals, annual_precip, by.x="year", by.y="fire_year")
    write_csv(annuals, filepath(summary_save_path, "annual_loads_rch_american.csv")) 
    
  #section 2.2: tule 
    #specify data info for running clean_flow function
    setwd("~/1_Research/4_Wenas_Thresholds/data/tule_scenario_outputs")
    outlet <- 1 
    area <- 249.9807#km2
    fire <- "2017-08-11"
    
    #get file names 
    files <- list.files()
    rch <- files[file_ext(files) == "rch"]
    
    #run cleaning script (this can take a bit)
    library(doSNOW)
    library(parallelly)
    library(foreach)
    nCores <- parallelly::availableCores() - 1
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = length(rch), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    annuals <- foreach(i=1:length(rch), .options.snow = opts,
                       .packages=c("data.table", "stringr", "dplyr", "lubridate"), 
                       .combine=rbind) %dopar%{
                         df <- clean_flow(rch[i], n_sub=21, file_loc="Tule",outlet=outlet, area=area, fire=fire)
                         return(df)}
    stopCluster(cl)
    annuals <- annuals[annuals$year != "1987",] #ran an extra year for the american, remove that to get 30 years
    
    #load precip data 
    n_stats <- 10 #number of precip stations
    setwd("C:/SWAT/tule River Simp2/tule River Simp2/Scenarios/tule River Simp2.Sufi2.SwatCup/Backup/")
    pcp <- read.table("pcp1.pcp", skip=4)
    header <- read_lines("pcp1.pcp", n_max = 4)
    col <- c(7, rep(5,n_stats)) #seven precip stations used
    names(col) <- c("date", paste0("pcp", 1:n_stats))
    pcp <- pcp %>% separate_wider_position(V1, widths=col)
    pcp$date <- as.Date(pcp$date, format="%Y%j") 
    pcp <-  pcp %>% mutate_if(is.character, as.numeric) #ensure values are numeric 
    pcp$avg <- rowMeans(pcp[,-1]) #get average across all sites
    pcp$fire_year <- fire_year(pcp$date, fire) #get fire year
    
    annual_precip <- pcp %>% group_by(fire_year) %>% summarise(precip_mm = sum(avg))
    
    annuals <- merge(annuals, annual_precip, by.x="year", by.y="fire_year")
    write_csv(annuals, filepath(summary_save_path, "annual_loads_rch_tule.csv")) 

  
#section 2: load and tidy HRU data to get flow path info ------
  #american 
    nhru <- 198 
    start <- as.Date("2005-01-01")
    fire <- as.Date("2017-08-11")
    path <- "~/1_Research/4_Wenas_Thresholds/data/american_scenario_outputs/"
    hru_files <- list.files(path, pattern=".hru")
    
    #run in parallel
    nCores <- parallelly::availableCores() - 1
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = length(hru_files), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    hru_sum <- foreach(i=1:length(hru_files), .options.snow = opts,
                       .packages=c("data.table", "stringr", "dplyr", "lubridate"), 
                       .combine=rbind) %dopar%{
                         df <-  get_hru_flows(path, hru_files[i], 
                                              start=start, fire=fire,nhru=nhru)
                         return(df)}
    stopCluster(cl)
    
    write.csv(hru_sum, file.path(summary_save_path, "hru_summary_american.csv"), row.names=F)
  
  #tule 
    nhru <- 224
    start <- as.Date("2005-01-01")
    fire <- as.Date("2017-08-11")
    path <- "~/1_Research/4_Wenas_Thresholds/data/tule_scenario_outputs/"
    hru_files <- list.files(path, pattern=".hru")
    
    #run in parallel
    nCores <- parallelly::availableCores() - 1
    cl <- parallel::makeCluster(nCores)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = length(hru_files), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    hru_sum <- foreach(i=1:length(hru_files), .options.snow = opts,
                       .packages=c("data.table", "stringr", "dplyr", "lubridate"), 
                       .combine=rbind) %dopar%{
                         df <-  get_hru_flows(path, hru_files[i], 
                                              start=start, fire=fire,nhru=nhru)
                         return(df)}
    stopCluster(cl)
    
    write.csv(hru_sum, file.path(summary_save_path, "hru_summary_tule.csv"), row.names=F)
    
  