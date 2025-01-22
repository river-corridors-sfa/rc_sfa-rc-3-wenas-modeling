##script to run different scenarios for wenas creek  

#written by Katie A. Wampler on 2022-06-28 
  #because I can't get swat_edit.exe to run within swat, run ahead of time and
  #save altered files in a separate folder


###to check

  #libraries 
    library(stringr)
    library(PBSmodelling)
    library(readr)
    library(gridExtra)
    library(dplyr)
    library(tidyr)
    library(Hmisc)
    library(lubridate)

  #functions to run the model 
    #save files somewhere else needs to be in quotes
      save_files <- function(filename){
      setwd("D:/Wenas Creek Scenarios")
      dir.create(filename, showWarnings = F)
      setwd(paste(headwd, sep=""))
      files <- c("output.hru", "output.rch", "output.std", "output.sub", "wild_fire.txt", "basins.bsn")
      file.copy(paste(headwd, files, sep=""), paste("D:/Wenas Creek Scenarios/", filename ,sep=""), overwrite = T)
    }
    #paste fresh files 
      fresh_files <- function(){
      setwd(paste(headwd, "Fresh Files",sep=""))
      files <- list.files()
      invisible(file.copy(files, paste(headwd ,sep=""), overwrite = T))
      
    } 
    #change land use  (118 is BARR, 15 is RNGE)
      land_update <- function(land_use){
      setwd(paste(headwd, sep=""))
      files <- list.files()
      mgt <- files[str_detect(files, ".mgt")]
      mgt <- mgt[str_detect(mgt, "00")]
      
      m <- mgt[6]
      land_use <- str_pad(as.character(land_use), 3, side = c("left"), pad = " ")
      #change code for land use (118 is BARR, 15 is RNGE)
      line <- paste("          0.150  1 ", land_use,"        2355.00000   0.00     0.00000 0.00   0.00  0.00", sep="")
      for (m in mgt){
        file <- readLines(m)
        if(str_detect(file[1], "FRSE") ==T ){
          file[31] <- line
          cat(file, file=m, append = F, sep="\n")
        } 
        print(m)
      } 
      } 
    #start fire 
      start_fire <- function(){
        setwd(headwd)
        basin <- readLines("basins.bsn")
        basin[132] <- "               1    | fire: for wildfire scenarios: 1 fire, 0 nofire"
        write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)
        file.copy(paste(scenariowd, "BASE_FIRE/wild_fire.txt", sep=""), headwd, overwrite = T)
        
        }
    #remove fire 
      remove_fire <- function(){
        setwd(headwd)
        basin <- readLines("basins.bsn")
        basin[132] <- "               0    | fire: for wildfire scenarios: 1 fire, 0 nofire"
        write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)}
    
  #specify model information 
    headwd <- "C:/SWAT/Wenas Creek_Hyunwoo/"
    scenariowd <- "C:/SWAT/Wenas Creek_Hyunwoo/Scenario Changes/"

    subbasin <- 313
    hru_num <- 5210
  
    fire_date <- "2019-08-31" #shifted a year earlier to account for lack of met data in 2021
    fire_date <- strftime(fire_date, format="%j")
    
    #12    | NBYR : Number of years simulated (includes 5 years pre-fire)
    #2009    | IYR : Beginning year of simulation
  
  #run normal unburned scenario ------
    fresh_files()
    remove_fire()
    file.copy(paste(scenariowd, "BASE_PCP/pcp1.pcp", sep=""), headwd, overwrite = T)
    setwd(headwd)
    system2("swat.exe")
    save_files("base_unburned")
    
  #run normal fire scenario ------
    start_fire()
    file.copy(paste(scenariowd, "BASE_FIRE/wild_fire.txt", sep=""), headwd, overwrite = T)
    setwd(headwd)
    system2("swat.exe")
    save_files("base_burned")
  
  #run scenario 1 (burn severity)------
    #uses overall base scenario
    scenarios1 <- c("LOW", "MOD", "HIGH")
    for(s in scenarios1){
      start_fire()
      
      #alter burn severity
      file.copy(paste(scenariowd, s, "_FIRE/wild_fire.txt", sep=""), headwd, overwrite = T)
      
      #run swat
      setwd(headwd)
      system2("swat.exe")
      
      #save files 
      save_files(s)
    } 
  
  #run scenario 2 (area burned) ------
    #uses overall base scenario
    scenarios2 <- c(paste("PER_", c(10,15,20,25,30,40,45,50,60,75,90,100), sep=""))
    for(s in scenarios2){
      for(sev in c("LOW", "MOD", "HIGH")){
        start_fire()
        
        #change the burned areas
        file.copy(paste(scenariowd, s, "/", sev, "/wild_fire.txt", sep=""), headwd, overwrite = T)
        
        #run swat
        print("running SWAT")
        setwd(headwd)
        system2("swat.exe")
        
        #save files 
        print("saving files")
        save_files(paste(s, sev, sep="_"))
        
        print(paste(s, sev, sep="_"))
      }
    }
    
  #run scenario 4 (catchment slope) ------
    #needs to run fire/nofire scenarios
    scenarios4 <- c(paste("HRU_SLP_", c(0.1,0.5,1), sep=""), paste("HRU_SLP_neg_", c(0.1,0.5,1), sep=""))
    for(s in scenarios4){
      #paste files with altered slope changes
      scen <- paste(scenariowd, s, sep="")
      files <- list.files(scen)
      file.copy(paste(scen, files, sep="/"), headwd, overwrite = T)
      
      #run swat with fire
      start_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_fire", sep=""))
        
      #run swat without fire
        remove_fire()
        setwd(headwd)
        system2("swat.exe")
        
        #save files 
        save_files(paste(s,"_nofire", sep=""))
      
      #get clean files
        fresh_files()
    }
  
  #run scenario 5 (amount of precipitation)------
    #needs to run fire/nofire scenarios
    scenarios5 <- c("DRY", "WET")
    for(s in scenarios5){
      #paste in altered pcp1 file
      scen <- paste(scenariowd, s, sep="")
      files <- list.files(scen)
      file.copy(paste(scen, files, sep="/"), headwd, overwrite = T)
      
      #run swat with fire
      start_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_fire", sep=""))
      
      #run swat without fire
      remove_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_nofire", sep=""))
        
      #get clean files
        fresh_files()
    }
    
  #run scenario 6 (vegetation type) ------
    #needs to run fire/nofire scenarios
    scenarios6 <- c("grass", "shrub", "deciduous", "coniferous")
    for(s in scenarios6){
      #get land use number
      if(s == "grass"){
        landuse <- 15} else if(s == "shrub"){
          landuse <- 16} else if(s== "deciduous"){
            landuse <- 7} else if(s == "coniferous"){
              landuse <- 8}
      
      #update landuse
      land_update(landuse)
      
      #run swat with fire
      start_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_fire", sep=""))
        
      #run swat without fire
      remove_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_nofire", sep=""))
        
      #get clean files
        fresh_files()
      }
    
  #run scenario 7 (streamflow source) ------
    #needs to run fire/nofire scenarios
    scenarios7 <- c("SURQ", "LATQ", "GWQ")
    for(s in scenarios7){
      #paste files with altered parameter changes
      scen <- paste(scenariowd, s, sep="")
      files <- list.files(scen)
      file.copy(paste(scen, files, sep="/"), headwd, overwrite = T)
      
      #run swat with fire
      start_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_fire", sep=""))
        
      #run swat without fire
      remove_fire()
      setwd(headwd)
      system2("swat.exe")
      
        #save files 
        save_files(paste(s,"_nofire", sep=""))
        
      #get clean files
        fresh_files()
    }
  #_____________________________________________________________________________
  #run scenario 3 (distance to main reach)  [WAIT ON THIS ONE] ---------
    #uses overall base scenario
    scenarios3 <- c("NEAR", "FAR")
    for(s in scenarios3){
    #change the burned areas
    file.copy(paste(scenariowd, s, "/wild_fire.txt", sep=""), headwd, overwrite = T)
    
    #run swat
    setwd(headwd)
    system2("swat.exe")
    
    #save files 
    save_files(s)
    }
