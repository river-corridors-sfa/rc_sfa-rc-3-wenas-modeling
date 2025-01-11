## script used to change the scenarios (wild_fire.txt and pcp1.pcp files) and run the model to get the outputs 
  #written by Katie A. Wampler on 2024-10-24 

#section 0: load functions and package ------ 
  #save files somewhere else, needs to be in quotes
    save_files <- function(filename, startwd, savewd, extra_files=NULL){
       # dir.create(file.path(savewd, filename), showWarnings = F)
        files <- c(extra_files, c("output.rch", "output.std", "output.sub", "basins.bsn", "output.hru"))
        file.copy(file.path(startwd, files), file.path(savewd, paste0(filename, "_", files)), overwrite = T)
      }

  #start fire (turn fire on in basin file) still need fire date in file and wild_fire txt in folder 
    start_fire <- function(wd){
    setwd(wd)
    basin <- readLines("basins.bsn")
    basin[132] <- "               1    | fire: for wildfire scenarios: 1 fire, 0 nofire"
    write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)
    
  }

  #remove fire  (turn fire off in basin file)
    remove_fire <- function(wd){
    setwd(wd)
    basin <- readLines("basins.bsn")
    basin[132] <- "               0    | fire: for wildfire scenarios: 1 fire, 0 nofire"
    write.table(basin, "basins.bsn", quote=F, row.names=F, col.names = F)} 
  
  #change run length 
    set_runyears <- function(wd, years, end=2021, warmup=10){
    setwd(wd)
    file <- readLines("file.cio")
    start <- end - warmup - years +1
    file[8] <- paste("              ", (years+warmup), "    | NBYR : Number of years simulated", sep="" )
    file[9] <- paste("            ", start, "    | IYR : Beginning year of simulation", sep="" )
    
    write.table(file, "file.cio", quote=F, row.names=F, col.names = F)
  }

#section 1: run American scenarios ------- 
  #specify working directories
  scenariowd <- "~/1_Research/4_Wenas_Thresholds/data/American River/Scenario Changes" #where files for scenarios are kept
  modelwd <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/American River Simp2.Sufi2.SwatCup" #where model lives
  cleandatwd <- "~/1_Research/4_Wenas_Thresholds/data/american_scenario_outputs" #where to put output files [NOTE: scenarios are large, need ~1.62 TB per model]
  
  per <- seq(from=5, to=100, by=5) #percent burned scenarios
  sev <- c("LOW", "MOD" , "HIGH") #burn severity scenarios
  years <- 1988:2017 #precipitation years
  
  scenarios <- c("UNBURN", paste(rep(per, length(sev)), rep(sev, each=length(per)), sep="_"))
  scenarios <- c( paste(rep(scenarios, length(years)), rep(years, each=length(scenarios)), sep="_") )

  scenarios <- c(paste("PER_", seq(from=5, to=100, by=5), sep=""))
  
  set_runyears(modelwd, 14, end=2018, warmup = 5)
  remove_fire(modelwd)   #ensure no burn

  #run unburned scenarios 
  for(y in years){
    paste(y)
    
    #change the precip file 
    file.copy(file.path(scenariowd, "Precip", y, "pcp1.pcp"), modelwd, overwrite=T) 
    
    #run swat
    setwd(modelwd)
    system2("swat.exe")
    
    #save files 
    save_files(paste0("UNBURN_", y), modelwd, cleandatwd)
  }
  
  #run fire scenarios
  for(s in scenarios){
    for(sev in c("LOW", "MOD", "HIGH")){
      start_fire(modelwd)
      
      #change the burned areas
      file.copy(file.path(scenariowd, paste(s, "/", sev, "/wild_fire.txt", sep="")), modelwd, overwrite = T)
      
      #run unburned scenarios 
      for(y in years){
        paste(y)
        
        #change the precip file 
        file.copy(file.path(scenariowd, "Precip", y, "pcp1.pcp"), modelwd, overwrite=T) 
        
        #run swat
        setwd(modelwd)
        system2("swat.exe")
        
        #save files 
        save_files(paste0(s,"_", sev, "_", y), modelwd, cleandatwd, "wild_fire.txt")
      }}}
  
 
  
#section 2: run Tule scenarios ------- 
  #specify working directories
  scenariowd <- "~/1_Research/4_Wenas_Thresholds/data/Tule_River/Scenario Changes" #where files for scenarios are kept
  modelwd <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/Tule River Simp2.Sufi2.SwatCup" #where model lives
  cleandatwd <- "~/1_Research/4_Wenas_Thresholds/data/tule_scenario_outputs" #where to put output files 
  
  per <- seq(from=5, to=100, by=5)
  sev <- c("LOW", "MOD" , "HIGH")
  years <- 1987:2017
  
  scenarios <- c("UNBURN", paste(rep(per, length(sev)), rep(sev, each=length(per)), sep="_"))
  scenarios <- c( paste(rep(scenarios, length(years)), rep(years, each=length(scenarios)), sep="_") )
  
  scenarios <- c(paste("PER_", seq(from=5, to=100, by=5), sep=""))
  
  set_runyears(modelwd, 14, end=2018, warmup = 5)
  remove_fire(modelwd)   #ensure no burn
  
  #run unburned scenarios 
  for(y in years){
    paste(y)
    
    #change the precip file 
    file.copy(file.path(scenariowd, "Precip", y, "pcp1.pcp"), modelwd, overwrite=T) 
    
    #run swat
    setwd(modelwd)
    system2("swat.exe")
    
    #save files 
    save_files(paste0("UNBURN_", y), modelwd, cleandatwd)
  }
  
  #run fire scenarios
  for(s in scenarios){
    for(sev in c("LOW", "MOD", "HIGH")){
      start_fire(modelwd)
      
      #change the burned areas
      file.copy(file.path(scenariowd, paste(s, "/", sev, "/wild_fire.txt", sep="")), modelwd, overwrite = T)
      
      #run unburned scenarios 
      for(y in years){
        paste(y)
        
        #change the precip file 
        file.copy(file.path(scenariowd, "Precip", y, "pcp1.pcp"), modelwd, overwrite=T) 
        
        #run swat
        setwd(modelwd)
        system2("swat.exe")
        
        #save files 
        save_files(paste0(s,"_", sev, "_", y), modelwd, cleandatwd, "wild_fire.txt")
      }}}
  
  
  