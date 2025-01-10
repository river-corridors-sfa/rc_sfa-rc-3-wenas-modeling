## misc scripts and functions that are helpful when running/calibrating a SWAT model in SWAT-cup 
  #written by Katie A. Wampler

#function 1: test a/b values for creating dummy DOC/nitrate records ------ 
  library(waterData)
  library(ggplot2)

  site <- "12488500"

  #download full flow file from USGS
  data <- importDVs(site, code = "00060", sdate = cal_start, edate = cal_end) 
  data <- fillMiss(data, block=15, pmiss = 10, model = "trend")  #fill in daily gaps
  data$val <- data$val * 0.0283168 #convert to from cfs to m3/s 
  
  #choose a/b values
  a <- 0.05
  b <- 1.2
  
  #convert flow in to dummy DOC
  data$val_adj <- a * data$val^b #doc/nitrate value
  
  #get stats 
  conv_test <- data %>% summarise(mean = mean(val_adj),
                                  median = median(val_adj),
                                  min = min(val_adj),
                                  max = max(val_adj),
                                  q90 = quantile(val_adj,0.90),
                                  q10 = quantile(val_adj,0.00))
  conv_test #get stats
  
  ggplot(data, aes(x=dates, y=val_adj)) + geom_line() #see plot


#function 2: fill in missing scenarios when using parallel processing script (when the pp doesn't finish successfully) ------ 
  library(data.table)
  library(stringr)
  
  pp_dir <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/ExecutionDIR/"
  pp_results <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/"
  ncores <- 11 
  
  breaks <- data.frame(core=1:11, start=c(1,46,91,136,181,226,271,316,361,406,451))
  breaks$end <- c(breaks$start[2:nrow(breaks)], 501)-1
  breaks$length <- breaks$end - breaks$start + 1
  pp_fix <- function(nrun=500, ncores=11, pp_dir){
    length <- floor(nrun/ncores)
    last_length <- nrun - (length*(ncores-1))
    for(x in 1:ncores){
      setwd(paste(pp_dir, "/", x,"/SUFI2.OUT", sep="")) 
      files<-list.files()
      rm_files <- files[str_detect(files, "_filled.txt")]
      unlink(rm_files)
      flow_files <- list.files()
      if(x == ncores){length <- last_length}
      for (f in flow_files){
        #find missing runs
        flow <- fread(f, fill=T) 
        unlink(f, recursive = FALSE, force = FALSE)
        start <- as.numeric(flow[1])
        end <- start + length - 1
        datalength <- which(flow$V1 == (start+1)) -  which(flow$V1 == start)
        suc_runs <- floor(nrow(flow)/ datalength)
        if(nrow(flow) != (suc_runs*datalength)){
          suc_runs <- suc_runs - 1
          flow <- flow[1:(suc_runs*datalength),]
        } 
        
        #fill in missing sims
        runs <- start:end
        suc_runs <- floor(nrow(flow)/ datalength) + start -1
        missing <- runs[!(runs %in% start:suc_runs)]
        fill <- flow[1:datalength,]
        for (m in missing){
          fill$V1[1] <- m
          flow <- rbind(flow, fill)
        }
        write.table(flow, paste(pp_dir, x,"/SUFI2.OUT/",f, "_filled.txt", sep=""), col.names = F, row.names = F, quote = F, append = F)
        print(f)
      }
    }
  }
  pp_fix(pp_dir=pp_dir)
  setwd("~") 
  
#function 3: repaste the results from the PP program (when the program pastes before all the runs are done) -----
  pp_dir <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/ExecutionDIR/"
  pp_results <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/"
  ncores <- 11
  
  paste_pp <- function(pp_dir, pp_results, ncores=11, filled=F){
    for(x in 1:ncores){
      results <- list.files(paste(pp_dir, x,"/SUFI2.OUT", sep=""))
      if(filled==T){
        results <- results[str_detect(results, "_filled")]
      }
      for(y in results){
        data <- read_csv(paste(pp_dir, x,"/SUFI2.OUT/",y, sep=""), show_col_types = F)
        if(filled==T){y <- str_split2(y, "_filled.txt", piece=1)}
        if(x == 1){
          write_csv(data, paste(pp_results,y, sep=""))}else{
            write_csv(data, paste(pp_results,y, sep=""), append=T, col_names = T) 
          }
      }
    }
    
  }  
  paste_pp(pp_dir, pp_results, filled=T) #run filled=T when you ran function 2, otherwise it should be false
  
#function 4: take best parameters and paste twice to test with swat check ------ 
  dir <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/American river simp2.Sufi2.SwatCup/" #swat-cup folder
  int <- "Iterations/1-3-flow-et-cal3-nse/" #interation with parameters you want to copy
  npar <- 39 #number of parameters 
  
  best_par <- read_table(paste(dir,
                               int, "Sufi2.out/best_par.txt", sep=""), 
                         skip=(npar+7), col_names = F, show_col_types = F)
  best_par$X3 <- best_par$X2
  
  line1<- paste(npar,"  : Number of Parameters (the program only reads the first 4 parameters or any number indicated here)")
  line2<- "1  : number of simulations"
  write.table(paste(line1, line2, "", sep="\n"), paste(dir, "Sufi2.in/par_inf.txt", sep=""), quote=F, row.names = F, col.names = F)    
  write.table(best_par, paste(dir, "Sufi2.in/par_inf.txt", sep=""), append=T, quote=F, row.names = F, col.names = F, sep="\t")    

