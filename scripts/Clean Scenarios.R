## this script is used to clean scenario outputs and save data and plots
#written by Katie A. Wampler on 2022-9-17 

#libraries 
  library(ggplot2)
  library(dplyr)
  library(readr) 
  library(ggthemes)
  library(gridExtra)
  library(stringr)
  library(scales)
  
#function to clean data 
  clean_flow <- function(file_name, start="2014-01-01", end="2020-12-30"){
    setwd(paste("D:/Wenas Creek Scenarios/",file_name, sep="")) 
    
    #stream data
    reach <- read_table("output.rch", col_names = FALSE, skip = 9)
    reach <- reach[,c(2,7,11,12,14,16,18,24,34)]
    colnames(reach) <- c("subbasin", "flow", "sed", "sed_con", "org_n", "org_p", 
                         "nitrate","othop","doc")
    
    dates <- seq(from=as.Date(start), to=as.Date(end), by="day")
    dates <- rep(dates, 313)
    reach$dates <- dates[order(dates)]
    
    #get concentrations
    reach$flow_L <- reach$flow * 86400* 1000 #flow in L per day  
    reach$org_n_mgL <- reach$org_n * 1000000 / reach$flow_L 
    reach$org_p_mgL <- reach$org_p * 1000000 / reach$flow_L 
    reach$nitrate_mgL <- reach$nitrate * 1000000 / reach$flow_L 
    reach$othop_mgL <- reach$othop * 1000000 / reach$flow_L
    reach$doc_mgL <- reach$doc * 1000000 / reach$flow_L 
    reach$sed_conc_check <- reach$sed  * 1000000 / reach$flow_L  *1000 
    
    reach$scenario <- file_name
    reach
    
    write.csv(reach, "clean_data.csv", quote=F, row.names = F)
  }
  plot_flow <- function(base_name, file_name){
    #load data 
    base <- read_csv(paste("D:/Wenas Creek Scenarios/", base_name, "/clean_data.csv", sep=""), show_col_types = FALSE)
    fire <- read_csv(paste("D:/Wenas Creek Scenarios/", file_name, "/clean_data.csv", sep=""), show_col_types = FALSE)
    #combine and plot ------- 
    df <- rbind(base, fire)
    outlet <- subset(df, df$subbasin == 313)
    outlet <- as.data.frame(outlet)
    
    #get percent change 
    out_sum <- outlet[,c(18 ,10, 2:4, 7,14, 9, 16)]
    out_sum_burned <- subset(out_sum, out_sum$scenario == file_name)
    out_sum_unburned <- subset(out_sum, out_sum$scenario == base_name)
    
    perc_change <- function(new, old){
      val <- (new-old)/ old
      val
    }
    
    df <- cbind(out_sum_burned, out_sum_unburned) 
    for(x in 3:9){
      change <- perc_change(out_sum_burned[,x], out_sum_unburned[,x])
      df <- cbind(df, change)
    }
    
    groups <- c("flow_m3s", "sed_kgday", "sed_mgL", "nit_kgday", "nit_mgL", 
                "doc_kgday", "doc_mgL")
    
    colnames(df) <- c("scenario", "dates", 
                               paste(groups, "_fire", sep=""),
                               "scenario", "dates",
                               paste(groups, "_nofire", sep=""),
                               paste(groups, "_%chg", sep=""))
    
    df <- df[,c(2,3,12,19,4,13,20,5,14,21,6,15,22,7,16,23,8,17,24, 9,18,25)]
    
    write.csv(df, paste("D:/Wenas Creek Clean Data/",file_name, "_results.csv", sep=""), quote=F, row.names=F)
    
    names <- read.csv("Z:/1_Research/3_Wenas Creek SWAT/Data/Base Testing2/plotting names.csv")
    
    outlet$scenario <- factor(outlet$scenario, ordered=T, levels=c(file_name, base_name))
    setwd(paste("D:/Wenas Creek Scenarios/", file_name, sep=""))
    dir.create("plot", showWarnings = F)
    for(x in 1:nrow(names)){
      col <- which(names$col_name[x]== colnames(outlet))
      plot <- ggplot(outlet, aes(x=dates,y=outlet[,col], color=scenario)) + geom_line(linewidth=0.25) + theme_clean() + 
        theme(plot.background = element_blank()) + labs(x="Date", y=names$nice_name[x], color="Scenario") +
        geom_vline(xintercept = as.Date("2019-08-31"), color="orange", linetype="dashed") + 
        scale_color_manual(values=c("red", "blue"))
      png(filename=paste("plot/", names$col_name[x], ".png", sep=""), 
          width=20, height = 12, units = 'cm', res = 300)
      grid.arrange(plot)
      dev.off()
    } 
    
    #get histogram of nitrate 
    plot <- ggplot(outlet, aes(x=nitrate_mgL, fill=scenario)) + 
      geom_histogram(binwidth = 0.05) + 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                     labels = trans_format("log10", math_format(10^.x)))+ 
      theme_clean() + theme(plot.background = element_blank()) + 
      labs(x="Nitrate (mg/L)", y="Count") + facet_wrap(~scenario) + 
      theme(legend.position = "none") + scale_fill_manual(values=c("red", "blue"))
    png(filename=paste("plot/", "nitrate_histogram.png", sep=""), 
        width=15, height = 12, units = 'cm', res = 300)
    grid.arrange(plot)
    dev.off() 
    
    plot <- ggplot(outlet, aes(x=sed_con, fill=scenario)) + 
      geom_histogram(binwidth = 0.05) + 
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)))+ 
      theme_clean() + theme(plot.background = element_blank()) + 
      labs(x="Sediment (mg/L)", y="Count") + facet_wrap(~scenario) + 
      theme(legend.position = "none") + scale_fill_manual(values=c("red", "blue")) 
    png(filename=paste("plot/", "sediment_histogram.png", sep=""), 
        width=15, height = 12, units = 'cm', res = 300)
    grid.arrange(plot)
    dev.off() 
    
    #get summary stats 
    #get just fire period 
    outlet <- subset(outlet, outlet$dates >="2019-08-31")
    mean <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
    min <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ min(.x, na.rm = TRUE)))
    max <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE)))
    median <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ median(.x, na.rm = TRUE)))
    sd <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))
    qr_95 <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ quantile(.x, probs=0.95, na.rm = TRUE)))
    qr_75 <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ quantile(.x, probs=0.75, na.rm = TRUE)))
    qr_25 <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ quantile(.x, probs=0.25, na.rm = TRUE)))
    qr_05 <- outlet %>% group_by(scenario) %>% summarise(across(where(is.numeric), ~ quantile(.x, probs=0.05, na.rm = TRUE)))
   
    #print results 
    write.table("MEAN", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = F)
    suppressWarnings(write.table(mean, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("MEDIAN", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(median, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("SD", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(sd, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("MAX", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(max, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("MIN", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(min, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("95% Quantile", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(qr_95, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("75% Quantile", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(qr_75, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("25% Quantile", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(qr_25, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    write.table("5% Quantile", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T)
    suppressWarnings(write.table(qr_05, paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = T, append = T, sep=","))
    suppressWarnings(write.table("", paste("D:/Wenas Creek Clean Data/stats_summaries_", file_name, ".csv", sep=""), quote=F, row.names=F, col.names = F, append = T, sep=","))
    
    

    }

#list scenarios 
  setwd("D:/Wenas Creek Scenarios")
  folders <- list.files()
  base_group <- folders[str_detect(folders, "fire|nofire", negate = T)]
  base_group <- base_group[base_group != "base_unburned"]
  fire_group <- folders[str_detect(folders, "fire")]
  nofire_group <- fire_group[str_detect(fire_group, "nofire")]
  fire_group <- fire_group[!(fire_group %in% nofire_group)]
  
#clean data 
  base <- clean_flow("base_unburned")
 # folders <- c("base_burned", "LOW", "MOD", "HIGH")
  for(x in folders){
    df <- clean_flow(x)
    print(x)
  }

#plot data where base group is the same
  #base_group <- base_group[!(base_group %in% c("base_burned", "LOW", "MOD", "HIGH"))]
  #base_group <- c("base_burned", "LOW", "MOD", "HIGH")
  for(s in base_group){
    plot_flow("base_unburned", s)
    print(s)
  }
  
#plot data where base group is different 
  for(s in 1:length(fire_group)){
    plot_flow(nofire_group[s], fire_group[s])
    print(fire_group[s])
  }

#rename stats summaries 
  for(x in folders){
    setwd(paste("D:/Wenas Creek Scenarios/", x, sep=""))
    if(file.exists("plot/stats_summaries.csv")){
      file.copy("plot/stats_summaries.csv", paste("D:/Wenas Creek Clean Data/stats_summaries_", x, ".csv", sep=""), overwrite = T)
      
    }
  }
  