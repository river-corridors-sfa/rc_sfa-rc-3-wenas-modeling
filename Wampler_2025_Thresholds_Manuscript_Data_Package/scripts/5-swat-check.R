### SWAT checks 
  #written by Katie A. Wampler on 2024-03-19 

  #used to make sure the outputs from a swat model look okay (a better version of SWAT check)
  #need a daily SWAT model outputs 

#set defaults 
#model runs through 2018 for getting fire simulations, but only calibrates to 2017
  model <- "tule"
  if(model == "american"){
    outlet <- 1 #67 
    outlet_area <- 20563.6296 * 10000   #in m2
    nhru <- 198 #1711 
    nsub <- 21 #67
    start <- as.Date("2005-01-01")
    end <- as.Date("2018-12-31")
    dates <- seq(from=start, to=end, by="day")
    mod_loc <- "C:/SWAT/American River Simp2/American River Simp2/Scenarios/american river simp2.Sufi2.SwatCup"
    doc_dat <- NULL
    DOC_unit <- "conc"
  }else if(model == "tule"){
    outlet <- 1 #67 
    outlet_area <-  24998.07 * 10000   #in m2
    nhru <- 224 #1711 
    nsub <- 21 #67
    start <- as.Date("2005-01-01")
    end <- as.Date("2018-12-31")
    dates <- seq(from=start, to=end, by="day")
    mod_loc <- "C:/SWAT/Tule River Simp2/Tule River Simp2/Scenarios/tule river simp2.Sufi2.SwatCup"
    doc_dat <- NULL
    DOC_unit <- "conc"
  }

  setwd(mod_loc)

#section 0: load libraries and functions ------- 
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(data.table)
  library(lubridate)
  library(tidyr)
  library(stringr)
  library(thorloki)

  #clear existing plots 
  unlink(paste("swat-check/", list.files("swat-check"), sep=""))
  dir.create("swat-check", showWarnings = F) 
  
  print_quant <- function(col, unit="mg/L", round=2){
    quant <- round(quantile(col, c(0.05,0.25,0.5,0.75,0.95), na.rm=T), round)
    cat(paste("5% Quant:", quant[1], unit),
        paste("25% Quant:", quant[2], unit),
        paste("50% Quant:", quant[3], unit),
        paste("75% Quant:", quant[4], unit),
        paste("95% Quant:", quant[5], unit), sep="\n")
  }  
  
  #optional: add doc data to check (mg/L), if non put NULL
    
  #optional: check soil nutrients
    soil_nut <- F

#section 1: get output files and tidy -------
  #section 1.1: output.hru 
    hru <- data.table::fread("output.hru", skip=9, fill=T)
    unlink("outlet.hru", recursive = FALSE, force = FALSE)
    cols <- c("LUCL","HRU","GIS","SUB","MGT","AREAkm2","PRECIPmm","SNOWFALLmm",
              "SNOWMELTmm","IRRmm","PETmm","ETmm","SW_INITmm","SW_ENDmm","PERCmm",
              "GW_RCHGmm","DA_RCHGmm","REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm",
              "DA_STmm","SURQ_GENmm","SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm",
              "WYLDmm","DAILYCN","TMP_AVdgC","TMPMXdgC","TMPMNdgC","SOL_TMPdgC",
              "SOLARMJ_m2","SYLDt_ha","USLEt_ha","N_APPkg_ha","P_APPkg_ha","NAUTOkg_ha",
              "PAUTOkg_ha","NGRZkg_ha","PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha","NRAINkg_ha",
              "NFIXkg_ha","F-MNkg_ha","A-MNkg_ha","A-SNkg_ha","F-MPKG_ha","AO-LPkg_ha",
              "L-APkg_ha","A-SPkg_ha","DNITkg_ha","NUPkg_ha","PUPkg_ha","ORGNkg_ha","ORGpkg_ha",
              "SEDPkg_ha","NSURQkg_ha","NLATQkg_ha","NO3Lkg_ha","NO3GWkg_ha","SOLPkg_ha","P_GWkg_ha",
              "W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt_ha","LAI","YLDt_ha","BACTPct","BACTLPct", 
              "WTAB","WTABELO","SNOmm","CMUPkg_ha","CMTOTkg_ha","QTILEmm","TNO3kg_ha",
              "LNO3kg_ha","GW_Q_Dmm","LATQCNTmm") 
    if(ncol(hru) == 85){cols <- c(cols, "TVAPkg_ha")}
    colnames(hru) <- cols
   
    hru$date <- rep(dates, each=nhru)
    hru$year <- year(hru$date)
    
    hru_yr <- hru %>% group_by(HRU, year) %>% summarise_at(c("PRECIPmm","ETmm",
                                                             "PERCmm","GW_RCHGmm","DA_STmm", "REVAPmm",
                                                             "SURQ_CNTmm","LATQGENmm","GW_Qmm","WYLDmm", 
                                                             "SYLDt_ha","F-MNkg_ha","A-MNkg_ha","A-SNkg_ha", 
                                                             "DNITkg_ha","NUPkg_ha","ORGNkg_ha",
                                                             "NSURQkg_ha","NLATQkg_ha","NO3Lkg_ha",
                                                             "NO3GWkg_ha", "YLDt_ha"), ~sum(.x, na.rm = T))
    
  #section 1.2: output.rch 
    rch <- fread("output.rch", skip = 9)
    unlink("outlet.rch", recursive = FALSE, force = FALSE)
    
    rch <- rch[,c(2,4,7,11,12,14,18,34,32,36)]
    colnames(rch) <- c("SUB", "MONTH","FLOW","SED_TON","SED_CON","ORG_N","NO3", "DOC", "POC", "TOC")
    rch$date <- rep(dates, each=nsub)
    rch$year <- year(rch$date) 
    
    rch_yr <- rch %>% group_by(SUB, year) %>% summarise_at(c("FLOW","SED_TON","ORG_N","NO3", "DOC", "POC", "TOC"), ~sum(.x, na.rm = T))
    
  #section 1.3: output.snu
    if(soil_nut == T){
      sol <- fread("output.snu", skip = 3)
      unlink("outlet.snu", recursive = FALSE, force = FALSE)
      
      colnames(sol) <- c("DUMMY", "DAY","HRU","SURFACE_SOL","SOL_P","NO3","ORG_N","ORG_P","CN")
      sol$date <- rep(dates, each=nhru)
      sol$year <- year(sol$date)
    }
    
  #section 1.4: cswat_daily.txt 
    cswat <- fread("cswat_daily.txt")
    
#section 2: check water balance ------- 
  #section 2.1: get average values 
    wtr_bal <- hru_yr %>% group_by(year) %>% summarise_at(c("PRECIPmm","ETmm",
                                                            "PERCmm","GW_RCHGmm","DA_STmm", "REVAPmm",
                                                            "SURQ_CNTmm","LATQGENmm","GW_Qmm","WYLDmm", 
                                                            "SYLDt_ha","F-MNkg_ha","A-MNkg_ha","A-SNkg_ha", 
                                                            "DNITkg_ha","NUPkg_ha","ORGNkg_ha",
                                                            "NSURQkg_ha","NLATQkg_ha","NO3Lkg_ha",
                                                            "NO3GWkg_ha", "YLDt_ha"), ~mean(.x, na.rm = T))

    #get overall summary
    cat(paste("P (mm):", round(mean(wtr_bal$PRECIPmm),2)),
        paste("ET (mm):", round(mean(wtr_bal$ETmm),2)),
        paste("Q (mm):",round(mean(wtr_bal$WYLDmm),2)), sep="\n")
    
    #get by HRU
    water_hru <- unique(hru$HRU[hru$LUCL == "WATR"])
    balance <- hru_yr %>% dplyr::select(HRU, year, ETmm, WYLDmm) %>% filter(!(HRU %in% water_hru)) %>% group_by(HRU) %>% 
      summarise(ETmm = mean(ETmm),WYLDmm=mean(WYLDmm)) %>% 
      pivot_longer(cols=c("ETmm","WYLDmm"),names_to="measure",values_to = "val_mm")
    
    p1<- ggplot(balance, aes(x=HRU, y=val_mm, fill=measure)) + geom_col() + 
      labs(x="HRU",y="Depth (mm)", fill="")
    png("swat-check/HRU Water Balance.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
  
  #section 2.2: check runoff ratios 
    rr <- hru_yr %>% mutate(RR = WYLDmm / PRECIPmm) %>% group_by(HRU) %>% summarise(avg_RR = mean(RR, na.rm = T))
    
    p1 <- ggplot(rr, aes(x=HRU, y=avg_RR)) + geom_col() + 
      geom_hline(yintercept = mean(rr$avg_RR), color="red",linetype="dashed") + 
      labs(x="HRU", y="Average Annual Runoff Ratio") 
    png("swat-check/HRU Runoff Ratio.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
  #section 2.3: check ratio of surq, latq, gwq 
    water_hru <- unique(hru$HRU[hru$LUCL == "WATR"])
    balance <- hru_yr %>% dplyr::select(HRU, year, SURQ_CNTmm,LATQGENmm,GW_Qmm) %>% filter(!(HRU %in% water_hru)) %>% group_by(HRU) %>% 
      summarise(SURQ_CNTmm=mean(SURQ_CNTmm), LATQGENmm=mean(LATQGENmm), GW_Qmm=mean(GW_Qmm)) %>% 
      pivot_longer(cols=c("SURQ_CNTmm","LATQGENmm","GW_Qmm"),names_to="path",values_to = "depth_mm")
    
    p1<- ggplot(balance, aes(x=HRU, y=depth_mm, fill=path)) + geom_col() + 
      labs(x="HRU",y="Depth (mm)", fill="Path")
    png("swat-check/HRU Flow Pathways.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    #section 2.3.2: get average percents
    balance <- hru_yr %>% dplyr::select(HRU, year, SURQ_CNTmm,LATQGENmm,GW_Qmm) %>% filter(!(HRU %in% water_hru)) %>% group_by(HRU) %>% 
      summarise(SURQ_CNTmm=mean(SURQ_CNTmm), LATQGENmm=mean(LATQGENmm), GW_Qmm=mean(GW_Qmm), tot=(SURQ_CNTmm+LATQGENmm+GW_Qmm)) %>% 
      mutate(SURQ=SURQ_CNTmm/tot * 100,LATQ=LATQGENmm/tot*100, GWQ=GW_Qmm/tot*100)
    
    cat(paste("SUR Q (%):", round(mean(balance$SURQ),5)),
        paste("LAT Q (%):", round(mean(balance$LATQ),5)),
        paste("GW Q (%):",round(mean(balance$GWQ),5)), sep="\n")
    
  #section 2.4: check reservoir storage 
    aquifer <- hru %>% group_by(HRU, year) %>% summarise(SA_ST=mean(SA_STmm), DA_ST=mean(DA_STmm))
    mean_aq <- aquifer %>% group_by(year) %>%summarise(SA_ST=mean(SA_ST), DA_ST=mean(DA_ST))
    
    p1 <- ggplot() + geom_line(aquifer, mapping=aes(x=year, y=SA_ST, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
      geom_line(mean_aq,mapping=aes(x=year, y=SA_ST), color="red") + 
      labs(x="Year", y="Shallow Aquifer Storage (mm)")
    png("swat-check/Shallow Aquifer.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot() + geom_line(aquifer, mapping=aes(x=year, y=DA_ST, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
      geom_line(mean_aq,mapping=aes(x=year, y=DA_ST), color="red") + 
      labs(x="Year", y="Deep Aquifer Storage (mm)")
    png("swat-check/Deep Aquifer.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
  #check soil water 
    soil_water <- hru %>% 
      mutate(month=floor_date(date, "month")) %>% 
      group_by(LUCL, month, HRU) %>% summarise_at(c("SW_ENDmm"), ~mean(.x, na.rm = T))
    
    soil_check <- soil_water %>% group_by(HRU, LUCL) %>% summarise(soil = mean(SW_ENDmm))
    
    ggplot(subset(soil_water, soil_water$LUCL == "RNGB"), 
           aes(x=month, y=SW_ENDmm, color=as.factor(HRU))) + 
      geom_line() +
      labs(x="date", y="soil water (mm)")
    
    p1 <- ggplot(soil_water, aes(x=month, y=SW_ENDmm, color=as.factor(HRU))) + 
      geom_line() + facet_wrap(~LUCL)  + theme(legend.position = "none") +
      labs(x="date", y="soil water (mm)")
    png("swat-check/Soil Water.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
#section 3: check ET and plants ------ 
    veg <- hru %>% mutate(month=floor_date(date, "month")) %>% group_by(month, HRU) %>% 
      summarise(LUCL=last(LUCL),ETmm=sum(ETmm), BIOM=mean(BIOMt_ha),LAI=mean(LAI),YLD=mean(YLDt_ha)) 
      
    veg_sum <- veg %>% group_by(LUCL, month) %>% summarise(ETmm=mean(ETmm),BIOM=mean(BIOM),
                                   LAI=mean(LAI),YLD=mean(YLD)) %>% filter(LUCL != "WATR")

    p1 <- ggplot(veg_sum, aes(x=month, y=ETmm, color=LUCL)) + geom_line() +
      labs(x="Year",y="ET (mm)", color="Land Use")  + facet_wrap(~LUCL)
    png("swat-check/LUCL ET.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(veg_sum, aes(x=month, y=BIOM, color=LUCL)) + geom_line() + 
      labs(x="Year",y="Avg Biomass (Mg/ha)", color="Land Use") 
    png("swat-check/LUCL Biomass.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(veg_sum, aes(x=month, y=LAI, color=LUCL)) + geom_line() +
      labs(x="Year",y="Avg LAI", color="Land Use") 
    png("swat-check/LUCL LAI.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(veg_sum, aes(x=month, y=YLD, color=LUCL)) + geom_line() +
      labs(x="Year",y="Avg Yield (Mg/ha)", color="Land Use") 
    png("swat-check/LUCL Yield.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    rnge <- subset(veg, veg$LUCL == "RNGE" & HRU %in% c(5,9, 10, 62, 167, 152))
    ggplot(rnge, aes(x=month, y=ETmm, color=as.factor(HRU))) + geom_line()
    
    
#section 4: soil nutrients ------ 
    if(soil_nut == T){
      #section 4.1: get yearly summary by HRU 
      sol_yr <- sol %>% group_by(HRU, year) %>% summarise(SOL_P=mean(SOL_P), NO3=mean(NO3),
                                                          ORG_N=mean(ORG_N),ORG_P=mean(ORG_P))
      
      mean_sol <- sol_yr %>% group_by(year) %>%summarise(SOL_P=mean(SOL_P), NO3=mean(NO3),
                                                         ORG_N=mean(ORG_N),ORG_P=mean(ORG_P))
      
      p1 <- ggplot() + geom_line(sol_yr, mapping=aes(x=year, y=SOL_P, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
        geom_line(mean_sol,mapping=aes(x=year, y=SOL_P), color="red") + 
        labs(x="Year", y="Soluble Phosphorus (kg/ha)")
      png("swat-check/Soil Soluble Phosphorus.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off()
      
      p1 <- ggplot() + geom_line(sol_yr, mapping=aes(x=year, y=NO3, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
        geom_line(mean_sol,mapping=aes(x=year, y=NO3), color="red") + 
        labs(x="Year", y="Nitrate (kg/ha)")
      png("swat-check/Soil Nitrate.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off()
      
      p1 <- ggplot() + geom_line(sol_yr, mapping=aes(x=year, y=ORG_N, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
        geom_line(mean_sol,mapping=aes(x=year, y=ORG_N), color="red") + 
        labs(x="Year", y="Organic Nitrogen (kg/ha)")
      png("swat-check/Soil Organic Nitrogen.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off() 
      
      p1 <- ggplot() + geom_line(sol_yr, mapping=aes(x=year, y=ORG_P, group=as.factor(HRU)), alpha=0.5, color="gray60") + 
        geom_line(mean_sol,mapping=aes(x=year, y=ORG_P), color="red") + 
        labs(x="Year", y="Organic Phosphorus (kg/ha)")
      png("swat-check/Soil Organic Phosphorus.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off()
    }
  
#section 5: check stream metrics at outlet ----- 
  out <- filter(rch, SUB == outlet)
    
    #get more metrics by outlet area 
      out$flow_mm <- out$FLOW*60 *60*24 *1000/ outlet_area 
      out$flow_L <- out$FLOW * 86400* 1000 #flow in L per day 
      out$nitrate_mgL <- out$NO3 * 1000000 / out$flow_L
      out$poc_mgL <- out$POC * 1000000 / out$flow_L
      out$toc_mgL <- out$TOC * 1000000 / out$flow_L
      
      if(DOC_unit == "load"){out$doc_mgL <- (out$DOC * 1000000 / out$flow_L)}
      if(DOC_unit == "conc"){out$doc_mgL <- out$DOC}

      wq_metrics <- out %>% dplyr::select(date, doc_mgL, FLOW, nitrate_mgL)  %>% 
        pivot_longer(doc_mgL:nitrate_mgL, names_to = "measurement", values_to = "value")
      wq_metrics$measurement <- factor(wq_metrics$measurement, levels = c("FLOW","doc_mgL","nitrate_mgL"), ordered=T)
      p1 <- ggplot(wq_metrics, aes(x=date, y=value)) + facet_wrap(~measurement, scales="free_y", ncol = 1) +
        geom_line() + labs(x="Date",y="Value (mg/L or m3/s)") 
      png("swat-check/flow_DOC_nitrate.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off()
      
      p1 <- ggplot(wq_metrics, aes(x=date, y=value)) + 
        facet_wrap(~measurement, scales="free_y", ncol = 1) +
        geom_line() + labs(x="Date",y="Value (mg/L or m3/s)")  +
        scale_x_date(limits=as.Date(c("2015-01-01", "2017-12-31")))
      png("swat-check/flow_DOC_nitrate_zoom.png", width=20, height = 12, res=300, units="cm")
      p1
      dev.off()
       
  #section 5.1: plot metrics over time 
    p1 <- ggplot(out, aes(x=date, y=FLOW)) + geom_line(color="darkblue") +
      labs(x="Date",y="Flow at Outlet (m3/s)") 
    png("swat-check/Outlet Streamflow.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(out, aes(x=date, y=nitrate_mgL)) + geom_line(color="darkblue") +
      labs(x="Date",y="Nitrate at Outlet (mg/L)") 
    png("swat-check/Outlet Nitrate.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(out, aes(x=date, y=doc_mgL)) + geom_line(color="darkblue") +
      labs(x="Date",y="DOC at Outlet (mg/L)") 
    png("swat-check/Outlet DOC.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    p1 <- ggplot(subset(rch, rch$SUB %in% sample(1:nsub, 5)), 
                 aes(x=date, y=DOC, color=as.factor(SUB))) + geom_line() +
      labs(x="Date",y="DOC (mg/L)") + facet_wrap(~SUB, scale="free_y")   
    png("swat-check/Subbasin DOC.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    p1 <- ggplot(out, aes(x=date, y=SED_CON)) + geom_line(color="darkblue") +
      labs(x="Date",y="Sediment at Outlet (mg/L)") 
    png("swat-check/Outlet Sediment.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    flow_doc <- out %>% mutate(month = cut(date, "week")) %>% group_by(month) %>% 
      summarise(doc_mgL = mean(doc_mgL), FLOW= mean(FLOW)) %>% filter(FLOW > 1)
    p1 <- ggplot(flow_doc, aes(x=FLOW, y=doc_mgL)) + geom_point(alpha=0.05) +
      labs(x="Weekly Average Flow (m3/s)",y="Weekly Average DOC at Outlet (mg/L)")  + geom_smooth(method="lm", se=F)
    png("swat-check/Outlet Flow vs DOC.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    flow_nit <- out %>% mutate(month = cut(date, "week")) %>% group_by(month) %>% 
      summarise(nitrate_mgL = mean(nitrate_mgL), FLOW= mean(FLOW)) %>% filter(FLOW > 1)
    p1 <- ggplot(flow_nit, aes(x=FLOW, y=nitrate_mgL)) + geom_point(alpha=0.05) +
      labs(x="Weekly Average Flow (m3/s)",y="Weekly Average Nitrate at Outlet (mg/L)") 
    png("swat-check/Outlet Flow vs Nitrate.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    #section 5.2: make sure there's no trend over time 
    out_yr <- out %>% group_by(year) %>% summarise(DOC=sum(DOC, na.rm=T),
                                                   NO3=sum(NO3, na.rm=T),
                                                   SED_TON=sum(SED_TON, na.rm=T),
                                                   FLOW = sum(flow_mm, na.rm=T))
    mod1 <- lm(DOC ~ FLOW, out_yr)
    mod2 <- lm(NO3 ~ FLOW, out_yr)
    mod3 <- lm(SED_TON ~ FLOW, out_yr)

    out_yr$resid_doc <- residuals(mod1)
    out_yr$resid_no3 <- residuals(mod2)
    out_yr$resid_sed <- residuals(mod3)
    
    p1 <- ggplot(out_yr, aes(x=year, y=resid_doc)) + geom_point() + geom_hline(yintercept=0, color="red", linetype="dashed") + 
      labs(x="Year",y="Residuals")
    png("swat-check/Outlet DOC Residuals.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    p1 <- ggplot(out_yr, aes(x=year, y=resid_no3)) + geom_point() + geom_hline(yintercept=0, color="red", linetype="dashed") +
      labs(x="Year",y="Residuals")
    png("swat-check/Outlet Nitrate Residuals.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    p1 <- ggplot(out_yr, aes(x=year, y=resid_sed)) + geom_point() + geom_hline(yintercept=0, color="red", linetype="dashed") +
      labs(x="Year",y="Residuals")
    png("swat-check/Outlet Sediment Residuals.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
  #section 5.3: check concentrations 
    p1 <- ggplot(out, aes(x=doc_mgL)) + geom_histogram(bins=100) + 
      labs(x="DOC (mg/L)") 
    png("swat-check/Outlet DOC Histogram.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
    
    p1 <- ggplot(out, aes(x=nitrate_mgL)) + geom_histogram(bins=100) + 
      labs(x="Nitrate (mg/L)") 
    png("swat-check/Outlet Nitrate Histogram.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off()  
    
    p1 <- ggplot(out, aes(x=SED_CON)) + geom_histogram(bins=100) + 
      labs(x="Suspended Sediment (mg/L)") 
    png("swat-check/Outlet Sediment Histogram.png", width=20, height = 12, res=300, units="cm")
    p1
    dev.off() 
     
#section 6: check WQ calibration ------ 
  if(is.null(doc_dat) == F){
    wqdat <- read.csv("Z:/1_Research/3_Wenas Creek SWAT/Data/Wenas Creek/YRBT_Water_NPOC_TN.csv", skip=13)
    colnames(wqdat) <- c("Field_Name","Sample_Name","Sample_Type", "NPOC_mgL","TN_mgL","Methods_Dev")
    
    #extract dates and sites 
    wqdat$date <-as.Date(str_sub(wqdat$Sample_Name, 10, 17), format="%Y%m%d")
    wqdat$site <- str_sub(wqdat$Sample_Name, 6, 8)
    
    #fix data 
    wqdat$DL_flag <- str_detect(wqdat$TN_mgL, "LOD")
    wqdat$NPOC_mgL[wqdat$NPOC_mgL == "-9999"] <- NA
    wqdat$TN_mgL[wqdat$TN_mgL == "-9999"] <- NA
    wqdat$TN_mgL[wqdat$DL_flag==T] <- str_split2(wqdat$TN_mgL[wqdat$DL_flag==T], "_", piece=2)
    wqdat$filter <- str_split2(wqdat$Sample_Name, "-", piece=2)
    wqdat <- subset(wqdat, wqdat$filter == "filt0.2")
    
    #remove NAs 
    wqdat <- na.omit(wqdat)
    
    #link with sub names 
    wqdat <- merge(wqdat, doc_sites, by.x="site", by.y="sites") 
    
    #extract data 
    wq_swat <- filter(rch, SUB %in% doc_sites$subbasin)
    
    #get more metrics by outlet area 
    wq_swat$flow_L <- wq_swat$FLOW * 86400* 1000 #flow in L per day 
    wq_swat$nitrate_mgL <- wq_swat$NO3 * 1000000 / wq_swat$flow_L
    wq_swat$doc_mgL <- wq_swat$DOC * 1000000 / wq_swat$flow_L
    
    #combine with observed to plot 
    wq_swat <- wq_swat %>% dplyr::select(SUB, date,nitrate_mgL, doc_mgL) %>% mutate(type ="modeled")
    wqdat <- wqdat %>% dplyr::select(subbasin, date, TN_mgL, NPOC_mgL) %>% mutate(type="observed")
    colnames(wq_swat) <- colnames(wqdat)
    
    wq <- rbind(wq_swat, wqdat)
    
    wq <- subset(wq, wq$date %in% wqdat$date)
    p1 <- ggplot(wq, aes(x=date, y=as.numeric(TN_mgL), color=type)) + geom_point() + geom_line() + 
      facet_wrap(~subbasin, ncol=1) + labs(x="Date", y="Total N (observed)/NO3 (modeled) (mg/L)")
    png("swat-check/Nitrate Observed.png", width=20, height = 25, res=300, units="cm")
    p1
    dev.off()
    
    p1 <- ggplot(wq, aes(x=date, y=as.numeric(NPOC_mgL), color=type)) + geom_point(alpha=0.1) + geom_line() + 
      facet_wrap(~subbasin, ncol=1) + labs(x="Date", y="Dissolved Organic Carbon (mg/L)")
    png("swat-check/DOC Observed.png", width=20, height = 25, res=300, units="cm")
    p1
    dev.off()
  }
    
#section 7: check sediment sources -------
    #section 7.1: get average yield for each hru 
      sed_sum <- hru %>% group_by(HRU, year) %>% summarise(SYLDt_ha=sum(SYLDt_ha),
                                                           USLEt_ha=sum(USLEt_ha),
                                                           LUCL=last(LUCL)) 
     sed_sum2 <- sed_sum %>%
      group_by(HRU)%>% 
      summarise(SYLDt_ha = mean(SYLDt_ha), USLEt_ha = mean(USLEt_ha), LUCL=last(LUCL))
     
     sed_sum3 <- sed_sum %>% group_by(year) %>% summarise(SYLDt_ha = mean(SYLDt_ha), USLEt_ha = mean(USLEt_ha))
     
     hru_lulc <- hru %>% group_by(LUCL, date) %>% summarise(SYLDt_ha=mean(SYLDt_ha),
                                                           USLEt_ha=mean(USLEt_ha),
                                                           LUCL=last(LUCL))
     
     p1<- ggplot(hru_lulc, aes(x=date, y=SYLDt_ha, color=LUCL)) + geom_line() + facet_wrap(~LUCL, scales="free_y") + 
       labs(x="Date",y="Sediment Yield (ton/ha)", color="Land Cover")
     png("swat-check/sediment export LUCL.png", width=30, height = 15, res=300, units="cm")
     p1
     dev.off()
#section 8: check carbon outputs -------
     carbon_out <- out %>% dplyr::select(SUB, date, DOC, POC) %>% 
       pivot_longer(DOC:POC, names_to = "type", values_to = "conc_mgL")
  p1 <- ggplot(carbon_out, aes(x=date, y=conc_mgL, color=type)) + geom_line()
#section 9: print manual readouts ------- 
    #get water balance
    cat("WATER BALANCE",
        paste("P (mm):", round(mean(wtr_bal$PRECIPmm),2)),
        paste("ET (mm):", round(mean(wtr_bal$ETmm),2)),
        paste("Q (mm):",round(mean(wtr_bal$WYLDmm),2)), sep="\n")
    
    #get flow pathways 
    cat("FLOW PATHWAYS",
        paste("SUR Q (%):", round(mean(balance$SURQ),2)),
        paste("LAT Q (%):", round(mean(balance$LATQ),2)),
        paste("GW Q (%):",round(mean(balance$GWQ),2)), sep="\n")
    
    print_quant(out$doc_mgL)
    print_quant(out$nitrate_mgL, round=4)
    print_quant(out$SED_CON, round=3)
    print_quant(out$FLOW, round=3, unit="m3/s")
     
    
