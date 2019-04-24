require(Bchron)
require(coda)

coreDir = "~/Dropbox/Data/epd/migration/epdagemodels/agemodels/clam/Cores/"

coreList = list.files(coreDir)

ents = read.csv("allEntities.csv")
ncore = nrow(ents)

for (i in 1:ncore) {
  
  coreName = paste0(ents$ent[i],"_",ents$sigle[i])
  if (ents$useBChron[i] != 0) {
    
    print(paste("Doing",i,"of",ncore,"|",coreName))
    ageFile = paste0(coreDir,coreName,"/",coreName,".csv")
    
    if (file.exists(ageFile)) {
      ages = read.csv(ageFile)
      ## Remove all 14C ages outside of calibration
      ages = subset(ages, (C14_age < 50000 & C14_age > 71)|is.na(C14_age))
      
      colnames(ages) <- c("ID", "C14_age", "cal_BP", "error", "reservoir", "depth", "thickness", "RCode")
      
      if (any(duplicated(ages$depth))) {
        print("Warning: deleting duplicates")
        dupID = which(duplicated(ages$depth))
        dupID = ages$depth %in% unique(ages$depth[ duplicated(ages$depth)]) 
        ages = ages[-which(dupID & is.na(ages$cal_BP)),]
      }
      
      depFile = paste0(coreDir,coreName,"/",coreName,"_depth.csv")
      
      if (file.exists(depFile)) {
        depths = read.csv(depFile)
        
        bchronFile = data.frame(ages=ifelse(is.na(ages$C14_age), ages$cal_BP, ages$C14_age),
                                ageSds=ages$error, 
                                calCurves=ifelse(is.na(ages$C14_age), "normal", "intcal13"),
                                position=ages$depth, 
                                thickness=ifelse(is.na(ages$thickness), 1, ages$thickness),
                                ids=seq(1:length(ages$C14_age)))
        
        start.time <- Sys.time()
        bchronOut = Bchronology(ages=bchronFile$ages,
                                ageSds=bchronFile$ageSds, 
                                calCurves=bchronFile$calCurves,
                                positions=bchronFile$position, 
                                positionThicknesses=bchronFile$thickness,
                                ids=bchronFile$ids, 
                                predictPositions=depths$Depth)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        time.taken
        
        pdf(paste0("./agemodels/Bchron/figs/",coreList[i],".pdf"))
        plot(bchronOut)
        lines(apply(bchronOut$thetaPredict,2,median, na.rm=TRUE), 
              depths$Depth, col="red", lwd=3)
        dev.off()
        
        subDir <- paste0("./agemodels/Bchron/cores/",coreList[i])
        if (!file.exists(subDir)){
          dir.create(file.path("./", subDir))
        }
        save(bchronFile, bchronOut, file=paste0(subDir,"/",coreList[i],".RData"))
        rm(bchronOut)
      } else {
        print("No depth information")
      }
    } else {
      print("No age information")
    }
  } else {
    print(paste("Skipping",i,"of",ncore,"|",coreName))
  }
  print("")
  stop()
}
