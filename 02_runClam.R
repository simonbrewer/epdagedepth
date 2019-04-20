source("./clam.R")

coreDir = "~/Dropbox/Data/epd/mapping/maps2010/clamOut/Cores/"

coreList = list.files(coreDir)

ncore = length(coreList)

for (i in 1:ncore) {
  print(paste("Doing",i,"of",ncore))
  ageFile = paste0(coreDir,coreList[i],"/",coreList[i],".csv")
  ages = read.csv(ageFile)
  
  depFile = paste0(coreDir,coreList[i],"/",coreList[i],"_depth.csv")
  depths = read.csv(depFile)
  
  clamFile = data.frame(ID = ages$ID,
                        C14_age = ages$C14_age,
                        cal_BP = ages$cal_BP,
                        error=ages$error, 
                        resevoir = NA,
                        depth=ages$depth)
  
  subDir <- paste0("./Cores/",coreList[i])
  if (!file.exists(subDir)){
    dir.create(file.path("./", subDir))
  }
  
  write.csv(clamFile, paste0("./Cores/",coreList[i],"/",coreList[i],".csv"),
            row.names=FALSE)
  
  clam(coreList[i], storedat = TRUE)
  stop()
  
}
