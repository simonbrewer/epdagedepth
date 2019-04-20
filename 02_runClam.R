library(clam)

coreDir = "./agemodels/clam/Cores/"

coreList = list.files(coreDir)

ncore = length(coreList)

for (i in 1:ncore) {
  print(paste("Doing",i,"of",ncore))
  ageFile = paste0(coreDir,coreList[i],"/",coreList[i],".csv")
  ages = read.csv(ageFile)
  nages = nrow(ages)
  
  ## Run clam
  if (nages >= 2) {
    clam(coreList[i], depths.file=TRUE, coredir = "./agemodels/clam/Cores/")
    
    if (nages >= 4) {
      clam(coreList[i], depths.file=TRUE, type = 4, coredir = "./agemodels/clam/Cores/")
      
    }
  }
  stop()
  
}
