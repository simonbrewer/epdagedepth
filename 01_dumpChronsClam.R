require(ggplot2)
require(maptools)
require(dplyr)
require(clam)

## Entities requiring age models

ents = read.csv("allEntities.csv")
nEnts = nrow(ents)

dbstring = "~/Dropbox/Data/epd/postgres/epd20171031/epd-postgres-distribution/dumps_epd_all_tables/"

epdEnts = read.delim(paste0(dbstring,"entity.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdEnts) <- c("E",	"Site","Sigle","Name","IsCore","IsSect","IsSSamp",
                       "Descriptor","HasAnLam","EntLoc","LocalVeg","Coll", 
                       "SampDate","DepthAtLoc","IceThickCM","SampDevice","CoreDiamCM",
                       "C14DepthAdj","Notes")
epdEnts = epdEnts %>% arrange(E)

epdSites = read.delim(paste0(dbstring,"siteloc.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdSites) <- c("Site","SiteName","SiteCode","SiteExists","PolDiv1","PolDiv2","PolDiv3",
                        "LatDeg","LatMin","LatSec","LatNS","LatDD","LatDMS",
                        "LonDeg","LonMin","LonSec","LonEW","LonDD","LonDMS",
                        "Elevation","AreaOfSite")

epdChrons = read.delim(paste0(dbstring,"chron.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdChrons) <- c("E","Chron","Default","Name","PreparedBy","DatePrepared",
                         "Model","Notes")

epdC14 = read.delim(paste0(dbstring,"c14.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdC14) <- c("E","Sample","AgeBP","AgeSDUp","AgeSDLo","GrThanAge",
                      "Basis","Enriched","LabNumber","DeltaC13","Notes")

epdGeochron = read.delim(paste0(dbstring,"geochron.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdGeochron) <- c("E","Sample","Method","DepthCM","Thickness","Material","Publ")

epdAgebasis = read.delim(paste0(dbstring,"agebasis.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdAgebasis) <- c("E","Chron","Sample","DepthCM","Thickness",
                           "AgeBP","AgeUp","AgeLo","RCode")

epdSample = read.delim(paste0(dbstring,"p_sample.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdSample) <- c("E","Sample","DepthCM","Thickness","Analyst","AnalyDate","Notes")

epdAgedpt = read.delim(paste0(dbstring,"p_agedpt.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdAgedpt) <- c("E","Chron","Sample","AgeBP","AgeUp","AgeLo","DepTime")

epdPolDiv1 = read.delim(paste0(dbstring,"poldiv1.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdPolDiv1) <- c("PolDiv","Region")

nbent = length(epdEnts$E)
out.df = data.frame(ent = epdEnts$E,
                    sigle = rep(NA, nbent),
                    siteID = rep(NA, nbent),
                    siteName = rep(NA, nbent),
                    lon = rep(NA, nbent),
                    lat = rep(NA, nbent),
                    poldiv1 = rep(NA, nbent),
                    region = rep(NA, nbent),
                    nsample = rep(NA, nbent),
                    ndate = rep(NA, nbent))

## Start at 1507 --- the first new entity
for (i in 1508:nbent) {
  print(paste("Doing",i,"of",nbent,"-",epdEnts$Sigle[i]))
  
  chrID = which(epdChrons$E == epdEnts$E[i] & epdChrons$Default == "Y")
  
  if (length(chrID) > 0) {
    ## Make directories if needed
    coreName = paste0(epdEnts$E[i],"_",epdEnts$Sigle[i])
    subDir <- paste0("./agemodels/clam/Cores/",coreName)
    if (!file.exists(subDir)){
      dir.create(file.path("./", subDir))
    }
    
    ## Control points
    sitebasis = epdAgebasis %>% filter(E == epdEnts$E[i] & Chron == epdChrons$Chron[chrID])
    nages = dim(sitebasis)[1]
    clamFile = data.frame(ID = paste0("E",epdEnts$E[i],"_",1:nages),
                          C14_age = ifelse(sitebasis$RCode!="C14", NA, sitebasis$AgeBP),
                          cal_BP = ifelse(sitebasis$RCode!="C14", sitebasis$AgeBP, NA),
                          error = abs(sitebasis$AgeUp-sitebasis$AgeBP),
                          reservoir = NA,
                          depth=sitebasis$DepthCM)
    clamFile$error = ifelse(clamFile$error<=10, 10, clamFile$error)
    clamFile$error = ifelse(is.na(clamFile$error), 100, clamFile$error)
    write.csv(clamFile, paste0(subDir,"/",coreName,".csv"), row.names = FALSE)
    
    ## Depths
    depths = epdSample %>% filter(E == epdEnts$E[i])
    depFile = data.frame(Sample = depths$Sample, Depth = depths$DepthCM)
    write.csv(depFile, paste0(subDir,"/",coreName,"_depths.csv"), row.names = FALSE)
    write.table(depFile$Depth, paste0(subDir,"/",coreName,"_depths.txt"), 
                row.names = FALSE, col.names = FALSE)
    
    ## Write general info file
    infoFile = paste0(subDir,"/",coreName,"_info.txt")
    siteName = epdSites %>% filter(Site == epdEnts$Site[i]) %>% select(SiteName)
    
    cat(paste(epdEnts$E[i], epdEnts$Sigle[i], siteName, "\nC14\n"), file=infoFile)
    sitec14 = epdC14 %>% filter(E == epdEnts$E[i])
    write.table(sitec14, infoFile, append=TRUE)
    cat(paste("Geochron\n"), file=infoFile, append=TRUE)
    sitegeochron = epdGeochron %>% filter(E == epdEnts$E[i])
    write.table(sitegeochron, infoFile, append=TRUE)
    cat(paste("Chron\n"), file=infoFile, append=TRUE)
    sitechron = epdChrons %>% filter(E == epdEnts$E[i])
    write.table(sitechron, infoFile, append=TRUE)
    cat(paste("AgeBasis\n"), file=infoFile, append=TRUE)
    sitebasis = epdAgebasis %>% filter(E == epdEnts$E[i])
    write.table(sitebasis, infoFile, append=TRUE)
    
    ## Run clam
    if (nages >= 2) {
      clam(coreName, depths.file=TRUE, coredir = "./agemodels/clam/Cores/")
      
      if (nages >= 4 & epdEnts$E[i]!=2145) {
        clam(coreName, depths.file=TRUE, type = 4, coredir = "./agemodels/clam/Cores/")
        
      }
    }
  }
  stop()
  
}

