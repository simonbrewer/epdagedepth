require(ggplot2)
require(maptools)
require(dplyr)
## Entities

oldEnts = read.csv("../../mapping/maps2010/entityDetails_v3.csv")
oldEnts = oldEnts[1:1089,]
oldEnts$E. <- as.numeric(as.character(oldEnts$E.))

dbstring = "~/Dropbox/Documents/epd/epd20171031/epd-postgres-distribution/dumps_epd_all_tables/"

epdents = read.delim(paste0(dbstring,"entity.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdents) <- c("E",	"Site","Sigle","Name","IsCore","IsSect","IsSSamp",
                       "Descriptor","HasAnLam","EntLoc","LocalVeg","Coll", 
                       "SampDate","DepthAtLoc","IceThickCM","SampDevice","CoreDiamCM",
                       "C14DepthAdj","Notes")
epdents = epdents[order(epdents$E),]

epdsites = read.delim(paste0(dbstring,"siteloc.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdsites) <- c("Site","SiteName","SiteCode","SiteExists","PolDiv1","PolDiv2","PolDiv3",
                        "LatDeg","LatMin","LatSec","LatNS","LatDD","LatDMS",
                        "LonDeg","LonMin","LonSec","LonEW","LonDD","LonDMS",
                        "Elevation","AreaOfSite")

epdchrons = read.delim(paste0(dbstring,"chron.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdchrons) <- c("E","Chron","Default","Name","PreparedBy","DatePrepared",
                         "Model","Notes")

epdc14 = read.delim(paste0(dbstring,"c14.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdc14) <- c("E","Sample","AgeBP","AgeSDUp","AgeSDLo","GrThanAge",
                      "Basis","Enriched","LabNumber","DeltaC13","Notes")

epdgeochron = read.delim(paste0(dbstring,"geochron.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdgeochron) <- c("E","Sample","Method","DepthCM","Thickness","Material","Publ")

epdagebasis = read.delim(paste0(dbstring,"agebasis.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdagebasis) <- c("E","Chron","Sample","DepthCM","Thickness",
                           "AgeBP","AgeUp","AgeLo","RCode")

epdsample = read.delim(paste0(dbstring,"p_sample.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdsample) <- c("E","Sample","DepthCM","Thickness","Analyst","AnalyDate","Notes")

epdagedpt = read.delim(paste0(dbstring,"p_agedpt.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdagedpt) <- c("E","Chron","Sample","AgeBP","AgeUp","AgeLo","DepTime")

epdpoldiv1 = read.delim(paste0(dbstring,"poldiv1.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdpoldiv1) <- c("PolDiv","Region")

newentID = which(epdents$E > max(oldEnts$E.))
nbent = length(epdents$E)

newchrons = epdchrons %>% 
  filter(E > 1506) %>% arrange(E)
write.csv(newchrons, "newchrons.csv", row.names=FALSE)
