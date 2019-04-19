require(ggplot2)
require(maptools)
## Entities

## NOTE THAT THESE PATHS ARE RELATIVE
oldEnts = read.csv("../../mapping/maps2010/entityDetails_v3.csv")
oldEnts = oldEnts[1:1089,]
oldEnts$E. <- as.numeric(as.character(oldEnts$E.))

dbstring = "~/Dropbox/Data/epd/postgres/epd20171031/epd-postgres-distribution/dumps_epd_all_tables/"

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

out.df = data.frame(ent = epdents$E,
                    sigle = rep(NA, nbent),
                    siteID = rep(NA, nbent),
                    siteName = rep(NA, nbent),
                    lon = rep(NA, nbent),
                    lat = rep(NA, nbent),
                    elev = rep(NA, nbent),
                    poldiv1 = rep(NA, nbent),
                    region = rep(NA, nbent),
                    nsample = rep(NA, nbent),
                    ndate = rep(NA, nbent),
                    clammodel = rep(NA, nbent),
                    use = rep(NA, nbent),
                    upperdepth = rep(NA, nbent),
                    lowerdepth = rep(NA, nbent))

for (i in 1:nbent) {
  siteID = which(epdsites$Site==epdents$Site[i])
  out.df$sigle[i] = as.character(epdents$Sigle[i])
  out.df$siteID[i] = epdsites$Site[siteID]
  out.df$siteName[i] = as.character(epdsites$SiteName[siteID])
  out.df$lon[i] = epdsites$LonDD[siteID]
  out.df$lat[i] = epdsites$LatDD[siteID]
  out.df$elev[i] = epdsites$Elevation[siteID]
  out.df$poldiv1[i] = as.character(epdsites$PolDiv1[siteID])
  out.df$region[i] = as.character(epdpoldiv1$Region[which(epdpoldiv1$PolDiv==out.df$poldiv1[i])])

  out.df$nsample[i] = length(which(epdsample$E==epdents$E[i]))
  out.df$ndate[i] = length(which(epdc14$E==epdents$E[i]))
  oeID = which(oldEnts$E. == epdents$E[i])
  if (length(oeID) > 0) {
    out.df$clammodel[i] = oldEnts$Age.model[oeID]
    out.df$use[i] = oldEnts$Use[oeID]
    out.df$upperdepth[i] = oldEnts$Upper.depth[oeID]
    out.df$lowerdepth[i] = oldEnts$Lower.depth[oeID]
  }

}

write.csv(out.df, "allEntities.csv", row.names=FALSE)

coordinates(out.df) <-  ~lon+lat
kmlPoints(out.df, "epd_20171105.kml", name=out.df$siteName, 
          description=paste("Ent:",
                            out.df$ent,"<br>Sigle:",
                            out.df$sigle,"<br>Name:",
                            out.df$siteName,"<br>NSample:",
                            out.df$nsample,"<br>NDate:",
                            out.df$ndate))

write.csv(subset(out.df, is.na(use)), "needAgeModel.csv", row.names=FALSE)
