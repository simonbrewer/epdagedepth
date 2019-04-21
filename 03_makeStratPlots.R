require(ggplot2)
require(maptools)
require(dplyr)
require(reshape2)
require(rioja)

nTaxaPage = 20

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

epdPCounts = read.delim(paste0(dbstring,"p_counts.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdPCounts) <- c("Ent","Sample","PVar","Count")

epdPVars = read.delim(paste0(dbstring,"p_vars.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdPVars) <- c("Var","AccVar","SynType","VarCode","VarName","HVar",
                        "MHVar","Auth","Notes")

epdPGroup = read.delim(paste0(dbstring,"p_group.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdPGroup) <- c("Set","PVar","Group")

epdSample = read.delim(paste0(dbstring,"p_sample.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdSample) <- c("E","Sample","DepthCM","Thickness","Analyst","AnalyDate","Notes")

epdGeochron = read.delim(paste0(dbstring,"geochron.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdGeochron) <- c("E","Sample","Method","DepthCM","Thickness","Material","Publ")

epdAgebasis = read.delim(paste0(dbstring,"agebasis.dump"), sep="\t", header = FALSE, na.strings = "\\N")
colnames(epdAgebasis) <- c("E","Chron","Sample","DepthCM","Thickness",
                           "AgeBP","AgeUp","AgeLo","RCode")

log_con <- file("stratplot.log", open = "w")
cat("EPD Stratiplots\n", file = log_con)
cat(paste0(date(),"\n"), file = "stratplot.log", append = TRUE)

## Start at 1507 --- the first new entity
for (i in 1508:nEnts) {
  cat(paste("Doing",i,"of",nEnts,"-",epdEnts$Sigle[i],"\n"), 
      file = "stratplot.log", append = TRUE)
  
  coreName = paste0(i,"_",epdEnts$Sigle[i])
  ageFile = paste0("agemodels/clam/Cores/",coreName,"/",coreName,"_interpolated_ages.txt")
  
  if (file.exists(ageFile)) {
    ages = read.table(ageFile,
                      header=TRUE)
    
    ## Get counts
    counts = epdPCounts %>% 
      filter(Ent == ents$ent[i]) 
    
    if (nrow(counts) > 0) {
      
      counts = counts %>% 
        dcast(Sample ~ PVar, value.var = "Count", fill = 0)
      
      ## Get groups for sums
      ppvars = as.numeric(names(counts)[-1])
      sumvar = rep(0, ncol(counts)-1)
      pnames = rep(NA, ncol(counts)-1)
      for (j in 1:length(sumvar)) {
        vargrp = epdPGroup$Group[epdPGroup$PVar == ppvars[j]]
        
        if (vargrp %in% c("TRSH","HERB","DWAR")) {
          sumvar[j] = 1
        }
        pnames[j] = as.character(epdPVars$VarName[epdPVars$Var == ppvars[j]])
      }
      
      counts = counts[,-1]
      sums = apply(t(t(counts) * sumvar),1,sum)
      sums = ifelse(sums == 0, 1, sums) ## In case of zero sums
      percs = counts / sums
      nTaxa = ncol(percs)
      
      if (nrow(counts) == nrow(ages)) {
        
        pdf(paste0("./stratplot/",coreName,".pdf"), width=12, height=7)
        # strat.plot(percs, yvar = ages$best, y.rev = TRUE,
        #            title = paste(ents$ent[i], ents$sigle[i]))
        keepID = which(apply(percs,2,max) > 0.05)
        strat.plot(percs[,keepID], yvar = ages$depth, y.rev = TRUE,
                   x.names = pnames, title = paste(ents$ent[i], ents$sigle[i], "Depth"),
                   ylabel = "Depth")
        strat.plot(percs[,keepID], yvar = ages$best, y.rev = TRUE,
                   x.names = pnames, title = paste(ents$ent[i], ents$sigle[i], "Time"),
                   ylabel = "AgeBP")
        # npages = ceiling(nTaxa/nTaxaPage)
        # for (j in 1:npages) {
        #   tStart = (j-1) * nTaxaPage + 1
        #   tEnd = j * nTaxaPage
        #   if (j == npages & tEnd > nTaxa) {
        #     tEnd = nTaxa
        #   }
        #   if (tStart == tEnd) { tStart = tStart - 1 } ## Avoid having only one taxa one page
        #   strat.plot(percs[,tStart:tEnd], yvar = ages$best, y.rev = TRUE)
        #   
        # }
        dev.off()
        
      } else {
        print(paste("Mismatch of samples:", epdEnts$E[i]))
        cat(paste("Mismatch of samples:", epdEnts$E[i],"\n"), 
            file = "stratplot.log", append = TRUE)
      }
      
      
    } else {
      print(paste("Missing counts:", epdEnts$E[i]))
      cat(paste("Missing counts:", epdEnts$E[i],"\n"), 
          file = "stratplot.log", append = TRUE)
    }
    
  }
  
}
cat(paste("Finished:", date(),"\n"), 
    file = "stratplot.log", append = TRUE)
