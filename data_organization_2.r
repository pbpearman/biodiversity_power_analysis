# data organization script

 setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/")
 load(file="Data/1086_surveys_pearman.RData") # Name of data.frame: surv


setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")

#data.name<-"1086_birds_pearman.RData"
#data.name<-"1086_butterflies_pearman.RData"
#data.name<-"1086_plants_pearman.RData"
#data.name<-"1086_snails_pearman.RData"

n.data.name <- "snails"   #  MAKE SURE TO CHANGE THIS NAME APPROPRIATELY
load(n.data.name)

data <- richness.data
data$pyear <- data$yr
data$yr <- NULL

data <- data[order(data$coordID,data$pyear),]

if((n.data.name=="birds")==TRUE) {
  surv.1 <- surv[,c(1,2,5,7,21)]
  } else {
    if((n.data.name=="butterflies")==TRUE) {
      surv.1 <- surv[,c(1,2,5,7)]
    } else {
      if((n.data.name=="plants")==TRUE) {
        surv.1 <- surv[,c(1,2,5,7,23)]
      } else {
        if((n.data.name=="snails")==TRUE) {
          surv.1 <- surv[,c(1,2,5,7,22)]
        }
      }
    }
  }



surv.1 <- surv.1[order(surv.1$coordID,surv.1$pyear),]

data.2 <- merge(data,surv.1,all.x=TRUE)

#names(data.2)

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/")
landcover <- read.csv("landcover_climate.csv")
land <- landcover[,c(13,16,31,46,87:94)]
land <- land[order(land$site.id),]
land$coordID <- land$site.id
land$site.id <- NULL

data.3 <- merge(data.2,land,all.x=TRUE)
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")

if((n.data.name=="plants")==TRUE) {
  print("yup, plants")
  for (i in 1:dim(data.3)[1]) {
    if((data.3$Bearbeiter_Pf[i]=="I. Kump")==TRUE) data.3$Bearbeiter_Pf[i] <- "Kump"
    if((data.3$Bearbeiter_Pf[i]=="U. Kradolfer")==TRUE) data.3$Bearbeiter_Pf[i] <-"Kradolfer"
    if((data.3$Bearbeiter_Pf[i]=="Ursula Kradolfer")==TRUE) data.3$Bearbeiter_Pf[i] <-"Kradolfer"
    if((data.3$Bearbeiter_Pf[i]=="Stalling Thomas")==TRUE) data.3$Bearbeiter_Pf[i] <- "Stalling"
    if((data.3$Bearbeiter_Pf[i]=="T. Stalling")==TRUE) data.3$Bearbeiter_Pf[i] <- "Stalling"
    if((data.3$Bearbeiter_Pf[i]=="Thomas Stalling")==TRUE) data.3$Bearbeiter_Pf[i] <- "Stalling"
  }
}

n.data.name <- paste(n.data.name,"_2",sep="")
richness.data <- data.3
save(richness.data,file=n.data.name)
