# further organization for birds


setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/")
load(file="Data/1086_birds_pearman.RData") # Name of data.frame: bi
bi.2 <- bi[!duplicated(bi$speciesID),c(3,4)]
bi.2 <- bi.2[which(bi.2$speciesID!=9999),]
bi.2 <- bi.2[!is.na(bi.2$speciesID),]
bi.2 <- bi.2[order(bi.2$speciesName),]

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/")
data.2 <- read.csv("rote_liste_voegel_2010_2.csv")
data.2$speciesName <- paste(data.2$genus,data.2$species,sep=" ")
data.2 <- data.2[order(data.2$speciesName),]

data.3 <- merge(bi.2,data.2,all.x=TRUE)  #  LOOK AT THE RESULTS OF THIS

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
fname <- "lanag_bird_names_rl.csv"
write.csv(data.3,file=fname)
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
rm(list=ls())


#further organization for molluscs
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/")
load(file="Data/1086_snails_pearman.RData") # Name of data.frame: bi
mo$speciesID[is.na(mo$speciesID)] <- 9999
mo.2 <- mo[!duplicated(mo$speciesID),c(3,4)]
mo.2 <- mo.2[order(mo.2$speciesName),]

eu27.snail.rl <- read.csv("eu27_snail_rlist.csv")
eu27 <- eu27.snail.rl
eu27$speciesName <- paste(eu27$Genus,eu27$Species,sep=" ")
eu27 <- eu27[order(eu27$speciesName),]

mo.3 <- merge(mo.2,eu27,all.x=TRUE)

fname <- "lanag_snails_names_rl.csv"
write.csv(mo.3,file=fname)

#further organization of plants
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/")
load(file="Data/1086_plants_pearman.RData") # Name of data.frame:
pf.2 <- pf[!duplicated(pf$speciesID),c(3,4)]

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/")
data.1 <- read.csv("redlistplants.csv")
names(data.1) <- c("BDMspeciesID","speciesName","redlist")
pf.3 <- merge(pf.2,data.1,all.x=TRUE)

# SET UNLISTED PLANTS TO NA

pf.3$redlist[which(pf.3$redlist=="not listed")] <- NA
pf.3 <- pf.3[which(!is.na(pf.3$speciesID)==TRUE),]

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
fname <- "lanag_plant_names_rl.csv"
write.csv(pf.3,file=fname)


# further organization of butterflies

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/")
rl.flies <- read.csv("CH-RL_Lepidoptera_2013_bafu.csv")

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
load("butterflies_3")
load("butterflies")

load("1086_butterflies_pearman.RData")

rl.flies$speciesName <- paste(rl.flies$GENUS,rl.flies$SPECIES,sep=" ")

fly.names <- bu[which(!duplicated(bu$speciesID)==TRUE),c(3,4)]
fly.names <- fly.names[!is.na(fly.names$speciesID),]
fly.names.a <- merge(fly.names,rl.flies,by="speciesName",all.x=TRUE)
fly.names.a$CAT[which(fly.names.a$speciesName=="Colias hyale-Komplex")] <- "LC"
fly.names.a$CAT[fly.names.a$speciesName=="Euphydryas aurinia"] <- "LC"
fly.names.a$CAT[fly.names.a$speciesName=="Leptidea sinapis-Komplex"] <- "LC"
fly.names.a$CAT[fly.names.a$speciesName=="Pieris sp."] <- "LC"

fname <- "lanag_fly_names_rl.csv"
write.csv(fly.names.a,file=fname)
