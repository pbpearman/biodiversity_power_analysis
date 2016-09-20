# data organization script

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")

#data.name<-"1086_birds_pearman.RData"
#data.name<-"1086_butterflies_pearman.RData"
#data.name<-"1086_plants_pearman.RData"
data.name<-"1086_snails_pearman.RData"

n.data.name <- "snails"   #  MAKE SURE TO CHANGE THIS NAME APPROPRIATELY
data <- load(data.name)

data.1 <- eval(parse(text=data))

data.1$speciesID[is.na(data.1$speciesID)] <- 9999


data.2 <- data.1[order(data.1$pyear,data.1$coordID,data.1$speciesID),]
if((n.data.name=="birds")==TRUE) {
  data.2 <- data.2[,c("pyear","coordID","speciesID","Gultig")]
  } else {
  data.2 <- data.2[,c("pyear","coordID","speciesID")]
}


fpyear <- factor(data.2$pyear)
years <- levels(fpyear)

fcoordID <- factor(data.2$coordID)
sites <- levels(fcoordID)
fspeciesID <- factor(data.2$speciesID)
species <- levels(fspeciesID)

df <- as.data.frame(matrix(data=NA,nrow=length(sites),ncol=length(species)))
names(df) <- species
df$coordID <- sites

data.4 <- df[1,]
data.4$richness <- NA
data.4$yr <- NA

for (i in years) eval(parse(text=paste("temp.",i," <- data.frame()",sep="")))

for (i in years) {
  eval(parse(text=paste("test.",i,"<- df",sep="")))
  data.3 <- data.2[which(data.2$pyear==i),]
  eval(parse(text=paste("test.",i," <- df[is.element(df$coordID,data.3$coordID),]",sep="")))
  eval(parse(text=paste("n.sp <- dim(test.",i,")[2] - 1",sep=""))) 
  eval(parse(text=paste("test.",i,"[,c(1:n.sp)] <- 0",sep="")))
  if((n.data.name=="birds") == TRUE) {
    data.3a <- data.3[which(data.3$Gultig==1),]
    } else {
      data.3a <- data.3
    }
  data.3a <- data.3a[!is.na(data.3a$speciesID),]

  for (j in 1:dim(data.3a)[1]) {
    eval(parse(text=paste("test.",i,"[which(test.",i,"$coordID==data.3a$coordID[",j,"]),as.character(data.3a$speciesID[",j,"])] <- 1",sep="")))
  }
  eval(parse(text=paste("test.",i,"$richness <- apply(test.",i,"[,c(1:n.sp)],1,sum)",sep="")))
  eval(parse(text=paste("yr <- rep(i,dim(test.",i,")[1])",sep="")))
  eval(parse(text=paste("test.",i,"$yr <- yr",sep="")))

  eval(parse(text=paste("temp.",i," <- test.",i,sep="")))

  eval(parse(text=paste("data.4 <- rbind(data.4,test.",i,")",sep="")))
  if((i==years[1])==TRUE) data.4 <- data.4[-1,]
}
  
richness.data <- data.4

save(richness.data,file=n.data.name)







  
