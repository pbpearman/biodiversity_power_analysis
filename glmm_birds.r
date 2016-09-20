# birds analysis and graphs

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
#setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme/")

load("birds_2")
#load("bird_data1.RData")

#setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/birds/")
x <- richness.data
#x <- data1
x$land.use <- x$land_use_bi
x$land.use <- as.character(x$land.use)
x$land.use[which(x$land.use=="keine_Hauptnutzung")] <- "Keine Hauptnutzung"
#x$land.use[which(x$land.use=="Lw")] <- "Landwirtschaft"
#x$land.use[which(x$land.use=="Si")] <- "Siedlung"
#x$land.use[which(x$land.use=="Wa")] <- "Wald"
x$yearBi <- x$year
x$richness <- as.numeric(x$richness)



#x$land.use <- x$land_use_bi
#x$land.use[which(x$land.use=="keine_Hauptnutzung")] <- "Keine Hauptnutzung"


# SIMPLE GRAPH BIRD RICHNESS DATA
data.1 <- aggregate(richness~yearBi,data=x,FUN=function(x) c(mean=mean(x),stdev=sd(x), count=length(x)))
data.2 <- as.data.frame(data.1$richness)
data.1 <- cbind(data.1$yearBi,data.2)

names(data.1) <- c("year","m.richness","stdev","count")
data.1 <- data.1[order(data.1$year),]


min.y <-  min(data.1$m.richness)-3
max.y <- max(data.1$m.richness)+2
min.year <- min(data.1$year)
max.year <- max(data.1$year)
axes.data <- c(min.year,max.year,min.y,max.y)
dim(axes.data) <- c(2,2)
#setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/fake_data_1")
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/lanag_data/")
png("bird_richness_by_planning_year.png",width=6,height=6,units="in",res=400)
par(mar=c(5.0,4.5,4.0,2.0),font.main=1)
plot(axes.data[,1],axes.data[,2],col=c("red"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl Voegel",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",main="LANAG Voegelarten")
points(data.1$year,data.1$m.richness,col="black",pch=24,bg="black",cex=1.7,type="p")
lines(lowess(data.1$year,data.1$m.richness),col="black",lwd=2.0)

graphics.off()

# GRAPH BIRD RICHNESS DATA BY LANAG LAND USE CLASS
data.3 <- aggregate(richness~yearBi+land.use,data=x,FUN=function(x) c(mean=mean(x),stdev=sd(x), count=length(x)))
data.4 <- as.data.frame(data.3$richness)
data.3 <- cbind(data.3$yearBi,data.3$land.use,data.4)
names(data.3) <- c("year","land.use","m.richness","stdev","count")

data.3$land.use <- factor(data.3$land.use)
data.3$land.use[which(data.3$land.use=="keine_Hauptnutzung")] <- "Keine Hauptnutzung"


png("bird_richness_by_land_use_class.png",width=6,height=6,units="in",res=400)
par(mfrow=c(2,2), mar=c(5.0,4.5,4.0,2.0),font.main=2)
for(i in levels(data.3$land.use)) {
  data.a <- data.3[which(data.3$land.use==i),]
  min.y <- floor(min(data.a$m.richness)-3)
  max.y <- ceiling(max(data.a$m.richness)+4)
  min.year <- min(data.a$year)
  max.year <- max(data.a$year)
  axes.data <- c(min.year,max.year,min.y,max.y)
  dim(axes.data) <- c(2,2)

  plot(axes.data[,1],axes.data[,2],col=c("red"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",cex.main=1.3,main=i)
  points(data.a$year,data.a$m.richness,col="black",pch=24,bg="black",cex=1.7,type="p")
  lines(lowess(data.a$year,data.a$m.richness),col="black",lwd=2.0)
}
graphics.off()



##########Calculate new species richness for Listed Species###################################
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")
rlist <- read.csv("lanag_bird_names_rl.csv")

common.species <- rlist$speciesID[which(rlist$redlist=="LC")]
richness.data.a <- richness.data
richness.data.a$land.use <- richness.data$land_use_bi
richness.data.a$land.use[which(richness.data.a$land.use=="keine_Hauptnutzung")] <- "Keine Hauptnutzung"
richness.data.a <- richness.data.a[!is.element(colnames(richness.data.a),common.species)]
richness.data.a <- richness.data.a[colnames(richness.data.a)!="9999"]



# CHANGE INDICES HERE
rl.richness <- apply(richness.data.a[,c(3:44)],1,sum)
richness.data.a$rl.richness <- rl.richness

data.5 <- aggregate(rl.richness~yearBi,data=x,FUN=function(x) c(mean=mean(x),stdev=sd(x), count=length(x)))
data.6 <- as.data.frame(data.5$rl.richness)
data.5 <- cbind(data.5$yearBi,data.6)

names(data.5) <- c("year","m.richness","stdev","count")
data.5 <- data.5[order(data.5$year),]

# SIMPLE GRAPH BIRD RICHNESS DATA, RED-LISTED SPECIES
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/lanag_data")
min.y <- 0 #min(data.5$m.richness)-3
max.y <- max(data.5$m.richness)+0.5
min.year <- min(data.5$year)
max.year <- max(data.5$year)
axes.data <- c(min.year,max.year,min.y,max.y)
dim(axes.data) <- c(2,2)
png("bird_richness_red_list_species_by_planning_year.png",width=6,height=6,units="in",res=400)
par(mar=c(5.0,4.5,4.0,2.0),font.main=1)
plot(axes.data[,1],axes.data[,2],col=c("red"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl Voegle",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",main="LANAG Rote Liste Voegelarten")
points(data.5$year,data.5$m.richness,col="black",pch=24,bg="black",cex=1.7,type="p")
lines(lowess(data.5$year,data.5$m.richness),col="black",lwd=2.0)

graphics.off()

# GRAPH BIRD RICHNESS DATA BY LANAG LAND USE CLASS, RED-LISTED SPECIES
data.7 <- aggregate(rl.richness~yearBi+land.use,data=richness.data.a,FUN=function(x) c(mean=mean(x),stdev=sd(x), count=length(x)))
data.8 <- as.data.frame(data.7$rl.richness)
data.7 <- cbind(data.7$yearBi,data.7$land.use,data.8)
names(data.7) <- c("year","land.use","m.richness","stdev","count")
png("bird_richness_red_list_species_by_land_use_class.png",width=6,height=6,units="in",res=400)
par(mfrow=c(2,2), mar=c(5.0,4.5,4.0,2.0),font.main=2)
for(i in levels(data.7$land.use)) {
  data.a <- data.7[which(data.7$land.use==i),]
  min.y <- 0  #floor(min(data.a$m.richness)-3)
  max.y <- max(data.a$m.richness)+0.5
  min.year <- min(data.a$year)
  max.year <- max(data.a$year)
  axes.data <- c(min.year,max.year,min.y,max.y)
  dim(axes.data) <- c(2,2)

  plot(axes.data[,1],axes.data[,2],col=c("red"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl Voegel",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",cex.main=1.3,main=i)
  points(data.a$year,data.a$m.richness,col="black",pch=24,bg="black",cex=1.7,type="p")
  lines(lowess(data.a$year,data.a$m.richness),col="black",lwd=2.0)
}
graphics.off()

############################################################
# CORRELATIONS AMONG VARIABLES
#############################################################

x1 <- x[!duplicated(x$coordID),]
x1 <- x1[!is.na(x1$pc.forest),]
cor(x1[,114:124],method="pearson")

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/")
png("percent_forest_by_LANAG_land_use_class.png",width=6,height=6,units="in",res=400)
boxplot(pc.forest ~ land.use,data=x1, main="Anteil Wald per Typ", xlab="LANAG Landnutzungs Typ",ylab="Anteil Wald",
        cex.main=1.5, cex.lab=1.3,cex.axis=0.75)
graphics.off()

png("percent_impervious_by_LANAG_land_use_class.png",width=6,height=6,units="in",res=400)
boxplot(pc.imperv ~ land.use,data=x1,main="Anteil Versiegelte Flaeche per Typ", xlab="LANAG Landnutzungs Typ",ylab="Anteil Versiegelte Flaeche",
        cex.main=1.5, cex.lab=1.3,cex.axis=0.75)
graphics.off()

png("percent_intens_agri_by_LANAG_land_use_class.png",width=6,height=6,units="in",res=400)
boxplot(pc.intagr ~ land.use,data=x1,main="Anteil Landwirtschaft per Typ", xlab="LANAG Landnutzungs Typ",ylab="Anteil Intensive Landwirtschaft",
        cex.main=1.5, cex.lab=1.3,cex.axis=0.75)
graphics.off()

# The result is that miyy_av is correlated with pryy_av and etyy_av, pc.forest is negatively correlated with pc.imperv

####################################################################################
##  ADD BLOCKS
#####################################################################################
block <- rep(NA,dim(x)[1])
for (i in 1:dim(x)[1]) {
  if ((x$pyear[i]=="1995")==TRUE) block[i]="a"
  if ((x$pyear[i]=="1996")==TRUE) block[i]="b"
  if ((x$pyear[i]=="1997")==TRUE) block[i]="c"
  if ((x$pyear[i]=="1998")==TRUE) block[i]="d"
  if ((x$pyear[i]=="1999")==TRUE) block[i]="e"

  if ((x$pyear[i]=="2000")==TRUE) block[i]="a"
  if ((x$pyear[i]=="2001")==TRUE) block[i]="b"
  if ((x$pyear[i]=="2002")==TRUE) block[i]="c"
  if ((x$pyear[i]=="2003")==TRUE) block[i]="d"
  if ((x$pyear[i]=="2004")==TRUE) block[i]="e"

  if ((x$pyear[i]=="2005")==TRUE) block[i]="a"
  if ((x$pyear[i]=="2006")==TRUE) block[i]="b"
  if ((x$pyear[i]=="2007")==TRUE) block[i]="c"
  if ((x$pyear[i]=="2008")==TRUE) block[i]="d"
  if ((x$pyear[i]=="2009")==TRUE) block[i]="e"

  if ((x$pyear[i]=="2010")==TRUE) block[i]="a"
  if ((x$pyear[i]=="2011")==TRUE) block[i]="b"
  if ((x$pyear[i]=="2012")==TRUE) block[i]="c"
}
x$block <- block

x$pyear <- as.numeric(x$pyear)

panel.meanline <- function(x,y){
  panel.xyplot(x,y) # show points
  y.avg <- tapply(y,x,mean)
  xvals <- as.numeric(names(y.avg))
  ord <- order(xvals)
  panel.xyplot(xvals[ord],y.avg[ord],type="l")
}


form <- formula(richness ~ pyear | block*land.use)

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/")
png("Bird richness by block landuse year.png",width=6,height=4,units="in",res=250)
xyplot(x=form,
       data=x,
       panel=panel.meanline,
       main="Artenzahl Voegel per Block und Landnutzungs Typ, 1995-2012",
       xlab="Jahr",
       ylab="Artenzahl Voegel",
       strip=strip.custom(style=1),
       #data=rlbirds,
       #layout=c(4,5),
       par.strip.text=list(cex=0.5),
       aspect=0.5,
       between=list(x=c(0.5,0.5,0.5,0.5),y=c(0.5,0.5,0.5,0.5,0.5))
       )

graphics.off()

####  MIXED MODEL ANALYSIS.#######################
library(lme4)
x$block <- as.factor(x$block)
x$pyear.sq <- x$pyear^2
x$land.use <- as.factor(x$land.use)

biglmm1 <-  lmer(richness ~  land.use + (1 | block), data=x, family=poisson,na.action=na.omit)

library(nlme)
library(MASS)

CompSym1 <- corCompSymm(value=0.1,
                        form= ~ 1 |block)
biglmm1 <-  glmmPQL(richness ~   pyear*land.use,  #+ pyear:land.use,  + land.use  + miyy_av+ pc.othfor + pc.extagr + pc.imperv +pc.others +pc.waters,
                    data=x,
                    correlation=CompSym1,
                    family=poisson,
                    random= list(block= ~ 1))

#######################
biglmm1 <-  lmer(richness ~  pyear + factor(land.use) + miyy_av+ pc.forest + pc.othfor + pc.extagr + pc.imperv +pc.others +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm2 <-  lmer(richness ~  pyear + factor(land.use) + miyy_av+ pc.forest + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm2a <-  lmer(richness ~  pyear + factor(land.use) + pc.forest + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm2b <-  lmer(richness ~  pyear + factor(land.use) + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

# This model fails to converge, so we really don't get a model just without pc.forest

biglmm2aa <-  lmer(richness ~   factor(land.use) + pc.forest + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm2ab <-  lmer(richness ~   factor(land.use)  + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

#  In this model pc.othfor has P=0.8, the ofter terms have P < 0.011 and AIC= 2301

biglmm1ac <-   lmer(richness ~   factor(land.use) + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

# this model has all terms at least p<0.06 and AIC = 2302 

biglmm1ad <-  lmer(richness ~   factor(land.use) + pc.extagr + pc.imperv  + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

# in this model, pc.extagr and pc.imperv are significant at about 0.02;AIC is 2304

biglmm1ae <-  lmer(richness ~   factor(land.use) + pc.imperv  + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

# here p.cimperv has P=0.065 and AIC is 2306

biglmm1af <-  lmer(richness ~   factor(land.use)  + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

# this is the same as biglmm5; althought all terms are significant, the AIC is 2341, making this model substantially worse
# than biglmm1ad

biglmm3 <-  lmer(richness ~  pyear + factor(land.use) + pc.forest + pc.othfor + pc.extagr + pc.imperv +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm4 <-  lmer(richness ~  pyear + factor(land.use)  + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

biglmm5 <-  lmer(richness ~  factor(land.use)  + (1 | block)+ (1 | coordID), family=poisson, data=x,REML=FALSE)

#  IN NO CASE IS PYEAR SIGNIFICANT AND MODELS WITHOUT IT HAVE THE SIMILAR AIC
# NO EVIDENCE THAT PYEAR HAS A SIGNIFICANT EFFECT

RE <- resid(biglmm1af,type="pearson")
boxplot(RE ~ as.factor(x$land.use),pars=list(cex.main=1.5,main="Pearson Residuals by Land Use"))

###################################################################################
# make another dataset that has block and the red-listed species
x2 <- x[!is.element(colnames(x),common.species)]
x2 <- x2[colnames(x2)!="9999"]

rl.richness2 <- apply(x2[,c(3:44)],1,sum)
x2$rl.richness <- rl.richness2

x2$land.use <- x2$land_use_bi
x2$land.use[which(x2$land.use=="keine_Hauptnutzung")] <- "keine Hauptnutzung"
x2 <- x2[order(x2$block,x2$land.use),]
x2$land.use <- factor(x2$land.use)
x2$pyear <- as.numeric(x2$pyear)

####  MIXED MODEL ANALYSIS #######################
###    RED LIST Species only     ##########
hist(richness.data.a$rl.richness)
x2$sample <- factor(x2$block:x2$land.use)


biglmm1 <-  lmer(rl.richness ~  pyear + land.use + miyy_av+ pc.othfor + pc.extagr + pc.imperv +pc.others +pc.waters + (1 | block)+ (1 | coordID), family=poisson, data=x2,REML=FALSE)

biglmm2 <-  lmer(rl.richness ~  land.use  + (1 | block/land.use), family=poisson, data=x2,REML=FALSE)

#   Neither of these models will converge.  Removal of pyear from biglmm2 leads to convergence

#  try glmmPQL

library(MASS)
library(nlme)
library(lattice)

panel.smoother <- function(x, y) {
  panel.xyplot(x, y) # show points
  panel.loess(x, y)  # show smoothed line 
}
panel.meanline <- function(x,y){
  panel.xyplot(x,y) # show points
  y.avg <- tapply(y,x,mean)
  xvals <- as.numeric(names(y.avg))
  ord <- order(xvals)
  panel.xyplot(xvals[ord],y.avg[ord],type="l")
}

rlbirds <- groupedData(rl.richness ~ pyear | block/land.use,
                       data=x2,
                       labels = list(x="Planning Year", y="Red List Richness"))

form <- formula(rl.richness ~ pyear | block*land.use)
xyplot(x=form,
       data=x2,
       panel=panel.meanline,
       xlab="Year",
       ylab="Richness of Red List Species",
       strip=strip.custom(style=1),
       #data=rlbirds,
       #layout=c(4,5),
       par.strip.text=list(cex=0.6),
       aspect=0.5,
       between=list(x=c(0.5,0.5,0.5,0.5),y=c(0.5,0.5,0.5,0.5,0.5))
       )
x2$f.land.use <- as.factor(x2$land.use)


CompSym1 <- corCompSymm(value=0.1,
                        form= ~ 1 |block/land.use)
CompSym2 <- corCompSymm(value=0.1,
                        form= ~ 1|block)
#CompSym1 <- Initialize(CompSym1,data=x2)
biglmm1 <-  glmmPQL(rl.richness ~   pyear + land.use + pyear:land.use,  #+ land.use  + miyy_av+ pc.othfor + pc.extagr + pc.imperv +pc.others +pc.waters,
                    data=x2,
                    correlation=CompSym1,
                    family=poisson,
                    random= list(block= ~ 1, land.use= ~ 1 ))

biglmm2 <- glmmPQL(rl.richness ~   pyear + land.use + pyear:land.use,  #+ land.use  + miyy_av+ pc.othfor + pc.extagr + pc.imperv +pc.others +pc.waters,
                    data=x2,
                    correlation=CompSym2,
                    family=quasipoisson,
                    random=  ~1|block)

anova(biglmm1,biglmm2)
