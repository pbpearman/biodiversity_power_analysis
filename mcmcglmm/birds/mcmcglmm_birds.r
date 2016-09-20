setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/data/lanag/Pearman/Data/")

load("birds_2")
#setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/birds/")
x <- richness.data
x$land.use <- x$land_use_bi
x$land.use[which(x$land.use=="keine_Hauptnutzung")] <- "Keine Hauptnutzung"


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
 x.1 <- x[,c(1,2,110,125,126)]
#####################################################################################

x.3 <- x.1


#####################################################################################



library(MCMCglmm)
x.3$land.use <- factor(x.3$land.use)
x.3$block <- factor(x.3$block)
x.3$year <- x.3$pyear 

x.3$coordID <- factor(x.3$coordID)

x.3a <- x.3[which(!is.na(x.3$richness)),]
x.3a$year <- x.3a$year - rep(min(x.3a$year),length(x.3a$year))
x.3a$year <- as.character(x.3a$year)
x.3a$year.n <- as.numeric(x.3a$year)
x.3a$year.n.sq <- x.3a$year.n^2
x.3a$obs <- c(1:dim(x.3a)[1])
x.3a$year.s <- scale(x.3a$year.n,center=TRUE,scale=FALSE)
x.3a$year.s.sq <- x.3a$year.s^2





prior.1 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002),G2 = list(V = 1, nu = 0.002)))   #, G2 = list(V = 1, nu = 0.002)))

m1 <- MCMCglmm(richness ~ -1 + year.n*land.use +year.n.sq*land.use, 
                               random = ~ coordID + block,                 #idh(block):coordID,
                               family = "poisson",
                               data = x.3a,
                               prior = prior.1,
                               nitt = 1005000,
                               thin=  500,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

plot(m1$VCV)
autocorr(m1$VCV)

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/mcmcglmm/results/")
sink("bird_results_m1.txt")
dim(autocorr(m1$VCV))  
autocorr(m1$VCV)
summary(m1)
sink()

W.1 <- cBind(m1$X,m1$Z)
p1 <- W.1 %*% posterior.mode(m1$Sol)   # What kind of matrix object is this?????
x.3a$year.n <- as.numeric(x.3a$year)

xyplot(richness +p1@x ~ year.n | land.use, data = x.3a)


coordIDs <- unique(x.3a$coordID)
#xyplot(richness ~ year.n | coordID,data=x.3a,subset    # too many panels
xyplot(richness + p1@x ~ as.numeric(year) | coordID, data=x.3a, subset=is.element(coordID,unique(coordID)[1:10]))

m1$DIC    # The DIC for this model is 9782.2
          # None of the interactions with year.n or year.n.sq were significant
          # The quadratic effect was significant (p=0.001), so it makes since to leave this in
          # The variance for the random effect block was a factor of 10 smaller than for coordID, so block could be dropped


############################################################################################
# Note: a previous model used the interaction of land.use and year.n.  None of the interaction effects
# were significant, meaning that there was no evidence that the slope differed for any of the land.use levels.
# So, I dropped the interaction terms and just used the land.use term to define the intercepts for the groups.
# The DIC of this model is 9778.4 and all terms are highly significant

prior.2 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002)))

date()
m2 <- MCMCglmm(richness ~ -1 + land.use + year.n + year.n.sq,
                               random = ~ coordID,                 
                               family = "poisson",
                               data = x.3a,
                               prior = prior.2,
                               nitt = 2505000,
                               thin=  2500,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)
date()

plot(m2$VCV)
dim(autocorr(m2$VCV))  
diag(autocorr(m2$VCV)[5,,])
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/mcmcglmm/results/")

sink(file="bird_results_m2.txt")
summary(m2)
autocorr(m2$VCV)
sink()

land.use.1 <- c("Other","Land","Sied","Wald")
land.use.2 <- c("Keine Hauptnutzung","Landwirtschaft","Siedlung","Wald")
p9 <- as.data.frame(predict(m2,marginal = ~coordID,type= "response",interval= "confidence"))
x.3b <- cbind(x.3a,p9)
rownames(x.3b) <- seq.int(from=1,to=dim(x.3b)[1],by=1)
x.3c <- x.3b[,c(2,4)]
x.3d <- unique(x.3c)
x.3e <- x.3b[as.numeric(rownames(x.3d)),]
x.3e <- x.3e[order(x.3e$year.n),]

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/")
png("bird_richness_by_land_use_class_w_prediction_m2.png",width=6,height=6,units="in",res=400)
par(mfrow=c(2,2), mar=c(5.0,4.5,4.0,2.0),font.main=2)
for(i in 1:4) {
  data.a <- x.3a[which(x.3a$land.use==land.use.2[i]),]
  data.b <- x.3e[which(x.3e$land.use==land.use.2[i]),]
  min.y <- floor(min(data.a$richness)-3)
  max.y <- ceiling(max(data.a$richness)+4)
  min.year <- min(data.a$pyear)
  max.year <- max(data.a$pyear)
  axes.data <- c(min.year,max.year,min.y,max.y)
  dim(axes.data) <- c(2,2)

  plot(axes.data[,1],axes.data[,2],col=c("black"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl Voegel",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",cex.main=1.3,main=land.use.2[i])
  points(data.a$pyear,data.a$richness,col="black",pch=1,bg="black",cex=1,type="p")
  lines(data.b$pyear,data.b$fit,col="red",lwd=2.0)
   lines(data.b$pyear,data.b$upr,col="red",lwd=2.0,lty=3)
   lines(data.b$pyear,data.b$lwr,col="red",lwd=2.0,lty=3)
}
graphics.off()
setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/mcmcglmm/results/")
save.image(file="bird_mcmc_workspace.RData")

####################################
#
#    summary for model m2
#
#######################################

> summary(m2)

 Iterations = 5001:2502501
 Thinning interval  = 2500
 Sample size  = 1000 

 DIC: 9836.439 

 G-structure:  ~coordID

        post.mean l-95% CI u-95% CI eff.samp
coordID   0.04703  0.03728  0.05666    874.8

 R-structure:  ~units

      post.mean  l-95% CI u-95% CI eff.samp
units 0.0007888 0.0001387 0.001695     1000

 Location effects: richness ~ -1 + land.use + year.n + year.n.sq 

                           post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
land.useKeine Hauptnutzung  2.477193  2.429077  2.528184     1000 <0.001 ***
land.useLandwirtschaft      2.114108  2.060427  2.161713     1000 <0.001 ***
land.useSiedlung            2.409009  2.321636  2.480188     1000 <0.001 ***
land.useWald                2.523811  2.463983  2.580545     1000 <0.001 ***
year.n                      0.053977  0.043320  0.063208     1000 <0.001 ***
year.n.sq                  -0.003059 -0.003613 -0.002510     1000 <0.001 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
######################################################################################





















plot(m2$Sol)

dim(autocorr(m2$Sol))          # How do the dimensions of this matrix arise??
diag(autocorr(m2$Sol)[5,,])

W.1 <- cBind(m2$X,m2$Z)
#p2 <- W.1 %*% posterior.mode(m2$Sol)   # What kind of matrix object is this?????  Why doesn't this give the same result as the following line??
p2 <- as.data.frame(predict(m2,marginal = ~block,type= "response",interval= "confidence"))

x.3a$year.n <- as.numeric(x.3a$year)

xyplot(richness +p2$fit ~ year.n | land.use, data = x.3a)


coordIDs <- unique(x.3a$coordID)
#xyplot(richness ~ year.n | coordID,data=x.3a,subset    # too many panels
xyplot(richness + p2$fit ~ as.numeric(year) | coordID, data=x.3a, subset=is.element(coordID,unique(coordID)[1:10]))

m2$DIC

################################################################################################

prior.3 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

m3 <- MCMCglmm(richness ~ as.numeric(year)*land.use,
                               random = ~ block+coordID,                 #  What does it imply to add coordID as a random effect??
                               family = "poisson",
                               data = x.3a,
                               prior = prior.3,
                               nitt = 105000,
                               thin=  50,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

plot(m3$VCV)
dim(autocorr(m3$VCV))  
diag(autocorr(m3$VCV)[5,,])

plot(m3$Sol)

dim(autocorr(m3$Sol))

W.3 <- cBind(m3$X,m3$Z)
#p2 <- W.3 %*% posterior.mode(m3$Sol)   # What kind of matrix object is this?????  Why doesn't this give the same result as the following line??
p3 <- as.data.frame(predict(m3,marginal = ~block+coordID,type= "response",interval= "confidence"))

x.3a$year.n <- as.numeric(x.3a$year)

xyplot(richness +p3$fit ~ year.n | land.use, data = x.3a)


coordIDs <- unique(x.3a$coordID)
#xyplot(richness ~ year.n | coordID,data=x.3a,subset    # too many panels
xyplot(richness + p3$fit ~ as.numeric(year) | coordID, data=x.3a, subset=is.element(coordID,unique(coordID)[1:10]))

m3$DIC

################################################################################################


prior.4 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = 1, nu = 0.002)))

m4 <- MCMCglmm(richness ~ -1 + poly(as.numeric(year),2,raw=TRUE) + land.use,
                               random = ~ block+coordID,                 #  What does it imply to add coordID as a random effect??
                               family = "poisson",
                               data = x.3a,
                               prior = prior.4,
                               nitt = 5005000,
                               thin=  2500,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               pl = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

plot(m4$VCV)
autocorr(m4$VCV) 
diag(autocorr(m4$VCV)[5,,])

plot(m4$Sol)

dim(autocorr(m4$Sol))
file <- "bird_results_m4.txt"
sink(file=file)
autocorr(m4$VCV) 
summary(m4)
sink()




W.4 <- cBind(m4$X,m4$Z)
#p4 <- W.4 %*% posterior.mode(m2$Sol)   # What kind of matrix object is this?????  Why doesn't this give the same result as the following line??
p4 <- as.data.frame(predict(m4,marginal = ~block+coordID,type= "response",interval= "confidence"))

x.3a$year.n <- as.numeric(x.3a$year)

xyplot(richness +p4$fit ~ year.n | land.use, data = x.3a)


coordIDs <- unique(x.3a$coordID)
#xyplot(richness ~ year.n | coordID,data=x.3a,subset    # too many panels
xyplot(richness + p4$fit ~ as.numeric(year) | coordID, data=x.3a, subset=is.element(coordID,unique(coordID)[1:10]))

m4$DIC

###############################################################################################
# just an example that includes a covar table for coordID which has diagonal form
prior.5 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = diag(3), nu = 2)))

m5 <- MCMCglmm(richness ~ land.use + poly(as.numeric(year),2,raw=TRUE),
                               random = ~us(1+poly(as.numeric(year),2,raw=TRUE)):coordID,                 #  What does it imply to add coordID as a random effect??
                               family = "poisson",
                               data = x.3a,
                               prior = prior.5,
                               nitt = 105000,
                               thin=  50,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

plot(m5$VCV)
dim(autocorr(m5$VCV))  
diag(autocorr(m5$VCV)[5,,])
plot(m5$Sol)
dim(autocorr(m4$Sol))

W.5 <- cBind(m5$X,m5$Z)
#p5 <- W.5 %*% posterior.mode(m2$Sol)   # What kind of matrix object is this?????  Why doesn't this give the same result as the following line??
p5 <- as.data.frame(predict(m5,marginal = ~us(1+as.numeric(year)):coordID,type= "response",interval= "confidence"))

x.3a$year.n <- as.numeric(x.3a$year)

xyplot(richness +p5$fit ~ year.n | land.use, data = x.3a)       # our use p5@x as the slot with the prediction when appropriate


coordIDs <- unique(x.3a$coordID)
#xyplot(richness ~ year.n | coordID,data=x.3a,subset    # too many panels
xyplot(richness + p5$fit ~ as.numeric(year) | coordID, data=x.3a, subset=is.element(coordID,unique(coordID)[1:10]))

m5$DIC

 ############################################################################################

############################################################################################
#  No block effect in this model, differences in DIC compared with similar models are no greater than ~ 2.
#  This is straight from the analysis of the bird data set and is just as a source of code.
prior.9b <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = 1, nu = 2)))

m9b <- MCMCglmm(richness ~ -1 + land.use* year.n,     
                               random = ~ coordID,           
                               family = "poisson",
                               data = x.3a,
                               prior = prior.9b,
                               nitt = 105000,
                               thin=  50,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

plot(m9b$VCV)

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/mcmcglmm/results/")
sink(file="results_m9b.txt")

summary(m9b)
# DIC for this model is 9439.3
#  DICs for these models are not highly variable.  Because of this, it seems ok to choose the simplest model, which here ignores block.

dim(autocorr(m9b$VCV))  
diag(autocorr(m9b$VCV)[5,,])
sink()
#plot(m9$Sol)
#dim(autocorr(m9$Sol))

###########################################################################################################
x.3a$year.f <- as.factor(x.3a$year.n)

prior.6 <- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V=diag(2)*0.02,nu=3),G2=list(V=1,nu=0.002)))

m6 <- MCMCglmm(richness ~ -1 + land.use*year.s + year.s.sq,     
                               random = ~ us(1+year.s):block + coordID,           
                               family = "poisson",
                               data = x.3a,
                               prior = prior.6,
                               nitt = 5005000,
                               thin=  2500,
                               burnin = 5000,
                               verbose = FALSE,
                               pr = TRUE,
                               saveX = TRUE,
                               saveZ = TRUE)

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/mcmcglmm/results/")
file= "m6.bird"
save(m6,file=file)

file <- "m6_summary.txt"
sink(file=file)
summary(m6)
sink()




















vec <- c(rep(1,100),seq(0,16,length.out=100))
dim(vec) <- c(100,2)
year.cont <- vec[,2]

land.use.1 <- c("Other","Land","Sied","Wald")
land.use.2 <- c("Keine Hauptnutzung","Landwirtschaft","Siedlung","Wald")

Beta.Other <- as.matrix(c(exp(posterior.mode(m9b$Sol[,1])) , exp(posterior.mode(m9b$Sol[,5]))),2,1)
Beta.Land <- c(exp(posterior.mode(m9b$Sol[,2])) , exp(posterior.mode(m9b$Sol[,5]) + posterior.mode(m9b$Sol[,6])))
Beta.Sied <- c(exp(posterior.mode(m9b$Sol[,3])), exp(posterior.mode(m9b$Sol[,5]) + posterior.mode(m9b$Sol[,7])))
Beta.Wald <- c(exp(posterior.mode(m9b$Sol[,1])), exp(posterior.mode(m9b$Sol[,5]) + posterior.mode(m9b$Sol[,8]))) 

y.Other <- vec%*%Beta.Other
y.Land <- vec%*%Beta.Land
y.Sied <- vec%*%Beta.Sied
y.Wald <- vec%*%Beta.Wald

p9 <- as.data.frame(predict(m9b,marginal = ~coordID,type= "response",interval= "confidence"))
x.3b <- cbind(x.3a,p9)
rownames(x.3b) <- seq.int(from=1,to=dim(x.3b)[1],by=1)
x.3c <- x.3b[,c(2,7)]
x.3d <- unique(x.3c)
x.3e <- x.3b[as.numeric(rownames(x.3d)),]


png("plant_richness_by_land_use_class_w_prediction.png",width=6,height=6,units="in",res=250)
par(mfrow=c(2,2), mar=c(5.0,4.5,4.0,2.0),font.main=2)
for(i in 1:4) {
  data.a <- x.3a[which(x.3a$land.use==land.use.2[i]),]
  data.b <- x.3e[which(x.3e$land.use==land.use.2[i]),]
  min.y <- floor(min(data.a$richness)-3)
  max.y <- ceiling(max(data.a$richness)+4)
  min.year <- min(data.a$year.n)
  max.year <- max(data.a$year.n)
  axes.data <- c(min.year,max.year,min.y,max.y)
  dim(axes.data) <- c(2,2)

  plot(axes.data[,1],axes.data[,2],col=c("black"),pch=19,cex=0.2,xlab="Jahr",ylab="Artenzahl Pflanzen",
         cex.lab=1.5,cex.axis=1.5,cex.axis=1.5,xaxs="r",yaxs="r",lwd=1.1, type="n",cex.main=1.3,main=land.use.2[i])
  points(data.a$year.n,data.a$richness,col="black",pch=1,bg="black",cex=1,type="p")
  lines(data.b$year.n,data.b$fit,col="red",lwd=2.0)
   lines(data.b$year.n,data.b$upr,col="red",lwd=2.0,lty=3)
   lines(data.b$year.n,data.b$lwr,col="red",lwd=2.0,lty=3)
}
graphics.off()


