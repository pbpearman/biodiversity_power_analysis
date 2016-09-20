library(lattice)

results.directory <- "/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme/birds_3_results/"
setwd(results.directory)

graphics.directory <- "/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/bird_graphs/fake_data_3"


filename <-  "birds_power_results_3"
load(filename)

data1$n.blocks <- as.numeric(data1$n.blocks)
data1$sites <- as.numeric(data1$sites)
data1$years <- as.numeric(data1$years)
data1$run.y <- as.numeric(data1$run.y)

data1 <- data1[which(data1$sites != 30 & data1$sites != 50),]
data1 <- data1[order(data1$n.blocks,data1$sites,data1$years),]
data1$n.blocks <- factor(as.character(data1$n.blocks))
data1$sites <- factor(as.character(data1$sites))
data1$years <- factor(as.character(data1$years))

data2 <- data1[order(data1$n.blocks,data1$sites,data1$years,data1$run.y),]
data2a <- data2[which(data2$n.blocks==1),]
data2b <- data2[which(data2$n.blocks==2),]
data2c <- data2[which(data2$n.blocks==5),]

data2a$power1 <- data2a$power; data2a$power <- NULL
data2a$power2 <- data2b$power
data2a$power5 <- data2c$power


data2a$n.blocks <- factor(as.character(data2a$n.blocks))


data2a$sites <- factor(as.character(data2a$sites))
data2a$years <- factor(as.character(data2a$years))

data2a$sites <- factor(data2a$sites,levels(data2a$sites)[c(4,1:3)])

data2a$years <- factor(data2a$years,levels(data2a$years)[c(3,1,2)])



panel.lines <- function(x,y){
#  for (i in 1:3){
   # x <- x[which(years==levels(years)[1])]
   # y <- y[which(years==levels(years)[1])]
    panel.xyplot(x,y,type="l")        
  
}
                                       
  #panel.abline(a=0.8,b=0)
      #y.avg <- tapply(y,x,mean)
 # yvals <- power
      #xvals <- as.numeric(names(y.avg))
  #xvals <- run.y
  #ord <- order(as.numeric(xvals))
  #panel.xyplot(xvals[ord],yvals[ord],type="l")

german.main <- "Statistische Aussagekraft ueber die Zeit"
german.xlab <- "Aufnahmejahr"
german.ylab <- "Statistische Aussagekraft"


form <- formula(power1 + power2 + power5 ~ run.y  | sites*years)

trellis.par.set(superpose.symbol=list(col=c("red","blue","black")))  # you can find these settings using trellis.par.get()

setwd(graphics.directory)
png("Power_by_blocks_sites_years_birds_3_deutsch.png",width=6,height=4,units="in",res=400)
trellis.par.set(superpose.symbol=list(col=c("red","blue","black")))

bird.graph.2 <- xyplot(x=form,
       data=data2a,
        type="o",
        col= c("red","blue","black"),
       #panel=panel.lines,
       main=german.main,
       xlab=german.xlab,
       ylab=german.ylab,
                       
       #main="Power over time, for numbers of blocks and sites",
       #xlab="Years of sampling",
       #ylab="Power",
       strip=strip.custom(style=1),
       auto.key=list(x=.8,y=.82,cex=0.45,col=c("red","blue","black")),
                              #lines=list(col=c(1,2,5))),
                              #points=list(col=c("red","blue","black"))),
                    #   lines=list(type="o",pch= c(1,1,1),col=c("red", "blue","black"))),                
       #data=rlbirds,
       #layout=c(4,5),
       par.strip.text=list(cex=0.5),
       aspect=0.5,
       between=list(x=c(0.5,0.5,0.5,0.5),y=c(0.5,0.5,0.5,0.5,0.5))
       )

#dimnames(bird.graph.2)$years <- c("10% increase after 5 years","10% increase after 10 years","10% increase after 15 years")
dimnames(bird.graph.2)$years <- c("10% Zunahme in 5 Jahren","10% Zunahme in 10 Jahren","10% Zunahme in 15 Jahren")

#dimnames(bird.graph.2)$sites <- c("5 sites","10 sites", "20 sites", "40 sites")
dimnames(bird.graph.2)$sites <- c("5 Aufnahmeflaeche","10 Aufnahmeflaeche"," 20 Aufnahmeflaeche","40 Aufnahmeflaeche")
#bird.graph.2$legend$inside$args$text <- c("one block","two blocks","five blocks")
bird.graph.2$legend$inside$args$text <- c("ein Block","zwei Bloecke","fuenf Bloecke")
plot(bird.graph.2)

dev.off()
trellis.par.set(old.pars)
