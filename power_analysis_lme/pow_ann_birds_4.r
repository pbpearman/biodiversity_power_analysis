# libraries and functions for doing power analysis

# this version incorporates variation in the slope among sites, at the same order as variation among intercepts.

library(lme4)


#      SOME VARIABLE DESCRIPTIONS

# prop       vector of length 'length(land.use)' of the proportion change that should be detected
#            after 'years'; e.g., 0.1 = 10% increase
# years      scalar of number of years for testing for  change, up to 30 probably. This will determine effect size
# run.y      scalar of number of years to generate data for
# blocks     vector of block names
# sites      base number of sites in each land.use category
# sites.dif  vector factors to modify in number sites for the land.use categories
# land.use   vector of land.use category names

# mu             vector of log intercepts for land.use categories

# sigma.coordID  this is variability of coordID within block
# sigma.block    this is variability among blocks
# sigma.error    this is variability at the observation level, leads to overdispersion
# determine effect sizes as log(1 + x)/years, years are in 5-year increments

#years <- 15
#land.use <- c("Kh","Lw","Si","Wa")
#sites.dif <- c(1,1,1,1)
#blocks <- c("a","b","c","d","e")
#prop <- c(0.1,0.1,0.1,0.1)
#run.y <- 40
#sigmasq.block <- 0.005409
#sigmasq.coordID <- 0.04565
#sigmasq.unit <- 0.0007952
#sites <- 30

#mu <- log(c(30,30,30,30))

fake.data <- function(run.y=NULL,years=NULL,n.blocks=NULL,sites=NULL,
                      sites.dif = c(1,1,0.25,1),
                      land.use = c("Kh","Lw","Si","Wa"),
                      mu = log(c(30,30,10,30)),
                      prop = c(0.1,0.1,0.1,0.1),
                      sigmasq.coordID = 0.04565,
                      sigmasq.block = 0.005409,
                      sigmasq.error = 0.0007952) {
  
  #print("call to fake data")
  data1 <- data.frame()
  n=0
  blocks <- c("1","2","3","4","5")
  #print(paste("the value of n.blocks is ",n.blocks,sep=""))

  n.blocks <- as.numeric(n.blocks)
  years <- as.numeric(years)
  sites <- as.numeric(sites)
  run.y <- as.numeric(run.y)
    
   len <- seq.int(from=1,to=as.numeric(n.blocks),by=1)
  #print("past setting of len")
  blocks <- blocks[len]
  #print("fake.data() made it past call to blocks")
  for(i in blocks){
    #print("in blocks for loop")
    date <- seq(from=n,to=run.y,by=length(blocks))       #  generate the sequence of years on which the block will be sampled
    #print("past call to set date")
    lu <- c(rep(land.use[1],round(sites*sites.dif[1])),    #  get a vector of land.use classifications for the block
           rep(land.use[2],round(sites*sites.dif[2])),     #  the length of lu is length(land.use) * sum(sites)
           rep(land.use[3],round(sites*sites.dif[3])),     #  and is one year's worth of observations
           rep(land.use[4],round(sites*sites.dif[4])))
    
    mu.lu <- c(rep(mu[1],round(sites*sites.dif[1])),    #  get a vector of log(intercepts) classifications for the block
           rep(mu[2],round(sites*sites.dif[2])),        #  the length of this is also length(land.use) * sum(sites)
           rep(mu[3],round(sites*sites.dif[3])),
           rep(mu[4],round(sites*sites.dif[4])))

    coef.block <- rep(rnorm(1,0,sd=sqrt(sigmasq.block)),length(lu))   # this is a draw to get coef intercept for this block
    coef.coordID <- rnorm(length(lu),0,sd=sqrt(sigmasq.coordID))
    coordID <- paste(i,1:length(lu),sep="")
    block <- rep(i,length(lu))
    
    b1 <- rep(log(1+prop[1])/(years-1),round(sites*sites.dif[1]))
    b2 <- rep(log(1+prop[2])/(years-1),round(sites*sites.dif[2]))
    b3 <- rep(log(1+prop[3])/(years-1),round(sites*sites.dif[3]))
    b4 <- rep(log(1+prop[4])/(years-1),round(sites*sites.dif[4]))

    sigma.b1 <- 0.3*b1    # set sigma of effect size to n%, i.e. a coefficient of variation of n
    sigma.b2 <- 0.3*b2
    sigma.b3 <- 0.3*b3
    sigma.b4 <- 0.3*b4

    v.sigma.b1 <- rnorm(round(sites*sites.dif[1]),0,sigma.b1)
    v.sigma.b2 <- rnorm(round(sites*sites.dif[2]),0,sigma.b2)
    v.sigma.b3 <- rnorm(round(sites*sites.dif[3]),0,sigma.b3)
    v.sigma.b4 <- rnorm(round(sites*sites.dif[4]),0,sigma.b4)
    
    beta <- c(b1+v.sigma.b1,  # this is the effect size (slope) that is necessary to get a '1+prop' change after 'years'
              b2+v.sigma.b2,  # here there is variation around the mean of 10%, or however is specified above
              b3+v.sigma.b3,
              b4+v.sigma.b4)
                                        
    date.series <- rep(date, each=length(lu))
    data <- as.data.frame(cbind(coordID=coordID,block=block,land.use=lu,mu.lu=mu.lu,coef.block=coef.block,coef.coordID=coef.coordID,beta=beta,year=date.series))
    data$mu.lu <- as.numeric(as.character(data$mu.lu))
    data$coef.block <- as.numeric(as.character(data$coef.block))
    data$coef.coordID <- as.numeric(as.character(data$coef.coordID))
    data$beta <- as.numeric(as.character(data$beta))
    data$year <- as.numeric(as.character(data$year))
    data$error <- rnorm(dim(data)[1],0,sd=sqrt(sigmasq.error))
    data$u <- exp(data$mu.lu + data$beta*data$year + data$coef.block + data$coef.coordID + data$error)
    data$richness <- apply(as.data.frame(data$u),2,rpois,lambda=t(data$u))
    data1 <- rbind(data1,data)
    n <- n+1
 
  }
  data1$richness <- as.numeric(data1$richness)
  return(data1)
}

#####################################################################################################
#####################################################################################################
# prop       vector of length 'length(land.use)' of the proportion change that should be detected
#            after 'years'; e.g., 0.1 = 10% increase
# years      vector of number of years for testing for  change, up to 30 probably. This will determine effect size
# run.y      scalar of number of years to generate data for
# blocks     vector of block names
# sites      base number of sites in each land.use category
# sites.dif  vector factors to modify in number sites for the land.use categories
# land.use   vector of land.use category names

# mu             vector of log intercepts for land.use categories

# sigma.coordID  this is variability of coordID within block
# sigma.block    this is variability among blocks
# sigma.error    this is variability at the observation level, leads to overdispersion
# determine effect sizes as log(1 + x)/years, years are in 5-year increments

##########################################################################################
#
#   FUNCTIONS TO CALCULATE POWER, RUNNING THROUGH A SPACE OF PARAMETERS THAT MODIFY THE SAMPLING DESIGN
#
###########################################################################################

aargau.pow <- function(n.sims=1000, alpha=0.05, n.blocks=NULL, sites=NULL,years=NULL,run.y=NULL){  # return power for design along with the values of the design
 
  signif <- rep(NA,n.sims)
  for(i in 1:n.sims){
    lmer.power <- fakeModelWithRestarts(n.blocks=n.blocks,sites=sites,years=years,run.y=run.y)
    if(!is.null(lmer.power)==TRUE){
      signif[i] <- summary(lmer.power)@coefs[1,4] < alpha
    }
    power <- mean(signif, na.rm = T)
   
  }
  return(power)
}
###############################################################
#                       TEST FUNCTION

#design1 <- function(){
#  return(data.frame(n.blocks=5,sites=5,years=5,run.y=6))
#       }

################################################################
design <- function(n.blocks = c(5,2,1),          # determine the parameter space and return a data.frame
                   n.years = c(5,10,15),
                   r.years = c(6,10,15,20),
                   n.sites = c(5,10,20,30,40,50)){
  # get lengths
  lb <- length(n.blocks)    
  ly <- length(n.years)
  lry <- length(r.years)
  ls <- length(n.sites)

  # expand vectors
  B <- rep(n.blocks,each = ls*ly*lry)       # 60      3
  Y <- rep(n.years,each = lry*ls,times = lb)   # 20   3
  r.y <- rep(r.years,each = ls, times = lb*ly) # 5    9
  S <- rep(n.sites,times = lb*ly*lry)          # 1    3*3*4

  return(data.frame(n.blocks=B,sites=S,years=Y,run.y=r.y))
}
           
modelFakeData <- function(f,...){      # Define the model here in this function
  f$year.s <- f$year - mean(f$year)
  if ((max(as.numeric(as.character(f$block))) > 1.1)==TRUE) {
    model <- glmer(richness ~ -1 + year.s + land.use + (1 | block/coordID), data=f, family=poisson(link="log"))
  } else {
    model <- glmer(richness ~ -1 + year.s + land.use + (1 | coordID),data=f, family=poisson(link="log"))
  }
  return(model)
}  

fakeModelWithRestarts <- function(n = 100,  run.y=NULL, years=NULL, n.blocks=NULL, sites=NULL){
  ## A Fake Model  Actually this runs a model on a fake dataset and restarts with a new dataset if the model fails
  ## Function by Todd Jobe, modified by pbpearman
  withCallingHandlers({
    i <- 0
    mod <- NULL
    while (i < n & is.null(mod)){
      mod <- withRestarts({
        #print(paste("now calling fake.data, restart = ",i,sep=""))
        f <- fake.data(run.y= run.y, years= years, n.blocks = n.blocks, sites = sites)
        #save(f,file=paste("fake_data_",i,sep=""))
        model <- modelFakeData(f)
        return(model)
      },
      rs = function(){
        i <<- i + 1
        return(NULL)
      })
    }
    if(is.null(model))
      warning("ExceededIterations")
    return(model)
  },
  error = function(e){
    print(paste(e))
    print(paste("calling invokeRestart from withhin error() on i = ",i,sep=""))
    invokeRestart("rs")
  },
  warning = function(w){
    if(w$message == "ExceededIterations")
      cat("\n", w$message, "\n")
    else{
      print(paste("calling invokeRestart at warning with i = ",i,sep=""))
      invokeRestart("rs")
    }
  })
}

  
                       
