# This script processes output from power analysis simulations that are produced
# by the scripts and functions in the following files:

#  pow_ann_birds_2.r
#  run_pow_ann_birds.sh
#  run_pow_ann_birds3.r

pow.sim.process <- function(directory=NULL,prefix=NULL) {
  source("pow_ann_birds_3.r")
  home.dir <- getwd()
  dat <- design()
  setwd(directory)
  data <- data.frame(matrix(NA,nrow=dim(dat)[1],ncol=dim(dat)[2] +1))
  names(data) <- c("n.blocks","sites","years","run.y","power")
                            
  for(i in 1:dim(dat)[1]) {
    file <- paste(prefix,i,sep="")
    if(file.exists(file)==TRUE){
      #print(paste("the file ",file," exists",sep=""))
      load(file)
      eval(parse(text=paste("data$n.blocks[",i,"] <- outdata.",i,"$n.blocks",sep="")))
      eval(parse(text=paste("data$sites[i] <- outdata.",i,"$sites",sep="")))
      eval(parse(text=paste("data$years[i] <- outdata.",i,"$years",sep="")))
      eval(parse(text=paste("data$run.y[i] <- outdata.",i,"$run.y",sep="")))
      eval(parse(text=paste("data$power[i] <- outdata.",i,"$power",sep="")))
      rm(file)
    } else {
      print(paste("the file ",file," DOES NOT EXIST",sep=""))
    }
  }
  setwd(home.dir) 
  return(data)
}

directory <- "/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme/birds_3_results/"
prefix <- "bird_power_results_3_"

setwd("/Network/Servers/lsd/lud11/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme")

data1 <- pow.sim.process(directory=directory,prefix=prefix)


setwd(directory)
filename <- "birds_power_results_3"
save(data1,file=filename)
