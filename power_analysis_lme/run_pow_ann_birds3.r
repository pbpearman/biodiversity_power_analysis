# this script makes a function to run using a bash script
# see another version of the script for a form that will run on a Socket cluster
# on a multi-processor machine using SNOW 

setwd("/home/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme/")
source("pow_ann_birds_4.r")
print("this analysis includes variation in slopes and CV slopes = 30%")
dat <- design()


args <- commandArgs()
n <- args[length(args)]
index <- as.numeric(n)

print(paste("index is ",index,sep=""))

x <- dat[index,]

print(paste("now on line ",index,sep=""))

power <- aargau.pow(n.blocks=x[1],sites=x[2],years=x[3],run.y=x[4])    # here supply the columns of dat (here called 'x') that aargau.pow() needs to run a simulation

eval(parse(text=paste("outdata.",index," <- list(n.blocks=x[1],sites=x[2],years=x[3],run.y=x[4],power=power)",sep="")))


setwd("/home/pearman/lud11_docs/wsl_research/projects/Kanton Aargau/r_code/power_analysis_lme/birds_3_results")

filename <- paste("bird_power_results_3_",index,sep="")

eval(parse(text=paste("save(outdata.",index,",file=filename)",sep="")))



