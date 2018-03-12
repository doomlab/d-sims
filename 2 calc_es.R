##setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/d-sims")
library(MOTE)

#change this later 
mydata = read.csv("testdata.csv")

#independent t
NRNTd = (mydata$mean1NRNT - mydata$mean2NRNT)/sqrt(((mydata$N - 1) * mydata$sd1NRNT^2 + (mydata$N - 1) * mydata$sd2NRNT^2)/(mydata$N + mydata$N - 2))

#dependent t averages
NRNTd = (mydata$mean1NRNT - mydata$mean2NRNT)/((mydata$sd1NRNT + mydata$sd2NRNT)/2)

tapply(NRNTd, list(mydata$N, mydata$stdev), mean)
