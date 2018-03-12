##setwd("C:/Users/eri2005/Desktop")
##setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/d-sims")

####simulations for data####
library(mvtnorm)
library(reshape)
##rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
##        method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)
simset = 10

####things to simulate####
##rotate around N values start at 20 in each level, then add 5 as you go
##keep means steady 4 and 5
##rotate around SDs making eta small medium large
##rotate around correlated error 0, .1, 3., .5, .7, .9

####create blank data from for data####
totalsims = simset*17*6*3
mydata = data.frame(N = 1:totalsims)

####keep track of the simulation rounds####
round = 0

####loop over N values here####
##loop a
Nloops = seq(20, 100, 5)
for (a in 1:length(Nloops)) {
  
simulate = 0
Means = c(4,5)

####loop over SD values here####
##loop c
SDloops = c(11.1, 4, 1.6)

for (c in 1:length(SDloops)) {

####loop over correlations here####
##loop d
corloops = c(0, .1, .3, .5, .7, .9)

for (d in 1:length(corloops)) {

####simulate simset rounds of data####

for (e in 1:simset) {
      ####make the data here####
      ##here we are going to want to make the cor / SD matrix pattern

      sigma = matrix(c(SDloops[c],corloops[d],
                       corloops[d],SDloops[c]), 
                       nrow = 2, ncol = 2)
    
      dataset = as.data.frame(rmvnorm(Nloops[a], Means, sigma))
      
      ##here we are simulating 1-7 Likert type data
      ##rounded versus not
      ##truncated versus not
      noround.notrun = dataset
      noround.trun = dataset
      round.notrun = dataset
      round.trun = dataset
      
      ##rounding
      round.notrun = round(round.notrun, digits = 0)
      round.trun = round(round.trun, digits = 0)
      
      ##truncating
      noround.trun[noround.trun < 1 ] = 1
      noround.trun[noround.trun > 7 ] = 7
      round.trun[round.trun < 1 ] = 1
      round.trun[round.trun > 7 ] = 7
      
      ####simulation information####
      round = round + 1
      simulate = simulate + 1
      mydata$N[round] = Nloops[a]
      mydata$stdev[round] = SDloops[c]
      mydata$correl[round] = corloops[d]
      mydata$simnum[round] = simulate
      
      ####means, sd information####
      #no round no trun
      mydata$mean1NRNT[round] = mean(noround.notrun$V1)
      mydata$mean2NRNT[round] = mean(noround.notrun$V2)
      mydata$sd1NRNT[round] = sd(noround.notrun$V1)
      mydata$sd2NRNT[round] = sd(noround.notrun$V2)
      mydata$sddiffNRNT[round] = sd((noround.notrun$V1 - noround.notrun$V2))
      mydata$corNRNT[round] = cor(noround.notrun$V1, noround.notrun$V2)
      #round no trun
      mydata$mean1RNT[round] = mean(round.notrun$V1)
      mydata$mean2RNT[round] = mean(round.notrun$V2)
      mydata$sd1RNT[round] = sd(round.notrun$V1)
      mydata$sd2RNT[round] = sd(round.notrun$V2)
      mydata$sddiffRNT[round] = sd((round.notrun$V1 - round.notrun$V2))
      mydata$corRNT[round] = cor(round.notrun$V1, round.notrun$V2)
      #no round trun
      mydata$mean1NRT[round] = mean(noround.trun$V1)
      mydata$mean2NRT[round] = mean(noround.trun$V2)
      mydata$sd1NRT[round] = sd(noround.trun$V1)
      mydata$sd2NRT[round] = sd(noround.trun$V2)
      mydata$sddiffNRT[round] = sd((noround.trun$V1 - noround.trun$V2))
      mydata$corNRT[round] = cor(noround.trun$V1, noround.trun$V2)
      #round trunc
      mydata$mean1RT[round] = mean(round.trun$V1)
      mydata$mean2RT[round] = mean(round.trun$V2)
      mydata$sd1RT[round] = sd(round.trun$V1)
      mydata$sd2RT[round] = sd(round.trun$V2)
      mydata$sddiffRT[round] = sd((round.trun$V1 - round.trun$V2))
      mydata$corRT[round] = cor(round.trun$V1, round.trun$V2)
      
} ##end sim loop

  #filename = paste(round, ".csv", sep = "")
  #datalines = (abs(round-999):round)
  #write.csv(mydata[ datalines, ], file = filename)
  
      
} ##end N loop

} ##end levels loop

} ##end SD loop


