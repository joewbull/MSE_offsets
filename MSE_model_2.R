####################################
## Management Strategy Evaluation ##
####################################

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

install.packages("scatterplot3d")
install.packages("plot3D")
install.packages("akima")
install.packages("rgl")

library(scatterplot3d)
library(plot3D)
library(akima)
library(rgl)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#SENSITIVITY ANALYSIS AKA TESTING OF DIFFERENT 'SCENARIOS'
iterations <- 1000
sensitivity.mat <- matrix(data=NA, nrow=iterations, ncol=12) #for however many iterations of the simulation run throughs; and on each row, 5 parameters, 2 continuous outcome metrics, and 2 binomial outcome metrics
colnames(sensitivity.mat) <- c("r","lambda","es.threshold","LC","CE","NNLb","NNLes","NNLbBIN","NNLesBIN","impacts","avoidance","offsets")

for(i in 1:iterations){

r <- runif(1,0.25,0.75) #this time, running with 50% uncertainty around US values. Originally, taking an approximate range from numbers given in the figure in Pianka (1970), but may need changing

log.lambda <- runif(1,0,2) #essentially means development impacts result in loss of all biology over a time period from between 0 and 100 years...but weighted towards 100 year development impacts
log.lambda <- log.lambda * -1
lambda <- 10^log.lambda
#lambda <- runif(1,0.0015,0.0045) #this is the sensitivity range assuming 50% uncertainty in US wetlands value

#es.threshold <- runif(1,2,98) #so the threshold can be anywhere in the range, from B and ES being directly related to ES almost not depending on B at all 
#es.threshold <- round(es.threshold)

CE <- runif(1,5,90) #based on the criteria for Critical Habitat for different components in IFC PS6
#CE <- runif(1,85,95) #arbitrary - avoidance criteria between 5% and 30% of remaining biology
CE <- round(CE)
LC <- runif(1,CE,99) #offsets required for anything from just above avoidance threshold to everything above avoidance threshold
LC <- round(LC)

sensitivity.mat[i,1] <- r
sensitivity.mat[i,2] <- lambda
sensitivity.mat[i,3] <- es.threshold
sensitivity.mat[i,4] <- LC
sensitivity.mat[i,5] <- CE


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#LOOP TO CARRY OUT MONTE CARLO SIMULATION FOR TESTING STRATEGY

monte.carlo <- 50
mc.mat <- matrix(data=NA, nrow=0, ncol=0)

for(k in 1:monte.carlo){


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#EXTRA LOOP TO CREATE 3D PLOTS (BLOCK OUT FOR MONTE CARLO AND SENSITIVITY ANALYSIS)  

#extra.loop <- 40
#extra.results.mat <- matrix(data=NA, nrow=extra.loop, ncol=extra.loop)

#lambda <- 0.0001
#log.10.lambda <- log(lambda, base = 10)

#for(k in 1:extra.loop){

  
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
  
#LOOP TO CAPTURE PARAMETER BEHAVIOUR ACROSS DIFFERENT STRATEGIES

model.runs <- 96 #increase CE from 1 to 97 (keep below LC)
CE <- 1 #parameter being tested

## alternative tests

#model.runs <- 17
#min.com <- 0.0
#max.com <- 0.2

#model.runs <- 40
#r <- 0.0001
#log.10.r <- log(r, base = 10)

#es.threshold <- 1 #parameter being tested
#m <- 100/es.threshold

results.mat <- matrix(data=NA, nrow=model.runs, ncol=5)
colnames(results.mat) <- c("CE","AbsoluteB","NetB","MagDevOff","AbsoluteDev")
#colnames(results.mat) <- c("AvCom","AbsoluteB","NetB","MagDevOff","AbsoluteDev")

for(j in 1:model.runs){

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#SIMULATION MODEL
#set.seed(33) #for the stochastic version of the model, this function can be used to kee the 'random' number sequence constant
time <- 100 #100 for basic version, 50 for US
  
obs.mat <- matrix(data=NA, nrow=time, ncol=7)
colnames(obs.mat) <- c("time","biology","obs.biology","impacts","compliance","offsets","avoidance")


#PARAMETERS
#initial biology levels and parameters for a logistic growth function 
biology <- 47 #47 for the US case study, basic version is 99
r <- 0.5 #block out when running sensitivity analysis, 0.5 for the US case study, basic value is 0.01 (in scenario 2 is 0.00)
K <- 100

#parameters for exponential biological decay function resulting from development impacts
lambda <- 0.003 #block out when running sensitivity analysis, 0.003 for the US case study, basic value is 0.01
delta.impacts <- 0

#parameter for offset store, and rate at which offsets are implemented
offset.store <- 0.0 #the offsets 'owed' to the system as a result of previous development impacts 
offset.impl.rate <- 0.8 #the amount of the remaining offset store implemented every year. Basic value is 0.2, US value is high due to banking (say 0.8)

#parameters for linear relationship between biology and ES, up until a certain threshold value of biology (at which point es = 100)
es.threshold <- 80 #block out when running sensitivity analysis
m <- 100/es.threshold
c <- 0

#sd for the uncertainty distribution, observations made of the state of biology PLUS uncertainty in compliance with the rules
stdev.biology <- 10
min.com <- 0.3 #min degree of compliance with the rules set by the policymaker (0.3 basic case)
max.com <- 0.5 #max degree of compliance with the rules set by the policymaker (0.5 basic case)
av.com <- (min.com + max.com)/2

#rule making parameters
#block out when running sensitivity analysis
LC <- 99  #upper threshold, above which no mitigation is necessary ('Least Concern')   
#CE <- 95  #lower threshold, above which offsets are required, below which avoidance is required ('Critically Endangered')
dummy1 <- 0  #dummy variable - ignore


#model looped for specified number of time steps
for(t in 1:time){ 

#-----------------------------------------------------------------------------#
# Operating model: biodiversity trends

#logistic growth in biodiversity (or subtract delta biology from biology a few lines down for logistic decline)
delta.biology <- r*biology*(1-(biology/K))
delta.biology <- rnorm(1,mean=delta.biology,sd=(0.2*delta.biology)) #stochasticity in biodiversity change 

B1 <- biology #dummy variables to be used in calculation of ecosystem services (see also B2)

biology <- biology + delta.biology

#fix for problems when running logarithmic values of r and l (3D plots)
#if(biology>=100){biology <- 99.99}
#if(biology<=0){biology <- 0.01}
  
B2 <- biology

#-----------------------------------------------------------------------------#
# Observation model: monitoring of biodiversity and developer compliance

#when there is some uncertainty in observation of biology
obs.biology <- rnorm(1,mean=biology,sd=stdev.biology) #uncertainty in observed diversity

#records actual and observed diversity
obs.mat[t,1] <- t
obs.mat[t,2] <- biology
obs.mat[t,3] <- obs.biology


#measure of the degree of uncertainty in compliance with the rules
degree.compliance <- sample(seq(from = min.com, to = max.com, by = 0.05), size = 1, replace = TRUE) #compliance is somewhere between the chosen value of the parameters 'min.com' and 'max.com' (total compliance = 1)

#-----------------------------------------------------------------------------#
# Implementation model: Developer Impacts and Mitigation

#if no mitigation required, exponential decay of biology as a result of development
if(dummy1==0){
  delta.impacts <- -1*lambda*biology
  biology <- biology + delta.impacts + (offset.store*offset.impl.rate*degree.compliance) #biology is impacted by development and any required offsets from the store
  offset.store <- offset.store - (offset.store*offset.impl.rate) #the store is reduced by the offsets specified (ignoring compliance)
  obs.mat[t,6] <- 0
  obs.mat[t,7] <- 0
  }

#if offsets are required, development occurs but also implement any biodiversity offsets required from previous timestep
if(dummy1==1){
  offset.store <- offset.store + (-1 * delta.impacts) #last years impacts are added to the offset store
  delta.impacts <- -1*lambda*biology #this years development
  biology <- biology + (offset.store*offset.impl.rate*degree.compliance) + delta.impacts
  offset.store <- offset.store - (offset.store*offset.impl.rate)
  obs.mat[t,6] <- offset.store*offset.impl.rate*degree.compliance #obs.mat records the offset that actually occurs
  obs.mat[t,7] <- 0 #avoidance = 0
  }

#if avoidance is required, development this year is not allowed to occur
if(dummy1==2){
  delta.impacts <- -1*lambda*biology  #impacts which would have occurred, but are to be avoided
  delta.impacts <- (1-degree.compliance)*delta.impacts #the amount of impacts which do occur because of a lack of compliance 
  biology <- biology + delta.impacts + (offset.store*offset.impl.rate*degree.compliance) #biology is affected by the impacts caused due to a lack of compliance, and offsets from the store
  offset.store <- offset.store - (offset.store*offset.impl.rate)
  obs.mat[t,6] <- offset.store*offset.impl.rate*degree.compliance #offsets from the store
  obs.mat[t,7] <- -1 * delta.impacts  #obs.mat records the impacts that actually occur
  }

obs.mat[t,4] <- delta.impacts
obs.mat[t,5] <- degree.compliance

#-----------------------------------------------------------------------------#
# Ecosystem services   #NOTE - not being studied in this version of the model, but could be incorporated at a later date

#ecosystem services are tied to biology, and fall linearly once biology crosses a specified threshold
#if(biology>=es.threshold){ecosystem.services <- 100}
#if(biology<es.threshold){ecosystem.services <- 100 - (m * (es.threshold-biology))}

#or we can try a different version in which ES are a deriviative of B (in this case, rate of change of biology from t1 to t2)
if(t==1){ecosystem.services <- 100}
if(t>=2){ecosystem.services <- (B2 - B1)/1}

#obs.mat[t,5] <- ecosystem.services

#-----------------------------------------------------------------------------#
# Decision model: policymaker sets rules for next time step

#rules for implementing offsets or avoidance, based on remaining state of OBSERVED biology 
if(obs.biology>LC){dummy1<-0}
if(obs.biology<LC){
  if(obs.biology>CE){dummy1<-1}
  if(obs.biology<CE){dummy1<-2}
}

#rules based on remaining benefits (ES)
#if(biology>es.threshold){dummy1<-0}
#if(biology<=es.threshold){dummy1<-1} #offset if biology ever falls below the ES threshold

#if(ecosystem.services > 0){dummy1<-0}
#if(ecosystem.services < 0){
#  mag <- ecosystem.services*ecosystem.services
#  if(mag<1){dummy1<-1}
#  if(mag>1){dummy1<-2}
#}

#rules based on pressures (impacts)

#rules based on actions (avoidance and offsets)

#-----------------------------------------------------------------------------#


} #end of the time loop, main model


#outcomes at the end of the simulation, for the different types of metric. This enables sensitivity analysis

#state i.e. state of biology (NG = +ve)
biology.dev.cf <- (100*100*exp(r*time))/(K+(100*exp(r*time)-1)) #final value of biology if no development counterfactual had occurred, assuming the logistic growth function
nnl.outcome.biology <- obs.mat[time,2] - biology.dev.cf
#biology.dev.cf <- 100*exp(-1*lambda*time) #final value of biology if development only counterfactual had occurred, assuming the exponential decay function. We do not use this as this is not a sensible counterfactual (see Bull et al., 2014; Maron et al., in prep.) 
#nnl.outcome.biology <- obs.mat[time,2] - biology.dev.cf

#benefit i.e. difference in es provision (NG = +ve)
#if(biology.dev.cf>=es.threshold){es.dev.cf <- 100}else{es.dev.cf <- m*biology.dev.cf}
#nnl.outcome.es <- obs.mat[time,5] - es.dev.cf
#mean.nnl.outcome.es <- mean(obs.mat[,5]) #may need to consider these too, because es provision is as much about provision during each time step as about the final provision
#cumulative.nnl.outcome.es <- sum(obs.mat[,5])

#compliance
average.compliance <- mean(obs.mat[,5])

#pressure i.e. total cumulative impacts on biodiversity
cumulative.impacts <- sum(obs.mat[,4])

#actions
cumulative.avoidance <- sum(obs.mat[,7])
cumulative.offsets <- sum(obs.mat[,6])


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

results.mat[j,1] <- CE
results.mat[j,2] <- obs.mat[time,2]
results.mat[j,3] <- nnl.outcome.biology
results.mat[j,4] <- cumulative.offsets #for US case study only
#results.mat[j,4] <- (-1 * cumulative.impacts) + cumulative.offsets
results.mat[j,5] <- (-1 * cumulative.impacts) #basic case

#extra.results.mat[j,k] <- nnl.outcome.biology

CE <- CE + 1
#min.com <- min.com + 0.05
#max.com <- max.com + 0.05
#log.10.r <- log.10.r + 0.1
#r <- 10^log.10.r
#es.threshold <- es.threshold + 1
#m <- 100/es.threshold

} #end of parameter test loop 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#r <- 0.0001
#log.10.r <- log(r, base = 10)

#log.10.lambda <- log.10.lambda + 0.1
#lambda <- 10^log.10.lambda

#} #end of extra (3D plot) loop (BLOCK OUT IF RUNNING MONTE CARLO OR SENSITIVITY ANALYSIS)

#write.csv(extra.results.mat, file="MSE_output_3Dplot.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


if(k==1){mc.mat <- results.mat}
if(k!=1){mc.mat <- cbind(mc.mat,results.mat)}


} #end of monte carlo loop

#process monte carlo simulation data
#process.mat <- matrix(data=0, nrow = model.runs, ncol = 4)
#colnames(process.mat) <- c("CE","AbsoluteB","NetB","MagDevOff")
#process.mat[,1] <- c(1:model.runs)
#process.mat[1,2] <- mean(mc.mat[1,2],mc.mat[1,6],mc.mat[1,10])
#testing <- as.matrix(subset(mc.mat, "AbsoluteB"))
#testing <- rowMeans(mc.mat, select="AbsoluteB")
#include.list <- "AbsoluteB"
#testing <- subset(mc.mat, select=c(AbsoluteB,AbsoluteB))
#testing <- apply(mc.mat, MARGIN="AbsoluteB", FUN=mean)
#testing <- dimnames(mc.mat)

write.csv(mc.mat, file="MSE_output_USoutcomes.csv")

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#place dependent variable results into matrix for sensitivity analysis
sensitivity.mat[i,6] <- nnl.outcome.biology
sensitivity.mat[i,7] <- nnl.outcome.es

sensitivity.mat[i,8] <- ifelse(nnl.outcome.biology>0, 1, 0)
sensitivity.mat[i,9] <- ifelse(nnl.outcome.es>0, 1, 0)

sensitivity.mat[i,10] <- cumulative.impacts
sensitivity.mat[i,11] <- cumulative.avoidance
sensitivity.mat[i,12] <- cumulative.offsets


} #end of loop, sensitivity analysis


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#PLOTS
#plot out the time series for last run of the model (check)
plot(obs.mat[,1],obs.mat[,2],type="l", xlab="time", ylab="biology", xlim=c(0,100), ylim=c(0,100))
plot(obs.mat[,1],obs.mat[,4],type="l", xlab="time", ylab="impacts")
plot(obs.mat[,1],obs.mat[,5],type="l", xlab="time", ylab="compliance")
plot(obs.mat[,1],obs.mat[,6],type="l", xlab="time", ylab="offsets")
plot(obs.mat[,1],obs.mat[,7],type="l", xlab="time", ylab="avoidance")

#plot out relationships between key parameters and dependent variables, where:
#1=r, 2=lambda, 3=es.threshold, 4=LC, 5=CE
#6=NNLb, 7=NNLes
#10=impacts, 11=avoidance, 12=offsets
plot(sensitivity.mat[,5],sensitivity.mat[,12])
abline(lm(sensitivity.mat[,5]~sensitivity.mat[,12]), col="red")

#plot results of parameter tests
plot(results.mat[,1], results.mat[,2], type="l", xlab="CE", ylab="Absolute Biodiversity")
plot(results.mat[,1], results.mat[,3], type="l", xlab="CE", ylab="Net Biodiversity")
plot(results.mat[,1], results.mat[,4], type="l", xlab="CE", ylab="Magnitude Development and Offsets")

#3 dimensional plots
#biology vs development vs avoidance threshold
scatterplot3d(sensitivity.mat[,1],sensitivity.mat[,2],sensitivity.mat[,5], xlab="logistic growth rate", ylab="exponential decay rate", zlab="avoidance threshold", angle = 45, grid=TRUE, pch=1)
scatterplot3d(sensitivity.mat[,1],sensitivity.mat[,2],sensitivity.mat[,6], xlab="logistic growth rate", ylab="exponential decay rate", zlab="NNLb", angle = 45, grid=TRUE, pch=1)

#surf3d(sensitivity.mat[,1],sensitivity.mat[,2],sensitivity.mat[,5])
#X <- sensitivity.mat[,1]
#Y <- sensitivity.mat[,2]
#Z <- sensitivity.mat[,5]
#surf3D(X,Y,Z)

#-----------------------------------------------------------------------------#
#Write out to CSV file
write.csv(results.mat, "offsets_CE_lpoint7.csv")

#-----------------------------------------------------------------------------#
#SENSITIVITY ANALYSES
sensitivity.DF <- as.data.frame(sensitivity.mat)

#continuous sensitivity analyses
sens.b.results <- glm(NNLb ~ r + lambda + es.threshold + LC + CE, family=poisson(link="log"), data=sensitivity.DF)
summary(sens.b.results)

sens.es.results <- glm(NNLes ~ r + lambda + es.threshold + LC + CE, family=poisson(link="log"), data=sensitivity.DF)
summary(sens.es.results)

#binomial sensitivity analyses
sens.b.results <- glm(NNLbBIN ~ r + lambda + es.threshold + LC + CE, family=binomial(link="logit"), data=sensitivity.DF)
summary(sens.b.results)

sens.es.results <- glm(NNLesBIN ~ r + lambda + es.threshold + LC + CE, family=binomial(link="logit"), data=sensitivity.DF)
summary(sens.es.results)

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#END