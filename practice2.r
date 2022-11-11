#
# Bayesian inference using JAGS
#

setwd("/home/taylan/Desktop/rapor/bayesian/Bayesian-meta-analysis-master/")
library(rjags)
library(coda)
x = c(0,1,0,1,1,1,1)
Ntotal = length(x)
dataInput = list(x = x,Ntotal = Ntotal)

#For uniform prior
modelString = "
model {
  for ( i in 1:Ntotal ) {
    x[i] ~ dbern( theta )
  }
  theta ~ dunif(0,1)
}
"

#For beta dist.
modelString = "
model {
  for ( i in 1:Ntotal ) {
    x[i] ~ dbern( theta )
  }
  theta ~ dbeta(5.055,5.055)
}
"
writeLines( modelString , con="beta1.model" )
initsInput = c()
params = c( "theta" )

#I have change the .model and .csv file names according to prior distribution I have used.
jagsModel = jags.model( file="beta1.model" , data=dataInput , inits=initsInput , n.chains=1 , n.adapt=500 )
update( jagsModel , n.iter=500 )
samps = coda.samples( jagsModel , params , n.iter=10000 )
save(samps,file = "beta1.csv",ascii = TRUE)
data = read.csv("beta1.csv")
summary(samps)

hist(data[1:10000,1],100, xlab = 'Theta')

#
# Bayesian inference for the Poisson distribution using JAGS
#


setwd("/home/taylan/Desktop/rapor/bayesian/Bayesian-meta-analysis-master/")
library(rjags)
library(coda)

x = c(41,33,30,33,32)
Ntotal = length(x)
## Set your datainput list as dataInput
dataInput = list(x = x,Ntotal = Ntotal)

#For the uniform dist. approach with a large value
modelString = "
model {
  for ( i in 1:Ntotal ) {
    x[i] ~ dpois( theta )
  }
  theta ~ dunif(0,100000)
}
"

#For the gamma dist approach
modelString = "
model {
  for ( i in 1:Ntotal ) {
    x[i] ~ dpois( theta )
  }
  theta ~ dgamma(0.0001,0.0001)
}
"

writeLines( modelString , con="poisson-unif.model" )
initsInput = c()
params = c( "theta" )

jagsModel = jags.model( file="poisson-unif.model" , data=dataInput , inits=initsInput , n.chains=1 , n.adapt=500 )
update( jagsModel , n.iter=500 )
samps = coda.samples( jagsModel , params , n.iter=10000 )
save(samps,file = "poisson-unif.csv",ascii = TRUE)
data = read.csv("poisson-gamma.csv")
data2=read.csv('poisson-unif.csv')
summary(samps)
plot(samps)

gamma=hist(data[1:10000,1],100)
unif=hist(data2[1:10000,1],100)

plot(gamma,col='black',xlim = c(25,45),xlab='Theta')
plot(unif,col='red',xlim = c(25,45),add=T)
legend("topright", 
       legend = c("Gamma", "Unif"),
       col=c('red','black'),
       pch=c('-','-'))





