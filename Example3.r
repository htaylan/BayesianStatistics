library(rjags)
library(coda)

x = c(4,5,7,3)
Ntotal = length(x)
## Set your datainput list as dataInput
dataInput = list(x = x,Ntotal = Ntotal)

#For the gamma dist approach
modelString = "
  model {
    for ( i in 1:Ntotal ) {
      x[i] ~ dpois( theta )
    }
    theta ~ dgamma(10000,10000)
  }
  "

writeLines( modelString , con="poisson-gamma2.model" )
initsInput = c()
params = c( "theta" )

jagsModel = jags.model( file="poisson-gamma2.model" , data=dataInput , inits=initsInput , n.chains=1 , n.adapt=500 )
update( jagsModel , n.iter=500 )
samps = coda.samples( jagsModel , params , n.iter=10000 )
save(samps,file = "poisson-gamma2.csv",ascii = TRUE)
data = read.csv("poisson-gamma2.csv")
summary(samps)
plot(samps)

plot(data,'l')


### Loss Function ###
alpha=1.5
beta=1
th1 = 1
th5 = 5
theta = rgamma(1000,shape=alpha,scale=beta)
thetahat = seq(0,10,0.1)

risk = function(thetahat,loss){
  mean(loss(thetahat))
}

loss2 = function(thetahat){
  1-exp(-th1*abs(thetahat-theta))
}

risk2 = function(thetahat){
  risk(thetahat,loss2)
}

thetahat <- seq(from=0, to = 10, by=0.01)
risktheta <- sapply(thetahat, risk2)
plot(risktheta)

risktheta = sapply(thetahat,risk)

thetahat[which.min(risktheta)]
print(mean(theta))
