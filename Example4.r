#HPD and ROPE in the Bernoulli case

setwd("/home/taylan/Desktop/rapor/bayesian/fourth/")
library(rjags)
library(coda)
library(TeachingDemos)
library(bayestestR)

#Head = 1, Tail= 0
x= rep(0:1,c(60,40))
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

writeLines( modelString , con="hpd-pois-dunif.model" )
initsInput = c()
params = c( "theta" )

jagsModel = jags.model( file="hpd-pois-dunif.model" , data=dataInput , inits=initsInput , n.chains=1 , n.adapt=500 )
update( jagsModel , n.iter=500 )
samps = coda.samples( jagsModel , params , n.iter=10000 )
save(samps,file = "hpd-pois-dunif.csv",ascii = TRUE)
data = read.csv("hpd-pois-dunif.csv",header = FALSE)
summary(samps)
plot(samps)

emp.hpd(data$V1,conf=0.95)
rope(data$V1,c(0.45,0.55))



#Bayes factor in the Bernoulli case
#H_0 = 0.5, H_1 = 0.4 
h0=0.5^40*(1-0.5)^60
h1=0.4^40*(1-0.4)^60
bf_h01=h0/h1
#H_0 = 0.5 , H_1 =! 0.5
thet=seq(0,1,0.0001)
dthet=thet[2]-thet[1]
bernol=thet^40*(1-thet)^60
intt = sum(bernol)*dthet
bf_h0b= h0/intt 


#Model comparison and Bayes factor
t=c(9.57,0.93,4.04,3.31,1.12,1.71,6.65,4.09,5.83,2.91)
theta=seq(0,10,0.01)
dt=theta[2]-theta[1]
m1=sum(theta*exp(-theta*(t+0.1)))*dt
m2=sum(theta/(((1+theta*t)^2)*(1+(theta/10))^2))*dt
bf=m1/m2




