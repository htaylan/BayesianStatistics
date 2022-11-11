# 1st question
x=c(1,0,0,1,1,1)
v=seq(0,1,0.01)
dv=v[2]-v[1]
likely=rep(0,length(v))
prior=1
likely=(v*(1-v)*(1-v)*v*v*v)
evidence=sum(likely*prior)*dv
posterior = likely * prior / evidence
plot(v,posterior,'l',col='red',xlab='θ',ylab='P')
abline(prior,0,col='blue')

# 2nd question
t=2.3
vt=seq(0,10,0.01)
dvt=vt[2]-v[1]
likelyt=rep(0,length(vt))
priort=1/sqrt(vt)
priort2=vt^3*exp(-vt)/6
likelyt=vt*exp(-vt*t)
#I have calculated the evidence from gamma function for first case. Numerically evaluate for the second.
evidencet=sqrt(pi)/2
evidencet2=sum(likelyt*priort2)*dvt
posteriort= likelyt*priort/evidencet
posteriort2=likelyt*priort2/evidencet2
plot(vt,posteriort,'l',col='red',xlab='θ',ylab='P')
plot(vt,posteriort2,'l',col='red',xlab='θ',ylab='P')
lines(vt,priort2,'l',col='blue')

# 3rd question
x=c(1,2,2,2,3,3,3,5)
v=seq(0,10,0.01)
dv=v[2]-v[1]
likely=rep(0,length(v))
prior=(2/pi)*(3/(9+v^2))
for (i in 1:length(v)){
  likely[i] = prod(v[i]^x*(exp(-v[i]))/factorial(x))
}
evidence=sum(likely*prior)*dv
posterior = likely * prior / evidence

xp=c(0:10)
xpp=rep(0,length(xp))
for (j in 1:length(xp)){
  xpp[j]=sum(posterior*v^xp[j]*exp(-v)/factorial(xp[j])*dv)
}
plot(v,posterior,'l',col='red',xlab='θ',ylab='P')
lines(v,prior,col='blue')
lines(xp,xpp,'p')
