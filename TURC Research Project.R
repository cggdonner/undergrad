install.packages("stats4")
library("stats4")

#Catherine Donner
#examples of maximum likelihood of estimation

#minus(-) log likelihood for exponential with rate lambda
nlogL=function(theta=1,x){
  lambda=theta
  -sum(dexp(x, rate=lambda, log=TRUE)) #log=T gives log density
}

#data x_i's n=10
x=c(0.60, 0.18, 0.99, 0.83, 0.63, 2.52, 0.03, 0.70, 0.25, 2.69)

#direct mle for lambda

lambdaHat=1/mean(x)
#lambdaHat
#[1] 1.061571

#numeric solution using optim
#method "Brent" used for one-dimmensional parameter

optim(par=2, fn=nlogL, x=x, method="Brent", lower=0, upper=max(x), hessian=TRUE)
#$par
#[1] 1.061571

#$value
#[1] 9.4025

#$counts
#function gradient 
#NA       NA 

#$convergence
#[1] 0

#$message
#NULL

#$hessian
#[,1]
#[1,] 8.873655

#######################
#gamma mle

#simulate 200 data samples from gamma
n=200
alpha=5 #shape
beta=1/2 #scale
x=rgamma(n, shape=alpha, scale=beta)

hist(x, prob=T, breaks=20)
rug(x)
curve(dgamma(x, shape=alpha, scale=beta), from=0, to=10, add=T, lwd=1, col="blue")

nlogL=function(theta,x){
  alpha=theta[1]
  beta=theta[2]
  -sum(dgamma(x, shape=alpha, scale=beta, log=TRUE))
}

fit=optim(par=c(5,.5), fn=nlogL, x=x, hessian=T)

#warning in dgamma NaNs produced

fit$par
#[1] 4.8092570 0.5324839

hist(x, prob=T, breaks=20)
rug(x)
curve(dgamma(x, shape=fit$par[1], scale=fit$par[2]), from=0, to=10, add=T, col="red")
curve(dgamma(x,shape=alpha,scale=beta), from=0, to=10,add=T,lwd =1,col="blue")  

#function examples
tst = 2.1
tst > 1.645
tst <= 1.645

set.seed(115)

x <- runif(10)
x
#return values
sumsq <- function(z){
  print(z)
}
sumsq(x)
x^2
#square values
sumsq <- function(z){
  z^2
}
sumsq(x)
#sum squares of values
sumsq <- function(z){
  sum(z^2)
}
sumsq(x)
#returns value of sum of squares
sumsq <- function(z){
  out <- sum(z^2)
  return(out)
}
sumsq(x)
#returns sum, then 3 times sum
sumsq <- function(z){
  out <- sum(z^2)
  return(c(out, 3*out))
}
sumsq(x)
#example: gamma mle
nlogL <- function(theta, x){
  alpha <- theta[1]
  beta <- theta[2]
  -sum(dgamma(x, shape=alpha, scale=beta, log=TRUE))
}
fit <- optim(par=c(5,.5), fn=nlogL, x=x, hessian=TRUE)
fit$par

#fitdistr examples
install.packages("fitdistrplus")
require(fitdistrplus)

x = c(0.60, 0.18, 0.99, 0.83, 0.63, 2.52, 0.03, 0.70, 0.25, 2.69)
descdist(x)
plotdist(data=x,histo=TRUE,demp=TRUE)

library("fitdistrplus")
data("groundbeef",package="fitdistrplus")
str(groundbeef)
plotdist(data=groundbeef$serving,histo=TRUE,demp=TRUE)
descdist(groundbeef$serving,boot=1000)

fw<-fitdist(groundbeef$serving, "Weibull")
summary(fw)

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw), legendtext = plot.legend)
ppcomp(list(fw), legendtext = plot.legend)

#bootstrap examples
install.packages("bootstrap")
library(bootstrap)
print(cor(law$LSAT, law$GPA))
print(cor(law82$LSAT, law82$GPA))
#set up bootstrap
B<-200        #number of replicates
n<-nrow(law)  #sample size
R<-numeric(B) #storage for replicates
#bootstrap estimate of standard error of R
for(b in 1:B){
  #randomly selects indices
  i<-sample(1:n, size=n, replace=TRUE)
  LSAT<-law$LSAT[i]   #i is a vector of indices
  GPA<-law$GPA[i]
  R[b]<-cor(LSAT,GPA)
}
#output
print(se.R<-sd(R))
hist(R, prob=TRUE)
r<-function(x, i){
  #correlation of columns 1 and 2
  cor(x[i,1], x[i,2])
}
library(boot)
obj<-boot(data=law, statistic=r, R=2000)
obj
y<-obj$t
sd(y)
#sample estimate for n=15
theta.hat<-cor(law$LSAT, law$GPA)
#bootstrap estimate of bias
B<-2000  #larger for estimating bias
n<-nrow(law)
theta.b<-numeric(B)
for(b in 1:B){
  i<-sample(1:n, size=n, replace=TRUE)
  LSAT<-law$LSAT[i]
  GPA<-law$GPA[i]
  theta.b[b]<-cor(LSAT, GPA)
}
bias<-mean(theta.b - theta.hat)
bias
data(patch, package="bootstrap")
patch
n<-nrow(patch)  #in bootstrap package
B<-2000
theta.b<-numeric(B)
theta.hat<-mean(patch$y) / mean(patch$z)
for(b in 1:B){
  i<-sample(1:n, size=n, replace=TRUE)
  y<-patch$y[i]
  z<-patch$z[i]
  theta.b[b]<-mean(y) / mean(z)
}
bias<-mean(theta.b) - theta.hat
se<-sd(theta.b)
print(list(est=theta.hat, bias=bias, se=se, cv=bias/se))

#jacknife examples
library(boot)
data(patch, package="bootstrap")
theta.boot<-function(data, ind){
  #function to compute statistic
  y<-dat[ind, 1]
  z<-dat[ind, 2]
  mean(y) / mean(z)
}
y<-patch$y
z<-patch$z
dat<-cbind(y, z)
boot.obj<-boot(dat, statistic=theta.boot, R=2000)
print(boot.obj)
print(boot.ci(boot.obj))
type=c("basic", "norm", "perc")
#calculations for bootstrap confidence intervals
alpha<-c(.025, .975)
#normal
print(boot.obj$t0 + qnorm(alpha) * sd(boot.obj$t))
#basic
print(2*boot.obj$t0 - quantile(boot.obj$t, rev(alpha), type=1))
#percentile
print(quantile(boot.obj$t, alpha, type=6))
library(boot)
data(law, package="bootstrap")
boot.obj<-boot(law, R=2000, statistic=function(x, i){cor(x[i,1], x[i,2])})
print(boot.ci(boot.obj, type=c("basic", "norm", "perc")))

#bootstrap sampling
#real sample from cdf F
y=c(0.60, 0.18, 0.99, 0.83, 0.63, 2.52, 0.03, 0.70, 0.25, 2.69)
nsize=length(y)
xbar=mean(y)
rate=1/xbar
print(rate)
hist(y, prob=T, breaks=10)
rug(y, lwd=2)
curve(dexp(x, rate=rate), from=0, to=3, n=101, add=TRUE)
plot(ecdf(y), verticals=TRUE)
rug(y, lwd=2)
curve(pexp(x, rate=rate), from=0, to=3, n=101, add=TRUE)
abline(v=1.06157)
set.seed(123)
B=100
rate.b=numeric(B)
for(b in 1:B){
  bsamp=sample(1:nsize, size=nsize, replace=TRUE)
  print(bsamp)
  print(y[bsamp])
  rate.b[b]=1/mean(y[bsamp])
}
print(rate.b)
hist(rate.b, prob=TRUE)
lines(density(rate.b))

#postive serial interval data
require(fitdistrplus)
SerialPlus=read.csv(file="COVID19_Serial-Positive.csv")
par("mar")
head(SerialPlus)
hist(SerialPlus$serial,prob=TRUE) 
lines(density(SerialPlus$serial),lty="dashed")
fit.w = fitdist(SerialPlus$serial,"weibull") 
summary(fit.w)
str(fit.w) 
sp=fit.w$estimate[1] #shape parameter 
sc = fit.w$estimate[2] #scale parameter
hist(SerialPlus$serial,prob=TRUE,main="Weibull PDF") 
lines(density(SerialPlus$serial),lty="dashed") 
curve(dweibull(x,shape=sp,scale=sc), from=0,to=20,add=TRUE,lwd=2,col="blue", xlab = "serial interval")
#percentile CIs for quantiles by bootstrap
curve(pweibull(x,shape=sp,scale=sc), 
      from=0,to=20, 
      lwd=1,col="grey", 
      xlab = "serial interval", 
      main = "Weibull CDF")
B = 100
sp.vec = numeric(length=B)
sc.vec = numeric(length=B)
# allocate storage for q.025, q.500, q.975
q.025 = numeric(length=B)
q.500 = numeric(length=B)
q.975 = numeric(length=B)
n = length(SerialPlus$serial)
for (b in 1:B) { 
  ind = sample(1:n,size=n,replace=TRUE) 
  fit.w = fitdist(SerialPlus$serial[ind],"weibull") 
  sp.vec[b]=fit.w$estimate[1] #shape parameter 
  sc.vec[b] = fit.w$estimate[2] #scale parameter
  curve(pweibull(x,shape=sp.vec[b],scale=sc.vec[b]), 
        from=0,to=20, 
        lwd=1,col="grey",add=TRUE, 
        xlab = "serial interval", 
        main = "Weibull CDF")
  q.025[b]<-qweibull(p=.025,shape=sp.vec[b],scale=sc.vec[b])
  q.500[b]<-qweibull(p=.500,shape=sp.vec[b],scale=sc.vec[b])
  q.975[b]<-qweibull(p=.975,shape=sp.vec[b],scale=sc.vec[b])
}
#compute quantiles at 2.5%, 50%, and 97.5%
abline(h=c(.025,.5,.975),lty="dotted")
quantile(q.025,p=c(.025,.5,.975))
quantile(q.500,p=c(.025,.5,.975))
quantile(q.975,p=c(.025,.5,.975))

#incubation period data
require(fitdistrplus)
IncubationPlus=read.csv(file="incubateDataSim.csv")
par("mar")
head(IncubationPlus)
hist(IncubationPlus$x,prob=TRUE) 
lines(density(IncubationPlus$x),lty="dashed")
fit.w = fitdist(IncubationPlus$x,"weibull") 
summary(fit.w)
str(fit.w) 
sp=fit.w$estimate[1] #shape parameter 
sc = fit.w$estimate[2] #scale parameter
hist(IncubationPlus$x,prob=TRUE,main="Weibull PDF") 
lines(density(IncubationPlus$x),lty="dashed") 
curve(dweibull(x,shape=sp,scale=sc), from=0,to=20,add=TRUE,lwd=2,col="blue", xlab = "incubation period")
#percentile CIs for quantiles by bootstrap
curve(pweibull(x,shape=sp,scale=sc), 
      from=0,to=20, 
      lwd=1,col="grey", 
      xlab = "incubation period", 
      main = "Weibull CDF")
B = 25
sp.vec = numeric(length=B)
sc.vec = numeric(length=B)
# allocate storage for q.025, q.500, q.975
q.025 = numeric(length=B)
q.500 = numeric(length=B)
q.975 = numeric(length=B)
n = length(IncubationPlus$x)
for (b in 1:B) { 
  ind = sample(1:n,size=n,replace=TRUE) 
  fit.w = fitdist(IncubationPlus$x[ind],"weibull") 
  sp.vec[b]=fit.w$estimate[1] #shape parameter 
  sc.vec[b] = fit.w$estimate[2] #scale parameter
  curve(pweibull(x,shape=sp.vec[b],scale=sc.vec[b]), 
        from=0,to=20, 
        lwd=1,col="grey",add=TRUE, 
        xlab = "incubation period", 
        main = "Weibull CDF")
  q.025[b]<-qweibull(p=.025,shape=sp.vec[b],scale=sc.vec[b])
  q.500[b]<-qweibull(p=.500,shape=sp.vec[b],scale=sc.vec[b])
  q.975[b]<-qweibull(p=.975,shape=sp.vec[b],scale=sc.vec[b])
}
#compute quantiles at 2.5%, 50%, and 97.5%
abline(h=c(.025,.5,.975),lty="dotted")
quantile(q.025,p=c(.025,.5,.975))
quantile(q.500,p=c(.025,.5,.975))
quantile(q.975,p=c(.025,.5,.975))
