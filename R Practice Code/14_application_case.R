library(ggplot2)
library(astsa)

#The code here uses the updated scripts in astsa version 2.0. 
#Details of the updates are in the help files of Kfilter, Ksmooth, and EM. 

#Example 6.1
tsplot(blood, type='o', col=c(6,4,2), lwd=2, pch=19, cex=1) 

#Example 6.2
tsplot(cbind(gtemp_land, gtemp_ocean), spaghetti=TRUE, lwd=2, pch=20, type="o", 
       col=astsa.col(c(4,2),.5), ylab="Temperature Deviations", main="Global Warming")
legend("topleft", legend=c("Land Surface", "Sea Surface"), lty=1, pch=20, col=c(4,2), bg="white")


#example 6.5
# generate data 
set.seed(1)  
num = 50
w   = rnorm(num+1,0,1)
v   = rnorm(num,0,1)
mu  = cumsum(w)     # states:  mu[0], mu[1], . . ., mu[50] 
y   = mu[-1] + v    # obs:  y[1], . . ., y[50]

# filter and smooth (Ksmooth does both)
mu0 = 0;  sigma0 = 1;  phi = 1;  sQ = 1;  sR = 1   
ks = Ksmooth(y, A=1, mu0, sigma0, phi, sQ, sR) 

# pictures 
par(mfrow=c(3,1))

tsplot(mu[-1], type='p', col=4, pch=19, ylab=expression(mu[~t]), main="Prediction", ylim=c(-5,10)) 
lines(ks$Xp, col=6)
lines(ks$Xp+2*sqrt(ks$Pp), lty=6, col=6)
lines(ks$Xp-2*sqrt(ks$Pp), lty=6, col=6)

tsplot(mu[-1], type='p', col=4, pch=19, ylab=expression(mu[~t]), main="Filter", ylim=c(-5,10)) 
lines(ks$Xf, col=6)
lines(ks$Xf+2*sqrt(ks$Pf), lty=6, col=6)
lines(ks$Xf-2*sqrt(ks$Pf), lty=6, col=6)

tsplot(mu[-1], type='p', col=4, pch=19, ylab=expression(mu[~t]), main="Smoother", ylim=c(-5,10)) 
lines(ks$Xs, col=6)
lines(ks$Xs+2*sqrt(ks$Ps), lty=6, col=6)
lines(ks$Xs-2*sqrt(ks$Ps), lty=6, col=6)

mu[1]; ks$X0n; sqrt(ks$P0n)   # initial value info
