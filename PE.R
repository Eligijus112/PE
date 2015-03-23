library(tidyr)
library(dplyr)
library(plyr)

##################
### 1 chapter ####
##################

# 1.1 exercise

Plot the pairs:
# x=(x1,x3,x5,...)
# y=(x2,x4,x6...)
# plot(x,y)

x=xx[seq(1, length(xx), 2)]
y=xx[seq(2, length(xx), 2)]
plot(x,y) #panasu i tolydu

# Look at the estimate autocorrelation function

acf(xx) #nepanasu i WN

# 1.2 exercise. 

set.seed(100) 
ru=runif(100) 
ru1=ru[seq(1,99,by=2)] 
ru2=ru[seq(2,100,by=2)]

RU1=ru[1:50]
RU2=ru[51:100]

cor.test(ru1, ru2)#nekoreliuoja
cor.test(RU1, RU2)#nekoreliuoja
cor.test(x, y)#nekoreliuoja

# nelabai tikslu, nes sis testas naudojamas normaliesiems


# 1.3 exercise 

Nsim <- 100000
Y    <- rexp(Nsim)
hist(Y)

##
MM <- matrix(nrow=2500, ncol=4)
for(i in 1:2500){
  MM[i, 1:4] <- rexp(4)
}

Y     <- apply(MM, 1, sum)
final <- cbind(MM, 2*Y)
colnames(final) <- c(paste0("X", 1:4), "Y")

hist(final[, "Y"])
shapiro.test(final[, "Y"])


##1.4 exercise
n <- 10000
fun.inverse <- function(x){
  y <- tan(pi*(x-1/2))
  return(y)
}

par(mfrow=c(1,2))

U <- runif(n)
hist(fun.inverse(U))
hist(rcauchy(U))

mean(fun.inverse(U))
mean(rcauchy(U))

plot(rcauchy(U))
plot(fun.inverse(U))

##1.5
n=1000
S <- numeric(n)
for(i in 1:n){
S[i] <- sum(runif(12, min = -1/2, max = 1/2))
}

mean(S)
var(S)

shapiro.test(S)
hist(S)
plot(S)

## 1.6 

U1 <- rnorm(1000, mean = 3, sd=4)
U2 <- rnorm(1000, mean = 3, sd=4)

X1 <- sqrt(-2*log(U1))*cos(2*pi*U2)
X2 <- sqrt(-2*log(U1))*sin(2*pi*U2)
hist(X1)
hist(X2)

shapiro.test(X1)
shapiro.test(X2)

##1.7

nn <- matrix(rnorm(2000), ncol=2)
m  <- 100 
B  <- matrix(rnorm(m*m), nrow=m, ncol=m)
NN <- B*mean(nn)

test <- B*t(B)
E <- mean(NN)
V <- var(NN)
shapiro.test(E)
test/V

## 1.8 exercise
bases       <- list()
bases.names <- list()
bases[['base_1']]  <- c('2',"3", "4", "5")
bases[['base_2']]  <- c('1', "5", "4")
bases[['base_3']]  <- c('1', "5", "4")
bases[['base_4']]  <- c("1", "2", "3")
bases[['base_5']]  <- c("2", "1", "3")
bases.names[['base_1']]  <- "Kanzasas"
bases.names[['base_2']]  <- "Koloradas"
bases.names[['base_3']]  <- "Misisipe"
bases.names[['base_4']]  <- "Nebraska"
bases.names[['base_5']]  <- "Oklahoma"
s <-numeric()
for(i in 1:1000){
pointer1 <- bases[['base_4']]
pointer2 <- bases[['base_5']]
j<-1
cat(bases.names[['base_4']],"||", bases.names[[paste0('base_5')]], "\n")
while(as.numeric(pointer1[1])!=as.numeric(pointer2[1]) | as.numeric(pointer1[2])!=as.numeric(pointer2[2]) | as.numeric(pointer1[3])!=as.numeric(pointer2[3]) | name1!=name2){
  x <- runif(1)
  y <- runif(1)
  if(length(pointer1)==3 & length(pointer2)==3){
  if(0<=x   & x<=0.33) index1=1
  if(0.33<x & x<=0.66) index1=2
  if(0.66<x & x<=1)    index1=3
  
  if(0<=y   & y<=0.33) index2=1
  if(0.33<y & y<=0.66) index2=2
  if(0.66<y & y<=1)    index2=3
  } else
    if(length(pointer1)==4 & length(pointer2)==3){
      if(0<=x   & x<=0.25) index1=1
      if(0.25<x & x<=0.5)  index1=2
      if(0.5<x & x<=0.75)  index1=3
      if(0.75<x & x<=1)    index1=4
      
      if(0<=y   & y<=0.33) index2=1
      if(0.33<y & y<=0.66) index2=2
      if(0.66<y & y<=1)    index2=3
      }
    if(length(pointer1)==3 & length(pointer2)==4){
      if(0<=y   & y<=0.25) index2=1
      if(0.25<y & y<=0.5)  index2=2
      if(0.5<y & y<=0.75)  index2=3
      if(0.75<y & y<=1)    index2=4
      
      if(0<=x   & x<=0.33) index1=1
      if(0.33<x & x<=0.66) index1=2
      if(0.66<x & x<=1)    index1=3
    }
    pos1 <- pointer1[index1]
    pos2 <- pointer2[index2]
    bases[[paste0('base_', pos1)]] -> pointer1
    bases[[paste0('base_', pos2)]] -> pointer2
    cat(bases.names[[paste0('base_', pos1)]],"||", bases.names[[paste0('base_', pos2)]], "\n")
    name1 <- bases.names[[paste0('base_', pos1)]]
    name2 <- bases.names[[paste0('base_', pos2)]]
    j= j+1

}
cat("----------------- \n")
s[i] <- j -1
}
mean(s)
var(s)

##1.8 example
example <- function(x){
  y <- 1/(1+sin(2*x)*log(x)^2)
  return(y)
}

plot(example(rnorm(1000, sd=0.4, mean=2)))
n   <- 100000
nn  <- runif(n, 0.1, 2.1)
int <- numeric(n-9)
ss  <- example(nn)
for(i in 1:n){
  int[i-9] <- sum(ss[1:i]/i)
}
int[i-9]

#

xx <- c(-1,1,1,-1,-1)
yy <- c(-1,-1,1,1,-1)
plot(xx,yy,type="l") 
x <- cos(seq(0,2*pi,length=100))
y <- sin(seq(0,2*pi,length=100)) 
polygon(x,y,col=3)
points(runif(500,-1,1),
       runif(500,-1,1),pch="*") 

xxx <- runif(100000,-1,1)
yyy <- runif(100000,-1,1) #
s <- numeric(500)
print(s) 
for(i in 1:500) {s[i] <- 4*sum(ifelse(xxx[1:(200*i)]^2+yyy[1:(200*i)]^2<=1,1 ,0))/(200*i)}
print(s) 
plot(1:500,s,type="l") 
lines(1:500,rep(pi,500))
s[500] 

#1.9 exercise
fun <- function(x){
  y <- 1/(1 + sinh(2*x)*log(x)^2)
  return(y)
}
z  <- seq(0.1, 2.1, by=0.001)
plot(fun(z), x=z, col="red", lwd=0.5) 
lines(y=yfit, x=z, col="blue", lwd=2)

library(truncnorm)
n=10000
set.seed(1)
inte <- function(x) {1/(1+sinh(2*x)*log(x)^2)}
rnn = rtruncnorm(n, a=0.1, b=2.1, mean = 1, sd = 0.4)
rezult=mean(inte(rnn)/dtruncnorm(rnn, a=0.1, b=2.1,
                                 mean = 1, sd = 0.4))
rezult 

saveGIF({
  ani.options(nmax = 40)
  animate.integral(inte, 0.1, 2.1) 
}, interval = 0.05, movie.name = "customf.gif", ani.width = 700, ani.height = 700)

#1.10 exercise

#1)
integrate(Vectorize(function(y) integrate(function(x) abs(x-y), -1, 1)$value), -1, 1)

#2) 
n <- 15000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
mean(4*abs(x-y))


#1.11 exercise

library(MASS)
library(mvtnorm)
n=1000
z=numeric()
zz=numeric()
cov <- matrix(c(9,4,4,4), nrow=2, ncol=2)
N<-rmvnorm(n, rep(0, 2), cov)
f <- function(r){
  for(i in 1:n){ 
    z[i] =sqrt(N[i,1]^2 + N[i,2]^2)
    zz[i]=z[i]<r
  }
  return(sum(zz)/n)
}

f(1)

#1.12 exercise

set.seed(2)
N=100
X=rnorm(N,sd=5)
eps=rnorm(N)
Y=2+0.3*X+eps # DGP
plot(X,Y)
summary(lm(Y~X)) 

BETA <- numeric()
for(j in 1:1000){
  eps <- rnorm(100)
  X   <- rnorm(100, sd=5)
  Y   <- 2 + 0.3*X +eps
  BETA[j] <- lm(Y~X)$coef[2]
}

sd(BETA)

#1.13

eps <- rnorm(1000)
X1  <- rnorm(1000)
X2  <- 3*X1 + eps
cor(X1, X2)

Y  <- -0.5 + 3*X1 + X2 + rnorm(1000)

BETA1 <- numeric()
BETA2 <- numeric()
for ( i in 1:1000){
  eps <- rnorm(1000)
  X1  <- rnorm(1000)
  X2  <- 3*X1 + eps
  #cor(X1, X2)
  
  Y  <- -0.5 + 3*X1 + X2 + rnorm(1000)
  BETA2[i] <- lm(Y~X1)$coef[2]
  BETA1[i] <- lm(Y~X1+X2)$coef[2]
}

mean(BETA2)
mean(BETA1)

#1.14

BETA1 <- numeric()
BETA2 <- numeric()
for ( i in 1:1000){
  eps <- rnorm(1000)
  X1  <- rnorm(1000)
  X2  <- 3*X1 + eps
  #cor(X1, X2)
  
  Y  <- -0.5 + 3*X1 + rnorm(1000)
  BETA1[i] <- lm(Y~X1)$coef[2]
  BETA2[i] <- lm(Y~X1+X2)$coef[2]
}

mean(BETA1)
mean(BETA2)

#1.15 exercise

T.n <- 15
y=cumsum(rnorm(T.n))
yts=ts(y)
library(dynlm)
T.value <- numeric()
for(j in 1:1000){
  T.n <- 15
  y=cumsum(rnorm(T.n))
  yts=ts(y)
  standart.error <- summary(dynlm(d(yts)~L(yts)-1))$coef[2]
  estimate       <- summary(dynlm(d(yts)~L(yts)-1))$coef[1]
  t.value        <- (estimate - 0)/ standart.error
  T.value[j]     <- t.value
}

hist(T.value)
quantile(T.value, c(.01, .05, .1))

#1.16 exercise

n  <- 10000
rc <- n %>% rcauchy
rc %>% cummax %>% plot
rc %>% cummin %>% plot

mean.rc   <- numeric()
var.rc    <- numeric()
median.rc <- numeric()

for(i in 1:n){
  mean.rc[i]   <- rc[1:i] %>% mean
  var.rc[i]    <- rc[1:i] %>% var
  median.rc[i] <- rc[1:i] %>% median
  cat(i, "\n")
  }
par(mfrow=c(1,3))
mean.rc   %>% plot(type="l")
var.rc    %>% plot(type="l")
median.rc %>% plot(type="l")


#1.17 The harbour system (uniform distribution and exponential distribution)

harbor.system <- function(iterations, nr.of.ships, min.arrive,
                           max.arrive, min.unload, max.unload, type="Uniform",
                           rate.arrive=NULL, rate.unload=NULL, comments=TRUE, ...){
results <- list()
final.results <- matrix(nrow=iterations, ncol=7) %>% as.data.frame
for(j in 1:iterations){
laiveliai   <- matrix(ncol=4, nrow=nr.of.ships) %>% as.data.frame
colnames(laiveliai)  <- c("Between", "Unload", "Arrive" ,"Finish")
row.names(laiveliai) <- paste0("laivas_", 1:(nr.of.ships))
if(type=="Exponential"){
  laiveliai[1, 1:2] <- c(1 %>% rexp(rate=1/rate.arrive) %>% round(digits=0), 1 %>% rexp(rate=1/rate.unload) %>% round(digits=0))
                         }else{
laiveliai[1, 1:2] <- c(1 %>% runif(min=min.arrive, max=max.arrive), 
                       1 %>% runif(min=min.unload, max=max.unload)) %>% round(digits=0)}
laiveliai[1, "Arrive"]   <- laiveliai[1, 1]
laiveliai[1, "Finish"]   <- sum(laiveliai[1,2], laiveliai[1,1])
HARTIME  <- laiveliai[1, "Unload"]
MAXHAR   <- laiveliai[1, "Unload"]
WAITIME  <- 0
MAXWAIT  <- 0
IDLETIME <- laiveliai[1, "Arrive"]

timediff <- numeric()
idle     <- numeric()
wait     <- numeric()
start    <- numeric()
finish   <- numeric()
harbor   <- numeric()
queue    <- numeric(nr.of.ships)

for(i in 2:(nr.of.ships)){
  if(type=="Exponential"){
    laiveliai[i, 1] <- 1 %>% rexp(rate=1/rate.arrive) %>% round(digits=0)
    laiveliai[i, 2] <- 1 %>% rexp(rate=1/rate.unload) %>% round(digits=0)
    }else{
  laiveliai[i, 1] <- 1 %>% runif(min=min.arrive, max=max.arrive) %>% round(digits=0)
  laiveliai[i, 2] <- 1 %>% runif(min=min.unload, max=max.unload) %>% round(digits=0)
    }
  laiveliai[i, "Arrive"] <- laiveliai[i-1, "Arrive"] + laiveliai[i, "Between"]
  timediff[i] <- laiveliai[i, "Arrive"] - laiveliai[i-1, "Finish"]
  
  if(timediff[i]<0){  
    wait[i] <- -1*timediff[i]
    idle[i] <- 0
  }
  if(timediff[i]>=0){ 
    wait[i] <- 0
    idle[i] <- timediff[i]
  }
  
  if(wait[i]!=0){ 
    tmp <- laiveliai[1:i, c("Arrive", "Finish")]
    if(length(which(tmp[1:(i-1), "Finish"] > tmp[i, "Arrive"]))==0) queue[i] <- 1              
    else queue[i] <- length(which(tmp[1:(i-1), "Finish"] > tmp[i, "Arrive"]))               
  } else queue[i] <- 0 
  start[i]   <- laiveliai[i, "Arrive"] + wait[i]
  laiveliai[i, "Finish"] <- start[i] + laiveliai[i, "Unload"]
  harbor[i]  <- wait[i] + laiveliai[i, "Unload"]
  HARTIME    <- sum(harbor, na.rm=T)
  
  if(harbor[i]>MAXHAR) MAXHAR <- harbor[i]
  if(wait[i]> MAXWAIT) MAXWAIT <- wait[i] 
  if(comments==TRUE){ 
    if(queue[i]==1) cat("When", i, "ship arrived it was the first in line\n")
    else{ if(queue[i-1]==0) cat("Ship", i, "didnt have to wait\n")
          else  cat("When", i, "ship arrived there were ",queue[i-1], "ships waiting\n")
   
    }
  }
}
WAITIME  <- sum(wait, na.rm=T)
IDLETIME <- sum(idle, na.rm=T)
avg.HARTIME  <- HARTIME/nr.of.ships
avg.WAITIME  <- WAITIME/nr.of.ships
prc.IDLETIME <- IDLETIME/laiveliai[nr.of.ships, "Finish"]
MAXqueue     <- max(queue)
AVGqueue     <- mean(queue)

final.results[j, ] <- c(avg.HARTIME, MAXHAR, avg.WAITIME, MAXWAIT, prc.IDLETIME*100, MAXqueue, AVGqueue %>% round(digits=2))
}
colnames(final.results) <- c("Average time in harbour", "Max time in Harbour",
                             "Average waiting time", "Max waiting time", "Perc of time beeing idle",
                             "Longest queue", "Average queue")
results[[1]] <- final.results
results[[2]] <- laiveliai
return(results)
}

table     <- harbor.system(10, 100, 15, 145, 45, 90)
results   <- table[[1]]
proccess  <- table[[2]] 
table1       <- harbor.system(10, 100, rate.arrive=50, rate.unload=50, type="Exponential", comments=FALSE) 
results.exp  <- table1[[1]]
proccess.exp <- table1[[2]]
#1.18 exercise

fi=seq(0,2*pi,length.out=6);fi
c.fi=cos(fi)
s.fi=sin(fi)
c.fi; s.fi
plot(c.fi,s.fi)
for(i in 1:5)
{
  segments(c.fi[i],s.fi[i],c.fi[i+1],s.fi[i+1]) 
  segments(0,0,c.fi[i],s.fi[i]) 
}
polygon(c(0,c.fi[1],c.fi[2],0),c(0,s.fi[1],s.fi[2],
                                 0),col=2)

u <- c(1, 0)
v <- c(c.fi[2], s.fi[2])
coord.matrix <- matrix(c(u,v), ncol=2, nrow=2)

x.plot <- numeric()
y.plot <- numeric()
for(i in 1:1000){
x <- runif(1)
y <- runif(1)
if(x+y>1) {
  x <- NA 
  y <- NA
}
x.plot[i] <- t(coord.matrix %*% as.matrix(c(x,y)))[1, 1]
y.plot[i] <- t(coord.matrix %*% as.matrix(c(x,y)))[1, 2]
}

x.plot <- x.plot[!is.na(x.plot)]
y.plot <- y.plot[!is.na(y.plot)]
points(x.plot, y.plot)

## filling the whole pentagon
for(j in 1:length(x.plot)){
theta=sample(0:5, 1)*(2*pi/5)
R = matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol=2, nrow=2)
ppqq <- as.matrix(R %*% t(as.matrix(cbind(x.plot[j], y.plot[j]))))
points(t(ppqq), col= "blue")
}

#########################
##### BOOTSTRAPING ######
#########################

set.seed(1)
sample(micex,replace=TRUE) 
micex <- read.table(file.choose(), header=F, check.names=F)
micex <- apply(micex, c(1,2), as.numeric)
b.median1 <- function(data, B=100) {
  resamples <- lapply(1:B, function(i) sample(data, replace=T))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)
}
set.seed(15)
b.med1=b.median1(micex)
b.med1$std.err


##2.2 exercise

## example ########

# Bootstrap 95% CI for regression coefficients
library(boot)
# function to obtain regression coefficients
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications
results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)
# view results
results
plot(results, index=1) # intercept
plot(results, index=2) # wt
plot(results, index=3) # disp
# get 95% confidence intervals
boot.ci(results, type="bca", index=1) # intercept 
boot.ci(results, type="bca", index=2) # wt
boot.ci(results, type="bca", index=3) # disp 

####################################

results <- boot(data=mtcars, statistic=bs,
                R=1000, formula=mpg~wt+disp)
N <- length(mtcars[, 1])

r.sq <- numeric()
for(i in 1:1000){
  index    <- sample(1:N, N, replace=TRUE) 
  new.data <- mtcars[index, ]
  r.sq[i]  <- summary(lm(mpg~wt+disp, data=new.data))$r.squared
}
hist(r.sq)
abline(v=summary(lm(mpg~wt+disp, data=mtcars))$r.squared, col="blue")
sorted <- sort(r.sq)
plot(sorted[25:975])
sorted[25]    #95 proc. confidence interval 
sorted[975]   #95 proc. confidence interval

##2.3 exercise (not yet finished)

library(MASS)
data(galaxies)
(gal <- galaxies/1000)
hist(gal, breaks=seq(5,35,by=2.5),freq=FALSE,ylim=c(0,0.16))
lines(density(gal)) 
m <- median(gal)
#i)
eps     <- 0.01
index   <- which(round(density(gal)$"x", digits=2)>=20.83-eps & round(density(gal)$"x", digits=2)<=20.83+eps)
y.value <- density(gal)$"y"[index]

var.boot <- sqrt(1/4*length(gal)*y.value)

