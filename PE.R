library(tidyr)
library(dplyr)
library(plyr)

##################
### 1 chapter ####
##################

# 1.1 exercise

# x=(x1,x3,x5,...)
# y=(x2,x4,x6...)
# plot(x,y)
uniform=function(n=10)
{
  x=numeric(n)
  x[1]=1899 # seed, <2^31-1
  mm=2^31-1
  for(i in 1:(n-1))
  {
    x[i+1]=(65539*x[i])%%mm # c=0
  }
  x/mm
} 

xx=uniform(1000)
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

draw.normal <- function(x, mean, sigma){
  y <- (1/(sigma*sqrt(2*pi)))*exp(-(x-mean)^2/(2*sigma^2))
  return(y)
}

draw.normal(z, 1, 0.4) %>% plot(x=z)
fun(z) %>% lines(x=z, col="red", lwd=2)

legend('topright', c("Gaussian", "Custom function") , lty=1, col=c('red', 'black'), bty='n', cex=.75, lwd=c(2,2))


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

cov <- matrix(c(9,4,4,4), nrow=2, ncol=2)
N   <-rmvnorm(n, rep(0, 2), cov)

f <- function(r, n, Variable){
  x      <- numeric()
  x.good <- numeric()
  for(i in 1:n){ 
    x[i]      <- sqrt(Variable[i,1]^2 + Variable[i,2]^2)
    x.good[i] <- x[i]<r
  }
  return(sum(x.good, na.rm=T)/n %>% sum(na.rm=T))
}

f(r=1, 1000, N)

i <- 1
for.plot <- numeric()
for(j in seq(0.1, 3, by=0.01)){
  if(i <=  (seq(0.1, 3, by=0.01) %>% length())){
  for.plot[i] <- f(r=j, 1000, N)
  }
  i <- i+1
}

for.plot %>% plot(x=seq(0.1, 3, by=0.01))
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
plot(queue, type="l")
final.results[j, ] <- c(avg.HARTIME, MAXHAR, avg.WAITIME, MAXWAIT, prc.IDLETIME*100, MAXqueue, AVGqueue) %>% round(digits=2)
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
table1       <- harbor.system(10, 100, rate.arrive=(15+145)/2, rate.unload=(45+90)/2, type="Exponential", comments=FALSE) 
results.exp  <- table1[[1]]
proccess.exp <- table1[[2]]

par <- c(4.5,12.5)
pdf("results.pdf", height=par[1], width=par[2])
grid.table(results, rows=NULL)
dev.off()

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

##2.3 exercise 

library(MASS)
data(galaxies)
(gal <- galaxies/1000)
hist(gal, breaks=seq(5,35,by=2.5),freq=FALSE,ylim=c(0,0.16))
plot(density(gal, from=20, to=21))


#i)
m <- median(gal)
dens    <- density(gal, from=20, to=21)
index   <- which((dens$x %>% round(digits=3))==round(m, digits=3))
f.value <- dens$y[index]

#ii.)
st.error <- sqrt(1/4*length(gal)*f.value)

#iv-v.)

tmp.median <- numeric()
for(j in 1:1000){
  index   <- sample(1:length(gal), length(gal), replace=T)
  gal.tmp <- gal[index]
  m       <- median(gal.tmp)
  tmp.median[j] <- m
}

mean.boot <- tmp.median %>% median()
sd.boot   <- tmp.median %>% var() %>% sqrt()

confidence.int.boot <- c(mean.boot - 2*sd.boot, mean.boot + 2*sd.boot)
confidence.int.boot
tmp.median %>% density() %>% plot()
tmp.median %>% hist()
tmp.median %>% shapiro.test()

#2.4

library(MASS)
data(shoes)
A <- shoes$A
B <- shoes$B
C <- A - B

#i)
t.test(C) # A - B nera lygu nuliui

#ii)

m.boot   <- numeric()
var.boot <- numeric()
for (i in 1:1000){
  index   <- sample(1:length(C), length(C), replace=T)
  C.tmp   <- C[index]
  m.boot[i]   <- C.tmp %>% mean()
  var.boot[i] <- (length(C.tmp)-1)*var(C.tmp/length(C.tmp)^2)
}

mean.fun <- function(d, i) {
  n <- length(i)
  c(mean(d[i]), (n-1)*var(d[i])/n^2)
}
shoes.boot <- boot(B-A, mean.fun, R=1000) 

shoes.boot %>% boot.ci(type="stud")
## because the confidence interval is not near 0.05, we conclude, that A - B is not equal to zero

###################################################
############## 3 chapter ##########################
### Least squares methods and their diagnostics ###
###################################################

#3.1
# OLS estimates by hand
x <- c(0, 1, 2, 3, 4)
y <- c(1, 4, 3, 8, 9) 

X         <- matrix(c(rep.int(1, length(x)), x), ncol=2) 
X.t       <- X %>% t()
XX.t      <- X.t %*% X
X.inverse <- XX.t %>% solve() 
part2     <- X.t %*% y 
part1     <- X.inverse
beta      <- part1 %*% part2

yhat <- X %*% beta

plot(x, y)
lines(yhat, x=x, col="red")

## using lm()

x=0:4
y=c(1,4,3,8,9)
mod=lm(y~x)
mod$res
summary(mod) 

### 3.2 
### Find the H matrix of the 3.1 exercise

e        <- y - yhat
e.sq     <- t(e) %*% e
var.hat  <- e.sq/(length(e)-2) 
st.error <- var.hat %>% sqrt()

cov(e)
H <- X %*% part1 %*% X.t

## 3.1 example

library(car)
data(Davis)
par(mfrow=c(1,2))
with(Davis,
{
  plot(height,weight)
  boxplot(height,xlab="height")
}
) 

wh.lm <- lm(weight~height, subset=sex=="F", data=Davis)
summary(wh.lm)
identify(height,weight) 
par(mfrow=c(2,2))
plot(wh.lm)

library(car)
data(Duncan)
attach(Duncan)
duncan.lm=lm(prestige~income+education)
par(mfrow=c(1,1))
plot(rstandard(duncan.lm),type="l")
lines(rstudent(duncan.lm),col=2,lty=2)

outlierTest(lm.D <- lm(prestige~income+education,data=Duncan),
            labels=row.names(Duncan)) 
plot(hatvalues(lm.D))
abline(h=c(2,3)*3/45, lty=2) 

plot(hatvalues(lm.D))

plot(cookd(duncan.lm),type="h") 
abline(h=4/42,lty=2) 
identify(1:45,cookd(duncan.lm), 
         row.names(Duncan))

##3.3 exercise
## Analyze the wh.lm model with respect to its residuals

wh.lm <- lm(weight~height, subset=sex=="F", data=Davis)
summary(wh.lm)
plot(Davis$height, Davis$weight)
abline(wh.lm, col="blue", lwd=2)
# Clearly there is a leverage point here
wh.lm %>% rstandard() %>% plot()
wh.lm %>% cooks.distance() %>% plot()
cook <- wh.lm %>% cooks.distance() 
n <- wh.lm %>% cooks.distance() %>% length()
text(x=1:n, cook, labels=1:n, cex=0.83, pos=1)
# the fourth point is very likely to be an influantial point
new.data <- Davis %>% filter(sex=="F")
wh.lm.v2 <- lm(weight~height, subset=sex=="F", data=new.data[-4, ])
wh.lm.v2 %>% summary()
plot(Davis$height, Davis$weight)
abline(wh.lm, col="blue", lwd=2)
abline(wh.lm.v2, col="red", lwd=2)
legend('bottomleft', c("With the 4th point", "Without the 4th point") , 
       lty=1, col=c('blue', 'red'), bty='n', cex=.75, lwd=c(2,2))

##3.4 exercise
## Box - Cox transformation

N=100 
set.seed(1234) 
x=runif(N,1,20) 
eps=rnorm(N) 
y1 <- (3-0.5*x+eps)^2        #seq 1 
eps=rnorm(N) 
y2 <- (1/0.5 - 3*x + eps)^2  #seq 2
eps=rnorm(N) 
y3 <- (3-0.5*x+eps)^4        #seq 3 
plot(y3)

box.cox <- function(liambda, y){
  if(liambda==0) x <- log(y)
  else x <- (y^liambda - 1)/liambda
  return(x)
}

liambdas <- seq(-1 , 1, by= 0.5)
par(mfrow=c(1, length(liambdas)))

for(j in 1:length(liambdas)){
  
  y.custom <- box.cox(liambdas[j], y1)
  plot(x, y.custom) 
  title(main=c("labmda=",paste(liambdas[j])))
  abline(lm(y.custom ~ x))
}

##3.2 example
## Box - Cox 

library(car) 
data(Ornstein) 
head(Ornstein) 

mod.ornstein <- lm(interlocks + 1 ~ log(assets) + nation + sector, data=Ornstein) 
summary(mod.ornstein) 

attach(Ornstein) 
par(mfrow=c(1,3)) 
hist(interlocks) 
qqPlot(mod.ornstein, pch=15) 
plot(density(rstudent(mod.ornstein))) 

boxCox(mod.ornstein, lambda = seq(0, 0.6, by=0.1))

## Transformed model

Ornstein1 <- transform(Ornstein, y1=bcPower(interlocks + 1, 0.2))
                                             
mod.ornstein.trans <- update(mod.ornstein, y1 ~ ., 
                             data=Ornstein1) 
summary(mod.ornstein.trans) 
qqPlot(mod.ornstein.trans, pch=15) 

## 3.5 exercise 
## Further implementation of the Box - Cox transformation
## We will analyse the duncan.lm model (3.1 example)
library(car)
data(Duncan)
attach(Duncan)
duncan.lm=lm(prestige~income+education)
summary(duncan.lm)
par(mfrow=c(1, 2))
qqPlot(duncan.lm, pch=15) 
plot(density(rstudent(duncan.lm))) ## the upper tail is skewed. Thus we should use the Box - Cox transformation

boxCox(duncan.lm, lambda = seq(0, 1.2, by=0.1)) # the value that maximizes the ML function is about 0.6-0.8
duncan.transformed <- transform(Duncan, y1=bcPower(prestige, 0.65))

lm.tDuncan  <- update(duncan.lm, y1 ~ ., data=duncan.transformed) 
summary(lm.tDuncan)
dev.off()
par(mfrow=c(1, 2))
qqPlot(lm.tDuncan, pch=15) 
plot(density(rstudent(lm.tDuncan))) 


### 4 chapter #########
## Robust regression ##

x <- c(rnorm(16),-512,-200,15,1000)
mn1 <- mean(x)
mn2 <- mean(x, trim=0.05)

set.seed(45) 
x=rnorm(40,sd=10) 
sort(x) 
cat("mean(x)=",mean(x),"\n") 
cat("mean(x,trim=0.05)=",mean(x,trim=0.05),"\n") 

## 4.2 exercise 

?anscombe
data <- anscombe

plot(data$x3, data$y3)
mod <- lm(y3 ~ x3, data=data)
summary(mod)
abline(mod, col="red", lwd=2)

## Removing outliers 

grubbs.test(data$y3)
which(data$y3==12.74)
new.data <- data[-3, c("x3", "y3")]
mod.1 <- lm(y3 ~ x3, data=new.data)
abline(mod.1, col="blue", lwd=2)
summary(mod.1)
legend('topleft', c("Without outliers", "With outliers") , 
       lty=1, col=c('blue', 'red'), bty='n', cex=.75, lwd=c(2,2))
##

## 4.2 example
library(MASS) 
library(car) # for data 

mod.ls <- lm(prestige ~ income + education, data=Duncan) 
summary(mod.ls) 

mod.ls.2 <- update(mod.ls, subset=-c(6,16)) 
summary(mod.ls.2) 

mod.huber <- rlm(prestige ~ income + education, data=Duncan) 
summary(mod.huber) 

plot(mod.huber$w, ylab="Huber Weight") 
smallweights <-  which(mod.huber$w < 0.8) 
showLabels(1:45, mod.huber$w, rownames(Duncan), id.method = smallweights, id.cex = .6) 

## 4.3 exercise

data.dem <- read.table('http://www.mif.vu.lt/~rlapinskas/2014-2015/Ekonometrija%203k.%20pratybos/Data%20Ekonometrija.I/weakliem.txt')
countries <- rownames(data.dem)
data.dem <- cbind(countries, data.dem)

#a) The gini coeficient is used to measure the distribution of wealth within a country.
   #If gini is equal to 1 (or 100 in percentage form), then all the wealth(income) is in the hands of a single household.
   #If gini is equal to 0, then all the household have the same wealth (income)

   gini.countries <- data.dem[order(data.dem[, "gini"]), ][, "countries"] %>% as.character()

#b)
mod.w <- lm(secpay ~ gini, data=data.dem)
summary(mod.w)
plot(data.dem$gini, data.dem$secpay)
abline(mod.w, col="red", lwd=2)

#c)
mod.inter <- lm(secpay ~ gini +  dem + dem*gini, data=data.dem)
abline(mod.inter)
mod.democracy <- lm(secpay ~ (gini) +dem, data=data.dem)
summary(mod.democracy)
abline(mod.democracy, col="blue")

#d)
nondemo=with(data.dem,data.dem[dem==0,])
mod.n = lm(secpay~gini,data=nondemo)
abline(mod.n)
summary(mod.n)

mod.n$res %>% plot()
mod.n$res %>% shapiro.test() 
qqPlot(mod.n, pch=15) 
cook <- mod.n %>% cooks.distance()
cook %>% plot()
#we can conclude that the estimators in mod.n are biased

#e) creating robus models
library(robustbase)
weights.h <- psi.huber(mod.n$res, 2*sd(mod.n$res))
mod.r     <- lm(secpay ~ gini, weights=weights.h, data=nondemo)
summary(mod.r)
##
cook <- mod.n %>% cooks.distance()
cook %>% plot()
abline(h=4/(length(cook)-2-1))
text(x=1:length(cook), cook, labels=1:length(cook), cex=0.83, pos=1)
# possible influantial points 5, 7, 26
mod.e <- lm(secpay ~ gini, data=nondemo[-c(5, 7, 26), ])
summary(mod.e)

plot(nondemo$gini, nondemo$secpay)
abline(mod.n, col="red")
abline(mod.r, col="blue")
abline(mod.e, col="green")

## 4.3 example
library(alr4) 
data(landrent) 
names(landrent)=c("rnt.till","cow.dens","prop.past"
                  ,"lime","rnt.alf") 
head(landrent) 
alf.till=landrent$rnt.alf/landrent$rnt.till # create relative rent 
# to make lime a factor  
rent=data.frame(alf.till,landrent[,-4],lime=factor(landrent[,4])) 
head(rent) 
plot(rent[rent$lime==0,-6])  # lime==0 
plot(rent[rent$lime==1,-6])  # lime==1 
# or 
library(lattice) 
splom(~rent[,-6]|rent$lime)               # or 
splom(~rent[,-6]|rent$lime,pscales=0)     # or 
splom(~rent[,-6]|rent$lime,pscales=0, type = c("g","p", "smooth"))   
# or (for superposition) 
splom(~rent[,-6],pscales=0, type = c("g", "p", "smooth")) # grid,point,smooth 
# or (including lime) 
splom(~rent,pscales=0, type = c("g", "p"))

## we want to create models rnt.alf and alf.till
rent.lm41n <-lm(rnt.alf ~ rnt.till + cow.dens + lime + cow.dens:lime, data=rent)  
summary(rent.lm41n) 

rent.lm12m <- lm(alf.till ~ lime * cow.dens , data=rent) 
summary(rent.lm12m) 

##4.4 exercise 
## identify influantial points of the models 
cooks.lm41 <- rent.lm41n %>% cooks.distance() 
plot(cooks.lm41)
abline(h=4/(length(cook)-2-1), col="blue")
text(x=1:length(cooks.lm41), cooks.lm41, labels=1:length(cooks.lm41), cex=0.83, pos=1)
#point 5 may be influantial

cooks.lm12 <- rent.lm12m %>% cooks.distance() 
plot(cooks.lm12)
abline(h=4/(length(cook)-2-1), col="blue")
text(x=1:length(cooks.lm12), cooks.lm12, labels=1:length(cooks.lm12), cex=0.83, pos=1)
#point 33 may be influantial

## 4.5 exercise 
## Lets say we know that points 5, 36 and 57 are influantial points. We will create various models to 
## diminish their effects 

library(alr4) 
data(landrent) 
names(landrent)=c("rnt.till","cow.dens","prop.past"
                  ,"lime","rnt.alf") 
head(landrent) 
alf.till=landrent$rnt.alf/landrent$rnt.till # create relative rent 
# to make lime a factor  
rent=data.frame(alf.till,landrent[,-4],lime=factor(landrent[,4])) 

rent.lm41n <-lm(rnt.alf ~ rnt.till + cow.dens + lime + cow.dens:lime, data=rent)  
summary(rent.lm41n) 

eliminate.points <- lm(rnt.alf ~ rnt.till + cow.dens + lime + cow.dens:lime, data=rent[-c(5, 36, 57), ])  
summary(eliminate.points)
# we can see that now all the explanatory variables are siginificant
  
weights.h <- psi.huber(rent.lm41n$res, 2*sd(rent.lm41n$res))
mod.huber <- rlm(rnt.alf ~ rnt.till + cow.dens + lime + cow.dens:lime, data=rent)
summary(mod.huber)

## comparing the models:

AIC.h  <- AIC(mod.huber)
AIC.el <- AIC(eliminate.points)
## model with the eliminated points gives a lower akaike
res.el <- eliminate.points %>% stdres()
plot(res.el)
shapiro.test(res.el) ## appear to be normal

res.hub <- mod.huber$residuals %>% as.numeric() %>% scale()
res.hub %>% plot()
shapiro.test(res.hub) ## not so normal. This should be expected because Huber weights do not change the 
                      ## actual residuals.   

## 4.6 exercise
## plot the three fitted lines from exercise 4.2

?anscombe
data <- anscombe

plot(data$x3, data$y3)
mod   <- lm(y3 ~ x3, data=data)
mod.2 <- lm(y3 ~ x3, data=data[-3, ])
mod.h <- rlm(y3 ~ x3, data=data)
summary(mod)
abline(mod, col="red", lwd=2)
abline(mod.2, col="blue", lwd=2)
abline(mod.h, col="brown", lwd=2)

## 4.8 exercise 
library(MASS) 
data <- Duncan

## OLS
mod <- lm(prestige ~ income + education, data=data)
summary(mod)  

## OLS w/out 6 and 16
mod1 <- lm(prestige ~ income + education, data=data[-c(6, 16), ])
summary(mod1)  

## Huber M estimator 

mod.H <- rlm(prestige ~ income + education, data=data)
summary(mod.H)

## bi-square MM estimation

mod.MM <- rlm(prestige ~ income + education, method="MM" ,data=data, psi=psi.bisquare)
summary(mod.MM)

## LTS estimation 



########################################################
## 5th chapter 
## Non parametric regression
data(mtcars)
duom <- mtcars

head(mtcars)  
mtc = with(mtcars, mtcars[order(wt),]) 
with(mtc, 
{ 
  plot(wt,mpg) 
  mod.inv=lm(mpg~I(1/wt)) 
  lines(wt,mod.inv$fit) 
  cat("AIC.mod.inv=",AIC(mod.inv),"\n") 
  mod.poly2=lm(mpg~wt+I(wt^2)) 
  lines(wt,mod.poly2$fit,col=2) 
  cat("AIC.mod.poly2=",AIC(mod.poly2), 
      "\n") 
  mod.poly3=lm(mpg~wt+I(wt^2)+I(wt^3)) 
  lines(wt,mod.poly3$fit,col=3) 
  cat("AIC.mod.poly3=",AIC(mod.poly3), 
      "\n") 
  legend(4.2,32,c("inv","poly2","poly3"), 
         lty=1,col=1:3) 
})

##5.1 exercise
data(ethanol)
duom <- ethanol
head(duom)

poli.6.mod <- lm(NOx ~ poly(E, 6, raw=TRUE), data=duom)
poli.6 <- lm(NOx ~ poly(E, 6, raw=TRUE), data=duom)$coef %>% polynomial()

poli.2.mod <- lm(NOx ~ poly(E, 2, raw=TRUE), data=duom)
poli.2 <- lm(NOx ~ poly(E, 2, raw=TRUE), data=duom)$coef %>% polynomial()

poli.3.mod <- lm(NOx ~ poly(E, 3, raw=TRUE), data=duom)
poli.3 <- lm(NOx ~ poly(E, 3, raw=TRUE), data=duom)$coef %>% polynomial()

poli.4.mod <- lm(NOx ~ poly(E, 4, raw=TRUE), data=duom)
poli.4 <- lm(NOx ~ poly(E, 4, raw=TRUE), data=duom)$coef %>% polynomial()

cat(AIC(poli.6.mod), "\n")
cat(AIC(poli.2.mod), "\n")
cat(AIC(poli.3.mod), "\n")
cat(AIC(poli.4.mod), "\n")

plot(poli.4, xlim=c(0.5, 1.3), ylim=c(0, 4))
lines(poli.3, col="blue")
lines(poli.2, col="red")
lines(poli.6, col="green")
points(duom$E, duom$NOx,)

## 5.2 example

eth=with(ethanol,ethanol[order(E),]) 
with(eth, 
{ 
  mod.nls=nls(NOx~a*exp(-b*(abs(E-c))^d), 
              start=list(a=1,b=1,c=1,d=2)) 
  print(summary(mod.nls)) 
  plot(E,NOx,main="exponential nonlinear 
       regression") 
  lines(E,fitted(mod.nls)) 
  cat("AIC.mod.nls=",AIC(mod.nls), 
      "\n") 
}) 

## 5.2 exercise

library(car)
duom <- (USPop)
head(duom)

## exponential model

mod.nls=nls(population ~ a + b*exp(c/100*year), 
            start=list(a=-1, b=1, c=1), trace=TRUE, data=duom, control=nls.control(maxiter=1000)) 
mod.nls
summary(mod.nls)

plot(duom$year, duom$population)
lines(duom$year, fitted(mod.nls))
#lines(duom$year, -15 + 13.897*10^(-12)*exp(1.552/100*duom$year), col="blue")

## logistic model 

mod.nls.logic=nls(population ~ (a*100)/(1 + exp(b*(year - 1916))), 
            start=list(a=1, b=-1), trace=TRUE, data=duom, control=nls.control(maxiter=1000)) 

mod.nls.logic
summary(mod.nls.logic)

lines(duom$year, fitted(mod.nls.logic))

## The exponential model is better

## 5.3 example. LOESS

library(car) # to reach Prestige 
plot(prestige ~ income, xlab="Average Income", ylab="Prestige", data=Prestige) 
with(Prestige, lines(lowess(income, prestige, f=0.5, iter=0), lwd=2)) 

## 5.3 exercise

data(ethanol)
duom <- ethanol
E   <- duom$E
NOx <- duom$NOx
plot(E, NOx)

eth=with(ethanol,ethanol[order(E),]) 
with(eth, 
{ 
  mod.nls=nls(NOx~a*exp(-b*(abs(E-c))^d), 
              start=list(a=1,b=1,c=1,d=2)) 
  print(summary(mod.nls)) 
  plot(E,NOx,main="exponential nonlinear 
       regression") 
  lines(E,fitted(mod.nls), col="brown", lwd=2) 
  cat("AIC.mod.nls=",AIC(mod.nls), 
      "\n") 
}) 

lines(lowess(E, NOx), lwd=2, col="red")
lines(poli.6, col="blue", lwd=2)

##
## Curse of dimensionality 

library(car) 
library(stats) 
Prest=Prestige[order(Prestige$income),] 
attach(Prest) 
par(mfrow=c(1,2)) 
# univariate regression 
mod.loess1=loess(prestige~income) 
summary(mod.loess1)           # equivalent number of parameters ~ 6 
plot(income,prestige) 
lines(income,mod.loess1$fit) 
lines(income,lm(prestige~poly(income,6))$fit,col=2)

# twovariate regression 
mod.loess2 <- loess(prestige~income+education) 
summary(mod.loess2)  
inc <- seq(min(income), max(income), len=25) 
ed  <- seq(min(education), max(education), len=25) 
newdata <- expand.grid(income=inc, education=ed) 
fit.prestige <- matrix(predict(mod.loess2, newdata), 25, 25) 
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed", 
      xlab="Income", ylab="Education", zlab="Prestige", expand=2/3, 
      shade=0.5) 

## Splines 
## 5.5 example

set.seed(14)                  # set the seed to reproduce this example  
e <- rnorm(200) 
x <- runif(200) 
y <- sin(2*pi*(1-x)^2)+x*e 

sx <- sort(x) 
fx <- sin(2*pi*(1-sx)^2) 
plot(x,y) 
lines(sx,fx,lwd=2) 
lines(supsmu(x,y),lty=2,lwd=2,col=2)            # Friedman's very  fast  variable  span  
                                                # bivariate  smoother 
lines(ksmooth(x,y),lty=3,lwd=2,col=3)           #  kernel-type scatterplot smoother 
lines(smooth.spline(x,y),lty=4,lwd=2,col=4)
lines(loess.smooth(x,y),lty=5,lwd=2,col=5)  

legend(0,2.2,c("perfect", "supsmu", "ksmooth", "smooth.spline", "loess"), 
       lty=1:5,lwd=2,col=1:5, cex=0.6) 

## 5.5 exercise

y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))  
ind <- 1:length(y18) 
xx  <- seq(1, length(y18), len = 201) 
plot(ind, y18)
mod.lm <- lm(y18 ~ ind)
mod.lm %>% abline(col="blue", lty=1, lwd=2)

f <- splinefun(ind, y18)
f(ind) %>% lines(col="red", lty=2, lwd=2)

smooth.spline(ind, y18) %>% lines(col="green", lty=3, lwd=2)
legend("topleft",c("linear", "linear spline", "cubic spline"), 
       lty=1:3,lwd=2,col=c("blue", "red", "green"), cex=0.7) 

spar_0.2 <- smooth.spline(ind, y18, spar=0.2)
lines(spar_0.2, col="coral", lty=4)

## 5.6 exercise

y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))  
ind <- 1:length(y18) 
xx  <- seq(1, length(y18), len = 201) 
plot(ind, y18)

smooth.spline(ind, y18) %>% lines(col=5, lty=5)

for(i in c(2, 4, 8)){
  smooth.spline(ind, y18, nknots=10, df=i) %>% lines(col=i, lty=6, lwd=2)
}

## the smaller the df value is, the more spline represents a linear curve 

plot(ind, y18)
smooth.spline(ind, y18) %>% lines(col=1, lty=1)
index <- 1
for(j in c(4, 12, 36)){
  index <- index + 1 
  smooth.spline(ind, y18, nknots=j, df=8) %>% lines(col=index, lty=index, lwd=2)
}

## as the number of knots increase, the spline tends to better approximate the points

# 5.7 exercise 
# Atidute towards inequality ~ gdp + gini using loess

data <- read.table('http://www.mif.vu.lt/~rlapinskas/2014-2015/Ekonometrija%203k.%20pratybos/Data%20Ekonometrija.I/weakliem.txt')
mod.loess <- loess(secpay ~ gdp + gini , data=data) 
fit.loes  <- mod.loess$fitted 
gini      <- data$gini 
gdp       <- data$gdp  

gini.persp <- seq(min(gini), max(gini), len=length(fit.loes))
gdp.persp  <- seq(min(gdp), max(gdp), len=length(fit.loes))
newdata    <- expand.grid(gdp=gdp.persp, gini=gini.persp) 
fit.persp  <- matrix(predict(mod.loess, newdata), length(fit.loes), length(fit.loes)) 

persp(gini.persp, gdp.persp, fit.persp, theta=45, phi=20, ticktype="detailed", 
      xlab="Gini", ylab="GDP", zlab="Atidute", expand=2/3, 
      shade=0.5) 

## Gam (Generalized additive regression models). 

#5.1 example

library(gam) 
library(car) 
data(Duncan) 
attach(Duncan) 

prestige.gam1 <- gam(prestige~lo(income)+lo(education)) 
summary(prestige.gam1) 

par(mfrow=c(1,2)) 
prestige.gam1 <- gam(prestige~lo(income)+lo(education)) 
inc <- seq(min(income), max(income), len=25) 
ed <- seq(min(education), max(education), len=25) 
newdata <- expand.grid(income=inc, education=ed) 
fit.prestige <- matrix(predict(prestige.gam1, newdata), 25, 25) 
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed", 
      xlab="Income", ylab="Education", zlab="Prestige", expand=2/3, shade=0.5) 

prestige.gam2 <- gam(prestige~lo(income,education))
inc <- seq(min(income), max(income), len=25) 
ed <- seq(min(education), max(education), len=25) 
newdata <- expand.grid(income=inc, education=ed) 
fit.prestige <- matrix(predict(prestige.gam2, newdata), 25, 25) 
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed", 
      xlab="Income", ylab="Education", zlab="Prestige", expand=2/3, shade=0.5) 

## 5.8 exercise 

library(scatterplot3d) 
library(lattice) 
data(ethanol) 
attach(ethanol) 

par(mfrow=c(1,2)) 
ee <- seq(min(E), max(E), len=25) 
cc <- seq(min(C), max(C), len=25) 
newdata <- expand.grid(E=ee, C=cc) 
scatterplot3d(E, C, NOx, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="ethanol 3d", pch=20) 
eth.lm=lm(NOx~poly(E,6)+poly(C,3)) 
fit.lm <- matrix(predict(eth.lm, newdata), 25, 25) 
persp(ee, cc, fit.lm, theta=30, phi=30, ticktype="detailed", xlab="E", ylab="C", zlab="NOx", expand=2/3, shade=0.5) 

eth.poly2 = gam(NOx~poly(cbind(E,C),degree=2,raw=TRUE)) 
eth.gam.poly2s=gam(NOx~poly(E,6)+s(C)) 
eth.gam.poly2=gam(NOx~poly(E,6)+C) 
eth.gam1=gam(NOx~s(E)+s(C)) 
eth.gam2=gam(NOx~lo(E,C)) 

anova(eth.poly2, eth.gam.poly2s, test="F") # these models are statistically different
anova(eth.poly2, eth.gam.poly2, test="F")  # these models are statistically different
anova(eth.poly2, eth.gam1, test="F")       # these models are statistically different
anova(eth.poly2, eth.gam2, test="F")       # ? 

anova(eth.gam.poly2s, eth.gam.poly2, test="F") # these models are not statistically different
anova(eth.gam.poly2s, eth.gam1, test="F")      # these models are statistically different
anova(eth.gam.poly2s, eth.gam2, test="F")      # these models are statistically different

anova(eth.gam.poly2, eth.gam1, test="F")     # ?
anova(eth.gam.poly2, eth.poly2, test="F")    # these models are statistically different

anova(eth.gam1, eth.gam2, test="F")          # these models are statistically different
 
## Ploting the graphs

fit.poly2 <- matrix(predict(eth.poly2, newdata), 25, 25)
persp(ee, cc, fit.poly2, theta=30, phi=30, ticktype="detailed", xlab="E", ylab="C", zlab="NOx", expand=2/3, shade=0.5) 

fit.gam.poly2s <- matrix(predict(eth.gam.poly2s, newdata), 25, 25)
persp(ee, cc, fit.gam.poly2s, theta=30, phi=30, ticktype="detailed", xlab="E", ylab="C", zlab="NOx", expand=2/3, shade=0.5) 

## 6 chapter 
## Generalized methods of moments 

g <- function(tet,x) 
{ 
  g1 = x-tet[1] 
  g2 = x^2 - tet[1]^2 - tet[2]^2 
  g3 = x^3 - tet[1]^3 - 3*tet[1]*tet[2]^2 
  g4 = x^4 - tet[1]^4 - 6*tet[1]^2*tet[2]^2 - 3*tet[2]^4 
  f <- cbind(g1,g2,g3,g4) 
  return(f) 
} 

Dg <- function(tet,x) 
{ 
  G <- matrix(c( -1,-2*tet[1],-3*tet[1]^2 ,-4*tet[1]^3-12*tet[1]*tet[2]^2, 0,
                 -2*tet[2],-6*tet[1]*tet[2],-12*tet[1]^2*tet[2]-12*tet[2]^3), 
              nrow=4,ncol=2) 
  return(G) 
} 

set.seed(1) 
n=150 
x1=rnorm(n,mean=5,sd=2) 

library(gmm) 
summary(gmm(g,x1,t0 = c(mu = 0, sig = 6), grad = Dg)) 

summary(gmm(g,x1,t0 = c(mu = 0, sig = 0), grad = Dg, type = "iter")) 

## 6.1 exercise 

n=150
sample <- rchisq(n, 3, ncp = 0)
sample %>% hist()
sigma <- sample %>% var()
mu    <- sample %>% mean() 

mod.gmm <- gmm(g, SP500, t0 =c(mu = 0, sig =  6), grad = Dg)
summary(mod.gmm) # we reject the hypothesis that 6.1 holds because of J - test 
                 # thus sample is not normal 

## 6.2 exercise
library(MASS)
data(SP500)
SP500

g.SP <- function(tet,x) 
{ 
  g1 = x - tet[1] 
  g2 = (x - tet[1])^2 - (tet[2]^2)*(tet[3]/(tet[3] - 2))
  g3 = (x - tet[1])^3
  g4 = (x - tet[1])^4 - (3*tet[3]^2*tet[2]^4)/((tet[3] - 2)*(tet[3] - 4))
  f <- cbind(g1,g2,g3,g4) 
  return(f) 
} 

Dg <- function(tet,x) 
{ 
  G <- matrix(c( -1,-2*tet[1],-3*tet[1]^2 ,-4*tet[1]^3-12*tet[1]*tet[2]^2, 0,
                 -2*tet[2],-6*tet[1]*tet[2],-12*tet[1]^2*tet[2]-12*tet[2]^3), 
              nrow=4,ncol=2) 
  return(G) 
} 

mod.gmm <- gmm(g.SP, SP500, t0 =c(mu = 0, sig =  1, v = 1), type="iter")
summary(mod.gmm)

## 6.3 exercise. Poisson distribution 

x=0:30 
size=7 
prob=0.4 
dnb=dnbinom(x, size, prob) 
plot(x,dnb,ylim=c(0,0.13),pch=15) 
(MEAN=size*(1-prob)/prob) 
lines(x,dpois(x,lambda=MEAN), type="h",lwd=2,col=2) 
legend(20,0.12,c("Neg.binom", "Poisson"),lty=0:1, pch=c(15,-1),col=1:2) 

rnb <- rnbinom(0:200, size, prob)

g.p <- function(teta, x){
  g1 <- teta[1]*(1 - teta[2])/teta[2]
  g2 <- teta[1]*(1 - teta[2])/(teta[2]^2)
  g3 <- (teta[1]*(teta[2] - 1)*(teta[2] - 2))/(teta[2]^3)
  g4 <- (3*(1 - teta[2])*(6 - 6*teta[2] + teta[2]^2 + 3*teta[1] - 3*teta[1]*teta[2]))/(teta[2]^4)
  f  <- cbind(g1, g2, g3, g4)
  return(f)
}

#Dg.p <- function(size, prob, x){
 # col1 <- c((1 - prob)/prob, (1 - prob)/(prob^2))  
 # col2 <- c((size/(prob^2) - size/prob), -2*size/(prob^3) + size/(prob^2))
 # Dg   <- matrix(ncol=2, nrow=2) 
 #  Dg[, 1] <- col1
 #  Dg[, 2] <- col2
 #  return(Dg)
#}

mod <- gmm(g.p, rnb, t0 = c(s = 7, p = 0.4), type="iter")

## 6.5 example

library(Ecdat) 
data(Mroz) 
head(Mroz) 
Mro=Mroz[Mroz$wagew!=0,]      # keep only working women 
mroz=with(Mro,data.frame(lwage=log(wagew),educw,educwm,educwf,expw=experience))
# keep only selected variables 
head(mroz) 
summary(lm(lwage~educw+expw+I(expw^2),data=mroz))    

library(AER) 
mroz.2SLS = ivreg(lwage ~ expw + I(expw^2) + educw | expw + I(expw^2) + educwm + educwf, data = mroz) 
summary(mroz.2SLS)   

mroz.gmm=gmm(lwage~expw+I(expw^2)+educw,~expw+I(expw^2)+educwm+educwf,data=mroz) 
summary(mroz.gmm)   

## 6.4 exercise

#i) 

N=200 
set.seed(123) 
eps=rnorm(N) 
X=runif(N,1,7) 
Y=numeric(N) 
Y[1]=0 
for(i in 2:N) Y[i]=2+0.5*X[i]+0.8*Y[i-1]+eps[i] 
y=ts(Y) 
library(dynlm) 
mod.ols=dynlm(y~X+L(y)) 
summary(mod.ols) 

#ii) 

arima(y[2:N],c(0,0,0),xreg=cbind(X[2:N],y[1:(N-1)]))  # c(0,0,0) for WN 

#iii)

set.seed(321) 
eps = arima.sim(N,model=list(ma=.6)) 
Y = numeric(N) 
Y[1]=0 
for(i in 2:N) Y[i]=2+0.5*X[i]+0.8*Y[i-1]+eps[i] 
y=ts(Y); plot(y) 
arima(y[2:N],c(0,0,1),xreg=cbind(X[2:N],y[1:(N-1)])) 

mod <- dynlm(L(Y) ~ eps)
summary(mod)

# iv) 

library(AER) 
mod.2SLS = ivreg(Y[5:N] ~ X[5:N] + Y[4:(N-1)] | X[5:N] +   Y[3:(N-2)] + Y[2:(N-3)] + Y[1:(N-4)]) 
summary(mod.2SLS) 

# v) Create a relevant gmm model for iii) case

# Two-stage-least-squares (2SLS), or IV with iid errors. 
# The model is: 
# Y(t) = b[0] + b[1]C(t) + b[2]Y(t-1) + e(t) 
# e(t) is an MA(1) 
# The instruments are Z(t)={1 C(t) y(t-2) y(t-3) y(t-4)} 

getdat <- function(n) { 
  e <- arima.sim(n,model=list(ma=.9)) 
  C <- runif(n,0,5) 
  Y <- rep(0,n) 
  Y[1] = 1 + 2*C[1] + e[1] 
  for (i in 2:n){ 
    Y[i] = 1 + 2*C[i] + 0.9*Y[i-1] + e[i] 
  } 
  Yt <- Y[5:n] 
  X <- cbind(1,C[5:n],Y[4:(n-1)]) 
  Z <- cbind(1,C[5:n],Y[3:(n-2)],Y[2:(n-3)],Y[1:(n-4)])  
  return(list(Y=Yt,X=X,Z=Z)) 
} 
d <- getdat(5000) 
res4 <- gmm(d$Y~d$X-1,~d$Z-1, vcov="iid"); 
summary(res4) 

#6.6 example. Smokers

library(AER) 
data("CigarettesSW") 
CigarettesSW$rprice <- with(CigarettesSW, price/cpi) 
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi) 
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi) 
c1995 <- subset(CigarettesSW, year == "1995") 
head(c1995) 

fm_s1 <- lm(log(rprice) ~ tdiff, data = c1995) 
summary(fm_s1) 

fm_s2 <- lm(log(packs) ~ fitted(fm_s1), data = c1995) 
summary(fm_s2) 

hc1 <- function(x) vcovHC(x, type = "HC1") 
fm_ivreg1 <- ivreg(log(packs) ~ log(rprice) | tdiff, data = c1995) 
coeftest(fm_ivreg1, vcov = hc1) 

fm_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff, data = c1995) 
coeftest(fm_ivreg2, vcov = hc1) 

fm_ivreg3 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax/cpi), data = c1995) 
coeftest(fm_ivreg3, vcov = hc1) 

res <- tsls(log(packs)~log(rprice) + log(rincome),~ log(rincome) +  tdiff + I(tax/cpi), data = c1995) 
summary(res) 

## exercise: create a similar model like fm_ivreg3 using gmm 

fm_gmm <- gmm(log(packs) ~ log(rprice) + log(rincome), ~log(rincome) + tdiff + I(tax/cpi), data = c1995)
summary(fm_gmm)

# The method of moments solves the system of equations, where the theoretical 4 moments of the 
# distribution are equated to zero. The theoretical means and variances are changed to the sample 
# means and variances and the gmm(mu, sigma) is beeing minimized with respect to the two parameters.



