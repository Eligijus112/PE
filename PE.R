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


#1.17 (work in progress)

n <- 1000 #number of ships
arrive.time <- numeric()
unload.time <- numeric()
laiveliai   <- matrix(ncol=2, nrow=n) %>% as.data.frame
for(i in 1:n){
arrive.time[i] <- runif(1, min=15, max=145) %>% round(digits=0)
unload.time[i] <- runif(1, min=45, max=90 ) %>% round(digits=0)
laiveliai[i, ] <- c(arrive.time[i], unload.time[i])
}
laiveliai[1,1] <- 0    #starting time
colnames(laiveliai)  <- c("Arrive", "Unload")
row.names(laiveliai) <- paste0("laivas_", 1:n)

##uniform laiveliu laukimo laikas
waiting.time <- numeric()
for(i in 2:n){
  if(laiveliai[i-1, 2]<laiveliai[i, 1]) waiting.time[i] <- 0 
  else waiting.time[i] <- laiveliai[i-1, 2] - laiveliai[i, 1] 
  cat(rownames(laiveliai)[i], "lauke", waiting.time[i], "min \n")
}
mean(laiveliai[, 1]) # vidutinis laivo uzsibuvimas uoste
max(laiveliai[, 1])  # maksimalus laivo laikas uoste

mean(waiting.time[1:n], na.rm=T) # vidutinis laukimo laikas
max(waiting.time, na.rm=T)       # maksimalus laukimo laikas

idle.time  <- laiveliai[2:n,1] - laiveliai[1:(n-1), 2] # teigiamos koordinates rodo, kiek laiko uoste nebuvo laivo tarp iskrovimu
sum.ships  <- 0
for(j in 1:length(idle.time)){
  if(idle.time[j]>0) sum.ships <- sum(sum.ships, idle.time[j])
}

total.time <- laiveliai[, 1] %>% abs %>% sum

perct.idle <- sum.ships*100/total.time

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
