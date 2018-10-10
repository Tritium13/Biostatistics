# Statistics
# http://www.bio.ic.ac.uk/research/crawley/statistics/exercises/R3Statistics.pdf

# Mode
rm(x)
distribution <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/mode.txt", header = T)
attach(distribution)
names(distribution)

par(mfrow = c(1,2))
barplot(fx, names = as.character(x))
barplot(fy, names = as.character(x))

# Median
y <- rep(x, fx)
y <- sort(y)
length(y)
ceiling(length(y)/2)
y[17]
median(y)

# Arithmetic mean
sum(y)/length(y)
mean(y)

# Geometric mean
prod(y)^(1/length(y))
meanlogy <- sum(log(y))/length(y)
meanlogy
exp(meanlogy)

geometric <- function(x) exp(sum(log(x))/length(x))
geometric(y)

aphid <- c(10,1,1,10,1000)
mean(aphid)
geometric(aphid)

# Harmonic mean
mean(c(1,2,4,1))
4/sum(1/c(1,2,4,1))

harmonic <- function(x) 1/mean(1/x)
harmonic(c(1,2,4,1))
harmonic(y)
mode(y)

par(mfrow=c(1,1))
barplot(fz, names = as.character(x))

w <- rep(x,fz)

# Measuring variation
y <- c(13,7,5,12,9,15,6,11,9,7,12)
plot(y, ylim = c(0,20))
range(y)
abline(mean(y),0)

# Degrees of freedom
# Variance
A <- c(3,4,4,3,2,3,1,3,5,2)
B <- c(5,5,6,7,4,4,3,5,6,5)
C <- c(3,3,2,1,10,4,3,11,3,10)
mean(A)
mean(B)
mean(C)

dA <- A-3
dB <- B-5
dC <- C-5

SSA <- sum(dA^2)
SSB <- sum(dB^2)
SSC <- sum(dC^2)

s2A <- SSA/9
s2B <- SSB/9
s2C <- SSC/9

s2A;s2B;s2C
s2A <- var(A)
var(A)
s2C/s2A
s2C/s2B

# Using variance
sqrt(s2A/10)
sqrt(s2B/10)
sqrt(s2C/10)

qt(.025,9)
qt(.975,9)
qt(.995,9)
qt(.9975,9)

qt(.975,9)*sqrt(s2B/10)

# Quantiles
z <- rnorm(1000)
mean(z)
quantile(z,c(.025,.975))

z <- rnorm(10000)
quantile(z,c(.025,.975))

# Robust estimators
y <- c(3,4,6,4,5,2,4,5,1,5,4,6)
mad(y)
sd(y)

y1 <- c(y,100)
mean(y1)
sqrt(var(y1))
mad(y1)

outlier <- function(x){
  if(sqrt(var(x)) > 4*mad(x)) print("Outliers present")
  else print("Deviation reasonable")}

outlier(y)
outlier(y1)

# Single-sample estimation
light <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/light.txt", header = T)
attach(light)
names(light)
hist(speed)
summary(speed)

# Inference in the 1-sample case
library(ctest)
wilcox.test(speed, mu = 990)
t.test(speed, mu = 990)

# Comparing two means
qt(.975, 18)
(mean(A)-mean(B))/sqrt(s2A/10 + s2B/10)
t.test(A,B)

# Wilcoxon rank sum test
par(mfrow = c(1,2))
hist(B, breaks = c(0.5:11.5))
hist(C, breaks = c(0.5:11.5))

combined <- c(B,C)
combined
sample <- c(rep("B",10), rep("C",10))
sample
rank.combi <- rank(combined)
rank.combi
sum(rank.combi[sample == "B"])
sum(rank.combi[sample == "C"])
tapply(rank.combi, sample, sum)
wilcox.test(B,C)

# Tests on paired samples
x <- c(20,15,10,5,20,15,10,5,20,15,10,5,20,15,10,5)
y <- c(23,16,10,4,22,15,12,7,21,16,11,5,22,14,10,6)
t.test(x,y)
t.test(x,y,paired=T)

# Central Limit Theorem
par(mfrow = c(1,2))
y <- rnbinom(1000,1,.2)
mean(y)
var(y)
table(y)
hist(y, breaks = -0.5:38.5)
my <- numeric(1000)
for (i in 1:1000){
  y <- rnbinom(30,1,0.2)
  my[i] <- mean(y)}
hist(my)

par(mfrow = c(1,1))
