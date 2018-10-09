# Scatterplot
x <- 1:10
y <- c(11,12,9,7,5,8,4,4,5,3)
plot(x,y,pch=2,xlab="Explanatory variable",ylab="Response variable")
abline(lm(y~x))
lines(c(0,10),c(12,0),lty=2)
v <- c(2,4,6,8,10)
w <- c(8,5,6,6,2)
points(v,w,pch=3)
abline(lm(w~v),lty=3)

# Boxplot
sex <- c("male","female")
names(weather.data)
attach(weather.data)
month <- factor(month)
is.factor(month)
plot(month,upper)

# Colour in R
pie(rep(1,30),col=rainbow(30),radius=0.9)
pie(rep(1,10),col=rainbow(10),radius=0.5)

# Colour with graphs
x <- seq(0,10,0.1)
y1 <- 2 + 3*x - 0.25*x^2
y2 <- 3 + 3.3*x - 0.3*x^2
par(bg="ghostwhite")
plot(x,y2,type="n",ylab="")
lines(x,y2,col="red")
lines(x,y1,col="blue")

# Coloured Scatterplots
attach(jantemps)
names(jantemps)
max(tmax)
min(tmin)
plot(day, tmax, ylim = c(-12,12), type="n", ylab="Temperatur")
points(day, tmax, col="red", pch=16)
points(day, tmin, col="blue", pch=16)
for (i in 1:31) lines(c(i,i), c(tmin[i], tmax[i]), col="green")

# Colour with histograms
x <- rnorm(1000)
par(bg="cornsilk")
hist(x, col="lavender", main="")

#Colour with pie charts
fate <- c(0.12,0.3,0.26,0.16,0.04,0.12)
names(fate) <- c("Ragwort","Thistle","Willowherb","Rush","Orchid","Knapweed")
pie(fate, col = c("purple","violetred1","green3","cornsilk","cyan","white"))

# Multivariate plots
pollution <- read.table("file:///Users/Tritium/Downloads/zipped/pollute.txt",header = T)
attach(pollution)
names(pollution)
pairs(pollution, panel=panel.smooth)

#Tree-based models
library(tree)
regtree <- tree(Pollution~.,data=pollution)
plot(regtree)
text(regtree)

#Conditioning plots
attach(pollution)
coplot(Pollution~Temp|Rain)

#Graphical parameters
par(mfrow = c(1,1))
par(mfrow = c(1,2))
par(mfrow = c(2,2))

#Logarithmic axes
plotdata <- read.table("file:///Users/Tritium/Downloads/zipped/plotdata.txt",header = T)
attach(plotdata)
names(plotdata)
par(mfrow = c(2,2))
plot(x, y, type="l")
plot(x, y,log="xy", type="l")
plot(x, y,log="y", type="l")
plot(x, y,log="x", type="l")

#Scaling the axes
par(mfrow = c(1,2))
plot(x, y, type="l")
plot(x, y, ylim = c(0,50), type="l")

#Text on graphs
text(0.8,45,"(b)")
map.places <- read.csv("file:///Users/Tritium/Downloads/zipped/map.places.csv", header=T)
attach(map.places)
names(map.places)
map.data <- read.csv("file:///Users/Tritium/Downloads/zipped/bowens.csv",header=T)
attach(map.data)
names(map.data)
nn <- ifelse(north < 60, north + 100, north)
par(mfrow = c(1,1))
plot(c(20,100),c(60,110), type = "n", xlab = "", ylab = "")
for (i in 1:length(X...wanted)){
  ii <- which(place == as.character(X...wanted[i]))
  text(east[ii], nn[ii], as.character(place[ii]),cex = 0.6)}

#Character alignment
labels <- letters[1:10]
labels
plot(1:10, 1:10, type="n")
text(1:10,1:10, labels, cex = 2)
plot(1:10, 1:10, type="n")
text(1:10,1:10, labels, cex = 2,srt=180)
