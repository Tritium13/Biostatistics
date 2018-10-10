# Analysing proportion data: binomial errors

y <- c(3,4,5,8)
N <- c(6,10,9,12)
N*20/37

p1 <- 2*(3*log(3/3.243)+3*log(3/(6-3.243))+4*log(4/5.405)+6*log(6/(10-5.405)))
p2 <- 2*(5*log(5/4.865)+4*log(4/(9-4.865))+8*log(8/6.486)+4*log(4/(12-6.486)))
p1+p2

rv <- cbind(y,N-y)
glm(rv~1, family = binomial)

p <- c(7/16,7/16,13/21,13/21)
p*N

p1 <- 2*(3*log(3/2.625)+3*log(3/(6-2.625))+4*log(4/4.375)+6*log(6/(10-4.375)))
p2 <- 2*(5*log(5/5.571)+4*log(4/(9-5.571))+8*log(8/7.429)+4*log(4/(12-7.429)))
p1+p2

treatment <- factor(c("A","A","B","B"))
glm(rv~treatment, family = binomial)

# Regression analysis with proportion data
taken <- c(3,3,6,16,26,39)
size <- c(4,6,10,40,60,100)

p <- taken/size
ls <- log(size)
plot(ls,p)
abline(lm(p~ls))

summary(lm(p~ls))

logit <- log(p/(1-p))
summary(lm(logit~ls))

left <- size - taken
y <- cbind(taken,left)
model <- glm(y~ls, binomial)
summary(model)

model2 <- glm(y~1,binomial)
anova(model, model2)
anova(model, model2,test = "Chi")

# Analysis of deviance with proportion data
germination <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/germination.txt", header = T)
attach(germination)
names(germination)

y <- cbind(count, sample-count)
levels(Orobanche)
levels(extract)
model <- glm(y~Orobanche*extract,binomial)
summary(model)

33.279/17
model <- glm(y~Orobanche*extract,quasibinomial)
model2 <- update(model,~.-Orobanche:extract)
anova(model, model2, test = "F")
anova(model2, test = "F")

model3 <- update(model2,~.-Orobanche)
anova(model2,model3, test = "F")
coef(model3)

1/(1+(exp(0.5122)))
1/(1+1/(exp(-0.5122+1.0574)))

tapply(predict(model3), extract, mean)
tapply(1/(1+1/exp(predict(model3))), extract, mean)
tapply(predict(model3, type = "response"), extract, mean)

p <- count/sample
tapply(p, extract, mean)

tapply(count, extract, sum)
tapply(sample, extract, sum)

ct <- as.vector(tapply(count, extract, sum))
sa <- as.vector(tapply(sample, extract, sum))
ct/sa

# Analysis of covariance with proportion data
logistic <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/logistic.txt", header = T)
attach(logistic)
names(logistic)
p <- dead/n
plot(logdose, p, type = "n")

points(logdose[product == "A"], p[product == "A"], pch = 16, col = 2)
points(logdose[product == "B"], p[product == "B"], pch = 16, col = 4)
points(logdose[product == "C"], p[product == "C"], pch = 16, col = 1)

y <- cbind(dead, n-dead)
model <- glm(y~product*logdose, binomial)
summary(model)

newfac <- factor(1+(product == "B"))
model2 <- update(model,~.-product*logdose + newfac*logdose)
anova(model, model2, test = "Chi")
summary(model2)

max(logdose)
min(logdose)

xv <- seq(0.1,1.7,0.01)
length(xv)
nf <- factor(c(rep(1,161), rep(2,161)))
xv <- c(xv,xv)
yv <- predict(model2, type = "response", data.frame(logdose=xv, newfac=nf))

yvc <- split(yv,nf)
xvc <- split(xv,nf)

lines(xvc[[1]],yvc[[1]])
lines(xvc[[2]],yvc[[2]])

# Binary response variable with both continuous and categorical explanatory variables: Logistic Ancova
parasite <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/parasite.txt", header = T)
attach(parasite)
names(parasite)

par(mfrow = c(1,2))
plot(infection, weight)
plot(infection, age)

table(infection, sex)
model <- glm(infection~age*weight*sex, family = binomial)
summary(model)
step(model)

model2 <- update(model,~.-age:weight:sex)
anova(model, model2, test = "Chi")[,-1]

model3 <- update(model2,~.-age:weight)
anova(model2, model3, test = "Chi")[,-1]

model4 <- update(model2,~.-age:sex)
anova(model2, model4, test = "Chi")[,-1]

model5 <- update(model2,~.-weight:sex)
anova(model2, model5, test = "Chi")[,-1]

model6 <- update(model2,~.-weight:sex-weight:age-sex:age)
model7 <- update(model6,~.+weight:age)
anova(model7, model6, test = "Chi")[,-1]

model8 <- update(model6,~.-weight)
anova(model6, model8, test = "Chi")[,-1]

model9 <- update(model6,~.-sex)
anova(model6, model9, test = "Chi")[,-1]

model10 <- update(model6,~. -age)
anova(model6,model10,test="Chi")[,-1]

model11 <-  glm(infection~sex+age+weight+I(age^2)+I(weight^2),family=binomial)

model12 <- glm(infection~sex+age+weight+I(age^2),family=binomial)
anova(model11,model12,test="Chi")[,-1]

model13 <- glm(infection~sex+age+weight+(weight^2),family=binomial)
anova(model11,model13,test="Chi")[,-1]

library(mgcv)
gam1 <- gam(infection~sex+s(age)+s(weight), family = binomial)
plot.gam(gam1)
model14 <- glm(infection~sex+age+I(age^2)+I((weight- 12)*(weight>12)),family=binomial)
model15 <- update(model14,~.-sex)
anova(model14,model15,test="Chi")

summary(model15)

# A 3x3 crossover trial
rm(y)
crossover <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/crossover.txt", header = T)
attach(crossover)
names(crossover)
table(drug)
tapply(y, drug, sum)
glm(c(22,61,69)~1, family = poisson)
table(drug, order)
tapply(y, list(drug, order), sum)
order <- factor(order)
model <- glm(y~drug*order + time, family = binomial)
summary(model)

model2 <- update(model,~. - drug:order)
anova(model,model2,test="Chi")

model3 <- update(model2,~. - time)
anova(model2,model3,test="Chi")

model4 <- update(model3,~. - order)
anova(model3,model4,test="Chi")
summary(model4)

relief <- c(22,61,69)
patients <- c(86,86,86)
treat <- factor(c("A","B","C"))

model <- glm(cbind(relief, patients-relief)~treat, binomial)
summary(model)

t2 <- factor(1+(treat!="A"))
model2 <- glm(cbind(relief,patients-relief)~t2,binomial)
anova(model, model2, test = "Chi")
