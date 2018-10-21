# T-test - hypothesis - problem an t/p-value check
# Laptop (foil 15 - t-distribution applications)

battery <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/laptop.txt", header = T)
attach(battery)
names(battery)

battery <- c(3.5, 3.3, 2.8, 2.85, 3.45, 3.5, 2.95, 3.65, 3.45, 3.05)

mean(battery)
sd(battery)

t.test(battery, y = NULL, mu = 3.5)

# Drugtest (foil 19 - t-distribution applications)
drug <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/drugtest.txt", header = T)
attach(drug)
names(drug)

mean(before); sd(before); var(before)
mean(after); sd(after); var(after)

t.test(before, after) # two-sided
t.test(before, y = NULL, mu = 231.4) # one-sided
t.test(after, y = NULL, mu = 237.3) # one-sided



# OUTPUT
View(battery)
> battery <- c(3.5, 3.3, 2.8, 2.85, 3.45, 3.5, 2.95, 3.65, 3.45, 3.05)
> mean(battery)
[1] 3.25
> sd(battery)
[1] 0.3091206
> t.test(battery, y = NULL, mu = 3.5)

	One Sample t-test

data:  battery
t = -2.5575, df = 9, p-value = 0.03081
alternative hypothesis: true mean is not equal to 3.5
95 percent confidence interval:
 3.028868 3.471132
sample estimates:
mean of x 
     3.25 

> # Drugtest (foil 19 - t-distribution applications)
> drug <- read.table("file:///Users/Tritium/Dropbox/Uni/Biostatistik/exercisedata/drugtest.txt", header = T)
> attach(drug)
> names(drug)
[1] "before" "after" 
> mean(before); sd(before); var(before)
[1] 237.3
[1] 30.21791
[1] 913.1222
> mean(after); sd(after); var(after)
[1] 231.4
[1] 39.22641
[1] 1538.711
> t.test(before, after) # two-sided

	Welch Two Sample t-test

data:  before and after
t = 0.3768, df = 16.9, p-value = 0.711
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -27.15112  38.95112
sample estimates:
mean of x mean of y 
    237.3     231.4 

> t.test(before, y = NULL, mu = 231.4) # one-sided

	One Sample t-test

data:  before
t = 0.61743, df = 9, p-value = 0.5523
alternative hypothesis: true mean is not equal to 231.4
95 percent confidence interval:
 215.6834 258.9166
sample estimates:
mean of x 
    237.3 

> t.test(after, y = NULL, mu = 237.3) # one-sided

	One Sample t-test

data:  after
t = -0.47563, df = 9, p-value = 0.6457
alternative hypothesis: true mean is not equal to 237.3
95 percent confidence interval:
 203.3391 259.4609
sample estimates:
mean of x 
    231.4 

> 
