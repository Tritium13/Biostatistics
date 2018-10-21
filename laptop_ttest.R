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
