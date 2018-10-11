##Bootstrap
#A method of significance testing by testing sample statistics of random sub-samples

library(datasets)

#Correlation Boot-strap
dat <- swiss
attach(dat)

#direct correlation
cor(Infant.Mortality,Fertility)

#length of index
n <- length(Infant.Mortality)

#empty vector for sample statistics
boot.r.all <- NULL

#bootstrap
for (i in 1:1000){
  index <- sample(1:n,replace=T)
  boot.mortality <- Infant.Mortality[index]
  boot.fertility <- Fertility[index]
  boot.r <- cor(boot.mortality,boot.fertility)
  boot.r.all <- c(boot.r.all,boot.r)
}

quantile(boot.r.all,prob=c(0.025,0.975))
hist(boot.r.all,breaks=40)

#Regression Boot-Strap
grass <- CO2
attach(grass)

#direct regression
lm(uptake~conc)
summary(lm(uptake~conc))

#Boot-strap for coefficients
n <- length(conc)
boot.b1.all <- NULL

for(i in 1:1000){
  index <- sample(1:n,replace=T)
  boot.con <- conc[index]
  boot.upt <- uptake[index]
  boot.model <- lm(boot.upt~boot.con)
  boot.b1 <- boot.model$coefficients[2] ##for intercept replace with [1]
  boot.b1.all <- c(boot.b1.all,boot.b1)
}

hist(boot.b1.all,breaks=40)

#Boot-strap for effect size
n <- length(conc)
boot.r2.all <- NULL

for(i in 1:1000){
  index <- sample(1:n,replace=T)
  boot.con <- conc[index]
  boot.upt <- uptake[index]
  boot.model <- lm(boot.upt~boot.con)
  boot.r2 <- summary(boot.model)$r.squared ##for intercept replace with [1]
  boot.r2.all <- c(boot.b1.all,boot.b1)
}

hist(boot.r2.all,breaks=40)