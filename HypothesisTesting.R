#T-testing
#t.test(x,y,default wo-tailed or greater or lesser)

library("datasets")

dat <- swiss
head(dat)
attach(dat)

meanIM <- mean(Infant.Mortality,na.rm=T)
meanF <- mean(Fertility,na.rm=T)

test <- t.test(Infant.Mortality,Fertility)
test$p.value

#Cohen's D
#the effect size independent of the sample size
n1 <- length(Infant.Mortality) - sum(is.na(Infant.Mortality))
n2 <- length(Fertility) - sum(is.na(Fertility))

meandiff <- meanIM-meanF

v1 <- var(Infant.Mortality,na.rm=T)
v2 <- var(Fertility,na.rm=T)

snum<- (n1-1)*v1 + (n2+1)*v2
sdenom<- (n1+n2-2)

s = sqrt(snum/sdenom)

cohend <- meandiff/s
##Effect Size
#0.01 - small
#0.06 - medium
#>0.14 - large

#ANOVA
#anova(linearmodel())
grass <- CO2
summary(grass)
attach(grass)

model1 <- lm(uptake~factor(Type))
anova(model)

chick<- ChickWeight
head(chick)
summary(chick)
attach(chick)
model2 <- lm(weight~factor(Diet))
anova(model2)

#Post-hoc testing
#pairwise.t.test(outcome,predictor,adjustment method)
pairwise.t.test(weight,Diet,adj="bonf")

#Two-Away ANOVA
mod <- lm(uptake~factor(Type)*factor(Treatment)+factor(Type)+factor(Treatment))
anova(mod)
cellmeans <- tapply(uptake,list(Type,Treatment),mean,na.rm=T)
barplot(cellmeans,beside=T,col=c("red","purple"),names.arg=c("Nonchilled","Chilled"),legend=c("Quebec","Mississippi"))

#Regression Model

