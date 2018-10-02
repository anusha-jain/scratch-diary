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

#ANOVA
#anova(linearmodel())
grass <- CO2
summary(grass)
attach(grass)

model <- lm(uptake~factor(Type))
anova(model)

#Two-Away ANOVA
mod <- lm(uptake~factor(Type)*factor(Treatment)+factor(Type)+factor(Treatment))
anova(mod)
cellmeans <- tapply(uptake,list(Type,Treatment),mean,na.rm=T)
barplot(cellmeans,beside=T,col=c("red","purple"),names.arg=c("Nonchilled","Chilled"),legend=c("Quebec","Mississippi"))

#Regression Model
