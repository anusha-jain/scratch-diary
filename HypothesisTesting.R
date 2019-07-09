#T-testing
#t.test(x,y,default wo-tailed or greater or lesser)

library(datasets)

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
detach(grass)

#Two-Away ANOVA
mod <- lm(uptake~factor(Type)*factor(Treatment)+factor(Type)+factor(Treatment))
anova(mod)
cellmeans <- tapply(uptake,list(Type,Treatment),mean,na.rm=T)
barplot(cellmeans,beside=T,col=c("red","purple"),names.arg=c("Nonchilled","Chilled"),legend=c("Quebec","Mississippi"))


chick<- ChickWeight
head(chick)
summary(chick)
attach(chick)
model2 <- lm(weight~factor(Diet))
anova(model2)

#Post-hoc testing
#pairwise.t.test(outcome,predictor,adjustment method)
pairwise.t.test(weight,Diet,adj="bonf")
detach(chick)

#Regression Model
#y = a +bx
#a= graphical intercept
#b= slope (unit change in y caused by unit change in x)
grass <- CO2
summary(grass)
attach(grass)

#linear model(y~x)
model3 <- lm(uptake~conc)
summary(model3)

model$coefficients
summary(model3)$r.squared
summary(model3)$p.value ##??

#alternative model
x <- conc
y <- uptake
sxy <- cov(x,y)
sx <- var(x)

b<- sxy/sx
a<- mean(y) - b*mean(x)

intrcpts <- cbind(a,b)
intrcpts

#alternative model 2
corxy <- cor(x,y)
sy <- sd(y)
sx <- sd(x)

bb <- corxy*sy/sx
aa <- mean(y) - b*mean(x)

intrcptss <- cbind(aa,bb)
intrcptss

##Significance Testing
#finding p value from z
#pnorm(z score)
pnorm(1)

#finding z score from p
#qnorm(p value)
qnorm(0.8413)

#finding p value from t
#pt(t score, df)
pt(1,20)

#finding t score from p
#qt(p value, df)
qt(0.8354,20)

#finding p value from F
#pf(F score, df1, df2)
pf(2,20,24)

#finding confidence interval
#qnorm(confidence levels, mean, se)
qnorm(c(0.025,0.975),mean=1,sd=0.1)

#finding cofidence interval of regression coefficients
#confint(linear model,level)
confint(model3,level=0.99)
#assumptions - 
#1 - residues are normally distributed
#2 - the sample is large enough