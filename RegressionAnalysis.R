#####Variable Selection
#Finding the simplest model with the strongest relationship
#Criteria for comparison - 
#R^2 (adjusted and unadjusted) - maximum
#Mallow's Cp - minimum
#AIC and BIC - minimum
#Residual Sum of Squares - minimum

library(leaps) ##package for analysis
library(MASS)  ##package for dataset Birthwt

head(birthwt)

#Compare criteria of models, excluding AIC
#regsubsets(DV~IV1+IV2+..+IVk, dataset used, number of top models to consider for each criteria, maximum number of IVs)
all <- regsubsets(bwt~age+lwt+race+smoke+ptl+ht+ui+ftv,data = birthwt,nbest = 1, nvmax = 5)
dat <- summary(all)

#extract criteria and bind them into table
criteria <- cbind(dat$which,cbind(rsq=dat$rsq,adjr2 = dat$adjr2,cp = dat$cp, bic = dat$bic, rss = dat$rss))
criteria

#Acc. to most criteria, Model 5 is best
attach(birthwt)
model1 <- lm(bwt~lwt+race+smoke+ht+ui)
summary(model1)

#Considering Cp - Cp values follow a parabola, find the minima
plot(dat$cp)
abline(0,1)
#find the Cp that falls below the line



#Coompare AIC
#establish most inclusive and least inclusive models
full <- lm(bwt~age+lwt+race+smoke+ptl+ht+ui+ftv)
null <- lm(bwt~1)

#brute-force AIC comparison by adding/removing variables from model 
#additive - keep adding variables
#used for data with a lot of variables
stepAIC(null, scope = list(lower=null,upper=full),data=birthwt,direction = 'forward')

#subtractive - remove variables
#used for smaller models
stepAIC(full, scope = list(lower=null,upper=full),data=birthwt,direction = 'backward')

#two-way
#more complrehensive
stepAIC(null, scope = list(lower=null,upper=full),data=birthwt,direction = 'both')

#final model acc. to AIC
model2 <- lm(bwt ~ ui + race + smoke + ht + lwt)
detach(birthwt)

##Model Transformation
#scale(object,whether the values are centered around the mean (T), whether the values are standardised acc to SD (T))

library(datasets)
deets <- swiss
head(swiss)
attach(swiss)

full <- lm(Infant.Mortality~Fertility+Agriculture+Examination+Education+Catholic)
null <- lm(Infant.Mortality~1)

stepAIC(null,scope=list(lower=null,upper=full),data=swiss,direction='forward')

model3 <- lm(Infant.Mortality~Fertility+Education)
summary(model3)

#centering just the intercept to increase interpretation 
#usually, intercept reflects value when all IV are 0 which is meaningless
model3.c <- lm(Infant.Mortality~scale(Fertility,scale=F)+scale(Education,scale=F))
summary(model3.c)
#all other coefficients constant, the intercept is centered such that it reflects expected value of y when rest are average

#standardising the entire model
model3.s <- lm(Infant.Mortality~scale(Fertility)+scale(Education))
summary(model3.s)

detach(swiss)

##Categorical Variables
#######when the Predictive Variables are Categorical
attach(birthwt)

head(birthwt)

#recall the best regression model
#lm(bwt~ui+race+smoke+ht+lwt)
#smoke and race are categorical variables with varying levels

#2 Levels - smoke
model4 <- lm(bwt~smoke)
summary(model4)
#the naive regression will consider levels 1 and 0 to be continuous values

#Convert smoke into a categorical variable
#factor(object,levels that exist,labels for levels)
smoke.cat <- factor(smoke,c(0,1),labels=c('No','Yes'))

model4.cat <- lm(bwt~smoke.cat)
summary(model4.cat)
#the beta value now describes the difference between the categories i.e. smoking decreases birthweight estimates by 283g

#3 Levels - race
model5 <- lm(bwt~race)
summary(model5)
#naive regression suggests a continuous change from one race group to another

race.cat <- factor(race,c(1,2,3),labels=c('White','Black','Other'))
#creating dummy variables that can be used to differentiate categories
contrasts(race.cat)

model5.cat <- lm(bwt~race.cat)
summary(model5.cat)
#the beta values now describe how the other 2 categories differ from the first = White

#significance testing requires P and F statistics
anova(model5.cat)

#Multiple Categorical Variables
model1 <- lm(bwt~race.cat)

model2 <- lm(bwt~race.cat+smoke.cat)
summary(model2)
#the regression formula will change based on the dummy values of both categorical variables

anova(model1,model2)
#there is a significant difference made to the model by adding smoke.cat

#to check how the two categorical variables interact
model <- lm(bwt~race.cat*smoke.cat)
summary(model)

#two compare two specific combinations of the 2 categorical variables
install.packages('contrast')
library(contrast)
?contrast
#contrast(the regression model used, the first combination, the second combination)
contrast(model, 
         list(race.cat='White',smoke.cat='Yes'),
         list(race.cat='Black',smoke.cat='No'))

#Appropriate alpha = (n(n-1))/2


#######When the Outcome Variable is Categorical
attach(birthwt)
#naive regression
model <- lm(ui~age+lwt)
summary(model)
#instead of looking at whether the value of y will be one category or another, look at the probability of y being any category
#the transformation means that y will not only be 0,1 or whatever groups but 0-1 decimal probability, more usable

model <- glm(ui~age+lwt, family=binomial(link='logit')) 
#family specifies qualities of distribution
summary(model)


#Moderation Effects

#The strength of the relationship between Predictor X and Outcome Y is influenced by a 3rd variable

#y = a + b1x1 +b2z + b3(x1z) <- over here, z influences x
#when you separate the models into the levels of z, you need different slopes

#y = a + b1x1 +b2z <- over here, the relationship between x and y is independent of z
#when you separate the models into the levels of z, you may have different intercepts but the same slope NOT MODERATION

#######Moderating Variable is Categorical
model <- lm(bwt ~ age + ui + age*ui)
summary(model)
#presence or absence of categorical variable changes intercept and slope by specific values
#without ui
#bwt = 2767.26 + 11.26age 
#with ui
#bwt = 2544.69 - 4.27age

#comparative plotting
plot(ui,bwt,type='n',main="Comparative Reg Model")
abline(2767.26, 11.26)
abline(2544.69,-4.27,lty=2)
legend('topright',c('With','Without'),lty=c(1,2))

#######Moderating Variable is Continuous
model <- lm(bwt~ age+lwt + age*lwt)
summary(model)

#Mediation Effects
medData <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv')
head(medData)
attach(medData)

#Variables that explain the direction of a relationship
#x -> mediator -> y

#Direct Effect of X on Y- unit change in X causes a change in Y of c' units
#Indirect Effect of X on M on Y- unit change in x causes M change in a which causes ab change in Y
#Total Effect C = a*b + c'

#step 1 - X -> Y
summary(lm(Y~X))
#significant

#step 2 - X -> M = a
summary(lm(M~X))
#significant

#step 3 X -> Y = c' and M -> Y = b
summary(lm(Y~X+M))
#X->Y not significant
#M -> significant

n <- length(X)
boot.ab.all <- NULL

for(i in 1:1000){
  index <- sample(1:n,replace=T)
  boot.X <- X[index]
  boot.M <- M[index]
  boot.Y <- Y[index]
  boot.a.model <- lm(boot.M~boot.X)
  boot.b.model <- lm(boot.Y~boot.M)
  boot.a <- boot.a.model$coefficients[2]
  boot.b <- boot.b.model$coefficients[2]
  boot.ab <- boot.a*boot.b
  boot.ab.all <- cbind(boot.ab.all,boot.ab)
}

boot.ab.all

#package coding
install.packages('bmem')
library(bmem)

#define model
med.model <- specifyModel()
X -> M, a
M -> Y, b
X -> Y, cp
X <-> X, s1   ##standard errors
M <-> M, s2
Y <-> Y, s3

##OR
me.model <- specifyEquations(exog,variances=T)
Y = b*M + cp*X
M = a*X

#indirect effect
mediator <- 'a*b'

#Sobel Test
#standard error estimate of ab
#se <- sqrt(a^2s^2 +b^2s^2)
#z score = ab/se

#bmem.sobel(data,model,effects)
med.res.sobel <- bmem.sobel(medData,med.model,mediator)

#Confidence intervals Bootstrap
#bmem(data,model,effects,bootstrap number)
med.res.boot <- bmem(medData,med.model,mediator,boot=1000)
#if CI does not include 0 it is significant
