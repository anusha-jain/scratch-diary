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
#when the Predictive Variables are Categorical
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


#When the Outcome Variable is Categorical
