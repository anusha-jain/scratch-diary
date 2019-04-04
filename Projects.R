library(tidyverse)

##the quantitative data variables
cleandat <- read.csv("dat_SSE3_clean.csv",header=T,na.strings="NA")
cleandat <- cleandat[,1:49]

sse17 <- read.csv("SSE_Full.csv",header=T,na.strings="NA")

##grab sociability variables from second dataset
sse17 %>% select(c("AP.ID","Soc1","Soc2","Soc3","Soc4","Soc5")) -> socsse17
head(socsse17)

##full dataset
cleandat %>% merge(socsse17,by="AP.ID",all.x=T) -> stuff
nrow(stuff)
names(stuff)

##user ID
realusers <- read.csv("pilot3_usernames_dropped.csv",header=T,na.strings="NA")
##second time-point data
sse18 <- read.csv("cleaned_time5 (2).csv",header=T,na.strings="NA")
sse18 <- sse18[,c(1,16:46)]

##demographic data
demog <- read.csv("cleaned_time02 (3).csv",header=T,na.strings="NA")
demog <- demog[,c(1,20:32,34:36)]
names(realusers)
names(demog)
names(sse18)

##full demographic dataset
realusers %>% merge(demog,by="id",all.x=F) %>% merge(sse18,by.x = "username",by.y = "Username", all.x=T) -> stuff18
nrow(stuff18)
nrow(realusers)
names(stuff18)
names(stuff)

##combine quanititative data with demographic data with second timepoint
stuff %>% select(c("AP.ID"  ,    "ClassLevel", "Race1"    ,  "Race2"   ,   
                   "Race3"    ,  "Race4"   ,   "Race5"   ,   "Race6"   ,   "Race7"  ,   
                   "Race8"  ,    "Race9"    ,  "Race10"   ,  "Race11" ,    
                   "RaceCat"  ,  "Edu.parent", "Sex"    ,    "Lunch"  ,    "EA1"    ,   
                   "EA2"    ,    "EA3"      ,  "EA4"      ,  "EA5R"   ,    
                   "EA6"      ,  "EA7"    ,    "EA8R"   ,    "EB1"    ,    "EB2"     ,  
                   "EB3"    ,    "EB4"      ,  "EB5"      ,  "EB6R"   ,    
                   "EB7"      ,  "EB8"    ,    "EC1"    ,    "EC2"    ,    "EC3"    ,   
                   "EC4"    ,    "EC5"      ,  "EC6"      ,  "EC7"    ,    "EC8" ,
                   "ES1"="Soc1"   ,  "ES2"="Soc2"   ,   "ES3"="Soc3"   , "ES4"="Soc4"   ,    
                   "Soc5"))  %>% add_row(AP.ID=stuff18$id, ClassLevel=stuff18$ClassLevel, Race1=stuff18$Race1,
                                         Race2=stuff18$Race2, Race3=stuff18$Race3, Race4=stuff18$Race4, 
                                         Race5=stuff18$Race5, Race6=stuff18$Race6, Race7=stuff18$Race7,
                                         Race8=stuff18$Race8, Race9=stuff18$Race9, Race10=stuff18$Race10,
                                         Race11=stuff18$Race11, RaceCat=stuff18$RaceCat, Edu.parent=stuff18$Edu.parent,
                                         Sex=stuff18$Sex, Lunch=stuff18$ReducedLunch, EA1=stuff18$EA1, 
                                         EA2=stuff18$EA2, EA3=stuff18$EA3, EA4=stuff18$EA4, EA5R=stuff18$EA5R,
                                         EA6=stuff18$EA6, EA7=stuff18$EA7, EA8R=stuff18$EA8R, EB1=stuff18$EB1,
                                         EB2=stuff18$EB2, EB3=stuff18$EB3, EB4=stuff18$EB4, EB5=stuff18$EB5,
                                         EB6R=stuff18$EB6R, EB7=stuff18$EB7, EB8=stuff18$EB8, EC1=stuff18$EC1,
                                         EC2=stuff18$EC2, EC3=stuff18$EC3, EC4=stuff18$EC4, EC5=stuff18$EC5,
                                         EC6=stuff18$EC6, EC7=stuff18$EC7, EC8=stuff18$EC8, ES1=stuff18$ES1,
                                         ES2=stuff18$ES2, ES3=stuff18$ES3, ES4=stuff18$ES4) -> finale


names(finale)
nrow(finale)

##add demography and survey

demo.c <- tibble(Sex = c(1,2,NA), demo = c(1,1,0) )
survey.c <- tibble(EA1 = c(1:5,NA), surv = c(rep.int(1,5),0) )

##join demographic data with both time-points
finale %>% left_join(demo.c) %>% left_join(survey.c) -> finale
save(finale,file="finale.RData")

######################################################
######################################################

dat <- read.csv(file='C:\\Users\XXXXXXXXXXXXXXXX',na.strings=c('-5','-4','-3','-2','-1'))
head(dat)

##relevant variables taken from NLSY codebook
master <- c('T3162502','T3162507','T3162600','T3162601','T3162602','T3162603','T3162700','T3162701','T3162702','T3162703','T2111400','T2111500',
            'T2781900', 'T2782000', 'T2782100', 'T2782200', 'T2782300', 'T2782400','T2785400', 'T2785500', 'T2785600', 'T2785800' ,'T2786000',
            'T2786300', 'T2786600','T2786800', 'T2787200', 'T2787400', 'T2789400', 'T3161800', 'T3162000' ,'T3162100', 'T3162400')

masterdat <- dat[,master]
head(masterdat)

install.packages('Rcpp')
install.packages('mice')
library(mice)

?mice
idat <- mice(as.matrix(masterdat),m=1)
one <- complete(idat,1)

conscienciousness <- c('T3162502','T3162507','T3162600','T3162601','T3162602','T3162603','T3162700','T3162701','T3162702','T3162703')
consci <- one[,conscienciousness]

outcome <- c('T2111400','T2111500','T2781900', 'T2782000', 'T2782100', 'T2782200', 'T2782300', 'T2782400','T2785400', 'T2785500', 
             'T2785600', 'T2785800' ,'T2786000',
             'T2786300', 'T2786600','T2786800', 'T2787200', 'T2787400', 'T2789400', 'T3161800', 'T3162000' ,'T3162100', 'T3162400')
crime <- c() ##ADD CRIME VARIABLES
religion <- c() ##ADD RELIGIOSITY VARIABLES

ov <- one[,crime]
mv <- one[,religion]

n <- length(crime)
m <- length(religion)
model.v <- NULL
oc <- NULL
mm <-NULL

##loop through regression testing all variables
## crime outcome ~ religiosity moderated by conscienciousness
for(i in 1:n) {
  for(j in 1:m){
    o <- crime[,i]
    m <- religion[,j]
    mode <- lm(o~ consci +
                 m + consci*m)
    deet <-summary(mode)
    if (deet$p.value < 0.05){
      oc <- cname(o)
      mm <- cname(m)
      out <-cbind(oc, mm)
      model.v <-rbind(model.v, ov)
    }
  }
}

######################################################
######################################################

head(X_data)
names(X_data)

table(X_data$RaceCat)
cbind(table(X_data$RaceCat),table(X_data$Edu.student))

library(tidyverse)
attach(X_data)

##table with demographic categories and their n
racetab <- NULL
racetab <- as_tibble(racetab)
racetab[1,1] <- "Race"
racetab[1,2] <- "  "
racetab <- rbind(racetab,cbind(levels(RaceCat[!is.na(RaceCat)]),table(RaceCat)))

levels(RaceCat)
table(RaceCat)

ed.stu <- NULL
edstu <- as_tibble(ed.stu)
edstu[1,1]<- "Edu.Student"
edstu[1,2] <- "  "
edstu <- rbind(edstu,cbind(levels(Edu.student[!is.na(Edu.student)]),table(Edu.student)))

ed.pa <- NULL
edpa <- as_tibble(ed.pa)
edpa[1,1] <- "Edu.Parent"
edpa[1,2] <- "  "
edpa <- rbind(edpa,cbind(levels(Edu.parent[!is.na(Edu.parent)]),table(Edu.parent)))

sex.student <- NULL
sex.student <- as_tibble(sex.student)
sex.student[1,1] <- "Sex"
sex.student[1,2] <- "  "   ##length(X_data$sex[!is.na(X_data$sex)])
sex.student <- rbind(sex.student,cbind(c("Male","Female"),table(sex)))

age.st <- NULL
age.st <- as_tibble(age.st)
age.st[1,1] <- "Age"
age.st[1,2] <-  "  " ##length(age[!is.na(age)])
age.st <- rbind(age.st,cbind(c(15:18,20),table(age)))

Reasons <- NULL
Reasons <- as_tibble(Reasons)
Reasons[1,1] <- "Reason"
Reasons[1,2] <- "  "
for (i in 1:5){
  a <- X_data[,26+i]
  Reasons[1+i,1] <- names(X_data[26+i])
  Reasons[1+i,2] <- sum(as.integer(a),na.rm=T)
}

categ <- rbind(racetab,ed.stu,ed.pa,sex.student,age.st,Reasons)
str(categ)
colnames(categ) <- c("Variable","N")
names(categ)
##FINAL TABLE 1
head(categ)

##descriptive statistics table
sumvar <- c("RaceCat","Edu.student","Edu.parent","sex","age","Reason1","Reason2","Reason3","Reason4","Reason5")
meanvar <- c("Extra.Soc","Extra.Asser","Extra.Energy","Agree.Comp","Agree.Resp","Agree.Trust","Consc.Org","Consc.Prod",
             "Consc.Resp","NegEmo.Anx","NegEmo.Dep","NegEmo.Emovol","Open.Intell","Open.Aesth","Open.Creat","APPred1",
             "Active","Passive","Grit_Total","TeacherSupport","AP.Exam.1","identity","engaff","engbeh","engcog")


##average of personality score
X_data %>% as_tibble() %>% select(c("Extra.Soc","Extra.Asser","Extra.Energy",
                                    "Agree.Comp","Agree.Resp",
                                    "Agree.Trust","Consc.Org","Consc.Prod",
                                    "Consc.Resp","NegEmo.Anx","NegEmo.Dep",
                                    "NegEmo.Emovol","Open.Intell",
                                    "Open.Aesth","Open.Creat","APPred1",
                                    "Active","Passive","Grit_Total",
                                    "TeacherSupport","AP.Exam.1",
                                    "identity","engaff","engbeh","engcog")) %>% 
  apply(2,function(x) mean(x,na.rm=T)) -> avg

##standard dev of personality scores
X_data %>% as_tibble() %>% select(c("Extra.Soc","Extra.Asser","Extra.Energy",
                                    "Agree.Comp","Agree.Resp",
                                    "Agree.Trust","Consc.Org","Consc.Prod",
                                    "Consc.Resp","NegEmo.Anx","NegEmo.Dep",
                                    "NegEmo.Emovol","Open.Intell",
                                    "Open.Aesth","Open.Creat","APPred1",
                                    "Active","Passive","Grit_Total",
                                    "TeacherSupport","AP.Exam.1",
                                    "identity","engaff","engbeh","engcog")) %>% 
  apply(2,function(x) sd(x,na.rm=T)) -> stand.dev

##missingness
percentna <- function(x) {
  sum(is.na(x))/length(x)
}
X_data %>% as_tibble() %>% select(c("Extra.Soc","Extra.Asser","Extra.Energy",
                                    "Agree.Comp","Agree.Resp",
                                    "Agree.Trust","Consc.Org","Consc.Prod",
                                    "Consc.Resp","NegEmo.Anx","NegEmo.Dep",
                                    "NegEmo.Emovol","Open.Intell",
                                    "Open.Aesth","Open.Creat","APPred1",
                                    "Active","Passive","Grit_Total",
                                    "TeacherSupport","AP.Exam.1",
                                    "identity","engaff","engbeh","engcog")) %>% 
  apply(2,function(x) percentna(x)) -> percent.NA

descript <- as_tibble(cbind(meanvar,avg,stand.dev,percent.NA))
type_convert(descript,col_types = cols(avg=col_number(),
                                       stand.dev=col_number(), percent.NA=col_number()))
##FINAL TABLE 2
view(descript)
detach(X_data)

CATEG <- as.data.frame(categ)
DESCRIPT <- as.data.frame(descript)

write.csv(CATEG, file="sumvar.csv")
write.csv(DESCRIPT, file="meanvar.csv")

FULL <- list(categ,descript)
save(FULL,file="MaxTable.RData")

######################################################
######################################################

library(mice)
set.seed(12345)
idat <- mice(full.data,m=35,maxit=30)

##brute force correlation matrix
co23 <- NULL
co24 <- NULL
co25 <- NULL
co34 <- NULL
co35 <- NULL
co45 <- NULL

for (i in 1:idat$m) {
  ft <- complete(idat,i)
  co23 <- rbind(co23,cor(ft[,2],ft[,3]))
  co24 <- rbind(co24,cor(ft[,2],ft[,4]))
  co25 <- rbind(co25,cor(ft[,2],ft[,5]))
  co34 <- rbind(co34,cor(ft[,3],ft[,4]))
  co35 <- rbind(co35,cor(ft[,3],ft[,5]))
  co45 <- rbind(co45,cor(ft[,4],ft[,5]))
}

cormat <- NULL
cormat <- as.data.frame(cormat)
cormat[1,1] <- colMeans(co23)
cormat[1,2] <- colMeans(co24)
cormat[1,3] <- colMeans(co25)
cormat[2,1] <- 1
cormat[2,2] <- colMeans(co34)
cormat[2,3] <- colMeans(co35)
cormat[3,1] <- colMeans(co34)
cormat[3,2] <- 1
cormat[3,3] <- colMeans(co45)
colnames(cormat) <- c("beh.eng","cog.eng","ap.pred")
rownames(cormat) <- c("aff.eng","beh.eng","cog.eng")
cormat

library(car)

##regression model on imputed dataset
intcpt <- NULL
coef1 <- NULL
coef2 <- NULL
coef3 <- NULL
coef4 <- NULL
intcpt.sd <- NULL
coef1.sd <- NULL
coef2.sd <- NULL
coef3.sd <- NULL
coef4.sd <- NULL
intcpt.p <- NULL
coef1.p <- NULL
coef2.p <- NULL
coef3.p <- NULL
coef4.p <- NULL
vifint <- NULL
vif1 <- NULL
vif2 <- NULL
vif3 <- NULL
vif4 <- NULL

for(i in 1:idat$m){
  fit <- glm(Y_take~aff.eng+beh.eng+I(cog.eng^2)+ap.pred ,data=complete(idat,i),family=binomial(link = "logit"))
  aaa <- summary.glm(fit)
  vvv <- vif(fit)
  intcpt <- rbind(intcpt,fit$coefficients[1])
  coef1 <- rbind(coef1,fit$coefficients[2])
  coef2 <- rbind(coef2,fit$coefficients[3])
  coef3 <- rbind(coef3,fit$coefficients[4])
  coef4 <- rbind(coef4,fit$coefficients[5])
  intcpt.sd <- rbind(intcpt.sd,aaa$coefficients[1,2])
  coef1.sd <- rbind(coef1.sd,aaa$coefficients[2,2])
  coef2.sd <- rbind(coef2.sd,aaa$coefficients[3,2])
  coef3.sd <- rbind(coef3.sd,aaa$coefficients[4,2])
  coef4.sd <- rbind(coef4.sd,aaa$coefficients[5,2])
  intcpt.p <- rbind(intcpt.p,aaa$coefficients[1,4])
  coef1.p <- rbind(coef1.p,aaa$coefficients[2,4])
  coef2.p <- rbind(coef2.p,aaa$coefficients[3,4])
  coef3.p <- rbind(coef3.p,aaa$coefficients[4,4])
  coef4.p <- rbind(coef4.p,aaa$coefficients[5,4])
  vif1 <- rbind(vif1,vvv[1])
  vif2 <- rbind(vif2,vvv[2])
  vif3 <- rbind(vif3,vvv[3])
  vif4 <- rbind(vif4,vvv[4])
}

##coefficient dataframe
info <- cbind(intcpt,coef1,coef2,coef3,coef4)
info.p <- cbind(intcpt.p,coef1.p,coef2.p,coef3.p,coef4.p)
colnames(info.p) <- c("intcpt.p","coef1.p","coef2.p","coef3.p","coef4.p")
info.sd <- cbind(intcpt.sd,coef1.sd,coef2.sd,coef3.sd,coef4.sd)
colnames(info.sd) <- c("intcpt.sd","coef1.sd","coef2.sd","coef3.sd","coef4.sd")
info.v <- cbind(vif1,vif2,vif3,vif4)
colnames(info.v) <- c("vif1","vif2","vif3","vif4")
head(info.p)
head(info)
head(info.sd)

##average coefficients and their p
colMeans(info)
colMeans(info.sd)
colMeans(info.p)
colMeans(info.v)


#############STOP#####################
#tidyverse and mice do not run together


##percent significant
library(tidyverse)
info.p <- as_tibble(info.p)
info.p %>% filter(intcpt.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef1.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef2.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef3.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef4.p < 0.05) %>% summarise(significant=n(),percent = n()/35)


#######################################################################################
#######################################################################################
##for within vs between teacher class differences in regression model coefficients

gc(reset = T)

dat <- read.csv("requested_data.csv",header=T)
head(dat)
names(dat)

unique(dat$AP.exam)

##convert AP.EXAM into binomial YES/NO variable

for( i in 1:nrow(dat)) {
  if(is.na(dat$AP.exam[i]) == T ){
    dat$AP.exam[i] = 0
  } else {
    dat$AP.exam[i] = 1
  }
}

unique(dat$AP.exam)

##brute-force adding a teacher tag = 1:7
unique(dat$teacher)
teachtag <- cbind(c('Ferris', 'Goodrich', 'Hernandez', 'Mackiewicz', 'Quandt', 'Steele', 'Wernke'),c(1:7))
teachtag <- as_tibble(teachtag)
colnames(teachtag) <- c("teacher","tag")
teachtag <- type_convert(teachtag,col_types = cols(Tag = col_double()))
head(dat)

##impute within each teacher's class
##if full dataset is imputed then between class differences are distorted
##each class must be imputed separately
dat %>% filter(tag==1) -> dat1
dat %>% filter(tag==2) -> dat2
dat %>% filter(tag==3) -> dat3
dat %>% filter(tag==4) -> dat4
dat %>% filter(tag==5) -> dat5
dat %>% filter(tag==6) -> dat6
dat %>% filter(tag==7) -> dat7

library(mice)
library(tidyverse)
library(car)

##Impute data
set.seed(12345)
##this will take a WHILE
idat1 <- mice(dat1,m=35,maxit=30)
idat2 <- mice(dat2,m=35,maxit=30)
idat3 <- mice(dat3,m=35,maxit=30)
idat4 <- mice(dat4,m=35,maxit=30)
idat5 <- mice(dat5,m=35,maxit=30)
idat6 <- mice(dat6,m=35,maxit=30)
idat7 <- mice(dat7,m=35,maxit=30)

idat <- NULL
idat <- as.list(idat)

##recombine the datasets into a list
for (i in 1:idat1$m) {
  d1 <- mice::complete(idat1,m=i)
  d2 <- mice::complete(idat2,m=i)
  d3 <- mice::complete(idat3,m=i)
  d4 <- mice::complete(idat4,m=i)
  d5 <- mice::complete(idat5,m=i)
  d6 <- mice::complete(idat6,m=i)
  d7 <- mice::complete(idat7,m=i)
  idat[[i]] <- bind_rows(d1[[1]],d2[[1]],d3[[1]],d4[[1]],d5[[1]],d6[[1]],d7[[1]])
}

intcpt <- NULL
coef1 <- NULL
coef2 <- NULL
coef3 <- NULL
coef4 <- NULL
coef5 <- NULL
coef6 <- NULL
coef7 <- NULL
coef8 <- NULL
coef9 <- NULL
coef10 <- NULL
intcpt.sd <- NULL
coef1.sd <- NULL
coef2.sd <- NULL
coef3.sd <- NULL
coef4.sd <- NULL
coef5.sd <- NULL
coef6.sd <- NULL
coef7.sd <- NULL
coef8.sd <- NULL
coef9.sd <- NULL
coef10.sd <- NULL
intcpt.p <- NULL
coef1.p <- NULL
coef2.p <- NULL
coef3.p <- NULL
coef4.p <- NULL
coef5.p <- NULL
coef6.p <- NULL
coef7.p <- NULL
coef8.p <- NULL
coef9.p <- NULL
coef10.p <- NULL

##regression model on within-class imputed dataset
for(i in 1:idat1$m) {
  testdat <- idat[[i]]
  testdat %>% as_tibble() %>% group_by(teacher) %>% 
    mutate(avg_a = mean(t4_micro_a_sum,na.rm=T), 
           centred_t4_m_a_sum = t4_micro_a_sum - avg_a, 
           avg_b = mean(t4_micro_b_sum,na.rm=T), 
           centred_t4_m_b_sum = t4_micro_b_sum - avg_b,
           avg_c = mean(t4_micro_c_sum,na.rm = T),
           centred_t4_m_c_sum = t4_micro_c_sum - avg_c,
           avg_apscore = mean(t4_predicted_ap_score, na.rm = T),
           centered_t4_predicted_ap_score_sum = t4_predicted_ap_score - avg_apscore,
           avg_t = mean(t4_teacher_support_sum, na.rm = T),
           centered_t4_teacher_support_sum = t4_teacher_support_sum - avg_t) %>%
    select(teacher,
           centred_t4_m_a_sum,
           centred_t4_m_b_sum,
           centred_t4_m_c_sum,
           centered_t4_predicted_ap_score_sum,
           centered_t4_teacher_support_sum,
           AP.exam,
           avg_a,
           avg_b,
           avg_c,
           avg_apscore,
           avg_t) -> fitdat
  mod <- glm(AP.exam ~ centred_t4_m_a_sum + 
               centred_t4_m_b_sum + 
               centred_t4_m_c_sum + 
               centered_t4_predicted_ap_score_sum +
               centered_t4_teacher_support_sum +
               avg_a+
               avg_b+
               avg_c+
               avg_apscore+
               avg_t ,
             data = fitdat,
             family=binomial())
  aaa <- summary.glm(mod)
  
  intcpt <- rbind(intcpt,aaa$coefficients[1,1])
  coef1 <- rbind(coef1,aaa$coefficients[2,1])
  coef2 <- rbind(coef2,aaa$coefficients[3,1])
  coef3 <- rbind(coef3,aaa$coefficients[4,1])
  coef4 <- rbind(coef4,aaa$coefficients[5,1])
  coef5 <- rbind(coef5,aaa$coefficients[6,1])
  coef6 <- rbind(coef6,aaa$coefficients[7,1])
  coef7 <- rbind(coef7,aaa$coefficients[8,1])
  coef8 <- rbind(coef8,aaa$coefficients[9,1])
  coef9 <- rbind(coef9,aaa$coefficients[10,1])
  coef10 <- rbind(coef10,aaa$coefficients[11,1])
  intcpt.sd <- rbind(intcpt.sd,aaa$coefficients[1,2])
  coef1.sd <- rbind(coef1.sd,aaa$coefficients[2,2])
  coef2.sd <- rbind(coef2.sd,aaa$coefficients[3,2])
  coef3.sd <- rbind(coef3.sd,aaa$coefficients[4,2])
  coef4.sd <- rbind(coef4.sd,aaa$coefficients[5,2])
  coef5.sd <- rbind(coef5.sd,aaa$coefficients[6,2])
  coef6.sd <- rbind(coef6.sd,aaa$coefficients[7,2])
  coef7.sd <- rbind(coef7.sd,aaa$coefficients[8,2])
  coef8.sd <- rbind(coef8.sd,aaa$coefficients[9,2])
  coef9.sd <- rbind(coef9.sd,aaa$coefficients[10,2])
  coef10.sd <- rbind(coef10.sd,aaa$coefficients[11,2])
  intcpt.p <- rbind(intcpt.p,aaa$coefficients[1,4])
  coef1.p <- rbind(coef1.p,aaa$coefficients[2,4])
  coef2.p <- rbind(coef2.p,aaa$coefficients[3,4])
  coef3.p <- rbind(coef3.p,aaa$coefficients[4,4])
  coef4.p <- rbind(coef4.p,aaa$coefficients[5,4])
  coef5.p <- rbind(coef5.p,aaa$coefficients[6,1])
  coef6.p <- rbind(coef6.p,aaa$coefficients[7,1])
  coef7.p <- rbind(coef7.p,aaa$coefficients[8,1])
  coef8.p <- rbind(coef8.p,aaa$coefficients[9,1])
  coef9.p <- rbind(coef9.p,aaa$coefficients[10,1])
  coef10.p <- rbind(coef10.p,aaa$coefficients[11,1])
  
}

info <- cbind(intcpt,coef1,coef2,coef3,coef4,coef5,coef6,coef7,coef8,coef9,coef10)
colnames(info) <- c("intcpt","coef1","coef2","coef3","coef4","coef5","coef6","coef7","coef8","coef9","coef1")
info.p <- cbind(intcpt.p,coef1.p,coef2.p,coef3.p,coef4.p,coef5.p,coef6.p,coef7.p,coef8.p,coef9.p,coef10.p)
colnames(info.p) <- c("intcpt.p","coef1.p","coef2.p","coef3.p","coef4.p","coef5.p","coef6.p","coef7.p","coef8.p","coef9.p","coef10.p")
info.sd <- cbind(intcpt.sd,coef1.sd,coef2.sd,coef3.sd,coef4.sd,coef5.sd,coef6.sd,coef7.sd,coef8.sd,coef9.sd,coef10.sd)
colnames(info.sd) <- c("intcpt.sd","coef1.sd","coef2.sd","coef3.sd","coef4.sd","coef5.sd","coef6.sd","coef7.sd","coef8.sd","coef9.sd","coef10.sd")

##average of coefficients
colMeans(info)
colMeans(info.p)
colMeans(info.sd)


##N imputations significant
info.p <- as_tibble(info.p)
info.p %>% filter(intcpt.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef1.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef2.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef3.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef4.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef5.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef6.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef7.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef8.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef9.p < 0.05) %>% summarise(significant=n(),percent = n()/35)
info.p %>% filter(coef10.p < 0.05) %>% summarise(significant=n(),percent = n()/35)