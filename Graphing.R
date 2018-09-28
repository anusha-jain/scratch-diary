##Setting Up Plot Parameters
#par(number of rows and number of graphs per row)
par(mfrow= c(1,1)) #default 1 row with 1 graph
#or
par(mfcol=c(1,1)) #same thing

#par(margin space by bottom, left, top, right)
par(mar=c(5,4,4,1)) #default margins

##Comparitive Graphs
#Basic Bargraph
#barplot(data,graph title,x axis label, y axis label, colour)
a <- c(1,2,2,1,3,2,1,2,1,3,1,3,3,1,2,2,1,1,1,2,1,3,1,1,3,3)
unique(a) #levels of unique values
table(a) #frequency of unique values

barplot(table(a),main="XYZ Frequency",xlab='X,Y,Z',ylab='Freq',col=c(2:4))

#data set - NLSY Spouse Wage Data 2015 https://www.nlsinfo.org/investigator/pages/search.jsp
wages <- read.table(file.choose(),header=T,na.strings=c(-1,-2,-3,-4,-5),sep=',')
colnames(wages) <- c('Index','Receive.Wages','Wage.Amt')
head(wages)

attach(wages)
barplot(table(Receive.Wages),main ="Does your spouse receive a wage?",names.arg=c('No','Yes'),col=c('red','orange'))
detach(wages)

#Pie Chart
#pie(data,labels,colour,main title,radius of circle)
a <- c(1,2,2,1,3,2,1,2,1,3,1,3,3,1,2,2,1,1,1,2,1,3,1,1,3,3)
pie(table(a),labels=c(1:3),col=rainbow(3),main="Pie Chart",radius=1)

#Boxplot
#boxplot(data,main title, name of each boxplot,colour)

install.packages("datasets")
library("datasets")

dat <- Titanic
head(dat)
dat

dat[,,,2]
dim(dat)

dat2 <- as.data.frame(dat)
dat2

attach(dat2)
table(dat2$Freq,dat2$Sex)

tabdat <- Freq~factor(Sex)+factor(Class)
boxplot(tabdat,main="Death Toll on Titanic",names=c("M 1st","F 1st","M 2nd","F 2nd","M 3rd","F 3rd","MCrew","FCrew"),col = c('red','yellow'))

avgdeath <- tapply(Freq,list(Sex,Class),mean,na.rm=T)

par(mfrow=c(2,2))
boxplot(Freq~factor(Sex), main = "By Sex")
boxplot(Freq~factor(Class), main="By Class")
boxplot(Freq~factor(Age), main="By Age")
boxplot(Freq~factor(Survived), main="By Survivorship")




#dataset - cause of death Federal stats
#https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013

