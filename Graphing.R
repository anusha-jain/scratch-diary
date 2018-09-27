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

table(Receive.Wages)
barplot(table(Receive.Wages),main ="Does your spouse receive a wage?",names.arg=c('No','Yes'),col=c('red','orange'))

#Complex Bar Graph
ironcontent <- 
