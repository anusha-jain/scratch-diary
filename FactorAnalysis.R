#Exploratory Factor Analysis
#dataset - Holzinger and Swineford original dataset
holz <- read.table(file.choose(),header=T)
head(holz)
dim(holz)

fadata<- holz[,7:25]

#step 1 - create a correlation matrix
facor <- cor(fadata)

#Bonus - simplify correlation matrix into heat graph
facor2 <- round(facor,2)
install.packages("reshape")
library(reshape)
meltedfacor <- melt(facor2)
head(meltedfacor)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = meltedfacor, aes(x=X1, y=X2, fill=value)) + 
  geom_tile()
?geom_tile

#Step 2a - find eigen values of correlation matrix
faeigen <- eigen(facor)
plot(faeigen$values,type="b")

#Step 2b - find parallel analysis values of correlation matrix
install.packages("paran")
library(paran)
paran(facor)

#Step 3a - factor analysis
#factanal(x=dataset OR covmat=the correlation matrix,factors,rotation, score type = Bartlett or regression)
fa.res <- factanal(covmat = facor,factors=3, rotation = "varimax", scores='Bartlett')

?factanal

#Step 4 - display loadings with minimum limit
print(fa.res, cut = .3)


#Confirmatory Factor Analysis
install.packages("lavaan")
library(lavaan)
#dataset
?HolzingerSwineford1939 

head(HolzingerSwineford1939)

#define your model
model <- 'Factor1 =~ general + paragrap + sentence + wordc + wordm
Factor2 =~ visual +cubes +paper +lozenge +code +straight +wordr +numberr +figurer +numberf +figurew
Factor3 =~ add +code +counting +straight +object +numberf'

#fit the data to the model
#cfa(model parameters, dataset)
?cfa
checkfit <- cfa(model=model,data=fadata)

#how valid is the fit? 
summary(checkfit,fit.measures=T)
