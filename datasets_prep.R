library(mlbench)
library(dplyr)
library(cheese)
library(caret)
library(spls)
library(HiDimDA)

## Datasets
data("BreastCancer")

Breastc <- na.omit(BreastCancer)
rm(BreastCancer)
Breastc <- Breastc %>%
  mutate(Y = model.matrix(~Class-1,Breastc)[,1]) %>%
  select(-c(Id, Class ))

#
data("PimaIndiansDiabetes")

Diabetes <- na.omit(PimaIndiansDiabetes)
rm(PimaIndiansDiabetes)

Diabetes <- Diabetes %>%
  mutate(Y = model.matrix(~diabetes-1,Diabetes)[,1]) %>%
  select(-c(diabetes))
#
data("heart_disease")
Heart_c <- na.omit(heart_disease)
rm(heart_disease)

#Experiments with the Cleveland database have concentrated on simply attempting to distinguish presence (values 1,2,3,4) from absence (value 0).
Heart_c$Y <- ifelse(Heart_c$HeartDisease == 'No',0,1)
Heart_c$HeartDisease <- NULL

Heart_c <-
  Heart_c %>%
  fastDummies::dummy_cols(remove_first_dummy = T,
                          remove_selected_columns = T)

#
Credit_a <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", sep=",", header=F, na.strings="?")
Credit_a <- na.omit(Credit_a)

Credit_a <-
  Credit_a %>%
  fastDummies::dummy_cols(remove_first_dummy = T,
                          remove_selected_columns = T) %>%
  rename(Y = `V16_+`)
#
Credit_g <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric", sep="", header=F)
Credit_g <- na.omit(Credit_g)

Credit_g <-
  Credit_g %>%
  mutate(Y = ifelse(V25 == 1,1,0)) %>%
  select(-V25)
#
#Adult <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep=",", header=F)
#Adult <- na.omit(Adult)

#Adult <-
#  Adult %>%
#  fastDummies::dummy_cols(remove_first_dummy = T,
#                          remove_selected_columns = T) %>%
#  rename(Y = `V15_ >50K`)
#
Vote <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data", sep=",", header=F, na.strings = '?')
#Vote <- na.omit(Vote)
Vote[is.na(Vote)] <- 'absent'
Vote <-
  Vote %>%
  fastDummies::dummy_cols(remove_first_dummy = T,
                          remove_selected_columns = T) %>%
  rename(Y = `V1_republican`)
#
Wdbc <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", sep=",", header=F, na.strings = '?')
Wdbc <- na.omit(Wdbc)

Wdbc <-
  Wdbc %>%
  mutate(Y = ifelse(V2 == 'M', 1, 0)) %>%
  select(-c(V1, V2))
#
Spambase <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", sep=",", header=F)
Spambase <- na.omit(Spambase)

Spambase <- Spambase %>%
  rename(Y = V58)
#

#
Banknote <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", sep=",", header=F, na.strings = '?')
Banknote <- na.omit(Banknote)

Banknote <-
  Banknote %>%
  rename(Y = V5)

# dhfr
data("dhfr")
dhfr <- na.omit(dhfr)
dhfr$Y <- as.numeric(dhfr$Y)-1 


# lymphoma
data(lymphoma)
lymphoma<-data.frame(x=lymphoma$x,Y=ifelse(lymphoma$y==0,0,1)) #y 0-1


# prostate
data(prostate)
prostate<-data.frame(x=prostate$x,Y=prostate$y)

# AlonDS
Alon_DS<-data.frame(x=AlonDS[,2:2001],Y=ifelse(AlonDS[,1]=="colonc",1,0))


# Artif
n<-2000
p<-20
p0<-5
p1<-p-p0
beta0<-0.01
beta<-c(rep(2,p0),rep(0,p1))
sigma<-function(x){exp(x)/(1+exp(x))}
set.seed(1000)
X<-matrix(rnorm(n*p),nrow=n)
Y<-as.vector(n)
for (j in 1:n)
{
  Y[j]<-sample(c(0,1),1,prob=c(1-sigma(beta0+X[j,]%*%beta),sigma(beta0+X[j,]%*%beta)))
}
Artif<-data.frame(X,Y)





  



