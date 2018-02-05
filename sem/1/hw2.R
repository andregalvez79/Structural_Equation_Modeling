#PART A
job <- read.csv2("C:\\Users\\s4600479\\Desktop\\EAS_n500(2).csv", sep = ";")

install.packages("lavaan")
library(lavaan)

install.packages("semPlot")
library(semPlot)

install.packages("VIM")
library(VIM)

install.packages("mvnmle")
library(mvnmle)

install.packages("BaylorEdPsych")
library(BaylorEdPsych)

install.packages("psych")
library(psych)

#missing values
job$gender <- factor(job$gender, levels = c("0","1"), labels = c("male", "female"))

jobf <- subset(job, gender == "female")
jobm <- subset(job, gender == "male")

miss1<- aggr(job)
miss1

missmales<- aggr(jobf)
missfemales<- aggr(jobm)

#identify missing values between continuous and categorical measures
d<- job[ ,c("gender","SOCIAL")]
barMiss(d)
d<- job[ ,c("gender","ANXIETY")]
histMiss(d)
d<- job[ ,c("gender","DISTRESS")]
histMiss(d)
d<- job[ ,c("gender","FEAR")]
histMiss(d)
d<- job[ ,c("gender","ANGER")]
histMiss(d)

###Little's MCAR test

LittleMCAR(job)
#this does not work very well with large data files like this, so use the alternative 

MCAR<-LittleMCAR(job)

MCAR$chi.square
MCAR$df
MCAR$p.value
MCAR$missing.patterns
MCAR$amount.missing

#pairwise VS LISTWISE deletion

corr.test(job[,2:7], use = "complete")
corr.test(job[,2:7], use = "pairwise")

#PART B

#two regressions (OLS)
lm1 <- lm(DISTRESS ~ ANXIETY + FEAR, job)
summary(lm1)
lm2 <- lm(SOCIAL ~ ANGER + DISTRESS, job)
summary(lm2)

#TWO REGRESSION IN SEM (ML)
#LISTWISE DELETION IS DEFAULT

model1 <- "DISTRESS ~ ANXIETY + FEAR"
model2 <- "SOCIAL ~ ANGER + DISTRESS"

fit1 <- sem(model1, data = job, missing="ML")
summary(fit1, standardized = T, rsquare=T)

fit2 <- sem(model2, data = job, missing="ML")
summary(fit2, standardized = T, rsquare=T)

#some plots
semPaths(fit1)
semPaths(fit1, residuals = F)
semPaths(fit1, what="std", residuals = F)

semPaths(fit2)
semPaths(fit2, residuals = F)
semPaths(fit2, what="std", residuals = F)

#same but with Full-Information Maximum Likelihood (FIML) estimation to include all participants in the analyses

model1.1 <- "DISTRESS ~ ANXIETY + FEAR"
model2.1 <- "SOCIAL ~ ANGER + DISTRESS"

fit1.1 <- sem(model1.1, data = job, missing="FIML")
summary(fit1.1, standardized = T, rsquare=T)

fit2.1 <- sem(model2.1, data = job, missing="FIML")
summary(fit2.1, standardized = T, rsquare=T)

####now testing both questions in one

modelmax <- "DISTRESS ~ ANXIETY + FEAR
SOCIAL ~ ANGER + DISTRESS"
fit <- sem(modelmax, data = job, missing="FIML")
summary(fit, standardized = T, rsquare=T)

semPaths(fit)
semPaths(fit, residuals = F)
semPaths(fit, what="std", residuals = F)

#Part C
summary(fit, standardized = T, rsquare=T)
