takeh <- read.csv2("C:\\Users\\s4600479\\Desktop\\takehome2016.csv", sep = ";")
takeh2 <- read.csv2("C:\\Users\\s4600479\\Desktop\\takehome2016.csv", sep = ";")
#run teakeh2 and don't run with normalized variables... or littile mcar will not work

takeh <- read.csv2("C:\\Users\\André\\Google Drive\\Master\\period 4\\sem\\midterm\\takehome2016.csv", sep = ";")
takeh2 <- read.csv2("C:\\Users\\André\\Google Drive\\Master\\period 4\\sem\\midterm\\takehome2016.csv", sep = ";")


summary(takeh)

install.packages("knitr")
library(knitr)

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

install.packages("pastecs")
library(pastecs)

install.packages("lattice")
library(lattice)

options (scipen = 12)


####################PART A########################

#skewed kurtosed

densityplot(takeh$sex)
stat.desc(takeh$sex, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$educate, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$income, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$involve, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$school, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$teach, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$accept, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$achieve, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)
stat.desc(takeh$CITO, basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)


#outliers

takeh$zsex <- scale(takeh$sex, center = TRUE, scale = TRUE)
describe(takeh$zsex)
#nrow(subset(manova_1, abs(scale(ESTEEM)) >3)) just to remember that i can know exactly which pp

takeh$zeducate <- scale(takeh$educate, center = TRUE, scale = TRUE) #this one
describe(takeh$zeducate)
length(which(abs(takeh$zeducate)>3))

takeh$zincome <- scale(takeh$income, center = TRUE, scale = TRUE)
describe(takeh$zincome)

takeh$zinvolve <- scale(takeh$involve, center = TRUE, scale = TRUE)
describe(takeh$zinvolve)

takeh$zschool <- scale(takeh$school, center = TRUE, scale = TRUE) #this one
describe(takeh$zschool)
length(which(abs(takeh$zschool)>3))

takeh$zteach <- scale(takeh$teach, center = TRUE, scale = TRUE) #this one
describe(takeh$zteach)
length(which(abs(takeh$zteach)>3))

takeh$zaccept <- scale(takeh$accept, center = TRUE, scale = TRUE) #this one
describe(takeh$zaccept)
length(which(abs(takeh$zaccept)>3))

takeh$zachieve <- scale(takeh$achieve, center = TRUE, scale = TRUE) #this one
describe(takeh$zachieve)
length(which(abs(takeh$zachieve)>3))

takeh$zCITO <- scale(takeh$CITO, center = TRUE, scale = TRUE)
describe(takeh$zCITO)


######################patterns in NA-s#########################

#missing values
takeh$sex <- factor(takeh$sex, levels = c("1","2"), labels = c("female", "male"))


takehf <- subset(takeh, sex == "female")
takehm <- subset(takeh, sex == "male")

miss1<- aggr(takeh)
miss1

missmales<- aggr(takehf)
missfemales<- aggr(takehm)

#identify missing values between continuous and categorical measures
d<- takeh[ ,c("sex","educate")]
barMiss(d)
d<- takeh[ ,c("sex","income")]
histMiss(d)
d<- takeh[ ,c("sex","involve")]
histMiss(d)
d<- takeh[ ,c("sex","school")]
histMiss(d)
d<- takeh[ ,c("sex","teach")]
histMiss(d)
d<- takeh[ ,c("sex","accept")]
barMiss(d)
d<- takeh[ ,c("sex","achieve")]
barMiss(d)
d<- takeh[ ,c("sex","CITO")]
barMiss(d)
###Little's MCAR test

MCAR<-LittleMCAR(takeh2)

MCAR$chi.square
MCAR$df
MCAR$p.value
MCAR$missing.patterns
MCAR$amount.missing

####correlations without nas listwise deletion
#pairwise VS LISTWISE deletion

corr.test(takeh[,2:9], use = "complete") #this is listwise
corr.test(takeh[,2:9], use = "pairwise")

###################PART B#######################
#9
modelp <- "schoolq =~ school + teach + accept
homeq =~ educate + income + involve
achieve ~ schoolq + homeq
CITO ~ achieve"
semPaths(modelp, residuals = F)

#specify the equations and the model
#school and home are latent vars because they are not observed, manifest vars are vars in the data file
modeleq <- "schoolq =~ 1*school + teach + accept
homeq =~ 1*educate + income + involve
homeq~~homeq
schoolq~~schoolq
schoolq~~homeq
educate~~educate
income~~income
involve~~involve
school~~school
teach~~teach
accept~~accept
accept~1
involve~1
school~1
teach~1
educate~1
income~1"

fit <- lavaan(modeleq, data = takeh, missing="FIML", estimator = "ML")
summary(fit, standardized = T, rsquare=T, fit.measures= T)

#plots
#semPaths(fit)
#semPaths(fit, residuals = F)
#semPaths(fit, what="std", residuals = F)
#semPaths(fit, what= "std", layout = "tree", rotation = 2, intercepts =T , residuals =F,  curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 8, sizeMan2 = 1.7)

##11
#goodness of fit
summary(fit, standardized = T, rsquare=T, fit.measures=T, modindices=T)

##12
#with robust estimator MLR
fit2 <- lavaan(modeleq, data = takeh, missing="FIML", estimator= "MLR")
summary(fit2, standardized = T, rsquare=T, fit.measures=T)

##13
#listwise deletion is default 
fit3 <- lavaan(modeleq, data = takeh, missing = "ML", estimator = "ML")
summary(fit3, standardized = T, rsquare=T, fit.measures=T)

##14

modelmax <- "schoolq =~ 1*school + teach + accept
homeq =~ 1*educate + income + involve
achieve ~ schoolq + homeq
CITO ~ achieve
homeq~~homeq
schoolq~~schoolq
educate~~educate
income~~income
involve~~involve
school~~school
teach~~teach
accept~~accept
accept~1
involve~1
school~1
teach~1
educate~1
income~1
CITO~1
achieve~1
CITO~~CITO
achieve~~achieve
homeq~~schoolq"



fitm <- lavaan(modelmax, data = takeh, missing="FIML", estimator= "ML")

##15
#goodness of fit
summary(fitm, standardized = T, rsquare=T, fit.measures=T)

##16
summary(fitm, standardized = T, rsquare=T, fit.measures=T, modindices=T)


##18

modelimp <- "schoolq =~ 1*school + teach + accept
homeq =~ 1*educate + income + involve
achieve ~ schoolq + homeq
CITO ~ achieve + schoolq +homeq
homeq~~homeq
schoolq~~schoolq
educate~~educate
income~~income
involve~~involve
school~~school
teach~~teach
accept~~accept
accept~1
involve~1
school~1
teach~1
educate~1
income~1
CITO~1
achieve~1
CITO~~CITO
achieve~~achieve
homeq~~schoolq
"


fitimp <- lavaan(modelimp, data = takeh, missing="FIML", estimator= "ML")

#19
summary(fitimp, standardized = T, rsquare=T, fit.measures=T)

semPaths(fitimp, what= "std", layout = "tree", rotation = 2, intercepts =T , residuals =F,  curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 8, sizeMan2 = 1.7, "Estimates")

# citations
setwd("C:\\Users\\André\\Desktop")
write_bib(x = .packages(all.available = T), file = "R-packages.bib",tweak = T)
