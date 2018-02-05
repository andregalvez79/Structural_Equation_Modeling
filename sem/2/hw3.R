cfadata <- read.csv2("C:\\Users\\s4600479\\Desktop\\cfa(2).csv", sep = ";")

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

#PART A
# specify the model
#its similar to pca but we kinda already know the theory and what the items represent... so you create the factor burnout
#as a fn of the items
model1 <- ' burnout =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9' 
# fit the model 
fit <- cfa(model1, data=cfadata) 
# summarize output 
summary(fit, standardized = T, fit.measures=T)

#draw it
#the dotted line is the fixed variable to the factor which is the default (always the first one)
semPaths(fit, what= "std", layout = "tree", rotation = 2, intercepts = F, residuals = F, curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 10, sizeMan2 = 5)

#PART B
#next model with specific factors and items
model2 <- ' ee =~ x1 + x2 + x3 
cy=~ x4 + x5 + x6 
rpa=~ x7 + x8 + x9' 
# fit the model 
fit2 <- cfa(model2, data=cfadata) 
# summarize output 
summary(fit2, standardized = T, fit.measures=T)
#draw it
semPaths(fit2, what= "std", layout = "tree", rotation = 2, intercepts = F, residuals = F, curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 10, sizeMan2 = 5)

#do the same but with lavaan compute covariances 
#the rpa~~ee for example just tells the model to make a relationship between factors
model3 <- ' ee =~ 1*x1 + x2 + x3 
cy=~ 1*x4 + x5 + x6 
rpa=~ 1*x7 + x8 + x9
ee~~cy
ee~~rpa
cy~~rpa
x1~~x1
x2~~x2
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x8~~x8
x9~~x9'

fit3 <- sem(model3, data = cfadata, missing="FIML")
summary(fit3, standardized = T, rsquare=T, fit.measures=T)

#some plots
semPaths(fit3)
semPaths(fit3, residuals = F)
semPaths(fit3, what="std", residuals = F)
semPaths(fit3, what= "std", layout = "tree", rotation = 2, intercepts = F, residuals = F, curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 10, sizeMan2 = 5)

#Part C

#so here we want to create an underlying concept "burnout" that is going to be correlated to the 3 factors
#we shouldn't add correlations between the factors because they will be "consindered"in the relationship with 
#the big concept burnout

model2 <- ' ee =~ x1 + x2 + x3 
cy=~ x4 + x5 + x6 
rpa=~ x7 + x8 + x9' 
# fit the model 
fit2 <- cfa(model2, data=cfadata) 
# summarize output 
summary(fit2, standardized = T, fit.measures=T)
#draw it
semPaths(fit2, what= "std", layout = "tree", rotation = 2, intercepts = F, residuals = F, curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 10, sizeMan2 = 5)

#do the same but with lavaan compute covariances 
#the rpa~~ee for example just tells the model to make a relationship between factors
model4 <- ' ee =~ 1*x1 + x2 + x3 
cy=~ 1*x4 + x5 + x6 
rpa=~ 1*x7 + x8 + x9
burnout=~ ee + cy + rpa
x1~~x1
x2~~x2
x3~~x3
x4~~x4
x5~~x5
x6~~x6
x7~~x7
x8~~x8
x9~~x9'

fit4 <- sem(model4, data = cfadata, missing="FIML")
summary(fit4, standardized = T, rsquare=T, fit.measures=T)

#some plots
semPaths(fit4)
semPaths(fit4, residuals = F)
semPaths(fit4, what="std", residuals = F)
semPaths(fit4, what= "std", layout = "tree2", rotation = 2, intercepts = F, residuals = F, curve = 2, nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 3, sizeMan2 = 3, sizeLat = 6)
