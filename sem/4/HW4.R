install.packages("lavaan")
library(lavaan)

install.packages("semPlot")
library(semPlot)

options (scipen = 12)

#data from corr matrix

matrix <- '
1.00
-0.13   1.00
-0.29   0.34   1.00
0.39   -0.05  -0.08  1.0
-0.03  -0.23  -0.16 -0.03  1.0'

mcov <- getCov(matrix, lower = T, diagonal=T, sds = c(36.80, 67.00, 62.48, 66.50, 37.95), names = c("fitness", "stress" , "health", "excercise", "hardiness"))


#2
modelhyp <- "health ~ c1hardiness
health ~ c2excercise
fitness ~ a12*excercise
stress ~ a21*excercise
fitness ~ a11*hardiness
stress ~ a21hardiness
health ~ b1*fitness
health ~ b2*stress"

fit <- sem(modelhyp, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(fit, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)

###with lavaan

model1 <- "health ~ c1*hardiness
health ~ c2*excercise

#indirect mediator path a
fitness ~ a12*excercise
stress ~ a11*excercise
fitness ~ a21*hardiness
stress ~ a22*hardiness

#mediator path b
health ~ b1*fitness
health ~ b2*stress

#covariance
excercise ~~ hardiness

#intercept
excercise ~1
hardiness ~1
health ~1
fitness ~1
stress ~1

#variances
health ~~ health
stress~~ stress
fitness~~fitness
hardiness~~hardiness
excercise~~excercise

#multypling effects
a11b2 := a11*b2
a12b1 := a12*b1
a22b2 := a22*b2
a21b1 := a21*b1
totala21b1 := c1 + (a21*b1)
totala22b2 := c1 + (a22*b2)
totala12b1 := c2 + (a12*b1)
totala11b2 := c2 + (a11*b2)"

           
fit2 <- lavaan(model1, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 

summary(fit2, standardized = T, rsquare=T, fit.measures= T, modindices=T)

semPaths(fit2, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)

#8

m1 <- "fitness ~ b*excercise
excercise ~ a*health
fitness~c*health
ab := a*b"

m1 <- sem(m1, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m1, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m1, standardized = T, rsquare=T, fit.measures= T, modindices=T)



m2 <- "health ~ b*excercise
excercise ~ a*fitness
health~c*fitness
ab := a*b"

m2 <- sem(m2, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m2, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m2, standardized = T, rsquare=T, fit.measures= T, modindices=T)



m3 <- "fitness ~ a*excercise
health ~ c*excercise
fitness ~ b*health
ab := a*b"

m3 <- sem(m3, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m3, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m3, standardized = T, rsquare=T, fit.measures= T, modindices=T)



m4 <- "excercise ~ b*fitness
excercise ~ c*health
fitness~a*health
ab := a*b"

m4 <- sem(m4, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m4, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m4, standardized = T, rsquare=T, fit.measures= T, modindices=T)



m5 <- "excercise ~ b*health
excercise ~ c*fitness
health~a*fitness
ab := a*b"

m5 <- sem(m5, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m5, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m5, standardized = T, rsquare=T, fit.measures= T, modindices=T)



m6 <- "fitness ~ b*health
health ~ a*excercise
fitness~c*excercise
ab := a*b"

m6 <- sem(m6, sample.cov = mcov, sample.nobs = 406, missing="FIML", estimator = "ML", fixed.x = F) 
semPaths(m6, layout = "tree", rotation = 2, intercepts = F , residuals =F,  curve = 1, curvePivot = TRUE ,nCharNodes = 0, edge.label.cex = 1, edge.color = "black", sizeMan = 5, sizeMan2 = 3)
summary(m6, standardized = T, rsquare=T, fit.measures= T, modindices=T)