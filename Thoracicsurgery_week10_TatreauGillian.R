install.packages("foreign")
install.packages("caTools")
library(ggplot2)
library(foreign)
library(caTools)

setwd("/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo")

file_name <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/ThoraricSurgery.arff"
surgery <- read.arff(file_name) 
head(surgery)

split <- sample.split(surgery, SplitRatio = 0.8)
split
train <- subset(surgery, split == "TRUE")
test <- subset(surgery, split == "FALSE")

is.na(surgery)
nrow(surgery)
colSums(is.na(surgery))

model1 <- glm(Risk1Yr ~ AGE, data = train, family = binomial())
model2 <- glm(Risk1Yr ~ AGE + DGN, data = train, family = binomial())
model3 <- glm(Risk1Yr ~ AGE + DGN + PRE4, data = train, family = binomial())
model4 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5, data = train, family = binomial())
model5 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6, data = train, family = binomial())
model6 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7, data = train, family = binomial())
model7 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8, data = train, family = binomial())
model8 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9, data = train, family = binomial())
model9 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10, data = train, family = binomial())
model10 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11, data = train, family = binomial())
model11 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14, data = train, family = binomial())
model12 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17, data = train, family = binomial())
model13 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19, data = train, family = binomial())
model14 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19 + PRE25, data = train, family = binomial())
model15 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19 + PRE25 + PRE30, data = train, family = binomial())
model16 <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 + PRE10 + PRE11 + PRE14 + PRE17 + PRE19 + PRE25 + PRE30 + PRE32, data = train, family = binomial())

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)

model17 <- glm(Risk1Yr ~ PRE14 + PRE9 + PRE17 + PRE30 + PRE4 + PRE5 + PRE6 + AGE, data = train, family = binomial())
summary(model17)

# compare model 1 and model 15
modelChi1 <- model1$deviance - model15$deviance
chidf1 <- model1$df.residual - model15$df.residual
chisq.prob1 <- 1 - pchisq(modelChi1, chidf1)
modelChi1; chidf1; chisq.prob1

# compare model 1 and model 17
modelChi2 <- model1$deviance - model17$deviance
chidf2 <- model1$df.residual - model17$df.residual
chisq.prob2 <- 1 - pchisq(modelChi2, chidf2)
modelChi2; chidf2; chisq.prob2

# compare model 17 and model 15
modelChi3 <- model17$deviance - model15$deviance
chidf3 <- model17$df.residual - model15$df.residual
chisq.prob3 <- 1 - pchisq(modelChi3, chidf3)
modelChi3; chidf3; chisq.prob3

res <- predict(model17, test, type = "response")
res
confmatrix <- table(Actual_Value = test$Risk1Yr, Predicted_Value = res > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)


res <- predict(model17, train, type = "response")
res

confmatrix <- table(Actual_Value = train$Risk1Yr, Predicted_Value = res > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
