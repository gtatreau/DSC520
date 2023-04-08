# import packages
library(ggplot2)
library(pastecs)

# save file directory and read file
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/student-survey.csv"
survey <- read.table(file = file_source, header = TRUE, sep = ",")
head(survey)

# covariance of time spent reading and time spent watching TV
cov(x = survey$TimeReading, y = survey$TimeTV)

# example for changing measurement of time watching TV
survey$TimeTV_min <- survey$TimeTV/60
cov(x = survey$TimeReading, y = survey$TimeTV_min)

# test for normality
stat.desc(survey["TimeReading"], norm = TRUE, options(digits = 2))
stat.desc(survey["TimeTV_min"], norm = TRUE, options(digits = 2))

# correlation for all variables
round(cor(survey), digits = 2)

# correlation for two variables (time watching TV and happiness)
cor(x = survey$TimeTV, y = survey$Happiness, method = "pearson")

# correlation for two variables, CI at 99%
cor.test(x = survey$TimeTV, y = survey$Happiness, method = "pearson", conf.level = .95)
cor.test(x = survey$TimeTV, y = survey$Happiness, method = "pearson", conf.level = .99)

# correlation coefficient and coefficient of determination
cor.test(x = survey$TimeReading, y = survey$TimeTV, method = "pearson", conf.level = .95)
cor(x = survey$TimeReading, y = survey$TimeTV, method = "pearson")^2

# install packages
install.packages("ppcor")
library(ppcor)
survey2 <- data.frame(survey$TimeReading, survey$TimeTV, survey$Happiness)
pcor(survey2, method = "pearson")
