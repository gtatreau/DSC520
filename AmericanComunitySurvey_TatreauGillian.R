# import packages
library(ggplot2)
library(pastecs)

# save file directory and read file
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/Week3_American_community_survey.csv"
survey <- read.table(file = file_source, header = TRUE, sep = ",")
head(survey)

# list of column names
colnames(survey)

# structure of data
str(survey)
nrow(survey)
ncol(survey)

# histogram of HSDegree variable
# use 12 bins (Sturges' law: 136 observations so sq. rt. of 136 is 11.66 so round to 12)
ggplot(data = survey) + geom_histogram(aes(x = HSDegree), bins = 12) + ggtitle("Percentage of Population with High School Degree per County") + xlab("Percent of Population with High School Degree") + ylab("Frequency")

# add normal curve to histogram
ggplot(data = survey, aes(x = HSDegree)) + geom_histogram(aes(y = ..density..), bins = 12) + ggtitle("Percentage of Population with High School Degree per County") + xlab("Percent of Population with High School Degree") + ylab("Density") + geom_density()

# probability plot of HSDegree variable
ggplot(data = survey, mapping = aes(sample = HSDegree)) + stat_qq(geom = "point", color = "grey50") + stat_qq_line(color = "red") + ggtitle("Normal Probability Plot of Percent of Population with High School Degrees") + xlab("Theoretical Quantiles") + ylab("High School Degree Quantiles")
# test for log-normal distribution
ggplot(data = survey, mapping = aes(sample = log(HSDegree))) + stat_qq(geom = "point", color = "grey50") + stat_qq_line(color = "red") + ggtitle("Normal Probability Plot of Percent of Population with High School Degrees") + xlab("Theoretical Quantiles") + ylab("High School Degree Quantiles")


# quantify normality
stat.desc(survey, options(digits = 2))
stat.desc(survey["HSDegree"], norm = TRUE, options(digits = 2))
