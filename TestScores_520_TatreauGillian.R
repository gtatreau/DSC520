# import packages
library(ggplot2)
library(pastecs)

# file source and read file to variable
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/scores.csv"
scores <- read.table(file = file_source, header = TRUE, sep = ",")
head(scores)

# create variables to hold just regular or just sports section
regular <- scores[ scores$Section == "Regular", ]
head(regular)
sports <- scores[ scores$Section == "Sports", ]
head(sports)

# plot scores to number of students with that score
ggplot(data = scores) + geom_point(aes(x = Score, y = Count, color = Section)) + ggtitle("Scores for Both Sections") + xlab("Scores") + ylab("Number of students with score")
ggplot(data = regular) + geom_point(aes(x = Score, y = Count)) + ggtitle("Scores for Regular Section") + xlab("Scores") + ylab("Number of students with score")
ggplot(data = sports) + geom_point(aes(x = Score, y = Count)) + ggtitle("Scores for Sports Section") + xlab("Scores") + ylab("Number of students with score")

# central tendency calculations
stat.desc(regular, options(digits = 2))
stat.desc(sports, options(digits = 2))

