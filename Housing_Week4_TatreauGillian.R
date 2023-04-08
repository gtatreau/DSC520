# import packages
library(readxl)
library(ggplot2)
library(plyr)
library(pastecs)
library(ggplot2)
library(dplyr)

# file source, read file
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/week-6-housing.xlsx"
housing <- read_excel(file_source)

# Sale Year
housing$Sale_Year <- format(housing$`Sale Date`, format="%Y")
head(housing$Sale_Year)

# Bathroom
housing$bathrooms <- with(housing, bath_full_count + (bath_half_count * 0.5) + (bath_3qtr_count * 0.75))
head(housing$bathrooms)

# Outside space
housing$sq_ft_outside <- with(housing, sq_ft_lot - square_feet_total_living)
head(housing$sq_ft_outside)


# apply function
apply(housing, 2, summary)

# aggregate function
aggregate(housing$`Sale Price` ~ housing$bedrooms + housing$bathrooms, housing, mean)

# plyr function
agg <- function(data)
{
  aggregate(housing$`Sale Price` ~ housing$sq_ft_outside, housing, mean)
}

Price_by_year <- ddply(housing, .variables = housing$Sale_Year, .fun = agg)
head(Price_by_year)

# distribution
stat.desc(housing$`Sale Price`, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = `Sale Price`), bins = 15) + xlab("Sale Price") + ylab("Frequency")
stat.desc(housing$square_feet_total_living, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = square_feet_total_living), bins = 15) + xlab("Number of Square Feet") + ylab("Frequency")
stat.desc(housing$bedrooms, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = bedrooms), bins = 15) + xlab("Number of bedrooms") + ylab("Frequency")
stat.desc(housing$bathrooms, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = bathrooms), bins = 24) + xlab("Number of bathrooms") + ylab("Frequency")
stat.desc(housing$year_built, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = year_built), bins = 15) + xlab("Year House was Built") + ylab("Frequency")

#create box plot and label outliers
housing$Sale_Year <- as.factor(housing$Sale_Year)
ggplot(housing, aes(x = Sale_Year, y = `Sale Price`)) + 
  geom_boxplot(outlier.colour="red")
ggplot(housing, aes(x = Sale_Year, y = square_feet_total_living)) + 
  geom_boxplot(outlier.colour="red")
ggplot(housing, aes(x = Sale_Year, y = bedrooms)) + 
  geom_boxplot(outlier.colour="red")
ggplot(housing, aes(x = Sale_Year, y = bathrooms)) + 
  geom_boxplot(outlier.colour="red")

# outliers by IQR, Sale Price
summary(housing$`Sale Price`)
tmin_price = 460000 - (1.5 * IQR(housing$`Sale Price`)) 
tmax_price = 750000 + (1.5 * IQR(housing$`Sale Price`)) 
housing$`Sale Price`[which(housing$`Sale Price` < tmin_price | housing$`Sale Price` > tmax_price)]

# outliers by IQR, sq.ft
summary(housing$square_feet_total_living)
tmin_sqft = 1820 - (1.5 * IQR(housing$square_feet_total_living)) 
tmax_sqft = 3110 + (1.5 * IQR(housing$square_feet_total_living)) 
housing$square_feet_total_living[which(housing$square_feet_total_living < tmin_sqft | housing$square_feet_total_living > tmax_sqft)]

# outliers by IQR, bedrooms
summary(housing$bedrooms)
tmin_bed = 3 - (1.5 * IQR(housing$bedrooms)) 
tmax_bed = 4 + (1.5 * IQR(housing$bedrooms)) 
housing$bedrooms[which(housing$bedrooms < tmin_bed | housing$bedrooms > tmax_bed)]

# outliers by IQR, bathrooms
summary(housing$bathrooms)
tmin_bath = 2.2 - (1.5 * IQR(housing$bathrooms)) 
tmax_bath = 2.5 + (1.5 * IQR(housing$bathrooms)) 
housing$bathrooms[which(housing$bathrooms < tmin_bath | housing$bathrooms > tmax_bath)]

