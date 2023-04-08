# import packages
library(readxl)
library(ggplot2)
library(plyr)
library(pastecs)
library(ggplot2)
library(dplyr)
library(QuantPsyc)
library(car)

# file source, read file
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/week-6-housing.xlsx"
housing <- read_excel(file_source)

# remove spaces in column names
names(housing) <- gsub(" ", "_", names(housing))
colnames(housing)

# Bathrooms
housing$bathrooms <- with(housing, bath_full_count + (bath_half_count * 0.5) + (bath_3qtr_count * 0.75))

# Sale Year
housing$Sale_Year <- format(housing$Sale_Date, format="%Y")

# Outside space
housing$sq_ft_outside <- with(housing, sq_ft_lot - square_feet_total_living)

# log transform of sale price
housing$log_price <- log(housing$Sale_Price)
stat.desc(housing$log_price, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = log_price), bins = 15) + xlab("Sale Price w/log transform") + ylab("Frequency")
ggplot(data = housing) + geom_histogram(aes(x = Sale_Price), bins = 15) + xlab("Sale Price w/o log transform") + ylab("Frequency")

# log transform of square feet
housing$log_sqft <- log(housing$square_feet_total_living)
stat.desc(housing$log_sqft, options(digits = 2))
ggplot(data = housing) + geom_histogram(aes(x = log_sqft), bins = 15) + xlab("Square Feet w/log transform") + ylab("Frequency")
ggplot(data = housing) + geom_histogram(aes(x = square_feet_total_living), bins = 15) + xlab("Square Feet w/o log transform") + ylab("Frequency")

# variables
price <- housing$log_price
year1 <- housing$Sale_Year
sqft <- housing$log_sqft
bed <- housing$bedrooms
bath <- housing$bathrooms
year2 <- housing$year_built

# simple regression
simple_lm <- lm(Sale_Price ~ square_feet_total_living, data = housing)
summary(simple_lm)
price_predict_df <- data.frame(sale_price = predict(simple_lm), square_feet=housing$square_feet_total_living)
ggplot(data = housing, aes(y = Sale_Price, x = square_feet_total_living)) +
  geom_point(color='blue') +
  geom_line(color='red',data = price_predict_df, aes(y=sale_price, x=square_feet))

# with log
simplelog_lm <- lm(price ~ sqft, data = housing)
summary(simplelog_lm)
pricelog_predict_df <- data.frame(logsale_price = predict(simplelog_lm), logsquare_feet=sqft)
ggplot(data = housing, aes(y = price, x = sqft)) +
  geom_point(color='blue') +
  geom_line(color='red',data = pricelog_predict_df, aes(y=logsale_price, x=logsquare_feet))

# data frame with only numeric columns
housing2 <- housing
housing2 %>% select(-c(sale_warning, sitetype, addr_full, ctyname, postalctyn, current_zoning, prop_type, Sale_Year, Sale_Date))

# correlation to see which variables to use as predictors
cor(price, sqft)
cor(price, bed)
cor(price, bath)
cor(price, as.numeric(year1))
cor(price, year2)
cor(price, as.numeric(housing$Sale_Date))
cor(price, housing$sq_ft_outside)


# multiple regression
multiple_lm <- lm(price ~ sqft + bed + bath + year2 + as.numeric(year1), data = housing)
summary(multiple_lm)
mult_price_predict <- data.frame(
  sale_price = predict(multiple_lm),
  square_feet=sqft, bed=bed, bath=bath,
  year2=year2, year1 = as.numeric(year1))

# standardized betas
lm.beta(multiple_lm)

# confidence intervals
confint(multiple_lm)

# analysis of variance
anova(simplelog_lm, multiple_lm)

# case-wise diagnostics for outliers/influential cases
housing$residuals <- resid(multiple_lm)
housing$stan_resid <- rstandard(multiple_lm)
housing$cooks <- cooks.distance(multiple_lm)
housing$leverage <- hatvalues(multiple_lm)
housing$covariance <- covratio(multiple_lm)
head(housing[c("residuals", "stan_resid", "cooks", "leverage", "covariance")], n=10)

# large residuals
housing$large_resid <- housing$stan_resid > 2 | housing$stan_resid < -2
head(housing$large_resid, n = 30)

# sum of large residuals
sum(housing$large_resid)

# which variables have large residuals
housing[housing$large_resid, c("log_price", "log_sqft", "Sale_Year", "bedrooms", "bathrooms", "year_built", "stan_resid")]

# leverage, Cook's distance, covariance ratios, k = 5
housing[housing$large_resid, c("leverage", "cooks", "covariance")]
housing$cooksprob <- housing$cooks > 1
avelev <- 6 / 12865
housing$badlev <- housing$leverage > (3 * avelev)
highcov <- 1 + ((3 * 6) / 12865)
lowcov <- 1 - ((3 * 6) / 12865)
housing$covissue <- housing$covariance > highcov | housing$covariance < lowcov
sum(housing$covissue)
sum(housing$badlev)
sum(housing$cooksprob)

housing[housing$large_resid, c("badlev", "cooksprob", "covissue")]

# assumption of independence
durbinWatsonTest(multiple_lm)

# assumption of no multicollinearity
vif(multiple_lm)
mean(vif(multiple_lm))
1/vif(multiple_lm)

# assumptions related to residuals
housing$fitted <- multiple_lm$fitted.values

plot(multiple_lm)
hist(housing$stan_resid)
