# open library packages
install.packages("purrr")
install.packages("stringr")
library(readxl)
library(tidyverse)
library(purrr)
library(pastecs)
library(stringr)

# file source, read file
file_source <- "/Users/gillian/Documents/Bellevue Grad Program/Fall 2022/DSC520/DSC520 Repo/week-6-housing.xlsx"
housing <- read_excel(file_source)

# remove spaces and replace with _
names(housing) <- gsub(" ", "_", names(housing))

# adding variables from last week's assignment
housing$bathrooms <- with(housing, bath_full_count + (bath_half_count * 0.5) + (bath_3qtr_count * 0.75))
housing$Sale_Year <- format(housing$Sale_Date, format="%Y")
head(housing)
colnames(housing)

# mutate function
housing %>% 
  mutate(price_per_sqft = Sale_Price / square_feet_total_living)

# select function
housing %>%
  select(matches("room"))

# filter function
housing %>% 
  filter(Sale_Price <= 300000, bedrooms > 1)

# summarize function
housing %>%
  summarize(AvgPrice = mean(Sale_Price), MedianPrice = median(Sale_Price), AvgBed = mean(bedrooms), AvgSqFt = mean(square_feet_total_living))

# group_by function
housing %>%
  group_by(Sale_Year) %>%
  summarize(AvgPrice = mean(Sale_Price))

# arrange function
housing %>%
  group_by(Sale_Year) %>%
  summarize(AvgPrice = mean(Sale_Price)) %>%
  arrange(AvgPrice)

# purr functions
housing2 <- compact(housing)
map(housing2, mean)
every(housing2$postalctyn, is.character)

# cbind 
price <- housing$Sale_Price
my_year <- housing$Sale_Year
sq_ft <- housing$square_feet_total_living

new_data1 <- cbind(price, sq_ft, my_year)
head(new_data1)

# rbind
jan_sales <- housing[format.Date(housing$Sale_Date, "%m")=="01",]
dec_sales <- housing[format.Date(housing$Sale_Date, "%m")=="12",]

new_data2 <- rbind(jan_sales, dec_sales)
head(new_data2)

# select string, split, concatenate back together
addr <- housing$addr_full
addr1 <- addr[[sample(1:length(addr), 1)]]
x <- str_split(string = addr1, pattern = " ")
x <- unlist(x)
paste(x, collapse = " ")

