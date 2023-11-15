# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# use a loop to load all datasets 
i <- 1
for (salesdata in all_sales){
  datafile <- read_csv(paste0("./sales/", salesdata))
  assign(paste0("dataset_", i), datafile)
  i <- i + 1
}

############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code
# Load necessary libraries
library(dplyr)
# I tried to do it this way first and it did not work.
salesdata<-full_join(datafile, dataset_1, dataset_10, dataset_11, dataset_12,
                     dataset_13, dataset_14, dataset_15, dataset_16,)

# looking online led me here- it works but seems inefficient. 
sales_data <- full_join(dataset_1, dataset_10) %>%
  full_join(dataset_11) %>%
  full_join(dataset_12) %>%
  full_join(dataset_13) %>%
  full_join(dataset_14) %>%
  full_join(dataset_15) %>%
  full_join(dataset_16) %>%
  full_join(dataset_2) %>%
  full_join(dataset_3) %>%
  full_join(dataset_4) %>%
  full_join(dataset_5) %>%
  full_join(dataset_6) %>%
  full_join(dataset_7) %>%
  full_join(dataset_8) %>%
  full_join(dataset_9)
# error messege ℹ If a many-to-many relationship is expected, set `relationship =
# "many-to-many"` to silence this warning.
# This warning is R saying there may be many variables that match when joining the data
# in two datasets. 
########################################

##### 3. Create summaries #####
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")
# There are too many observations, it should not be over 2000.

# Daily sales
# Create "proper" dates
sales_data$date <- ymd(paste(sales_data$year, "/", sales_data$month, "/", sales_data$day))

# Summarize data
sales_summary_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(total_sales = sum(number))

# Plot
ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, color = pizza))+
  geom_line()

ggplot(data = sales_summary_daily, aes(x = date, y = total_sales, fill = pizza))+
  geom_bar(stat = "identity")

# Average data
sales_ave_daily <- sales_data %>%
  group_by(pizza, date) %>% 
  summarize(ave_sales = mean(number))

ggplot(data = sales_ave_daily, aes(x = date, y = ave_sales, fill = pizza))+
  geom_bar(stat = "identity", position = "dodge")

# The data in these figures do not make any sense for the actual data
# and the ylim is too high. 