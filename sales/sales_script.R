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
sales_data_full <- full_join(datafile,dataset_1, by = c("day","month","year",
                                                        "pizza","number")) 
sales_data_full <- full_join(sales_data_full,dataset_2, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_3, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_4, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_5, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_6, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_7, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_8, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_9, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_10, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_11, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_12, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_13, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_14, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_15, 
                             by = c("day","month","year",
                                    "pizza","number"))
sales_data_full <- full_join(sales_data_full,dataset_16, 
                             by = c("day","month","year",
                                    "pizza","number"))
head(sales_data_full)
tail(sales_data_full)
sales_data <- sales_data_full

## The warning message that R gives us prompts the user to verify the 
## the relationship between x and y. Specifically this warning is becuase the 
## pizza number variable for each of the data tables being joined are often 
## very similar. For example, the first vegetarian pizza sold on two seperate 
## days will both recieve the pizza number of 1. R sees this as a "many-to-
## many" relationship, which triggers the warning message. 

## The final dataset has 6 columns because "dataset_15" contained 6 columns,
## unlike the other datasets which only contained 5. The full_join function 
## implies that we want to keep ALL information, so it created an extra column
## for all datasets used in the full_join. Where they didnt have a value for that
## column, R assigns "NA".

## This data looks misleading at first glance because of the way the data is 
## keeping track of the pizza number variable. It seems that the pizza number
## is stating what number of that type of pizza sold on that day, not that the
## pizzeria sold more than one pizza per row. 
########################################

##### 3. Create summaries #####
sales_summary <- sales_data %>%
  group_by(pizza, month) %>% 
  summarize(total_sales = sum(number))

ggplot(data = sales_summary, aes(x = pizza, y = total_sales))+
  geom_bar(stat = "identity")

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

