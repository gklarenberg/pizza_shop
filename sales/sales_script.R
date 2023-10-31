# Pizza sales

##### 1. Load packages #######
library(tidyverse)
library(lubridate)

##### 2. Load data #######
all_sales <- list.files(path = "./sales", pattern = ".csv")

# use a loop to load all datasets 
i <- 1
datafile_list <- c()
for (salesdata in all_sales){
  datafile <- read_csv(paste0("./sales/", salesdata))
  assign(paste0("dataset_", i), datafile)
  data_name <- datafile
  i <- i + 1
  datafile_list <- c(datafile_list, data_name)
}

############## JOIN ###############
# Use a tidyverse join to join all the data together into one file
# called sales_data, then run the rest of the code

#Attempted some garbage
#datafile_list
#datafile_list<- datafile_list[-1]
#sales_data <- dataset_1
#for (i in datafile_list) {
#  sales_data <- full_join(sales_data,datafile_list[i])
#}
#sales_data
#str(sales_data)

#str(datafile_list)

#actual working code
sales_data <- full_join(dataset_1, dataset_2)
sales_data <- full_join(sales_data, dataset_3)
sales_data <- full_join(sales_data, dataset_4)
sales_data <- full_join(sales_data, dataset_5)
sales_data <- full_join(sales_data, dataset_6)
sales_data <- full_join(sales_data, dataset_7)
sales_data <- full_join(sales_data, dataset_8)
sales_data <- full_join(sales_data, dataset_9)
sales_data <- full_join(sales_data, dataset_10)
sales_data <- full_join(sales_data, dataset_11)
sales_data <- full_join(sales_data, dataset_12)
sales_data <- full_join(sales_data, dataset_13)
sales_data <- full_join(sales_data, dataset_14)
sales_data <- full_join(sales_data, dataset_15)
sales_data <- full_join(sales_data, dataset_16)

str(sales_data)
#errors include multiple values for same dates

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

#Answers: 
# The final dataset has 6 columns because dataset 15 has 6 columns 

# If the pizzeria only sells 30 pizza a day, some of the 
#dates for the pizza types are showing over 100 for that day
#which could make the data turn out very strange