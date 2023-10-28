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

# get the list of dataframes to join
df_list <- list()
for (i in 1:16){
  df_list[[i]] <- get(paste0("dataset_", i))
}

# join list of dataframes
sales_data <- Reduce(function(x, y) full_join(x, y), df_list)

# Warning messages:
#   1: In full_join(x, y) :
#   Detected an unexpected many-to-many relationship between
# `x` and `y`.
# ℹ Row 63 of `x` matches multiple rows in `y`.
# ℹ Row 1 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set
# `relationship = "many-to-many"` to silence this warning.
# 2: In full_join(x, y) :
#   Detected an unexpected many-to-many relationship between
# `x` and `y`.
# ℹ Row 180 of `x` matches multiple rows in `y`.
# ℹ Row 15 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set
# `relationship = "many-to-many"` to silence this warning.
# 3: In full_join(x, y) :
#   Detected an unexpected many-to-many relationship between
# `x` and `y`.
# ℹ Row 568 of `x` matches multiple rows in `y`.
# ℹ Row 18 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set
# `relationship = "many-to-many"` to silence this warning.
# 4: In full_join(x, y) :
#   Detected an unexpected many-to-many relationship between
# `x` and `y`.
# ℹ Row 573 of `x` matches multiple rows in `y`.
# ℹ Row 29 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set
# `relationship = "many-to-many"` to silence this warning.

# there seems to be rows that match multipel other rows during a number  of the 
# joins that were done. This is due to many-to-many relationships in the data.

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

# 1.c.a
# There is one of the datasets (dataset_15) has 6 columns instead of 5.

# 1.c.b
# There seems to be irregular sales of pizzas. There are huge spikes of pizzas
# sold on some days and none on most others.