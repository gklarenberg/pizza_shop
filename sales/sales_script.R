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
all_sales_df <- datafile %>% 
  full_join(dataset_1) %>% 
  full_join(dataset_2) %>% 
  full_join(dataset_3) %>% 
  full_join(dataset_4) %>% 
  full_join(dataset_5) %>% 
  full_join(dataset_6) %>% 
  full_join(dataset_7) %>% 
  full_join(dataset_8) %>% 
  full_join(dataset_9) %>% 
  full_join(dataset_10) %>% 
  full_join(dataset_11) %>% 
  full_join(dataset_12) %>% 
  full_join(dataset_13) %>% 
  full_join(dataset_14) %>% 
  full_join(dataset_15) %>% 
  full_join(dataset_16)

## My dataframe upon full_join has 841 rows and 6 columns.

#### Warning Messages ####
# Warning messages:
#   1: In full_join(., dataset_3) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 125 of `x` matches multiple rows in `y`.
# ℹ Row 1 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# 2: In full_join(., dataset_5) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 242 of `x` matches multiple rows in `y`.
# ℹ Row 15 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# 3: In full_join(., dataset_11) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 630 of `x` matches multiple rows in `y`.
# ℹ Row 18 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# 4: In full_join(., dataset_12) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 635 of `x` matches multiple rows in `y`.
# ℹ Row 29 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# 5: In full_join(., dataset_16) :
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 16 of `x` matches multiple rows in `y`.
# ℹ Row 16 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.





##1c. The final dataset has 6 columns because full_join is going to merge the 
## datasets based on all the rows and columns present in all the datasets given. So,
##among the given datasets, dataset_15 does have 6 columns. Hence, the final 
## combined dataset has 6 columns too.

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