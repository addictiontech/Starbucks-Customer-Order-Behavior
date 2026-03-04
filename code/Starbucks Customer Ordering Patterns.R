install.packages("RKaggle")
library(RKaggle)
library(tidyverse)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(janitor)
library(lubridate)
library(readr)

starbuck_customer_data <- get_dataset("likithagedipudi/starbucks-customer-ordering-patterns")
head(starbuck_customer_data)
summary(starbuck_customer_data)


#average order starbuck
starbuck_average_ordered <- starbuck_customer_data %>% 
  summarise(mean_item = round(mean(cart_size, na.rm = TRUE)))
print (starbuck_average_ordered)

#customer age distribution
age_distribution <- starbuck_customer_data %>% 
  count(customer_age_group, name = "number") %>% 
  mutate(percentage = round(number / sum(number) * 100, 2))

print(age_distribution)

# Classification digital vs physical
starbuck_customer_data <- starbuck_customer_data %>%
  mutate(channel_type = case_when(
    order_channel %in% c("Mobile App", "Kiosk") ~ "Digital",
    order_channel %in% c("Cashier", "Drive-Thru") ~ "Physical"
  )) %>% 
  filter(!is.na(channel_type))

# Print terpisah
print(unique(starbuck_customer_data$channel_type))
  
migration_analysis <- starbuck_customer_data %>%
    group_by(customer_age_group, channel_type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(customer_age_group) %>%
    mutate(percentage = round(count / sum(count) * 100, 2)) %>%
    arrange(customer_age_group)
  
  print(migration_analysis)

#visualization of digital adoption based on age group
  ggplot(migration_analysis, 
         aes(x = customer_age_group, 
             y = percentage, 
             fill = channel_type)) +
    
    geom_col(position = "dodge", width = 0.7) +
    
    geom_text(aes(label = paste0(percentage, "%")), 
              position = position_dodge(width = 0.7),
              vjust = -0.5,
              fontface = "bold") +
    
    theme_minimal(base_size = 14) +
    
    labs(
      title = "Digital vs Physical Channel Usage by Age Group",
      x = "Age Group",
      y = "Percentage (%)",
      fill = "Channel Type"
    )

#predictive Analysis using RFM score
rfm_data <- starbuck_customer_data %>%
  group_by(customer_id) %>%
  summarise(
    Recency = as.numeric(as.Date("2024-01-01") - max(as.Date(order_date))), # Hari sejak order terakhir
    Frequency = n(),                                                        # Total kunjungan
    Monetary = sum(total_spend, na.rm = TRUE),                              # Total pengeluaran
    Avg_Customization = mean(num_customizations, na.rm = TRUE)              # Tambahan untuk Coffee Enthusiast
  )

#coffee vs morning Commuters data
rfm_segments <- rfm_data %>%
  mutate(persona = case_when(
    Frequency >= 6 & Avg_Customization >= 2.0 ~ "Coffee Enthusiast",
    Frequency >= 6 & Avg_Customization < 2.0 ~ "Morning Commuter",
    Frequency < 6 & Frequency >= 3 ~ "Regular Customer" 
  ))%>%
  filter(!is.na(persona))

#coffee vs morning Commuters data visualization 
ggplot(rfm_segments, aes(x = Frequency, y = Monetary, color = persona)) +
  geom_point(aes(size = Avg_Customization), alpha = 0.6) +
  theme_minimal() +
  # Menggunakan warna brand Starbucks agar lebih profesional
  scale_color_manual(values = c(
    "Coffee Enthusiast" = "#00704A", 
    "Morning Commuter" = "#EAC117", 
    "Regular Customer" = "#607D8B"
  )) +
  labs(
    title = "Customer Persona Segmentation",
    subtitle = "Coffee Enthusiasts (High Customization) vs Morning Commuters",
    x = "Visit Frequency",
    y = "Total Spending (Monetary)",
    color = "Persona Group",
    size = "Avg Customization"
  )
#regression models to predict total_spend based on time of day, store location type, and customization counts.
#multiple model regression

# 1. Preparing data with clean time grouping
reg_data <- starbuck_customer_data %>% 
  mutate(
    # Take the first 2 digits of the hour (e.g., “08” from “08:00”)
    hour = as.numeric(substr(order_time, 1, 2)),
    # Creating time categories
    time_category = case_when(
      hour >= 5 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 17 ~ "Afternoon",
      TRUE ~ "Evening"
    ),
    # Converting variables into factors    
    time_category = as.factor(time_category),
    store_location_type = as.factor(store_location_type)
  ) %>%
  # Ensure there are no empty lines that cause variable length errors
  filter(!is.na(total_spend), !is.na(num_customizations))

# 2. Build the model using ‘time_category’ instead of the messy ‘order_time’.
revenue_model <- lm(total_spend ~ time_category + store_location_type + num_customizations, 
                    data = reg_data)

# 3. See the results
summary(revenue_model)

#visualization
reg_data$predicted_spend <- predict(revenue_model)

ggplot(reg_data, aes(x = predicted_spend, y = total_spend)) +
  geom_point(alpha = 0.3, color = "#00704A") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Actual vs Predicted Spending",
       x = "Model Prediction ($)",
       y = "Actual Spending ($)")

#Analyze fulfillment_time_min to find operational inefficiencies between Drive-Thru and Mobile Order-Ahead during peak morning rushes.
# 1. Filter data for morning rush hour (6:00 a.m. to 10:00 a.m.)
# And compare Drive-Thru vs. Mobile Order-Ahead
morning_rush <- starbuck_customer_data %>%
  mutate(hour = as.numeric(substr(order_time, 1, 2))) %>%
  filter(hour >= 6 & hour <= 10) %>%
  filter(order_channel %in% c("Drive-Thru", "Mobile App"))

# 2. Calculate Bottleneck Statistics
bottleneck_analysis <- morning_rush %>%
  group_by(order_channel) %>%
  summarise(
    avg_wait_time = mean(fulfillment_time_min, na.rm = TRUE),
    max_wait_time = max(fulfillment_time_min, na.rm = TRUE),
    total_orders = n()
  )

print(bottleneck_analysis)

# 3. Visualization for Bottleneck Identification
ggplot(morning_rush, aes(x = order_channel, y = fulfillment_time_min, fill = order_channel)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  scale_fill_manual(values = c("Drive-Thru" = "#EAC117", "Mobile App" = "#00704A")) +
  theme_minimal() +
  labs(
    title = "Operational Bottleneck: Morning Rush (06:00 - 10:00)",
    subtitle = "Comparing Fulfillment Time: Drive-Thru vs Mobile App",
    x = "Order Channel",
    y = "Wait Time (Minutes)"
  )

# Loyalty Program Impact Study: Evaluate how "Rewards Member" status
#correlates with basket size, visit frequency, and overall customer satisfaction. 

#Member vs non-Member performance
loyalty_impact <- starbuck_customer_data %>% 
  group_by(is_rewards_member) %>% 
  summarise(
    avg_basket_size = mean(cart_size, na.rm = TRUE),
    avg_frequency = n() / n_distinct(customer_id),
    avg_spend = mean(total_spend, na.rm = TRUE),
    total_customers = n_distinct(customer_id)
  )
print(loyalty_impact)

#T Test to make sure the loyalty program make big difference to our customers
t_test_basket <- t.test(cart_size ~ is_rewards_member, data = starbuck_customer_data)
print(t_test_basket)

t_test_spend <- t.test(total_spend ~ is_rewards_member, data = starbuck_customer_data)
print(t_test_spend)


#visualization loyalty impact
# Grafik 1: Basket Size
ggplot(loyalty_impact, aes(x = is_rewards_member, y = avg_spend, fill = is_rewards_member)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Yes" = "#00704A", "No" = "#A6A6A6")) +
  theme_minimal() +
  labs(
    title = "Impact of Loyalty Program on Spending",
    subtitle = "Member vs Non-Member Average Total Spend",
    x = "Is Rewards Member?",
    y = "Average Spending ($)"
  ) +
  geom_text(aes(label = round(avg_spend, 2)), vjust = -0.5, fontface = "bold")

# Counting amount of customization per drink category
menu_analysis <-  starbuck_customer_data %>% 
  group_by(drink_category) %>% 
  summarise(
    avg_customization = mean(num_customizations, na.rm = TRUE),
    total_orders = n(),
    avg_spend = mean(total_spend, na.rm = TRUE)
  ) %>% 
  arrange(desc(avg_customization))
print(menu_analysis)

#visualization
ggplot(menu_analysis, aes(x = total_orders, y = avg_customization, size = avg_spend, label = drink_category)) +
  geom_point(alpha = 0.6, color = "#00704A") +
  geom_text(vjust = 1.5, size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(
    title = "Menu Optimization for Mobile-Only Features",
    subtitle = "Higher customization indicates better fit for Digital App",
    x = "Total Orders (Popularity)",
    y = "Average Customization Level",
    size = "Avg Spend ($)"
  )
