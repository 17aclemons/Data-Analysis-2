library(imputeTS)
library(forecast)
library(tidyverse)
library(lubridate)
library(zoo)


df <- readr::read_csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/Pattern-Project/training_data_20200403.csv")
names(df)
df$avg_retail_price <- df$sales / df$units
df[is.nan(df$avg_retail_price),"avg_retail_price"] <- NA
df$multiple_vendors <- (df$vendor_id_max == df$vendor_id_min)
df$asin <- as.factor(df$asin)
df$ordering_region <- as.factor(df$ordering_region)
df$basket_size <- df$units / df$order_items_total
df[is.nan(df$basket_size),"basket_size"] <- 0

#### Chunk 1 Wasted Ad spend ####
# To-do --
# 1) Calculate adspend on products on date (like below)
# except omit the dates preceding periods where the product
# is never purchased again (0 for available
# going forward, or available for at least eight weeks going forward)
# If they are advertising a product they are intentionally stocking out
# of because it's seasonal or they don't want to carry it anymore,
# that's okay
temp <- df
ad <- temp %>%
  filter(available == 0, ad_spend > 0) %>% #product isn't purchased again
  arrange(asin, ordering_region) %>%
  group_by(asin, ordering_region) %>%
  mutate(lead56 = lead(units, 56)) %>% #available for next 8 weeks
  filter(lead56 > 0 ) %>% #products are potentially ordered
  summarise(ad_waste = sum(ad_spend))

sum(ad$ad_waste)

# 2) How much is the ad spending in the preceding week (not just final date)
# before stockout? Apply the cleaning step from #1.

previous <- temp %>%
  filter(available == 0, ad_spend > 0) %>%
  arrange(asin,ordering_region) %>%
  group_by(asin,ordering_region) %>%
  mutate(lag7 = lag(ad_spend, 7)) %>%
  summarise(PreviousWeek = sum(lag7, na.rm = TRUE))

sum(previous$PreviousWeek)

# 3) Aggregate #2 this spending by ordering_region, asin, and month (floor_date(x,unit = "month"))
# Have a Unproductive AdSpend, Units, Total Revenue, Average Price, and Average Wholesaleprice
# in this summary table. Only include Jan-01-2019 going forward.

table <- temp %>%
  filter(available == 0, ad_spend >0) %>%
  arrange(ordering_region, asin) %>%
  group_by(YMD = floor_date(date, unit = "month")) %>%
  mutate(lag7 = lag(ad_spend, 7)) %>%
  mutate("Wasted Ad Spending" = lag7, Revenue = sales)

table <- table %>%
  select(ordering_region, asin, YMD, "Wasted Ad Spending", units, Revenue, avg_retail_price, max_wholesale_price)

work <- table %>%
  group_by(ordering_region, asin) %>%
  mutate(waste = sum(`Wasted Ad Spending`,na.rm = TRUE))
  
df <- df[unique(df$asin),]  
  
summary(table)
