library(imputeTS)
library(forecast)
library(tidyverse)
library(lubridate)
library(zoo)

#setwd("C:/Users/XPS/Desktop/Software/Data-Analysis-2/Pattern-Project/training_data_20200403.csv")
df <- readr::read_csv("C:/Users/XPS/Desktop/Software/Data-Analysis-2/Pattern-Project/training_data_20200403.csv")
names(df)
df$avg_retail_price <- df$sales / df$units
df[is.nan(df$avg_retail_price),"avg_retail_price"] <- NA
df$multiple_vendors <- (df$vendor_id_max == df$vendor_id_min)
df$asin <- as.factor(df$asin)
df$ordering_region <- as.factor(df$ordering_region)
df$basket_size <- df$units / df$order_items_total
df[is.nan(df$basket_size),"basket_size"] <- 0


temp <- df %>%
  filter(available == 0) %>%
  group_by(floor_date(date, unit = "month")) %>%
  summarise(ad_spend_on_zero = sum(ad_spend))
temp %>% slice(tail(row_number(), 5)) %>%
  summarise(mean_spend = mean(ad_spend_on_zero))
# a mininmum of 51,344 spent on advertising on the same day that inventory is selling out to zero
rm(temp)

df1 <- df %>% 
  select(-X1, -vendor_id_min, -units_pending, -units_returned, -units_sns, -sales,
         -sales_returned, -sales_sns, -sales_pending, -discount_sns, -discount_coupons, -min_wholesale_price,
         -inventory, -inbound, -warehouse_id_min, -warehouse_id_max, -ad_sales,
         -sku_sales, -buybox_approximate_units_suppressed,
         -order_flag, -conversions, -clicks, -impressions)
names(df1)

# Work on subsample and we don't know demand if available == 0
set.seed(54321)
df1 <- df1[df1$asin %in% sample(unique(df1$asin), 500),]
df1[df1$available <= 0,"units"] <- NA
rm(df)


# Bad days where sales was essentially 0 across most products
bad_days <- df1 %>%
  group_by(time = floor_date(date, unit="day")) %>%
  summarise(units = sum(units, na.rm = TRUE)) %>%
  filter(time > ymd("2019-01-21")) #%>%
#  filter(units < 600) and also remove the day after

ggplot(bad_days, aes(x = time, y = units)) + geom_line()
bad_days <- ymd(c("2019-10-25","2019-10-26","2019-10-27","2019-10-28",
                  "2020-03-06","2020-03-07","2020-03-08","2020-03-09",
                  "2020-03-10","2020-03-11","2020-03-12","2020-03-13",
                  "2020-03-14"))

dfclean <- df1 %>%
  filter(!(date %in% bad_days)) %>%
  arrange(asin, ordering_region) %>%
  group_by(asin, ordering_region) %>%
  mutate(avg_retail_price = zoo::na.locf(avg_retail_price, na.rm = FALSE),
         avg_retail_price = zoo::na.locf(avg_retail_price, na.rm = FALSE, fromLast = TRUE),
         pct_suppressed_28_day = rollapply(lag(buybox_supressed_percentage), 28, by.column = T, 
                                           FUN = function(x) mean(x, na.rm=TRUE), 
                                           align = 'right', fill = NA),
         suppressed_yesterday = lag(buybox_supressed_percentage > 0),
         avg_buybox_28_day = rollapply(lag(buybox_approximate_units_total), 28, FUN = function(x) mean(x, na.rm = TRUE),
                                       align = 'right', fill = NA),
         avg_price_28_day = rollapply(lag(avg_retail_price), 28, FUN = function(x) mean(x, na.rm = TRUE),
                               align = 'right', fill = NA),
         avg_basketsize_28_day = rollapply(lag(basket_size), 28, FUN = function(x) mean(x, na.rm = TRUE),
                                           align = 'right', fill = NA),
         avg_discount_28_day = rollapply(lag(discount), 28, FUN = function(x) mean(x, na.rm = TRUE),
                                         align = 'right', fill = NA),
         week_day = as.factor(wday(date)),
         lag_1_units = lag(units, 1),
         lag_2_units = lag(units, 2),
         lag_3_units = lag(units, 3),
         lag_4_units = lag(units, 4),
         lag_5_units = lag(units, 5),
         lag_6_units = lag(units, 6),
         lag_7_units = lag(units, 7),
         back_in_stock = !(is.na(units)) & (is.na(lag(units)) | is.na(lag(units,n = 2))), # Inventory back in, so artificial demand right when it is back in
         avg_units_28_day = rollapply(lag(units), 28, FUN = function(x) mean(x, na.rm = TRUE),
                                      align = 'right', fill = NA)) %>%
  select(-discount, -order_items_total, -max_wholesale_price, -buybox_approximate_units_total,
         -buybox_percentage, -buybox_supressed_percentage, -basket_size, -ad_spend,
         -vendor_id_max, -available, -buybox_approximate_units_lost)


#### Clean data ####
dfclean1 <- dfclean
dfclean1[dfclean1$back_in_stock,"units"] <- NA # only take into account normal days

dfclean1 <- dfclean1 %>%
  ungroup() %>%
  mutate(unique_pair = paste(asin, ordering_region, sep = '_')) %>%
  select(-asin, -ordering_region) %>%
  select(unique_pair, everything()) %>%
  group_by(unique_pair) %>%
  filter(sum(is.na(units),sum(units %in% 0)) <= (n()-28)) %>% # Remove if no unit sales for the asin/region
  ungroup()


#### Impute missing through ARIMA model ####

model_missing <- function(x) {
  fit <- auto.arima(x)
  kr <- KalmanRun(x, fit$model)
  id.na <- which(is.na(x))
  for (i in id.na)
    x[i] <- fit$model$Z %*% kr$states[i,]
  return(x)
}

model_fitted <- function(x) {
  fit <- auto.arima(x)
  kr <- KalmanRun(x, fit$model)
  return(fitted_values = as.vector(x) + as.vector(kr$resid))
}


## For testing with temp df ##
#temp <- dfclean1[dfclean1$unique_pair %in% sample(dfclean1$unique_pair, 1),] #To try out with just a few to make sure it's running
#setDT(temp)
#results_missing <- temp[, lapply(.SD, model_missing), by = unique_pair, .SDcols = "units"]
#results_fitted <- temp[, lapply(.SD, model_fitted), by = unique_pair, .SDcols = "units"]
library(data.table)

setDT(dfclean1)
results_missing <- dfclean1[, lapply(.SD, model_missing), by = unique_pair, .SDcols = "units"]
results_missing[results_missing$units < 0, "units"] <- 0
results_fitted <- dfclean1[, lapply(.SD, model_fitted), by = unique_pair, .SDcols = "units"]
results_fitted[results_fitted$units < 0 & !is.na(results_fitted$units), 
               "units"] <- 0
# results_missing is the original units but the missing data is imputed
# results_fitted is predicted values for all days, not just the missing



#### Simplified model for advertising effectiveness / Complex Models ####
dfclean2 <- dfclean1 %>%
  filter(!is.na(units)) # no y variable means there is nothing we can train on these
dfclean2 <- dfclean2[complete.cases(dfclean2),] #this is for a version of the simple model only allowing non-NA's
# you could also pare this down for the simple model
# the complex model should be structured to handle the NAs

library(forecast)

modelaki <- function(x) {model <- auto.arima(x)
result = forecast(model,3)  
return(list(result, model))}

dfclean2 <- as.data.table(dfclean2)
results <- dfclean2[, lapply(.SD, modelaki), by = unique_pair, .SDcols = "units"]

