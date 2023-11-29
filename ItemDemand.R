library(vroom)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(patchwork)
library(modeltime)
library(timetk)
library(forecast)

setwd("C:/Users/davis/OneDrive - Brigham Young University/Documents/skool/new/stat 348/StoreItemDemand/ItemDemandChallenge")

train <- vroom("train.csv")
test <- vroom("test.csv")


# ACF plots
acf1 <- train %>%
  filter(item == 1, store == 1) %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("Item 1 Store 1 ACF", subtitle = "Data week to week helps predict")

acf2 <- train %>%
  filter(item == 15, store == 4) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365) +
  ggtitle("Item 15 Store 4 ACF", subtitle = "Data around the same time in a year helps predict")

acf3 <- train %>%
  filter(item == 34, store == 7) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365*2) +
  ggtitle("Item 34 Store 7 ACF", subtitle = "Data year to year helps predict")

acf4 <- train %>%
  filter(item == 50, store == 10) %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max = 365*5) +
  ggtitle("Item 50 Store 10 ACF", subtitle = "Data becomes less helpful in predicting over time")

(acf1 + acf2) / (acf3 + acf4)

ggsave("acfplots.pdf", height = 8, width = 12)


# Time Series feature engineering
train <- train %>%
  filter(store == 6, item == 27) %>%
  select(-store, -item)

recipe <- recipe(sales ~ ., data = train) %>%
  step_date(date, features = "dow") %>%
  step_date(date, features = "doy") %>%
  step_date(date, features = "year") %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(cosday = cos(date_doy)) %>%
  step_rm(date) %>%
  step_normalize(all_numeric_predictors())

model <- rand_forest(mtry = tune(),
                     min_n = tune(),
                     trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model)

grid <- grid_regular(mtry(range = c(1, 10)), min_n(), levels = 3)  

folds <- vfold_cv(train, v = 5, repeats = 1)

results <- wf %>%
  tune_grid(resamples = folds,
            grid = grid,
            metrics = metric_set(smape))

best <- results %>%
  select_best("smape")

collect_metrics(results) %>%
  filter(mtry == 10, min_n == 40) %>%
  pull(mean)


# Exponential smoothing
# Combo 1
train1 <- train %>% filter(store == 4, item == 20)

split1 <- time_series_split(train1, assess = "3 months", cumulative = TRUE)

es_model1 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales ~ date, data = training(split1))

results1 <- modeltime_calibrate(es_model1, new_data = testing(split1))

fullfit1 <- results1 %>%
  modeltime_refit(train1)

es_preds1 <- fullfit1 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

# Combo 2
train2 <- train %>% filter(store == 7, item == 40)

split2 <- time_series_split(train2, assess = "3 months", cumulative = TRUE)

es_model2 <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales ~ date, data = training(split2))

results2 <- modeltime_calibrate(es_model2, new_data = testing(split2))

fullfit2 <- results2 %>%
  modeltime_refit(train2)

es_preds2 <- fullfit2 %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

# Plots
es1 <- results1 %>%
  modeltime_forecast(new_data = testing(split1),
                     actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=TRUE)

es2 <- fullfit1 %>%
  modeltime_forecast(h = "3 months", actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

es3 <- results2 %>%
  modeltime_forecast(new_data = testing(split2),
                     actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

es4 <- fullfit2 %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(es1, es3, es2, es4, nrows = 2)


# ARIMA
arima_recipe <- recipe(sales ~ ., data = train) %>%
  step_rm(store, item)

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
  set_engine("auto_arima")

arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(split1))

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data = training(split2))

results1 <- modeltime_calibrate(arima_wf1, new_data = testing(split1))

results2 <- modeltime_calibrate(arima_wf2, new_data = testing(split2))

arima1 <- results1 %>%
  modeltime_forecast(new_data = testing(split1),
                     actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=TRUE)

arima2 <- results1 %>%
  modeltime_forecast(h = "3 months", actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=FALSE)

arima3 <- results2 %>%
  modeltime_forecast(new_data = testing(split2),
                     actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

arima4 <- results2 %>%
  modeltime_forecast(h = "3 months", actual_data = train2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(arima1, arima3, arima2, arima4, nrows = 2)
