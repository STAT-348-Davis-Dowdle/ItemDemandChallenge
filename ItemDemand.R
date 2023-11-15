library(vroom)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(patchwork)

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
