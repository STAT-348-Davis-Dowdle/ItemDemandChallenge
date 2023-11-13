library(vroom)
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
