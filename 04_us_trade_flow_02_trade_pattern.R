# Medical Goods
# created: 2025-07-27
# last updated: 2025-07-27
# Note: summarize US trade pattern

rm(list = ls())
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(ggplot2)
library(stringr)
library(stringi)
library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(zoo)
library(clock)
library(reshape2)
library(data.table)

setwd("C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\")

htscode <- read.csv(".\\Data\\processed\\med_goods_hts22.csv") %>% 
  mutate(HTS22=as.character(HTS22),HS6=as.character(HS6)) %>% 
  mutate(HTS22=ifelse(HTS22=="510004040", "0510004040", HTS22))

df <- read.csv(".\\Data\\raw\\us_medical_goods_imports.csv") %>% 
  separate(HTS22, into = c("HTS22", "production2"), sep = " ", extra = "merge", fill = "right")

df1 <- left_join(df,htscode %>% select(-source),by="HTS22")


top_countries <- df1 %>%
  group_by(cat, time, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(cat, time) %>%
  arrange(desc(total_value)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5) %>%
  select(cat, time, rank, country)

# Step 2: Convert to wide format: 1 row per cat+time, 5 columns for country names
wide_top_countries <- top_countries %>%
  pivot_wider(
    names_from = rank,
    values_from = country,
    names_prefix = "Top"
  ) %>%
  arrange(cat, time)

  
# Function to compute concentration metrics per htscode-year group
concentration_stats <- function(df_group) {
  df_sorted <- df_group %>%
    arrange(desc(value)) %>%
    mutate(
      share = value / sum(value),
      cum_share = cumsum(share)
    )
  
  n_50 <- which(df_sorted$cum_share >= 0.5)[1]
  n_75 <- which(df_sorted$cum_share >= 0.75)[1]
  n_90 <- which(df_sorted$cum_share >= 0.9)[1]
  
  hhi <- sum((df_sorted$share * 100)^2)  # in percentage terms, e.g., 10000 is monopoly
  
  return(tibble(
    countries_50pct = n_50,
    countries_75pct = n_75,
    countries_90pct = n_90,
    hhi = hhi
  ))
}

# Apply by htscode and time
result <- df1 %>%
  group_by(HTS22, time) %>%
  group_modify(~ concentration_stats(.x)) %>%
  ungroup()

result1 <- left_join(result,htscode %>% select(-source),by="HTS22")

df_hhi_banded <- result1 %>%
  mutate(hhi_band = case_when(
    hhi<1000 ~ "<1000",
    hhi>=1000 & hhi<1800~"1000-1799",
    hhi>=1800 & hhi<=5000~"1800-5000",
    hhi>5000~">5000"
  ))

# Step 2: Count number of products in each band per category and time
hhi_summary <- df_hhi_banded %>%
  group_by(cat, time, hhi_band) %>%
  summarise(n_products = n(), .groups = "drop") %>%
  arrange(cat, time, hhi_band) %>% 
  pivot_wider(
    names_from = cat,
    values_from = n_products,
    values_fill = 0
  ) %>%
  arrange(time, hhi_band)

library(xtable)

xt <- xtable(hhi_summary,
             caption = "HHI-banded products by category and time",
             label = "tab:hhi_summary",
             align = c("l", rep("r", ncol(hhi_summary))))  # left + right-align cols



write.csv(result1,".\\Data\\processed\\us_trade_pattern.csv")

#

# Step 1: Filter for 2024 and HHI > 5000
df_filtered <- df1 %>%filter(time==2024) %>% 
  left_join(result %>% filter(time==2024),by="HTS22") %>% 
  filter(hhi > 5000)

# Step 2: Compute total value per product
df_top5 <- df_filtered %>%
  group_by(HTS22) %>%
  mutate(
    total_value = sum(value, na.rm = TRUE),
    market_share = value / total_value
  ) %>%
  arrange(HTS22, desc(value)) %>%
  slice_head(n = 5) %>%
  ungroup()


# Step 3: Optional: Format market share as percentage
df_top5_1 <- df_top5 %>%
  mutate(market_share = round(100 * market_share, 2))


write.csv(df_top5_1,".\\Data\\processed\\top_sourcing.csv")

