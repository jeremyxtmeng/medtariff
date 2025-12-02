# Summarizing US trade pattern
# created: 2025-07-27
# last updated: 2025-07-27

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
library(censusapi)
library(foreign)

setwd("C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\")

hts <- read_csv(".\\Data\\processed\\med_goods_hts22_final.csv", col_types = cols(.default = col_character())) 


#load(".\\Data\\processed\\med_imports_from_2017_01_to_2025_04_part1.Rdata")
#downloaded_code <-med_import %>% select(I_COMMODITY) %>% distinct()

# If an existing dataset is already in your env, we'll append to it.
rm(med_import)

# imports in 2017
if (!exists("med_import"))med_import <- data.frame()

end=length(unique(hts$HTS22))

#Skipping 1507902000 (no data / error): 204, no content was returned.
#Skipping 3003430000 (no data / error): 204, no content was returned.

for (code in hts$HTS22[1:end]) {
  tmp <- tryCatch(
    {
      getCensus(
        name  = "timeseries/intltrade/imports/hs",
        key   = "71178d1b890b8187d0200c97ff30acd14e2c1a28",
        vars  = c("CTY_CODE","GEN_VAL_YR"),
        time  = "2017-12",
        I_COMMODITY = code     # or: I_COMMODITY = get("code") if you insist on the literal "code"
      ) %>% mutate(I_COMMODITY=format(I_COMMODITY, scientific = FALSE)) %>% 
            mutate(I_COMMODITY=as.character(I_COMMODITY))
    },
    error = function(e) {
      message(sprintf("Skipping %s (no data / error): %s", code, e$message))
      data.frame()  # return empty so the loop continues
    }
  )
  
  # Append
  med_import <- bind_rows(med_import, tmp)
}

med_import_2017 <- med_import %>% filter(time=="2017-12")

rm(med_import)
# imports in 2017
if (!exists("med_import"))med_import <- data.frame()

end=length(unique(hts$HTS22))

#Skipping 1507902000 (no data / error): 204, no content was returned.
#Skipping 3003430000 (no data / error): 204, no content was returned.

for (code in hts$HTS22[1:end]) {
  tmp <- tryCatch(
    {
      getCensus(
        name  = "timeseries/intltrade/imports/hs",
        key   = "71178d1b890b8187d0200c97ff30acd14e2c1a28",
        vars  = c("CTY_CODE","GEN_VAL_YR"),
        time  = "2024-12",
        I_COMMODITY = code     # or: I_COMMODITY = get("code") if you insist on the literal "code"
      ) %>% mutate(I_COMMODITY=format(I_COMMODITY, scientific = FALSE)) %>% 
        mutate(I_COMMODITY=as.character(I_COMMODITY))
    },
    error = function(e) {
      message(sprintf("Skipping %s (no data / error): %s", code, e$message))
      data.frame()  # return empty so the loop continues
    }
  )
  
  # Append
  med_import <- bind_rows(med_import, tmp)
}

# loading country name -------

cntrycode <- read.csv(".\\Data\\raw\\census_country_code.csv") %>% mutate(CTY_CODE=as.character(code))
med1 <- left_join(med_import_2017, cntrycode, by="CTY_CODE") %>% filter(!is.na(name))
med2 <- left_join(med_import, cntrycode, by="CTY_CODE") %>% filter(!is.na(name))

df <- rbind(med1,med2) %>% select(time, country=name,HTS22=I_COMMODITY, value=GEN_VAL_YR)

df1 <- left_join(df,hts %>% mutate(HTS22=ifelse(HTS22=="0510004040","510004040", HTS22)),by="HTS22")

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

result1 <- left_join(result,hts %>% mutate(HTS22=ifelse(HTS22=="0510004040","510004040", HTS22)),by="HTS22")

df_hhi_banded <- result1 %>%
  mutate(hhi_band = case_when(
    hhi<1000 ~ "<1000",
    hhi>=1000 & hhi<1800~"1000-1799",
    hhi>=1800 & hhi<=5000~"1800-5000",
    hhi>5000~">5000"
  ))

# Step 2: Count number of products in each band per category and time
hhi_summary_share <- df_hhi_banded %>%
  group_by(cat,time) %>% 
  mutate(product_n=n()) %>% 
  group_by(cat, time, hhi_band,product_n) %>%
  summarise(n_products = n(), .groups = "drop") %>%
  mutate(share=round(100*n_products/product_n,2)) %>% 
  select(-product_n,-n_products) %>% 
  arrange(cat, time, hhi_band) %>% 
  pivot_wider(
    names_from = cat,
    values_from = share,
    values_fill = 0
  ) %>%
  arrange(time, hhi_band)

library(xtable)

xt <- xtable(hhi_summary_share,
             caption = "HHI-banded products by category and time",
             label = "tab:hhi_summary",
             align = c("l", rep("r", ncol(hhi_summary_share))))  # left + right-align cols


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

xt2 <- xtable(hhi_summary,
             caption = "HHI-banded products by category and time",
             label = "tab:hhi_summary",
             align = c("l", rep("r", ncol(hhi_summary))))  # left + right-align cols



write.csv(result1,".\\Data\\processed\\us_trade_pattern.csv")
