# Medical Goods
# created: 2025-07-27
# last updated: 2025-07-27
# Note: summarize trade pattern

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

load(".\\Data\\processed\\baci23.Rda")
load(".\\Data\\processed\\baci17.Rda")
load(".\\Data\\processed\\baci02.Rda")


cntry_geo <- read.csv(".\\Data\\processed\\country_geo_info.csv")

cntry <- read.csv(".\\Data\\raw\\baci\\BACI_HS22_V202501\\country_codes_V202501.csv") %>% 
  select(code=country_code, name=country_name) %>% 
  mutate(name=case_when(name=="TÃ¼rkiye"~"Turkey",
                        name=="Bolivia (Plurinational State of)"~"Bolivia",
                        name=="China, Hong Kong SAR"~"Hong Kong",
                        name=="China, Macao SAR"~"Macao",
                        name=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
                        name=="Dem. Rep. of the Congo"~"Congo",
                        name=="Dominican Rep."~"Dominican",
                        name=="Lao People's Dem. Rep."~ "Lao",
                        name=="United Rep. of Tanzania"~"Tanzania",
                        name=="Russian Federation"~"Russian",
                        name=="Rep. of Korea"~"Korea",
                        name=="United Arab Emirates"~"UAE",
                        name=="United Kingdom"~"UK",
                        name=="Rep. of Moldova"~"Moldova",
                        name=="Sudan (...2011)"~"Sudan",
                        name=="Dem. People's Rep. of Korea"~"Korea",
                        name=="Other Asia, nes"~"Taiwan",
                        TRUE~name)) %>% 
  mutate(i=code, j=code)

# Other related medical supplies -------
## export 02 -----
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="i") %>% 
  select(name,share)

## import 02 -----
baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(j) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="j") %>% 
  select(name,share)

## export 17 -----
baci17_1 <- baci17 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="i") %>% 
  select(name,share)

## import 17 -----
baci17_2 <- baci17 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(j) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="j") %>% 
  select(name,share)

## export 23 -----
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="i") %>% 
  select(name,share)

## import 23 -----
baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(j) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(share =round(100* trade_flow / sum(trade_flow),2)) %>% 
  slice_head(n=5) %>% 
  left_join(cntry,by="j") %>% 
  select(name,share)



[1] "APIs"                                           "Dosage forms"                                  
[3] "Medical device"                                                    
[5] "Other related medical supplies"                 "Pharmaceutical preparations"  
