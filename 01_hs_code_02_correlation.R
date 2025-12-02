# Medical Goods
# created: 2025-07-15
# last updated: 2025-07-27
# Note: compile and identify pharmaceutical and medical goods in
# HS2017, HS2012, HS2007, HS2002, HS1996

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
library(GephiForR)

setwd("C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\")

# step 1: identify HS codes-------------------
# Identifying goods in HS6 versions 2017, 2012, 2007, 2002, 1996 using the 
# HS correlation table from UN Stats.
# (https://unstats.un.org/unsd/classifications/Econ)

hs22 <- read.csv(".\\Data\\processed\\med_goods_hs6.csv",colClasses=c("HS6"="character")) %>% select(cat, product,HS6)
## version 2017 -----
hs17 <- read_excel(".\\Data\\raw\\un_stats_hs_correlation\\HS2022toHS2017ConversionAndCorrelationTables.xlsx", sheet="HS2022-HS2017 Conversions") %>% 
  select(HS6=`From HS2022`, k=`To HS2017`) %>% 
  full_join(hs22,by="HS6") %>% 
  select(-HS6)

# matched codes
hs17m <- hs17 %>% filter(!is.na(cat)& !is.na(k))
# unique codes
hs17unique <- left_join(hs17m, hs17m[duplicated(hs17m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>%
  filter(is.na(dup)) %>% 
  select(k, cat, product)
# duplicates
hs17dup <- left_join(hs17m, hs17m[duplicated(hs17m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>% 
  filter(dup==1) %>% 
  group_by(k) %>% 
  arrange(cat) %>% 
  mutate(cat2= paste0(unique(cat), collapse = ","), product2= paste0(product, collapse = "; ")) %>% 
  mutate(cat3=case_when(cat2=="Dosage forms,Pharmaceutical preparations"~"Pharmaceutical preparations",
                        cat2=="Medical device,Other related medical supplies"~"Medical device",
                        TRUE~cat2)) %>% 
  select(k,cat=cat3,product=product2) %>% 
  distinct(k,.keep_all = TRUE)
hs17f <- rbind(hs17unique,hs17dup)
# unmatched
hs17u <- hs17 %>% filter(!is.na(cat)& is.na(k)) %>% mutate(k_HS_version="2017")

## version 2012 -----
hs12 <- read_excel(".\\Data\\raw\\un_stats_hs_correlation\\HS2022toHS2012ConversionAndCorrelationTables.xlsx", sheet="HS2022-HS2012 Conversions") %>% 
  select(HS6=`From HS 2022`, k=`To HS 2012`) %>% 
  full_join(hs22,by="HS6")
# matched codes
hs12m <- hs12 %>% filter(!is.na(cat)& !is.na(k))
# unique codes
hs12unique <- left_join(hs12m, hs12m[duplicated(hs12m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>%
  filter(is.na(dup)) %>% 
  select(k, cat, product)
# duplicates
hs12dup <- left_join(hs12m, hs12m[duplicated(hs12m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>% 
  filter(dup==1) %>% 
  group_by(k) %>% 
  arrange(cat) %>% 
  mutate(cat2= paste0(unique(cat), collapse = ","), product2= paste0(product, collapse = "; ")) %>% 
  mutate(cat3=case_when(k=="300210"~"Pharmaceutical preparations", # check with commodity_translation_wizard
                        k=="902780"~"Medical device",              # selecting year=2012
                        k=="854370"~"Medical device",
                        k=="382490"~"Other related medical supplies",
                        k=="902780"~"Medical device",
                        TRUE~cat2)) %>% 
  select(k,cat=cat3,product=product2) %>% 
  distinct(k,.keep_all = TRUE)
hs12f <- rbind(hs12unique,hs12dup)
# unmatched
hs12u <- hs12 %>% filter(!is.na(cat)& is.na(k)) %>% mutate(k_HS_version="2012")

## version 2007 -----
hs07 <- read_excel(".\\Data\\raw\\un_stats_hs_correlation\\HS2022toHS2007ConversionAndCorrelationTables.xlsx", sheet="HS2022-HS2007 Conversions") %>% 
  select(HS6=`From HS 2022`, k=`From HS 2007`) %>% 
  full_join(hs22,by="HS6")
# matched codes
hs07m <- hs07 %>% filter(!is.na(cat)& !is.na(k))
# unique codes
hs07unique <- left_join(hs07m, hs07m[duplicated(hs07m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>%
  filter(is.na(dup)) %>% 
  select(k, cat, product)
# duplicates
hs07dup <- left_join(hs07m, hs07m[duplicated(hs07m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>% 
  filter(dup==1) %>% 
  group_by(k) %>% 
  arrange(cat) %>% 
  mutate(cat2= paste0(unique(cat), collapse = ","), product2= paste0(product, collapse = "; ")) %>% 
  mutate(cat3=case_when(k=="300210"~"Pharmaceutical preparations", # check with commodity_translation_wizard
                        k=="300290"~"Pharmaceutical preparations", # selecting year=2007
                        k=="854370"~"Medical device",
                        k=="382490"~"Other related medical supplies",
                        k=="902780"~"Medical device",
                      TRUE~cat2))%>% 
  select(k,cat=cat3,product=product2)%>% 
  distinct(k,.keep_all = TRUE)
hs07f <- rbind(hs07unique,hs07dup)
# unmatched
hs07u <- hs07 %>% filter(!is.na(cat)& is.na(k)) %>% mutate(k_HS_version="2007")

## version 2002 -----
hs02 <- read_excel(".\\Data\\raw\\un_stats_hs_correlation\\HS2022toHS2002ConversionAndCorrelationTables.xlsx", sheet="HS2022-HS2002 Conversions") %>% 
  select(HS6=`From HS 2022`, k=`From HS 2002`) %>% 
  full_join(hs22,by="HS6")
# matched codes
hs02m <- hs02 %>% filter(!is.na(cat)& !is.na(k))
# unique codes
hs02unique <- left_join(hs02m, hs02m[duplicated(hs02m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>%
  filter(is.na(dup)) %>% 
  select(k, cat, product)
# duplicates
hs02dup <- left_join(hs02m, hs02m[duplicated(hs02m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>% 
  filter(dup==1) %>% 
  group_by(k) %>% 
  arrange(cat) %>% 
  mutate(cat2= paste0(unique(cat), collapse = ","), product2= paste0(product, collapse = "; ")) %>% 
  mutate(cat3=case_when(k=="300210"~"Pharmaceutical preparations", # check with commodity_translation_wizard
                        k=="300290"~"Pharmaceutical preparations", # selecting year=2007
                        k=="854370"~"Medical device",
                        k=="382490"~"Other related medical supplies",
                        k=="854389"~"Other related medical supplies", # new change for 2012
                        k=="902780"~"Medical device",
                        TRUE~cat2))%>%  
  select(k,cat=cat3,product=product2)%>% 
  distinct(k,.keep_all = TRUE)
hs02f <- rbind(hs02unique,hs02dup)
# unmatched
hs02u <- hs02 %>% filter(!is.na(cat)& is.na(k)) %>% mutate(k_HS_version="2002")

## version 1996 -----
hs96 <- read_excel(".\\Data\\raw\\un_stats_hs_correlation\\HS2022toHS1996ConversionAndCorrelationTables.xlsx", sheet="HS2022-HS1996 Conversions") %>% 
  select(HS6=`From HS 2022`, k=`To HS 1996`) %>% 
  full_join(hs22,by="HS6")
# matched codes
hs96m <- hs96 %>% filter(!is.na(cat)& !is.na(k))
# unique codes
hs96unique <- left_join(hs96m, hs96m[duplicated(hs12m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>%
  filter(is.na(dup)) %>% 
  select(k, cat, product)
# duplicates
hs96dup <- left_join(hs96m, hs96m[duplicated(hs96m$k),] %>% select(k) %>%distinct(k) %>%  mutate(dup=1),by="k") %>% 
  filter(dup==1) %>% 
  group_by(k) %>% 
  arrange(cat) %>% 
  mutate(cat2= paste0(unique(cat), collapse = ","), product2= paste0(product, collapse = "; ")) %>% 
  mutate(cat3=case_when(k=="300210"~"Pharmaceutical preparations", # check with commodity_translation_wizard
                        k=="300290"~"Pharmaceutical preparations", # selecting year=2007
                        k=="854370"~"Medical device",
                        k=="382490"~"Other related medical supplies",
                        k=="854389"~"Other related medical supplies", # new change for 2012
                        k=="300490"~"Dosage forms", # new change for 1996
                        k=="902780"~"Medical device",
                        TRUE~cat2))%>% 
  select(k,cat=cat3,product=product2)%>% 
  distinct(k,.keep_all = TRUE)
hs96f <- rbind(hs96unique,hs96dup)
# unmatched
hs96u <- hs96 %>% filter(!is.na(cat)& is.na(k)) %>% mutate(k_HS_version="1996")


# inspecting unmatched
# HS6_2022 981000 cannot be matched with other codes
rm(hs17u, hs07u,hs12u,hs96u, hs02u,hs17, hs07,hs12,hs96, hs02)

save(hs17f,file=".\\Data\\processed\\hs_correlation\\hs17f.Rda")
save(hs12f,file=".\\Data\\processed\\hs_correlation\\hs12f.Rda")
save(hs07f,file=".\\Data\\processed\\hs_correlation\\hs07f.Rda")
save(hs02f,file=".\\Data\\processed\\hs_correlation\\hs02f.Rda")
save(hs96f,file=".\\Data\\processed\\hs_correlation\\hs96f.Rda")

