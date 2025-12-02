# Medical Goods
# created: 2025-07-15
# last updated: 2025-07-27
# Note: Trade flows using BACI data

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

hs22 <- read.csv(".\\Data\\processed\\med_goods_hs6.csv",colClasses=c("HS6"="character")) %>% select(cat, product,HS6)

load(".\\Data\\processed\\hs_correlation\\hs17f.Rda")
load(".\\Data\\processed\\hs_correlation\\hs12f.Rda")
load(".\\Data\\processed\\hs_correlation\\hs07f.Rda")
load(".\\Data\\processed\\hs_correlation\\hs02f.Rda")
load(".\\Data\\processed\\hs_correlation\\hs96f.Rda")

# step 1: load BACI with relevant HS codes---------------
# note: values in thousands of US dollars
#  HS code
# (https://www.usitc.gov/harmonized_tariff_information/hts/archive/list)

cntry <- read.csv(".\\Data\\raw\\baci\\BACI_HS22_V202501\\country_codes_V202501.csv") %>% select(i=country_code, name=country_name, iso3=country_iso3) %>% 
  mutate(j=i)

baci23 <- read.csv(".\\Data\\raw\\baci\\BACI_HS22_V202501\\BACI_HS22_Y2023_V202501.csv") %>%
  left_join(hs22 %>% select(cat, product,k=HS6)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci23,file=".\\Data\\processed\\baci23.Rda")


baci18 <- read.csv(".\\Data\\raw\\baci\\BACI_HS17_V202501\\BACI_HS17_Y2018_V202501.csv") %>%
  left_join(hs17f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci18,file=".\\Data\\processed\\baci18.Rda")

baci17 <- read.csv(".\\Data\\raw\\baci\\BACI_HS17_V202501\\BACI_HS17_Y2017_V202501.csv") %>%
  left_join(hs17f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci17,file=".\\Data\\processed\\baci17.Rda")


baci13 <- read.csv(".\\Data\\raw\\baci\\BACI_HS12_V202501\\BACI_HS12_Y2013_V202501.csv") %>%
  left_join(hs12f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci13,file=".\\Data\\processed\\baci13.Rda")


baci08 <- read.csv(".\\Data\\raw\\baci\\BACI_HS07_V202501\\BACI_HS07_Y2008_V202501.csv") %>%
  left_join(hs07f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci08,file=".\\Data\\processed\\baci08.Rda")


baci03 <- read.csv(".\\Data\\raw\\baci\\BACI_HS02_V202501\\BACI_HS02_Y2003_V202501.csv") %>%
  left_join(hs02f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci03,file=".\\Data\\processed\\baci03.Rda")

baci02 <- read.csv(".\\Data\\raw\\baci\\BACI_HS02_V202501\\BACI_HS02_Y2002_V202501.csv") %>%
  left_join(hs02f %>% select(cat, product,k)%>% mutate(k=as.numeric(k)), by="k") %>% filter(!is.na(cat)) %>%
  group_by(t, cat, i, j) %>% summarise(total_v=sum(v,na.rm = TRUE),total_q=sum(q,na.rm = TRUE)) %>% ungroup()

save(baci02,file=".\\Data\\processed\\baci02.Rda")


# step 3: prep data for gephi -------------
#https://odsc.medium.com/graph-viz-exploring-analyzing-and-visualizing-graphs-and-networks-with-gephi-and-chatgpt-c45928cf1f08


baci23_im <- baci23 %>% select(-product) %>% group_by(t,cat,j) %>% 
  summarise(total_import=round(sum(v,na.rm = TRUE)/10e3,digits=2)) %>% 
  left_join(cntry,by="j") %>% 
  arrange(t, cat,desc(total_import))

# year 22
baci22 <- read.csv(".\\Data\\raw\\baci\\BACI_HS22_V202501\\BACI_HS22_Y2022_V202501.csv") %>% 
  left_join(hs22 %>% select(cat, product,k=HS6), by="k") %>% filter(!is.na(cat))

baci22_ex <- baci22 %>% select(-product) %>% group_by(t,cat,i) %>% 
  summarise(total_export=round(sum(v,na.rm = TRUE)/10e3,digits=2)) %>% 
  left_join(cntry,by="i") %>% 
  arrange(t, cat,desc(total_export))
  
baci22_im <- baci22 %>% select(-product) %>% group_by(t,cat,j) %>% 
  summarise(total_import=round(sum(v,na.rm = TRUE)/10e3,digits=2)) %>% 
  left_join(cntry,by="j") %>% 
  arrange(t, cat,desc(total_import))


#write.csv(baci22_im,".\\Data\\processed\\import_22.csv",row.names = FALSE) 
#write.csv(baci22_ex,".\\Data\\processed\\export_22.csv",row.names = FALSE) 

