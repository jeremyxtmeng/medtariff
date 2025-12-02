# Medical Goods
# created: 2025-07-15
# last updated: 2025-07-27
# Note: preparing data for Gephi

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

# Other related medical supplies----------
## export 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  filter(i %in% baci02_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_ex_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_ex_node.csv",row.names = FALSE) 

## import 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  filter(j %in% baci02_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_im_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_im_node.csv",row.names = FALSE) 


# Other related medical supplies----------
## export 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  filter(i %in% baci23_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_ex_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_ex_node.csv",row.names = FALSE) 

## import 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Other related medical supplies") %>%
  filter(j %in% baci23_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_im_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_im_node.csv",row.names = FALSE) 

# Medical device----------
## export 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  filter(i %in% baci02_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_ex_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_ex_node.csv",row.names = FALSE) 

## import 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  filter(j %in% baci02_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_im_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_02_im_node.csv",row.names = FALSE) 


# Medical device----------
## export 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  filter(i %in% baci23_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_ex_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_ex_node.csv",row.names = FALSE) 

## import 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Medical device") %>%
  filter(j %in% baci23_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_im_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Medical_device_23_im_node.csv",row.names = FALSE) 



# API----------
## export 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  filter(i %in% baci02_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_02_ex_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_02_ex_node.csv",row.names = FALSE) 

## import 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  filter(j %in% baci02_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_02_im_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_02_im_node.csv",row.names = FALSE) 


# API----------
## export 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  filter(i %in% baci23_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_23_ex_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_23_ex_node.csv",row.names = FALSE) 

## import 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="APIs") %>%
  filter(j %in% baci23_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_23_im_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\APIs_23_im_node.csv",row.names = FALSE) 




# Dosage forms----------
## export 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  filter(i %in% baci02_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_02_ex_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_02_ex_node.csv",row.names = FALSE) 

## import 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  filter(j %in% baci02_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_02_im_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_02_im_node.csv",row.names = FALSE) 


# Dosage forms----------
## export 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  filter(i %in% baci23_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_23_ex_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_23_ex_node.csv",row.names = FALSE) 

## import 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Dosage forms") %>%
  filter(j %in% baci23_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_23_im_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Dosage forms_23_im_node.csv",row.names = FALSE) 




# Pharmaceutical preparations----------
## export 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  filter(i %in% baci02_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_02_ex_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_02_ex_node.csv",row.names = FALSE) 

## import 02----------
baci02_1 <- baci02 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci02_2 <- baci02 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  filter(j %in% baci02_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci02_3 <- rbind(baci02_2%>%ungroup()%>%select(code=i),baci02_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci02_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci02_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_02_im_edge.csv",row.names = FALSE) 
write.csv(baci02_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_02_im_node.csv",row.names = FALSE) 


# Pharmaceutical preparations----------
## export 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  group_by(i) %>% 
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(i!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  filter(i %in% baci23_1$i) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(source) %>% 
  arrange(source,desc(weight2))%>%
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=i,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_23_ex_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_23_ex_node.csv",row.names = FALSE) 

## import 23----------
baci23_1 <- baci23 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  group_by(j) %>% # change
  summarize(trade_flow=sum(total_v,na.rm = TRUE)/1000) %>% 
  filter(j!=891) %>%  # Serbia and Montenegro (...2005)")
  arrange(desc(trade_flow))%>%
  mutate(cumulative_share = cumsum(trade_flow) / sum(trade_flow),selected = cumulative_share <= 0.95) %>% 
  filter(selected==TRUE) 

baci23_2 <- baci23 %>% select(-t) %>% 
  filter(cat=="Pharmaceutical preparations") %>%
  filter(j %in% baci23_1$j) %>% 
  left_join(cntry %>% select(i,source=name),by="i") %>% 
  left_join(cntry %>% select(j,target=name),by="j") %>% 
  mutate(weight2=total_v/1000) %>% 
  mutate(weight=ifelse(weight2>=10,log(weight2),0)) %>%
  group_by(target) %>%  
  arrange(target,desc(weight2))%>% 
  mutate(cumulative_share = cumsum(weight2) / sum(weight2), selected = cumulative_share <= 0.75 ) %>% 
  filter(selected==TRUE) %>% 
  mutate(type="directed") 

baci23_3 <- rbind(baci23_2%>%ungroup()%>%select(code=i),baci23_2%>%ungroup()%>%select(code=j)) %>% 
  distinct_all() %>% 
  full_join(baci23_1 %>% select(code=j,trade_flow),by="code") %>% 
  left_join(cntry,by="code") %>% 
  left_join(cntry_geo,by="name") %>% 
  mutate(trade_flow=ifelse(is.na(trade_flow),0,trade_flow)) %>% 
  mutate(id=name) %>% 
  select(id,label=name,trade_flow,latitude,longitude)

write.csv(baci23_2%>% select(source, target, weight, type),"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_23_im_edge.csv",row.names = FALSE) 
write.csv(baci23_3,"C:\\Users\\xiang\\OneDrive\\planner\\gephi\\Pharmaceutical preparations_23_im_node.csv",row.names = FALSE) 




[1] "APIs"                                           "Dosage forms"                                  
[3] "Medical device"                                                    
[5] "Other related medical supplies"                 "Pharmaceutical preparations"  






