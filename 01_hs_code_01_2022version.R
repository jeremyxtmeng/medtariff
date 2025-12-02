# Medical Goods
# created: 2025-07-04
# updated: 2025-07-27
# Note: compile and identify pharmaceutical and medical goods in HS 2022
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

#  HS code
# (https://www.usitc.gov/harmonized_tariff_information/hts/archive/list)
vhts22 <- read.csv(".\\Data\\raw\\hts_2022_basic_csv.csv") %>%
  select(HTS22=HTS.Number, level=Indent, product=Description) %>%
  mutate(HTS22=str_remove(HTS22, "[.]")) %>%
  mutate(HS6=substr(HTS22,1,6)) %>%
  mutate(HTS22=str_remove(HTS22, "[.]")) %>%
  mutate(HTS22=str_remove(HTS22, "[.]")) 


# part 1: using HS2022 census end-use codes ------------
# Note: downloading the file from https://dataweb.usitc.gov/classification/commodity-translation
# Once downloaded, 1) selecting year=2022 and census_issue_date=January 2022
#                  2) renaming "hts10" to "HTS22"
#                  3) saving the file as "commodity_translation_wizard.xlsx"

enduse<- read_excel(".\\Data\\raw\\commodity_translation_wizard.xlsx", sheet = "Import Concordance") %>% 
  select(-naics,-year,-census_issue_date) %>% 
  filter(end_use=="21610"|end_use=="40100") %>% 
  mutate(description_long=tolower(description_long),description_short=tolower(description_short)) %>% 
  mutate(HS6=substr(HTS22,1,6)) %>% 
  mutate(CHP=substr(HS6,1,2)) %>% 
  mutate(HEADING=substr(HS6,1,4)) %>% 
  mutate(cat_enduse=case_when(CHP=="29"& end_use=="40100"~"APIs",      # going to HTS 2022 Basic Edition (01/26/2022)
                              HS6=="300310" & end_use=="40100"~"APIs", # and manually identifying APIs 
                              HS6=="300320" & end_use=="40100"~"APIs", # (e.g. "not put up in measured doses or in forms or packings for retail sale)
                              HS6=="300331" & end_use=="40100"~"APIs", # and dosage forms  Chapter 30. 
                              HS6=="300339" & end_use=="40100"~"APIs",
                              HS6=="300341" & end_use=="40100"~"APIs",
                              HS6=="300342" & end_use=="40100"~"APIs",
                              HS6=="300343" & end_use=="40100"~"APIs",
                              HS6=="300349" & end_use=="40100"~"APIs",
                              HS6=="300360" & end_use=="40100"~"APIs",
                              HS6=="300390" & end_use=="40100"~"APIs",
                              HS6=="300213" & end_use=="40100"~"APIs", # ..., not put up in measured doses or in forms or packings for retail sale
                              HS6=="300214" & end_use=="40100"~"APIs",
                              HS6=="300693"& end_use=="40100"~"Dosage forms",
                              HS6=="300215"& end_use=="40100"~"Dosage forms",
                              HEADING=="3004"& end_use=="40100"~"Dosage forms")) %>% 
  mutate(HTS22=case_when(HTS22=="2936240000"~"2936240010", # reformatting HTS codes to match ones in the HTS 2022 Basic Edition
                         HTS22=="3002410000"~"30024100",
                         TRUE~HTS22)) %>% 
  left_join( vhts22 %>% select(HS6_0=HS6, HTS22),by="HTS22") %>% 
  filter(!is.na(HS6_0)) %>% 
  select(-HS6_0)
  #mutate(HTS22=case_when(HTS22=="30024100"~"3002410000", TRUE~HTS22)) %>% 

# validation 
# Note: validating HTS code in enduse can be 
#        matched to the HTS22 basic edition. For unmatched codes,
#        manually modifying HTS in enduse. 
#vhts22_2 <- left_join(enduse, vhts22,by="HTS22")



# part 2: medical device ----------

## part 2.1: The Inter-American Coalitions's HS codes -----------
device<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "TAB1_MedicalDeviceHTSCodes") %>% 
  select(-note) %>% 
  mutate(HTS22=str_remove(HTS22, "[.]")) %>% 
  mutate(HTS22=str_remove(HTS22, "[.]")) %>% 
  mutate(HTS22=str_remove(HTS22, "[.]")) %>% 
  mutate(HS6=substr(HTS22,1,6)) %>% 
  filter(HS6!="871300") %>% 
  mutate(cat_device="Medical device") %>% 
  mutate(HTS22=case_when(HTS22=="300211"~"3822110000", # reformatting HTS codes to match ones in the HTS 2022 Basic Edition
                         HTS22=="40151101"~"4015121000",
                         HTS22=="9025198040"~"90251980",
                         HTS22=="90278045"~"90278945",
                         TRUE~HTS22)) %>% 
  left_join(vhts22,by="HTS22") %>% 
  select(-level,-product) %>% 
  mutate(HTS22 = case_when(is.na(HS6.y)& str_length(HTS22) == 6 ~ str_c(HTS22, "00"), TRUE ~ HTS22)) %>% 
  left_join(vhts22,by="HTS22") %>% 
  select(-level,-product) %>% 
  mutate(HTS22 = case_when(is.na(HS6)& str_length(HTS22) == 8 ~ str_c(HTS22, "00"), TRUE ~ HTS22)) %>% 
  left_join(vhts22,by="HTS22")%>% 
  filter(!is.na(product)) %>% 
  select(cat_device, product_device, HS6=HS6.y.y, HTS22)

## part 2.2: Chad Bown's HS codes ----------
# missed some HTS 10 digit codes
bown<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "bown") %>% 
  select(-include_other_product) %>%
  mutate(HTS22=as.character(HTS22)) %>%
  distinct(HS6, .keep_all = TRUE) %>% 
  mutate(cat_bown="PPE") %>% 
  select(cat_bown, product_bown, HS6) %>% 
  mutate(HS6=as.character(HS6)) 

## part 2.3: use Bai et al.'s HS codes --------
bai<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "bai_md") %>% 
  mutate(HS6=as.character(HS6)) %>%
  filter(HS6!="9018") %>% 
  mutate(cat_bai_device="Medical device") 

## part 2.4: WITS HS codes ------
wits<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "wits") %>% 
  mutate(HS6=as.character(HS6)) %>% 
  distinct(HS6, .keep_all = TRUE)

## part 2.5: WCO's HS codes ------
wco<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "wco") %>% 
  mutate(HS6=as.character(HS6)) %>%
  mutate(HS6=str_remove(HS6, "[.]")) %>%
  filter(HS6!="901800") %>% 
  distinct(HS6, .keep_all = TRUE)

## part 2.6: WTO's HS codes -------
wto2<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "wto_2") %>% 
  select(cat_wto_2=product_wto_2_2022, HS6=HS22, product_wto_2) %>% 
  mutate(HS6=as.character(HS6)) 

wto1<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "wto_1") %>% 
  select(cat_wto_1=product_wto_1, HS6,product_wto_1=product_n_wto_1) %>% 
  mutate(HS6=as.character(HS6)) 

## part 2.7: World Bank's HS codes -----
wb<- read_excel(".\\Data\\raw\\existing_medical_goods_hs_code.xlsx", sheet = "wb") %>%
  select(HS6=HS17,cat_wb,product) %>% 
  mutate(HS6=as.character(HS6)) %>%
  filter(HS6!="3005") %>% 
  filter(HS6!="3401") %>% 
  mutate(product_wb=trimws(str_extract(product, ".*(?=\\-)")) ) %>% 
  select(HS6, cat_wb, product_wb)

## part 2.8: combining everything together -----

med01 <- full_join(enduse %>% distinct(HS6, .keep_all = TRUE) %>% select(-CHP,-HEADING),device %>% select(-HTS22) %>% distinct(HS6, .keep_all = TRUE) ,by="HS6")
med02 <- full_join(med01,bown,by="HS6")
med03 <- full_join(med02,bai,by="HS6")
med04 <- full_join(med03,wits,by="HS6")
med05 <- full_join(med04,wco,by="HS6")
med06 <- full_join(med05,wto2,by="HS6")
med07 <- full_join(med06,wto1,by="HS6")
med08 <- full_join(med07,wb,by="HS6")
rm(med01,med02,med03,med04,med05,med06,med07)

# part 3: combining codes -----------
med09 <- med08 %>% 
  mutate(cat=case_when(!is.na(HTS22)&cat_enduse=="APIs"~"APIs",
                       !is.na(HTS22)&cat_enduse=="Dosage forms"~"Dosage forms",
                       is.na(cat_enduse)&!is.na(cat_device)~cat_device,
                       !is.na(HTS22)&end_use=="40100" & is.na(cat_enduse)~"Pharmaceutical preparations",
                       !is.na(HTS22)&end_use=="21610" & is.na(cat_device)~"Medical device", # 07/27: "Other medical device" to "Medical device"
                       TRUE~"Other related medical supplies")) %>% 
  mutate(sub_cat_other_med_supplies=case_when(cat=="Other related medical supplies"&!is.na(cat_wits)~cat_wits,
                                              cat=="Other related medical supplies"&is.na(cat_wits)&!is.na(cat_wco)~cat_wco,
                                              cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&!is.na(cat_wto_2)~cat_wto_2,
                                              cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&!is.na(cat_wto_1)~cat_wto_1,
                                              cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&is.na(cat_wto_1)&!is.na(cat_wb)~cat_wb)) %>% 
  mutate(product=case_when(!is.na(HTS22)&cat_enduse=="APIs"~description_short,
                       !is.na(HTS22)&cat_enduse=="Dosage forms"~description_short,
                       is.na(cat_enduse)&!is.na(cat_device)~product_device,
                       !is.na(HTS22)&end_use=="40100" & is.na(cat_enduse)~description_short,
                       !is.na(HTS22)&end_use=="21610" & is.na(cat_device)~description_short,
                        cat=="Other related medical supplies"&!is.na(cat_wits)~product_wits,
                        cat=="Other related medical supplies"&is.na(cat_wits)&!is.na(cat_wco)~product_wco,
                        cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&!is.na(cat_wto_2)~product_wto_2,
                        cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&!is.na(cat_wto_1)~product_wto_1,
                        cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&is.na(cat_wto_1)&!is.na(cat_wb)~product_wb)) %>% 
  mutate(source=case_when(!is.na(HTS22)&cat_enduse=="APIs"~"Census end-use code",
                           !is.na(HTS22)&cat_enduse=="Dosage forms"~"Census end-use code",
                           is.na(cat_enduse)&!is.na(cat_device)~"Inter-American Coalition for Regulatory Convergence for the Medical Technology Sector",
                           !is.na(HTS22)&end_use=="40100" & is.na(cat_enduse)~"Census end-use code",
                           !is.na(HTS22)&end_use=="21610" & is.na(cat_device)~"Census end-use code",
                          cat=="Other related medical supplies"&!is.na(cat_wits)~"WITS",
                          cat=="Other related medical supplies"&is.na(cat_wits)&!is.na(cat_wco)~"WCO",
                          cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&!is.na(cat_wto_2)~"WTO source 2",
                          cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&!is.na(cat_wto_1)~"WTO source 1",
                          cat=="Other related medical supplies"&is.na(cat_wits)&is.na(cat_wco)&is.na(cat_wto_2)&is.na(cat_wto_1)&!is.na(cat_wb)~"World Bank")) %>% 
  arrange(HTS22,HS6) %>% 
  filter(HS6!="300219"&
         HS6!="300220"&
         HS6!="300620"&
         HS6!="340212"&
         HS6!="340213"&
         HS6!="382200"&
         HS6!="401511"&
         HS6!="902780") %>% 
  mutate(HS6=case_when(HS6=="340220"~"340250", TRUE~HS6)) 

# validation 2
# HS6 code
# https://unstats.un.org/unsd/classifications/Econ
 # hs22 <- read_excel(".\\Data\\raw\\HSCodeandDescription.xlsx", sheet = "HS22") %>% mutate(yr1="22")
 # hs17 <- read_excel(".\\Data\\raw\\HSCodeandDescription.xlsx", sheet = "HS17") %>% mutate(yr2="17")
 # 
 # med10 <- med09 %>% filter(is.na(HTS22)) %>% select(HS22=HS6) %>% mutate(HS17=HS22) %>% 
 #   left_join(hs22 %>% select(HS22=Code, yr1),by="HS22") %>% 
 #   left_join(hs17 %>% select(HS17=Code, yr2),by="HS17") %>% 
 #   filter(is.na(yr1)& !is.na(yr2))
 
 # manually validating codes with the ref. from WCO:
 # https://www.wcoomd.org/-/media/wco/public/global/pdf/topics/nomenclature/instruments-and-tools/hs-nomenclature-2022/table-i_en.pdf?la=en
 
# delete: 300219
# delete 300220 (to 300241)
# delete 300620 (to 382213)
# delete 340212 (to 340241)
# delete 340213 (to 340242)
# delete 382200 (to 382212)
# delete 401511 (to 401512)
# delete 902780 (to 902781)
# 340220 (to 340250)

 
# part 4: finalizing medical products HS6 ----------
med10 <- med09 %>% 
  filter(HS6!="240412"& HS6!="240419"&HS6!="240492"&HS6!="240499") %>%  # tobacco products
  mutate(sub_cat_other_med_supplies=case_when(sub_cat_other_med_supplies=="Anti-epidemic goods"~"Disinfectants and sterilisation products",
                                              sub_cat_other_med_supplies=="personal protective equipment"~"Protective garments and the like",
                                              sub_cat_other_med_supplies=="personal protective equipments"~"Protective garments and the like",
                                              sub_cat_other_med_supplies=="personal protective products"~"Protective garments and the like",
                                              sub_cat_other_med_supplies=="Oxygen Therapy equipment and pulse oximeters"~"Medical equipment",
                                              sub_cat_other_med_supplies=="Instruments and apparatus used in Diagnostic Testing"~"Medical supplies",
                                              sub_cat_other_med_supplies=="Other medical devices and equipment"~"Medical Equipment",
                                              sub_cat_other_med_supplies=="Other Medical related goods"~"Medical Equipment",
                                              sub_cat_other_med_supplies=="other medical supplies"~"Medical Supplies",
                                              product=="Medical Masks"~"Protective garments and the like",
                                              product=="Protective garments"~"Protective garments and the like",
                                              product=="Gloves"~"Protective garments and the like",
                                              product=="Medical Masks"~"Protective garments and the like",
                                              product=="Soap"~"Disinfectants and sterilisation products",
                                              product=="Liquid or cream hand or skin washes put up for retail sale"~"Disinfectants and sterilisation products",
                                              TRUE~sub_cat_other_med_supplies)) %>% 
  mutate(product=case_when(product=="Other" & product_wb=="Gloves"~"Gloves",
                           product=="Other" & product_wb=="Disposable medical headwear"~"Disposable medical headwear",
                           TRUE~product)) %>% 
  mutate(product=tolower(product),sub_cat_other_med_supplies=tolower(sub_cat_other_med_supplies)) %>% 
  mutate(HTS_v2022_code=case_when(!is.na(HTS22)|cat=="Medical device"~"Yes", TRUE~"No")) %>% 
  select(cat,sub_cat_other_med_supplies,product,HS6,source,HTS_v2022_code) 
                                               
#write.csv(med10,".\\Data\\processed\\med_goods_hs6.csv",row.names = FALSE) 


# part 5: finalizing medical products HTS10 ----------

# HTS10 from Inter-American Coalition for Regulatory Convergence for the Medical Technology Sector
device1 <- device %>% filter(str_length(HTS22)==6)
device2 <- device %>% filter(str_length(HTS22)==8)
device3 <- device %>% filter(str_length(HTS22)==10)

vhts22_1 <- vhts22 %>% select(HTS22_6=HTS22) %>% 
  filter(stri_length(HTS22_6)==10) %>% 
  mutate(HTS22=substr(HTS22_6,1,6)) %>% 
  left_join(device1,by="HTS22") %>% 
  filter(!is.na(cat_device)) %>% 
  select(cat_device, product_device,HS6,HTS22=HTS22_6)


vhts22_2 <- vhts22 %>% select(HTS22_6=HTS22) %>% 
  filter(stri_length(HTS22_6)==10) %>% 
  mutate(HTS22=substr(HTS22_6,1,8)) %>% 
  full_join(device2,by="HTS22") %>% 
  filter(!is.na(cat_device))%>% 
  select(cat_device, product_device,HS6,HTS22=HTS22_6)

device4 <- rbind(vhts22_1, vhts22_2)
device_final <- rbind(device4,device3) %>% distinct(HTS22,.keep_all = TRUE)

med11 <- full_join(enduse %>% select(-CHP,-HEADING),device_final,by="HTS22") %>% 
  mutate(cat=case_when(!is.na(HTS22)&cat_enduse=="APIs"~"APIs",
                       !is.na(HTS22)&cat_enduse=="Dosage forms"~"Dosage forms",
                       is.na(cat_enduse)&!is.na(cat_device)~cat_device,
                       !is.na(HTS22)&end_use=="40100" & is.na(cat_enduse)~"Pharmaceutical preparations",
                       !is.na(HTS22)&end_use=="21610" & is.na(cat_device)~"Medical device")) %>% # 07/27: "Other medical device" to "Medical device"
  mutate(product=case_when(!is.na(HTS22)&cat_enduse=="APIs"~description_short,
                       !is.na(HTS22)&cat_enduse=="Dosage forms"~description_short,
                       is.na(cat_enduse)&!is.na(cat_device)~product_device,
                       !is.na(HTS22)&end_use=="40100" & is.na(cat_enduse)~description_short,
                       !is.na(HTS22)&end_use=="21610" & is.na(cat_device)~description_short)) %>% 
  mutate(HS6=case_when(!is.na(HS6.x)~HS6.x,!is.na(HS6.y)~HS6.y)) %>% 
  select(cat,product,HS6,HTS22) %>% 
  left_join(med10 %>% filter(HTS_v2022_code=="Yes") %>% select(source,HS6),by="HS6") %>% 
  filter(HTS22!="9810008000"&HTS22!="9810008500") %>% 
  mutate(HTS22=case_when(HTS22=="30024100"~"3002410000", TRUE~HTS22))

write.csv(med11,".\\Data\\processed\\med_goods_hts22.csv",row.names = FALSE) 

# part 6: medical products HTS 10 2022 and 2017 ------------
vhts18 <- read.csv(".\\Data\\raw\\hts_2018_basic_csv.csv") %>%
  select(HTS18=HTS.Number, level=Indent, product_18=Description) %>%
  mutate(HTS18=str_remove(HTS18, "[.]")) %>%
  mutate(HS6_18=substr(HTS18,1,6)) %>%
  mutate(HTS18=str_remove(HTS18, "[.]")) %>%
  mutate(HTS18=str_remove(HTS18, "[.]")) 

load(".\\Data\\processed\\hs_correlation\\hs17f.Rda")

hs17f <- rename(hs17f, HS6 =k,cat_17f=cat,product_17f=product)

med12 <- read.csv(".\\Data\\processed\\med_goods_hts22.csv") %>% 
  mutate(HTS18=case_when(HTS22==510004040~"0510004040",
                         TRUE~as.character(HTS22))) %>% 
  mutate(HS6=case_when(HS6==51000~"051000",
                         TRUE~as.character(HS6))) %>% 
  left_join(vhts18,by="HTS18") %>% 
  filter(is.na(level)) %>% 
  select(cat:source)

write.csv(med12,".\\Data\\processed\\unmatched_from_hts22_to_hts17.csv",row.names = FALSE) 

#full_join(hs17f %>% filter(cat_17f!="Other related medical supplies"),by="HS6")



# optional 1: cleaning WTO Pharma agreement data--------------
# products are copied and pasted into this file
# source: https://www.wto.org/english/tratop_e/pharma_ag_e/pharma_agreement_e.htm
sheets <- c("annex_1", "annex_3", "annex_4")

combined_df <- map_dfr(sheets, function(sheet) {
  read_excel(".\\Data\\raw\\wto_pharma_agreement.xlsx", sheet = sheet) %>%
    separate(name, into = c("wto_ref_code", "HS6", "revision_round", "product"),
             sep = " ", extra = "merge") %>%
    mutate(sheet = sheet)
})

#write.csv(combined_df,".\\Data\\processed\\wto_pharma_agreement_product_list.csv",row.names = FALSE) 




