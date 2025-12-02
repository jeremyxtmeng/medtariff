# Medical Goods
# created: 2025-07-28
# last updated: 2025-08-14
# last updated: 2025-04-05: updated list of HS codes and adding July 2025
# Note: constructing trade flows using Census API

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
library(censusapi)
library(foreign)

setwd("C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\")

#https://www.hrecht.com/censusapi/articles/getting-started.html

# api:71178d1b890b8187d0200c97ff30acd14e2c1a28

#df_name <- listCensusApis(name = NULL, vintage = NULL)

# Time Series International Trade: Monthly U.S. Imports by Harmonized System (HS) Code
# timeseries/intltrade/imports/hs

# api_sahie <- listCensusApis(name = "timeseries/intltrade/imports")
# sahie_vars2 <- listCensusMetadata( name = "timeseries/intltrade/imports/hs",  type = "variables")

#GEN_VAL_MO (15-digit General Imports, Total Value)
#GEN_CIF_MO (15-digit General Imports, CIF Value)
#DUT_VAL_MO (15-digit Imports for Consumption, Dutiable Value)
#GEN_QY1_MO General Imports, Quantity 1
#CAL_DUT_MO (15-digit Imports for Consumption, Calculated Duty)

hts <- read_csv(".\\Data\\processed\\med_goods_hts22_final.csv", col_types = cols(.default = col_character()))
                
#load(".\\Data\\processed\\med_imports_from_2017_01_to_2025_04_part1.Rdata")
#downloaded_code <-med_import %>% select(I_COMMODITY) %>% distinct()

# If an existing dataset is already in your env, we'll append to it.
rm(med_import)

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
        vars  = c("CTY_CODE","GEN_VAL_MO","GEN_CIF_MO","DUT_VAL_MO","GEN_QY1_MO","CAL_DUT_MO"),
        time  = "from 2017-01",
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

save(med_import,file=".\\Data\\processed\\med_imports_from_2017_01_to_2025_07_all_data.Rdata")
write.csv(med_import,".\\Data\\processed\\med_imports_from_2017_01_to_2025_07_all_data.csv")


med_summary <- med_import %>% filter( I_COMMODITY == "3821000000" )

#
library(haven)
mydata <- read_dta("C:\\Users\\xiang\\Downloads\\rtp kenney fajebaum tariff\\rtp\\data\\analysis\\m_flow_hs10_fm_new.dta", n_max=10000) 

canada_mydata <- mydata %>% filter(year==2014 & month==8) %>% filter(hs10==102294054)

# testing
sahie_counties2 <- getCensus(
  name  = "timeseries/intltrade/imports/hs",
  key= "71178d1b890b8187d0200c97ff30acd14e2c1a28",
  vars  = c("CTY_CODE", "GEN_VAL_MO","GEN_CIF_MO","DUT_VAL_MO","GEN_QY1_MO","CAL_DUT_MO"),
  time="2014-08",
  CTY_CODE="1220",
  I_COMMODITY ="0102294054")
