# Created; 09/01/2025
# Updated: 09/03/2025
# Based on HTS codes of med goods in 2022 Basic Edition to 
# create a concordance to take account of additions and deletions
# of codes

import pandas as pd
from pathlib import Path
import numpy as np

htscodes_csv= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\processed\med_goods_hts22.csv")

directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw")
directory_2= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")

## 1 verify that HS codes all exist at the Basic Edition 2022
# 1) Load med_goods_hts22.csv, keep HTS22 as string, fix any 9-digit entries by left-padding to 10
med = pd.read_csv(htscodes_csv, dtype={"HTS22": "string"})

# 2) Add "HTS Number" with dots after 4th, 6th, and 8th characters (####.##.##.##)
def to_hts_number(s):
    if pd.isna(s) or len(s) < 10:
        return pd.NA
    s = str(s)
    return f"{s[:4]}.{s[4:6]}.{s[6:8]}.{s[8:10]}"

med["HTS Number"] = med["HTS22"].apply(to_hts_number)
med = med[med['HTS22'] != "9810008500"] # deleting code in Ch 98
med = med[med['HTS22'] != "9810000080"] # deleting code in Ch 98

# 3) Load Basic Edition%2022.csv
basic_22 = pd.read_csv(directory_2/"Basic Edition%2022.csv", dtype="string")

basic_22['HTS Number'] = basic_22['HTS Number'].replace("3002.41.00", "3002.41.00.00")
basic_22=basic_22[['HTS Number','Indent', 'Description', 'General Rate of Duty','Special Rate of Duty','Column 2 Rate of Duty']]

basic_22['hts'] = basic_22['HTS Number'].str[:10]

columns_to_fill = ['General Rate of Duty', 'Special Rate of Duty', 'Column 2 Rate of Duty']

# 1) Treat "" as missing
tmp = basic_22.copy()
tmp[columns_to_fill] = tmp[columns_to_fill].replace('', pd.NA)

# 2) First non-missing value per group/column, repeated to row shape
firsts = tmp.groupby('hts', sort=False)[columns_to_fill].transform('first')

# 3) Fill only the empties/NaNs with that first value; keep others as-is
basic_22[columns_to_fill] = tmp[columns_to_fill].fillna(firsts).fillna('')


merged = med.merge(basic_22, on="HTS Number", how="left")

## ----------------- using existing 6 digit level concordance -----------
## checking hs codes in 2017:
# excel_path=Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\un_stats_hs_correlation\HS2022toHS2017ConversionAndCorrelationTables.xlsx")
# df_con = pd.read_excel(excel_path, sheet_name="HS2022-HS2017 Correlations", dtype=str).fillna("")
# med['HS2022'] = med['HTS22'].str[:6]
# df_2017 = med.merge(df_con, on="HS2022", how="left")
# df_2017_unmatched= df_2017[df_2017['Relationship'] != "1:1"] # deleting code in Ch 98

## -------- constructing tariff table ------------------
csv_path = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
sheet_nm="ch98_99_link"
df_csv = pd.read_excel(csv_path, sheet_name=sheet_nm, dtype=str).fillna("")
csv_rev = df_csv[df_csv['csv_link'] == 'Y']
csv_rev = csv_rev['file_name'].unique()

hscode=med[['HTS Number']]

def clean_tariff_from_csv(csv_path, hscode):
    df = pd.read_csv(csv_path, dtype='string')
    df=df[['HTS Number','Indent', 'General Rate of Duty','Special Rate of Duty','Column 2 Rate of Duty']]

    df['HTS Number'] = df['HTS Number'].replace({'3002.41.00': '3002.41.00.00'})
    df['hts'] = df['HTS Number'].str[:10]
    duty_cols =  ['General Rate of Duty', 'Special Rate of Duty', 'Column 2 Rate of Duty']
    if duty_cols:
        tmp = df.copy()
        tmp[duty_cols] = tmp[duty_cols].replace('', pd.NA)
        firsts = tmp.groupby('hts', sort=False)[duty_cols].transform('first')
        df[duty_cols] = tmp[duty_cols].fillna(firsts).fillna('')

    out = hscode.merge(df, on='HTS Number', how='left')
    return out

test=clean_tariff_from_csv(directory_2/"Basic Edition%2018.csv",hscode) # missing 60
test_25=clean_tariff_from_csv(directory_2/"Revision 1%2025.csv",hscode) # missing 20
test_23=clean_tariff_from_csv(directory_2/"Revision 1%2023.csv",hscode) # missing 4


# def fill_first_non_empty(group):
#     for col in columns_to_fill:
#         # Find the first non-empty, non-NaN value in the column
#         first_value = group[col].iloc[0]
#         # Fill only empty/NaN values with the first non-empty value
#         group[col] = group[col].replace('', pd.NA).fillna(first_value).replace(pd.NA, '')
#     return group

# basic_22 = basic_22.groupby('hts', group_keys=False).apply(fill_first_non_empty, include_groups=False).reset_index(drop=True)


def build_master(directory_2: str, csv_rev: list[str], hscode: pd.DataFrame) -> tuple[str, pd.DataFrame]:
    # 0) start with an empty master data frame
    master_df = pd.DataFrame()
    base_dir = Path(directory_2)
    for name in csv_rev:
        p = base_dir / name
        try:
            cleaned = clean_tariff_from_csv(p, hscode)
            cleaned["file_name"] = name  # optional provenance column
            # 7) append to the master
            master_df = pd.concat([master_df, cleaned], ignore_index=True)
        except Exception as e:
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df


duty_master=build_master(directory_2,csv_rev, hscode)

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\duty_master.csv"

duty_master.to_csv(output_path, index=False)