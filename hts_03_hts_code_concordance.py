# Created; 09/01/2025
# Updated: 09/03/2025
# Based on HTS codes of med goods in 2022 Basic Edition to 
# create a concordance to take account of additions and deletions
# of codes

import pandas as pd
from pathlib import Path
import numpy as np
import re

htscodes_csv= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\processed\med_goods_hts22.csv")
excel_path=Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw")
directory_2= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")

# loading the change records of tariff codes
# To use the following for the sheet already sorted by "Item changed"
columns_to_read = ['Item changed', 'Nature of change', 'selection_note','Edition','Year'] 
change_record = pd.read_excel(excel_path, sheet_name="change_record_cleaned", nrows=7000, usecols=columns_to_read, dtype=str).fillna("")

def to_hts_adding(s):
    if pd.isna(s) or len(s) < 12:
        return pd.NA
    s = str(s)
    return re.sub(r'^(.{10})', r'\1.',s)

change_record['HTS Number'] =change_record['Item changed'].apply(to_hts_adding)

## 1 verify that HS codes all exist at the Basic Edition 2022
# 1) Load med_goods_hts22.csv, keep HTS22 as string, fix any 9-digit entries by left-padding to 10
med = pd.read_csv(htscodes_csv, usecols=['HTS22'], dtype={"HTS22": "string"})

sheet_nm="ch98_99_HTScode"

# 1: getting relevant chapters
df = pd.read_excel(excel_path, sheet_name="ch98_99_link", dtype=str).fillna("")
csv_link_df = df.loc[df['csv_link'] == 'Y', ['file_name']]


# 2) Add "HTS Number" with dots after 4th, 6th, and 8th characters (####.##.##.##)
def to_hts_number(s):
    if pd.isna(s) or len(s) < 10:
        return pd.NA
    s = str(s)
    return f"{s[:4]}.{s[4:6]}.{s[6:8]}.{s[8:10]}"

med = med[med['HTS22'] != "9810008500"] # deleting code in Ch 98
med = med[med['HTS22'] != "9810000080"] # deleting code in Ch 98

med["HTS Number"] = med["HTS22"].apply(to_hts_number)

def merge_one_csv(med:pd.DataFrame, directory:Path):
    basic_22 = pd.read_csv(directory, dtype="string")
    basic_22['HTS Number'] = basic_22['HTS Number'].replace("3002.41.00", "3002.41.00.00")
    basic_22=basic_22[['HTS Number','Indent']]
    merged = med.merge(basic_22, on="HTS Number", how="left")
    merged= merged[merged['Indent'].isna()] # deleting code in Ch 98
    return merged


def build_master(directory_2: str, csv_link_df: list[str], med:pd.DataFrame) -> tuple[str, pd.DataFrame]:
    # 0) start with an empty master data frame
    master_df = pd.DataFrame()
    base_dir = Path(directory_2)
    for name in csv_link_df:
        p = base_dir / name
        try:
            cleaned = merge_one_csv(med,p)
            cleaned["file_name"] = name  # optional provenance column
            # 7) append to the master
            master_df = pd.concat([master_df, cleaned], ignore_index=True)
        except Exception as e:
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df

master = build_master(directory_2, csv_link_df.iloc[:,0],med)

hs_missing = master[['HTS Number']].drop_duplicates().assign(missing='Y')
#hs_merge=change_record.merge(hs_missing, on="HTS Number", how="outer")

key = "HTS Number"

# 1) Ensure hs_missing is unique on the key (avoid row blow-ups)
hs_missing_u = hs_missing.drop_duplicates(subset=[key])

# 2) Left-join to preserve the original order of change_record
left = change_record.merge(hs_missing_u, on=key, how="left", sort=False)

# 3) Anti-join: rows in hs_missing not present in change_record
extra = hs_missing_u.loc[~hs_missing_u[key].isin(change_record[key])]

# 4) Keep union of columns and append unmatched at the very end
all_cols = left.columns.union(extra.columns)
hs_merge = pd.concat(
    [left.reindex(columns=all_cols), extra.reindex(columns=all_cols)],
    ignore_index=True
)

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\concordance_raw.csv"

hs_merge.to_csv(output_path, index=False)
