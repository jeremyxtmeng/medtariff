# Created: Aug 25, 2024
# Updated:
# Note: Appending descriptions and chapter note information

from pathlib import Path
import pandas as pd
import requests 
import numpy as np
import re

excel_path = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")
sheet_nm="ch98_99_HTScode"

# 1: getting relevant chapters
df = pd.read_excel(excel_path, sheet_name=sheet_nm, dtype=str).fillna("")
df['csv_name'] = df['hts_name']+'.csv'
revised_edition = df['csv_name'].unique()
revised_edition=np.delete(revised_edition,0) # remove empty ones

# 2: compiling hts code for relevant chapters
# and appending descriptions for codes subject to changes

def process_one_csv(path: Path) -> pd.DataFrame:
    # 2) load the csv (keep HTS Number as string)
    df = pd.read_csv(path, dtype={"HTS Number": str})

    # 3) trim rows before the first row whose HTS Number starts with "98"
    df["HTS Number"] = df["HTS Number"].astype(str).str.strip()
    mask_98 = df["HTS Number"].str[:2].eq("98")
    if mask_98.any():
        first_idx = mask_98.idxmax()   # index of first True
        df = df.loc[first_idx:].reset_index(drop=True)

    # 4) create L0..L4
    levels = list(range(5))  # 0..4  (L0, L1, L2, L3, L4)
    for k in levels:
        df[f"L{k}"] = pd.NA

    # 5) copy Description into the matching level based on Indent
    df["Indent"] = pd.to_numeric(df["Indent"], errors="coerce")
    for k in levels:
        sel = df["Indent"].eq(k)
        df.loc[sel, f"L{k}"] = df.loc[sel, "Description"]

    # 6) downward fills (hierarchical “dynamic programming” style)
    # 6.0 fill L0 globally
    df["L0"] = df["L0"].ffill()

    # 6.1–6.4: within each block defined by higher levels, fill the next level
    for k in range(1, 5):
        group_cols = [f"L{j}" for j in range(k)]  # L0, then L0-L1, etc.
        df[f"L{k}"] = df.groupby(group_cols, dropna=False)[f"L{k}"].ffill()

    return df

def build_master(directory_1: str, revised_edition: list[str]) -> tuple[str, pd.DataFrame]:
    # 0) start with an empty master data frame
    master_df = pd.DataFrame()

    base_dir = Path(directory_1)
    for name in revised_edition:
        p = base_dir / name
        try:
            cleaned = process_one_csv(p)
            cleaned["csv_name"] = name  # optional provenance column
            # 7) append to the master
            master_df = pd.concat([master_df, cleaned], ignore_index=True)
        except Exception as e:
            # catch-all just in case
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df


master = build_master(directory_1, revised_edition)
master["Item changed"] = master["HTS Number"] 

# 4: extract subheading
# everything after "provided for in subjeading" with start with ####

merged = df.merge(
    master,
    how="left",
    on=["Item changed", "csv_name"],
    suffixes=("_orig", "_mst")
)

note_pattern = r'(?i)\bnote\b\s*(.*?)\s*\bto\b'
for k in range(5):
    col = f"L{k}"
    out = f"L{k}_note"
    if col in merged.columns:
        merged[out] = (
            merged[col]
            .astype(str)
            .str.extract(note_pattern, expand=False)
            .str.strip()
        )

# keep your "start at first ####" pattern
sub_pat = r'(?i)(\d{4}[\d.,\s-]*(?:\b(?:and|or)\b\s*\d{4}[\d.,\s-]*)*)'
# CAS numbers look like 2–7 digits - 2 digits - 1 digit
cas_pat = r'\b\d{2,7}-\d{2}-\d\b'

for k in range(5):
    col = f"L{k}"
    out = f"L{k}_subheading"
    if col in merged.columns:
        # get ALL matches, then remove any that are CAS numbers; keep the last remaining
        matches = merged[col].astype(str).str.findall(sub_pat, flags=re.I)
        merged[out] = matches.apply(
            lambda lst: next((x for x in reversed(lst) if not re.fullmatch(cas_pat, x)), None)
        )

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_HTScode_processed_01.csv"
merged.to_csv(output_path, index=False)
