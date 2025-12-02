# Created: Aug 24, 2024
# Updated:
# Note: Downloading chapters 98 and 99 for all HTS editions since 2017
# and saving PDFs with names in format edition_name%year%chapter_number

# It also downloads all HTS csv files.

from pathlib import Path
import pandas as pd
import requests

excel_path = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")
directory_2 = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\ch98_99_note_pdf")

sheet_nm = "ch98_99_link"

df = pd.read_excel(excel_path, sheet_name=sheet_nm, dtype=str).fillna("")

# some links are wrong and downloading using corrected ones
redownload_df = df[df['re_download'] == 'Y']
csv_link_df = df[df['csv_link'] == 'Y']
csv_redownload_df = df[df['csv_re_download'] == 'Y']

for i, row in csv_redownload_df.iterrows():
    url = row["download_link"].strip()  
    fname = row["file_name"].strip()        
    out_path = directory_1 / fname     

    if out_path.exists():
        print(f"[{i}] Exists, skipping: {out_path.name}")
        continue

    try:
        with requests.get(url, stream=True, timeout=60) as r:
            r.raise_for_status()
            with open(out_path, "wb") as f:
                for chunk in r.iter_content(chunk_size=1 << 14):
                    if chunk:
                        f.write(chunk)
        print(f"[{i}] Saved: {out_path.name}")
    except Exception as e:
        print(f"[{i}] ERROR downloading {url} -> {out_path.name}: {e}")

# ensure 284 files are downloaded for Rev 1 2017 until Rev 18 2025

