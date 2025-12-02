# Created: Sept 8, 2025
# Updated:
# Note: using CSV files to construct baseline tariffs

from pathlib import Path
import pandas as pd
import numpy as np
import re

excel_path=Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw")
directory_2= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")

#--------------loading csv path---------------------------
df = pd.read_excel(excel_path, sheet_name="ch98_99_link", dtype=str).fillna("")
csv_link_df = df.loc[df['csv_link'] == 'Y', ['file_name']]


csv_date = df.loc[df['csv_link'] == 'Y', ['file_name','valid_from','valid_to']]
csv_date['valid_from'] = pd.to_datetime(csv_date['valid_from'], errors='coerce').dt.strftime('%Y-%m-%d')
csv_date['valid_to'] = pd.to_datetime(csv_date['valid_to'], errors='coerce').dt.strftime('%Y-%m-%d')

#--------------loading HS codes ------------------------
htscodes_csv= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\processed\med_goods_hts22_final.csv")
med = pd.read_csv(htscodes_csv, usecols=['HTS22'], dtype={"HTS22": "string"})

#  Add "HTS Number" with dots after 4th, 6th, and 8th characters (####.##.##.##)
def to_hts_number(s):
    if pd.isna(s) or len(s) < 10:
        return pd.NA
    s = str(s)
    return f"{s[:4]}.{s[4:6]}.{s[6:8]}.{s[8:10]}"

med["HTS Number"] = med["HTS22"].apply(to_hts_number)

#------------- loading csv files------------------------
def process_one_csv(path: Path) -> pd.DataFrame:
    # 2) load the csv (keep HTS Number as string)
    df = pd.read_csv(path,  dtype="string")
    df['HTS Number'] = df['HTS Number'].replace("3002.41.00", "3002.41.00.00")

    mask = (
    df['Special Rate of Duty'].isna() &
    df['General Rate of Duty'].astype('string').str.strip().eq('Free'))
    df.loc[mask, 'Special Rate of Duty'] = 'Free'

    cols = ['General Rate of Duty', 'Special Rate of Duty', 'Column 2 Rate of Duty']

    # 1) HS2 = first 10 chars only when length >= 10; otherwise NA
    s = df['HTS Number'].astype('string')
    df['HS2'] = s.where(s.str.len() >= 10, pd.NA).str[:10]

    # 2) Forward-fill those three columns within each HS2 group (preserves row order)
    df[cols] = df.groupby('HS2', sort=False)[cols].ffill()
    df=df[['HTS Number','Indent','General Rate of Duty', 'Special Rate of Duty', 'Column 2 Rate of Duty']]

    return df

# merge med to csv
def merge_one_csv(med:pd.DataFrame, directory:Path):
    one_edition= process_one_csv(directory)
    merged = med.merge(one_edition, on="HTS Number", how="left")
    merged = merged.dropna(subset=['Indent']) # keeping merged valued
    return merged

#name=directory_2/csv_link_df.iloc[131,0]
#test=merge_one_csv(med,name)

# combine all csv
def build_master(directory_2: str, med:pd.DataFrame, csv_link_df: list[str]) -> tuple[str, pd.DataFrame]:
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
            # catch-all just in case
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df


master=build_master(directory_2,med, csv_link_df.iloc[:,0])

#------------- format general rate of duty --------------
# removing HS codes without duties
no_duty = master['General Rate of Duty'].astype('string').str.match(r'(?i)^\s*The rate applicable to the article\b')
master = master.loc[~no_duty].reset_index(drop=True)

# removing specific duties
TOK_WITH_SLASH = re.compile(r'(?<!\S)\S*/\S*(?!\S)')

def _slash_rule_cell(x):
    if pd.isna(x):
        return x
    s = str(x)
    if '+' in s:
        # delete any token containing '/'
        out = TOK_WITH_SLASH.sub('', s)
    else:
        # replace any token containing '/' with 'Free'
        out = TOK_WITH_SLASH.sub('Free', s)
    out = ' '.join(out.split()).strip()  # normalize spaces
    return pd.NA if out == '' else out

# forcing cells with only specific duties to be free
master['General Rate of Duty'] = master['General Rate of Duty'].astype('string').apply(_slash_rule_cell)
master['General Rate of Duty'] =master['General Rate of Duty'].str.replace('+', '', regex=False).str.replace(r'\s+', '', regex=True).str.strip()

master['Column 2 Rate of Duty'] = master['Column 2 Rate of Duty'].astype('string').apply(_slash_rule_cell)
master['Column 2 Rate of Duty'] =master['Column 2 Rate of Duty'].str.replace('+', '', regex=False).str.replace(r'\s+', '', regex=True).str.strip()


# removing spaces
master['Special Rate of Duty'] =master['Special Rate of Duty'].str.replace(r'\s+', '', regex=True).str.strip()

# pattern: Free ( ... ) → capture what's inside the parentheses
pat_capture = r'(?i)\bFree\s*\(([^)]*)\)'
pat_remove  = r'(?i)\bFree\s*\([^)]*\)'

s = master['Special Rate of Duty'].astype('string')

# 1) extract the bracket contents right after "Free" into a new column
master['free_note'] = s.str.extract(pat_capture)
master['Special Rate of Duty2']=master['Special Rate of Duty']
# 2) delete "Free(...)" from the original column and clean spaces
master['Special Rate of Duty2'] = (
    s.str.replace(pat_remove, '', regex=True)
     .str.replace(r'\s+', ' ', regex=True)
     .str.strip()
     .replace('', pd.NA)
)

master['Special Rate of Duty2'] =master['Special Rate of Duty2'].str.replace('A+', 'Aplus', regex=False)
master['Special Rate of Duty2'] =master['Special Rate of Duty2'].str.replace('A*', 'Astar', regex=False)

#--------- -------cleaning FTA------------------------

# 1) EXTRACT all (...) contents from Special Rate of Duty2 → Special Rate of Duty3 (comma-joined)
extracted = master['Special Rate of Duty2'].astype('string').str.findall(r'\(([^()]*)\)')

def join_or_na(lst):
    if not isinstance(lst, list):
        return pd.NA
    joined = ','.join(t.strip() for t in lst if isinstance(t, str) and t.strip())
    return joined if joined else pd.NA

master['Special Rate of Duty3'] = extracted.map(join_or_na).astype('string')

s2 = master['Special Rate of Duty2'].astype('string')
mask_no_pct = ~s2.str.contains('%', na=False)
master.loc[mask_no_pct, 'Special Rate of Duty2'] = pd.NA

# 3) Build "special duty":
#    if Special Rate of Duty2 is empty → combine free_note and Special Rate of Duty3 (comma if both present)
#    else → just free_note
free_note = master['free_note'].astype('string')
sr3 = master['Special Rate of Duty3'].astype('string')

combo = (
    free_note.fillna('') +
    np.where(free_note.notna() & sr3.notna(), ',', '') +
    sr3.fillna('')
).str.strip().replace('', pd.NA)

master['special duty'] = np.where(master['Special Rate of Duty2'].isna(), combo, free_note)

master['Special Rate of Duty2'] = (
    master['Special Rate of Duty2'].astype('string')
    .str.replace(r'\([^()]*\)', '', regex=True)  # drop (...) chunks
    .str.replace(r'\s+', ' ', regex=True)        # collapse spaces
    .str.strip()
    .replace('', pd.NA)
)

master['special duty']=master['special duty'].str.replace(r'\s+', ' ', regex=True).str.strip()

mask_empty = s2.isna() | s2.str.fullmatch(r'\s*')  # treat NaN or whitespace-only as empty
master.loc[mask_empty, 'Special Rate of Duty3'] = pd.NA

#-----------------Pharma and Chemical FTA---------------
# K or L Free
s_pharma = master['special duty'].astype('string')

# match K or L as a comma-separated token (handles start/end and spaces)
pattern_pharma = r'(?i)(^|,)\s*(K|L|C)\s*(,|$)'
mask = s_pharma.str.contains(pattern_pharma, regex=True, na=False)

master.loc[mask, ['special duty', 'General Rate of Duty']] = ['', 'Free']


#---------------- adding dates --------------------------
master=master.merge(csv_date,on="file_name", how="left")


output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\tariff_ch1_97.csv"
    
master.to_csv(output_path, index=False)




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
