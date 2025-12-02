# Created: Aug 30, 2025
# Updated: Aug 31, 2025
# Extracting shadings of subheadings and notes related to China's Section 301 tariffs in Ch99

from pathlib import Path
import fitz
import pandas as pd
import re
import numpy as np

# 1: List of revisions
# Starting Rev 7 2018
excel_path = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")
directory_2 = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\ch98_99_note_pdf")

sheet_nm = "ch98_99_link"

df_rev = pd.read_excel(excel_path, sheet_name=sheet_nm, dtype=str).fillna("")
sec301_rev = df_rev.loc[df_rev['section301'] == 'Y', ['file_name']]

# sec301_rev = df_rev[df_rev['section301'] == 'Y'][['edition', 'year', 'file_name']]
# sec301_rev.iloc[3:5] select rows 3 and 4
# sec301_rev.loc[3:5] select index with labels 3, 4, 5 if they exist

# for loop can seach all or stop when a search found
# def search_num(start, end):
#     k=[]
#     for i in range(start, end):
#         if i>12: 
#             k.append(i)
#     return k

# search_num(10,15)

# helper functions ------------------

def page_has_block(page, targets) -> bool:
    for blk in page.get_text("blocks"):
        if blk[4] in targets:
            return True
    return False

# Find the smallest page number with a specific text block
def find_page_by_block(doc, targets, start=None, end=None):
    start = 0 if start is None else start
    end = len(doc) if end is None else end
    
    for i in range(start, end):
        if page_has_block(doc[i], targets):
            return i # for loop stops when it is found
    return -99  # Return -99 if no matching page is found

def detect_page_range(doc):
    p_start = find_page_by_block(doc, ["SUBCHAPTER III\n"])
    p_end   = find_page_by_block(doc, ["SUBCHAPTER IV\n"])-1
    p_tab   = find_page_by_block(doc, ["1/ See chapter 99 statistical note 1.\n"],p_start,p_end)
    p_sec301= find_page_by_block(doc, ["1/\n9903.88.01\n"],p_tab, p_end)
    # p_sec301  = find_page_by_block(doc, ['For the purposes of heading 9903.88.01,'],p_start,p_tab)
    return p_end, p_sec301

# regular expression helpers -----------------
NUMERIC_RE    = re.compile(r"\d")
LETTER_RE     = re.compile(r"[A-Za-z]")
WHITESPACE_RE = re.compile(r"\s+")

def is_numeric_text(s: str) -> bool:
    return (s is not None) and (NUMERIC_RE.search(s) is not None) and (LETTER_RE.search(s) is None)

def clean_text(s: str) -> str:
    t = (s or "").replace("\u00A0", " ").replace("Â", "")
    t = t.replace("1/", "").replace("2/","")
    return WHITESPACE_RE.sub("", t)

# detecting shading area ------------------
def page_shaded_rects(page, white_tol=0.98):
    rects = []
    for d in page.get_drawings():
        fill = d.get("fill")
        if fill is None:
            continue
        if isinstance(fill, (list, tuple)) and fill:
            if (sum(fill) / len(fill)) < white_tol:
                if d.get("rect") is not None:
                    rects.append(fitz.Rect(d["rect"]))
                else:
                    pts = []
                    for it in d.get("items", []):
                        if it[0] == "re":
                            rects.append(fitz.Rect(it[1]))
                        else:
                            for p in it[1:]:
                                if hasattr(p, "x") and hasattr(p, "y"):
                                    pts.append((p.x, p.y))
                    if pts:
                        xs, ys = zip(*pts)
                        rects.append(fitz.Rect(min(xs), min(ys), max(xs), max(ys)))
    return rects

def block_is_shaded(brect, shaded_rects, thr=0.15):
    for r in shaded_rects:
        inter = brect & r
        if inter is not None and inter.get_area() > 0:
            if inter.get_area() / max(brect.get_area(), 1e-6) > thr:
                return True
    return False

# 2: Identify locations 
def process_pdf(pdf_path: str) -> pd.DataFrame:
    doc = fitz.open(pdf_path)
    rng = detect_page_range(doc)
    p_end, p_sec301 = rng

    rows = []
    for i in range(p_sec301, p_end + 1):
        page = doc[i]
        shaded_rects = page_shaded_rects(page)
        for blk in page.get_text("blocks"):
            if len(blk) < 5:
                continue
            txt = (blk[4] or "").strip().replace("\n", " ")
            if not is_numeric_text(txt):
                continue
            # Need the block bbox for overlap test, but we don't return it
            brect = fitz.Rect(float(blk[0]), float(blk[1]), float(blk[2]), float(blk[3]))
            if not block_is_shaded(brect, shaded_rects):
                continue
            cleaned = clean_text(txt)
            if cleaned:
                rows.append({"HTS Number": cleaned})
    return pd.DataFrame(rows, columns=["HTS Number"])

#name=sec301_rev.iloc[30,0]
#pdf_path=directory_2/name
#test=process_pdf(pdf_path)

def build_master(directory_2: str, sec301_rev: list[str]) -> tuple[str, pd.DataFrame]:
    # 0) start with an empty master data frame
    master_df = pd.DataFrame()
    base_dir = Path(directory_2)
    for name in sec301_rev:
        p = base_dir / name
        try:
            cleaned = process_pdf(p)
            cleaned["file_name"] = name  # optional provenance column
            # 7) append to the master
            master_df = pd.concat([master_df, cleaned], ignore_index=True)
        except Exception as e:
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df

master = build_master(directory_2, sec301_rev.iloc[:,0])
master = master[master['HTS Number'] != ".................................................."]
# master = master.drop(columns='shading_indator')
master.loc[:, 'expiration_indicator'] = 'Y'
master['edition'] = master['file_name'].str.replace('%99.pdf', '', regex=False)

# loading all csv files and extract suspension

# 1: getting relevant chapters
sec301_rev['csv_name'] = sec301_rev['file_name'].str.replace('%99.pdf', '.csv', regex=False)

def process_one_csv(path: Path) -> pd.DataFrame:
    # 1) load the csv (keep HTS Number as string)
    df = pd.read_csv(path, dtype={"HTS Number": str})

    # 2) keep subheadings 9903.88
    df["HTS Number"] = df["HTS Number"].astype(str).str.strip()
    mask = df["HTS Number"].str[:7].eq("9903.88")
    df = df[mask].reset_index(drop=True)
    
    # if there are indentation of 1 use:
    # mask= mask[mask.fillna(False)]
    # if mask.any():
    #     first_idx = mask.index.min()  # Index of first True
    #     last_idx = mask.index.max()   # Index of last True
    #     df = df.loc[first_idx:last_idx].reset_index(drop=True)
    df['suspension_indicator'] = df['Description'].str.contains('provision suspended', case=False, na=False).map({True: 'Y', False: ''})
    
    return df

def build_sec301_heading(directory_1: str, revised_edition: list[str]) -> tuple[str, pd.DataFrame]:
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

# name=sec301_rev.iloc[2,1]
# pdf_path=directory_1/name
# test=process_one_csv(pdf_path)

sec301_headings = build_sec301_heading(directory_1, sec301_rev.iloc[:,1])
sec301_headings['edition'] = sec301_headings['csv_name'].str.replace('.csv', '', regex=False)

merged = sec301_headings.merge(
    master,
    how="left",
    on=["HTS Number", "edition"],
    suffixes=("_orig", "_mst")
)

merged =merged.drop(['Indent','Unit of Quantity'], axis=1)

def clean_matches(matches):
    if not matches:
        return ""
    s = ", ".join(matches)            # remove list brackets/quotes
    s = re.sub(r"\bor\b", "", s, flags=re.I)   # drop 'or'
    s = re.sub(r"\s*,\s*", ", ", s)            # tidy commas
    s = re.sub(r"\s+", " ", s).strip(" ,")     # trim extras
    return s

sub_pat = r'(?i)(\b9\d{3}[\d.,\s-]*(?:\b(?:and|or)\b\s*\b9\d{3}[\d.,\s-]*)*)' # every group starting with 4 numbers and the first number is 9
note_pat = r'(?i)(?:U\.?\s*S\.?\s*)?notes?\s*(\d+(?:\([^)]+\))*)'

merged["L0_excl_code"] = merged["Description"].str.findall(sub_pat,  flags=re.I).apply(clean_matches)
merged['L0_note']      = merged['Description'].str.findall(note_pat, flags=re.I).apply(clean_matches)

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_HTScode_processed_02_sec301.csv"
merged.to_csv(output_path, index=False)
