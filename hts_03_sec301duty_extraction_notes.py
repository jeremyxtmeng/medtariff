# Created: Aug 30, 2025
# Updated: Sept 3, 2025
# Extracting shadings of subheadings and notes related to China's Section 301 tariffs in Ch99

from pathlib import Path
import fitz
import pandas as pd
import re
import numpy as np

#---------List of revisions pdfs------------------------
# Starting Rev 7 2018
excel_path = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\HTS_schedule.xlsx")
htscodes_csv= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\processed\med_goods_hts22_final.csv")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")
directory_2 = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\ch98_99_note_pdf")

# pdf links
df_rev = pd.read_excel(excel_path, sheet_name="ch98_99_link", usecols=['section301','file_name'], dtype=str).fillna("")
sec301_rev = df_rev.loc[df_rev['section301'] == 'Y', ['file_name']]
# list of hs codes
df_hscode= pd.read_csv(htscodes_csv, usecols=['HTS22'], dtype={"HTS22": "string"})
#----------------------------------------------------

#------------- building text searches----------------
# def build_master_regex(code_list):
#     sep = r'[.\s\u00A0-]?'
#     parts = []
#     for c in code_list:
#         a,b,c2,d = c[:4], c[4:6], c[6:8], c[8:10]
#         parts.append(rf'(?P<C{c}>{a}{sep}{b}{sep}{c2}(?:{sep}{d})?)')
#     return re.compile(r'(?<!\d)(?:' + '|'.join(parts) + r')(?!\d)')

# # Regular expression pattern based on hs codes
# master_re = build_master_regex(df_hscode['HTS22'])

# def find_codes_in_block_once(txt, master_re):
#     out = []
#     for m in master_re.finditer(txt):
#         # find which named group matched
#         code = next(k[1:] for k,v in m.groupdict().items() if v is not None)  # drop leading 'C'
#         #out.append((code, m.group(0))) # the code appeared in the text
#         out.append(code)
#     return out
from collections import defaultdict

def build_master_regex(code_list):
    sep = r'[.\s\u00A0-]?'
    parts = []
    for c in code_list:
        c = re.sub(r'\D+', '', str(c))  # just digits
        a,b,c2,d = c[:4], c[4:6], c[6:8], c[8:10]
        # unique named group per code (kept for specificity)
        parts.append(rf'(?P<C{c}>{a}{sep}{b}{sep}{c2}(?:{sep}{d})?)')
    return re.compile(r'(?<!\d)(?:' + '|'.join(parts) + r')(?!\d)')

# Precompute an index: 8-digit prefix -> list of 10-digit codes
def _prefix_index(code_list):
    by8 = defaultdict(list)
    for c in code_list:
        digits = re.sub(r'\D+', '', str(c))
        if len(digits) == 10:
            by8[digits[:8]].append(digits)
    return by8

BY8 = _prefix_index(df_hscode['HTS22'])
master_re = build_master_regex(df_hscode['HTS22'])

def find_codes_in_block_once(txt, master_re=master_re, by8=BY8, dedup=True):
    out, seen = [], set()
    for m in master_re.finditer(txt):
        # normalize matched substring to digits only
        digits = re.sub(r'\D+', '', m.group(0))
        if len(digits) == 10:
            if (not dedup) or (digits not in seen):
                out.append(digits); seen.add(digits)
        elif len(digits) == 8:
            for code in by8.get(digits, []):
                if (not dedup) or (code not in seen):
                    out.append(code); seen.add(code)
        # else: ignore weird lengths
    return out

# test="1507902000 test" # 
# test="150790200 test"  # nothing out
# test="150790200 test 0510004040" # 0510004040 out
# test="15079020 test 05100040.40" # both out
# test="150790.20 test 051000.40.40" # both out
# test="15079020 test 051000.4040" # both out
#test="test 1.2936.299.15\n another test is 3002.15.900.00"
# print(find_codes_in_block_once(test, master_re))
#----------------------------------------------------


#--------------loading expiration dates derived from shadings--------
shading_path="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_HTScode_processed_02_sec301.csv"

df_shading= pd.read_csv(shading_path, usecols=['file_name','expiration_indicator','L0_note'])
df_shading=df_shading[df_shading['expiration_indicator']=="Y"]

# Remove "20", "(" and ")" from L0_note
df_shading["L1"] = (
    df_shading["L0_note"]
      .astype(str)
      .str.replace(r"(20|\(|\))", "", regex=True)   # drop "20", "(" and ")"
      .str.strip()
)
df_shading=df_shading[['file_name','expiration_indicator','L1']]


# Open and extract
# test_pdf_path=directory_2/"Preliminary Revision 1%2021%test.pdf"
# test_doc = fitz.open(test_pdf_path)
# test_page= test_doc[0]
# test_extracted= test_page.get_text("blocks")
#--------------------------------------------------------------------

#-------------helper for identifying page locations--------------------
#                  detecting the start of note 20

def page_has_block(page, targets) -> bool:
    for blk in page.get_text("blocks"):
        if blk[4] in targets:
            return True
    return False

def target_in_block(page) -> bool:
    for blk in page.get_text("blocks"):
        if "For the purposes of heading 9903.88.01, products of China" in blk[4]:
            return True
    return False

# Find the smallest page number with a specific text block
def find_page_by_block(doc, targets, start=None, end=None):
    start = 0 if start is None else start
    end = len(doc) if end is None else end
    
    for i in range(start, end):
        if page_has_block(doc[i], targets):
            return i # for loop stops when it is found
    if len(str(targets))>50:
        for i in range(start, end):
            if target_in_block(doc[i]):
                return i
    return -99  # Return -99 if no matching page is found

def detect_page_range(doc):
    p_start = find_page_by_block(doc, ["SUBCHAPTER III\n"])
    p_end   = find_page_by_block(doc, ["SUBCHAPTER IV\n"])-1
    p_tab   = find_page_by_block(doc, ["1/ See chapter 99 statistical note 1.\n"],p_start,p_end)
    #p_sec301= find_page_by_block(doc, ["1/\n9903.88.01\n"],p_tab, p_end)
    p_sec301  = find_page_by_block(doc, ['For the purposes of heading 9903.88.01, products of China'],p_start, p_tab)
    return p_tab, p_sec301
#----------------------------------------------------------

#----------- building data-----------

def clean_one_page(page):
    record=[]
    for blk in page.get_text("blocks"):
        # only clean block if it is below y0>75
        if blk[1]>75:
            text=blk[4]
            x0=float(blk[0])
            norm_text = re.sub(r"\s*\n\s*", " ", (text or "").replace("\r", "\n")).strip()
            L0=L1=""
            stripped = norm_text.lstrip()

            m_num = re.match(r"^(\d+)", stripped)
            if x0 < 48 and m_num:
                L0 = m_num.group(1)
            else:
                if 61 < x0 < 79.5:
                    m_paren = re.match(r'^\(\s*([^\W\d_]+)\s*\)', stripped)  # letters-only
                    if m_paren:
                        L1 = m_paren.group(1)
     
            record.append({
                "x0": float(x0),
                "text": norm_text,
                "L0": L0, "L1": L1
            })
    return pd.DataFrame(record)

# test_path=directory_2/"Revision 18%2025%99.pdf"
# test_doc = fitz.open(test_path)
# test_page=test_doc[218].get_text("blocks")
# aa_test=clean_one_page(test_doc[219])

def process_pdf(pdf_path: str) -> pd.DataFrame:
    doc = fitz.open(pdf_path)
    rng = detect_page_range(doc)
    p_tab, p_sec301 = rng
    rows = pd.DataFrame()
    for i in range(p_sec301, p_tab):
        page = doc[i]
        cleaned_page=clean_one_page(page)
        rows = pd.concat([rows, cleaned_page], ignore_index=True)

    return rows

def extract_bracket_content(s):
    EXCLUDE_NORM = "Compiler's note: numbers in this table read from left to right"

    if pd.isna(s):
        return np.nan
    s = str(s)
    # Grab all bracketed contents (without the brackets)
    matches = re.findall(r"\[([^\]]+)\]", s, flags=re.DOTALL)
    # Return the first non-excluded one (None/NaN if none)
    for m in matches:
        if m != EXCLUDE_NORM:
            return m.strip()
    return np.nan

def process_master(master: pd.DataFrame, df_shading: pd.DataFrame, master_re) -> pd.DataFrame:
    df = master.copy()
    # fill values
    df["L0"] = df["L0"].astype(str).replace(r"^\s*$", pd.NA, regex=True)
    df["L1"] = df["L1"].astype(str).replace(r"^\s*$", pd.NA, regex=True)

    df["L0"] = df.groupby("file_name", sort=False)["L0"].transform("ffill")
    df["L1"] = df.groupby(["file_name", "L0"], sort=False)["L1"].transform("ffill")

    df = df.merge(df_shading, on=["file_name", "L1"], how="left", suffixes=("", "_sh"))
    df=df[df['expiration_indicator']!="Y"]

    # compiler_note from text
    df["compiler_note"] = df["text"].apply(extract_bracket_content)

    # HTS from text using your regex helper
    df["HTS"] = df["text"].apply(find_codes_in_block_once, master_re=master_re)
    return df


def build_master(directory_2: str, sec301_rev: list[str], df_shading, master_re) -> tuple[str, pd.DataFrame]:
    master_df = pd.DataFrame()
    base_dir = Path(directory_2)
    deleted_divisions=df_shading
    search_pattern=master_re
    for name in sec301_rev:
        p = base_dir / name
        try:
            cleaned = process_pdf(p)
            cleaned["file_name"] = name  # optional provenance column
            # Convert L0 to numeric (non-numeric -> NaN), then find first row with L0 > 20
            m = pd.to_numeric(cleaned["L0"], errors="coerce") > 20 
            # deleting notes other than 20 (ATTENTION: unusual format of note 21 in "(21a)" not deleted)
            if m.any():
                first_pos = np.nonzero(m.to_numpy())[0][0]   # position of first True
                cleaned = cleaned.iloc[:first_pos].copy()    # keep everything *before* that row (drops it and all after)
            # clean results from one revision
            cleaned_df=process_master(cleaned,deleted_divisions,search_pattern)
            # append to the master
            master_df = pd.concat([master_df, cleaned_df], ignore_index=True)
        except Exception as e:
            print(f"[skip] {p.name}: {type(e).__name__}: {e}")
            continue
    return master_df   

# ---------- testing --------------
# if not using process_master, then use below to clean

# master = build_master(directory_2, sec301_rev.iloc[1:2,0], df_shading, master_re)
# master["L0"] = master["L0"].astype(str).replace(r"^\s*$", pd.NA, regex=True)
# master["L1"] = master["L1"].astype(str).replace(r"^\s*$", pd.NA, regex=True)
# master["L0"] = master.groupby("file_name", sort=False)["L0"].transform("ffill")
# master["L1"] = master.groupby(["file_name", "L0"])["L1"].ffill()
# master=master.merge(df_shading, on=["file_name", "L1"], how="left")
# master=master[master['expiration_indicator']!="Y"]
# master["compiler_note"] = master["text"].apply(extract_bracket_content)
# master['HTS']=master['text'].apply(find_codes_in_block_once, master_re=master_re)
#-----------------------------------

master = build_master(directory_2, sec301_rev.iloc[:,0], df_shading, master_re)

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_note_20_updated.csv"
master.to_csv(output_path, index=False)



