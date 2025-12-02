# Created: Aug 30, 2025
# Updated: Sept 3, 2025
# Extracting shadings of subheadings and notes related to China's Section 301 tariffs in Ch99

from pathlib import Path
import pandas as pd
import re


htscodes_csv= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\processed\med_goods_hts22_final.csv")
directory_1= Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\hts_csv")
directory_2 = Path(r"C:\Users\xiang\Documents\backup_T580\Videos\Videos\Dropbox\tariff_medgoods_2025\data\raw\tariff\ch98_99_note_pdf")

df_hscode= pd.read_csv(htscodes_csv, usecols=['HTS22'], dtype={"HTS22": "string"})
#----------------------------------------------------

#------------- building text searches----------------
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

#test="90183100 test" # 
# test="150790200 test"  # nothing out
# test="150790200 test 0510004040" # 0510004040 out
# test="15079020 test 05100040.40" # both out
# test="150790.20 test 051000.40.40" # both out
# test="15079020 test 051000.4040" # both out
test="test 1.2936.299.15\n another test is 3002.15"

s = """ 
7406.10.00	7406.20.00	7407.10.15
7407.10.30	7407.10.50	7407.21.15
7407.21.30	7407.21.50	7407.21.70
7407.21.90	7407.29.16	7407.29.34
7407.29.38	7407.29.40	7407.29.50
7408.11.30	7408.11.60	7408.19.00
7408.21.00	7408.22.10	7408.22.50
7408.29.10	7408.29.50	7409.11.10
7409.11.50	7409.19.10	7409.19.50
7409.19.90	7409.21.00	7409.29.00
7409.31.10	7409.31.50	7409.31.90
7409.39.10	7409.39.50	7409.39.90
7409.40.00	7409.90.10	7409.90.50
7409.90.90	7410.11.00	7410.12.00
7410.21.30	7410.21.60	7410.22.00
7411.10.10	7411.10.50	7411.21.10
7411.21.50	7411.22.00	7411.29.10
7411.29.50	7412.10.00	7412.20.00
7413.00.10	7413.00.50	7413.00.90
7415.10.00	7415.21.00	7415.29.00
7415.33.05	7415.33.10	7415.33.80
7415.39.00	7418.10.00	7418.20.10
7418.20.50	7419.20.00	7419.80.03
7419.80.06	7419.80.09	7419.80.15
7419.80.16	7419.80.17	7419.80.30
7419.80.50	8544.42.10	8544.42.20
8544.42.90	8544.49.10	null			
				
 """

print(find_codes_in_block_once(s, master_re))

