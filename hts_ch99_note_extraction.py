# Requirements:
# 1) Use get_text("blocks") to extract blocks: save page number, x0, y0, text.
# 2) Drop blocks by top margin: y0<150 if page=1; y0<75 if page>1.
# 3) Generate L1-L4 according to rules.
# 4) If text starts with a number, assign to L1.
#    If text starts with "(" extract content inside first "()" and, based on x0,
#    assign to L2 if 60<x0<64, to L3 if 77<x0<80, and to L4 if 93<x0<96.
# 5) Apply to the uploaded PDF and write a CSV.

import fitz  # PyMuPDF
import re
import pandas as pd
from pathlib import Path

PDF_PATH = Path("/mnt/data/sample hts.pdf")
OUT_CSV = Path("/mnt/data/hts_blocks_L1234.csv")

def extract_blocks_with_levels(pdf_path: Path):
    doc = fitz.open(pdf_path)
    records = []
    for pno in range(len(doc)):
        page = doc[pno]
        page_num = pno + 1

        # Extract blocks
        blocks = page.get_text("blocks")

        for b in blocks:
            # PyMuPDF tuples: (x0, y0, x1, y1, text, block_no, block_type)
            x0 = b[0]; y0 = b[1]; text = b[4]

            # 2) Margin filter
            if (page_num == 1 and y0 < 150) or (page_num > 1 and y0 < 75):
                continue

            # Normalize text for CSV
            norm_text = re.sub(r"\s*\n\s*", " ", (text or "").replace("\r", "\n")).strip()

            # 3–4) Levels
            L1 = L2 = L3 = L4 = ""
            stripped = norm_text.lstrip()

            # numeric at start -> L1
            m_num = re.match(r"^(\d+)", stripped)
            if m_num:
                L1 = m_num.group(1)
            else:
                # starts with "(" -> grab inside parens, then place by x0 range
                if stripped.startswith("("):
                    m_paren = re.match(r"^\(([^)]*)\)", stripped)
                    if m_paren:
                        inside = m_paren.group(1)
                        if 60 < x0 < 64:
                            L2 = inside
                        if 77 < x0 < 80:
                            L3 = inside
                        if 93 < x0 < 96:
                            L4 = inside

            records.append({
                "page": page_num,
                "x0": float(x0),
                "y0": float(y0),
                "text": norm_text,
                "L1": L1, "L2": L2, "L3": L3, "L4": L4
            })

    return pd.DataFrame.from_records(records)

df = extract_blocks_with_levels(PDF_PATH)
df.to_csv(OUT_CSV, index=False, encoding="utf-8")
print(f"CSV written to: {OUT_CSV}")