# Create CSV for get_text outputs (modes: "text", "blocks", "words") from the uploaded PDF.
import fitz  # PyMuPDF
import json
import csv
from pathlib import Path
import pandas as pd
import caas_jupyter_tools as cj

pdf_path = Path("/mnt/data/Chapter 99 (4) Extract[466-469].pdf")
out_path = Path("/mnt/data/ch99_get_text_text_blocks_words.csv")

modes = ["text", "blocks", "words"]
rows = []

# Open and extract
doc = fitz.open(pdf_path)
for pno in range(len(doc)):
    page = doc[pno]
    for mode in modes:
        try:
            extracted = page.get_text(mode)
            if isinstance(extracted, (list, dict, tuple)):
                extracted_str = json.dumps(extracted, ensure_ascii=False)
            else:
                extracted_str = str(extracted)
            rows.append([pno + 1, mode, extracted_str])
        except Exception as e:
            rows.append([pno + 1, mode, f"<<ERROR: {type(e).__name__}: {e}>>"])

# Write CSV
with out_path.open("w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["page_number", "mode", "content"])
    writer.writerows(rows)

# Preview for user
df = pd.DataFrame(rows, columns=["page_number", "mode", "content"])

df_cleaned.to_csv(output_path, index=False)
