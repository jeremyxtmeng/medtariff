import fitz  # PyMuPDF
import pandas as pd

# Path to the latest uploaded PDF
pdf_path = "/mnt/data/sample hts.pdf"

# Open PDF
doc = fitz.open(pdf_path)

rows = []

# Define margins in points (1 inch = 72 points)
margin_page1_top = 2.5 * 72   # 2.5 inches
margin_other_top = 1.3 * 72   # 1.3 inches

# Iterate over pages
for page_num, page in enumerate(doc, start=1):
    blocks = page.get_text("blocks")  # Extract text blocks with coordinates
    for b in blocks:
        x0, y0, x1, y1, text, *_ = b
        # Apply margin rules
        if page_num == 1:
            if y0 >= margin_page1_top:  # below 2.5 in for page 1
                rows.append({"Page": page_num, "Text": text.strip()})
        else:
            if y0 >= margin_other_top:  # below 1.3 in for other pages
                rows.append({"Page": page_num, "Text": text.strip()})

# Create DataFrame
df = pd.DataFrame(rows).drop_duplicates().reset_index(drop=True)

# Save to CSV
csv_path = "/mnt/data/digitized_below_top_margin.csv"
df.to_csv(csv_path, index=False)

import caas_jupyter_tools
caas_jupyter_tools.display_dataframe_to_user("Digitized Text Below Top Margins", df)

csv_path



import re
import pandas as pd
import fitz  # PyMuPDF
import ace_tools as tools

# Use the latest upload
pdf_path = "/mnt/data/Untitled revision 16 2019.pdf"

# Extract text from the PDF
doc = fitz.open(pdf_path)
text = ""
for page in doc:
    text += page.get_text()

# Limit to section (d) up to before (e) — same pattern as your reference
start_marker = "(d) Articles provided for in a provision"
end_markers = ["\n(e) Notwithstanding", "(e) Notwithstanding"]
start = text.find(start_marker)
end = -1
for m in end_markers:
    pos = text.find(m)
    if pos != -1:
        end = pos if end == -1 else min(end, pos)
section = text[start:end] if (start != -1 and end != -1 and end > start) else text

# Normalize whitespace
section_norm = re.sub(r"\s+", " ", section).strip()

# Regex: capture "country list" followed by HTS code ####.##.##
# (greedy on the country list to avoid dropping earlier countries for the same code)
pair_re = re.compile(r"([A-Za-z\(\) ,;&\-\–\/\.\'’]+)\s+(\d{4}\.\d{2}\.\d{2})")
pairs = pair_re.findall(section_norm)

rows = []
for country_list, code in pairs:
    # Split on semicolons (multiple countries share the same code)
    countries = [c.strip(" ,") for c in country_list.split(";") if c.strip(" ,")]
    for c in countries:
        # Clean country names
        c = re.sub(r"\s{2,}", " ", c).strip()
        # Filter out obvious artifacts
        low = c.lower()
        if any(bad in low for bad in [
            "harmonized tariff schedule", "annotated for statistical", "gsp", "gn p."
        ]):
            continue
        rows.append({"Country": c, "Tariff Code": code})

df = pd.DataFrame(rows).drop_duplicates().reset_index(drop=True)

# Save CSV
csv_path = "/mnt/data/country_tariff_code_mapping_rev16.csv"
df.to_csv(csv_path, index=False)

# Show a preview to the user
tools.display_dataframe_to_user(name="Country–Tariff Code Mapping (Rev. 16, digitized)", dataframe=df)

csv_path
