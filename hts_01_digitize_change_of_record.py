import pdfplumber
import pandas as pd
import re

## tables with horizontal lines
# Path to your PDF file
# revisions: 2025
pdf_path = "C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\Data\\raw\\tariff\\Change Record Basic edition 2025.pdf"

# Container for all table rows
all_tables = []

# Open the PDF and extract tables
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        tables = page.extract_tables()
        for table in tables:
            if table:  # Make sure table is not empty
                all_tables.append(table)

# Flatten the table and filter rows with exactly 4 columns
flattened_rows = []
for table in all_tables:
    for row in table:
        if row and len(row) == 4:
            flattened_rows.append(row)

# Create DataFrame
columns = ["Item changed", "Nature of change", "Effective date", "Source"]
df = pd.DataFrame(flattened_rows, columns=columns)

output_path = "C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\Data\\raw\\tariff\\test.csv"
df.to_csv(output_path, index=False)


## tabels without horizontal lines--------------------------------------
## basic editions: 2025 2024 2023 2021
## Preliminary editions: 2021
# Path to your PDF file
pdf_path="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\" \
    "Data\\raw\\tariff\\revisions 2025\\Change Record Revision 18 2025.pdf"

# Store structured table rows
structured_rows = []

with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        # Detect vertical lines to define column edges
        vertical_edges = sorted(set(round(edge["x0"], 1) for edge in page.edges if edge["orientation"] == "v"))
        if len(vertical_edges) < 2:
            continue  # Skip if not enough vertical lines detected

        col_edges = sorted(vertical_edges)
        column_count = len(col_edges) - 1

        # Extract all words with position info
        words = page.extract_words(keep_blank_chars=True, use_text_flow=True)

        # Group words into rows by vertical position
        lines = {}
        for word in words:
            y0 = round(word["top"], 1)
            if y0 not in lines:
                lines[y0] = []
            lines[y0].append(word)

        # For each visual row, allocate words into columns
        for y0 in sorted(lines):
            row = [''] * column_count
            for word in lines[y0]:
                x_center = (word['x0'] + word['x1']) / 2
                for i in range(column_count):
                    if col_edges[i] <= x_center < col_edges[i + 1]:
                        row[i] += word['text'] + ' '
                        break
            row = [cell.strip() for cell in row]
            structured_rows.append(row)

# Save as CSV
df = pd.DataFrame(structured_rows)


# Step 1: Drop the first column (likely row number or misaligned content)
df = df.drop(columns=[0])

# Step 2: Locate the header row containing "Item changed" (case-insensitive)
def is_header_row(text):
    if pd.isna(text):
        return False
    return "item" in text.lower() and "changed" in text.lower()

header_mask = df.iloc[:, 0].apply(is_header_row)

if header_mask.any():
    header_index = header_mask.idxmax()
else:
    raise ValueError("Could not find a row containing 'Item changed'")

# Use the contents of that row as column names
df.columns = df.iloc[header_index]
df = df.iloc[header_index + 1:].reset_index(drop=True)

# Step 3: Drop columns with empty or NaN column names
df = df.loc[:, df.columns.notna() & (df.columns.str.strip() != '')]

# Identify 'Edition' column name; fall back to 'Source' if not found
if "Edition" in df.columns:
    merge_check_col = "Edition"
elif "Source" in df.columns:
    merge_check_col = "Source"
else:
    # Fallback to the last column if neither exists
    merge_check_col = df.columns[-1]

# Merge rows where merge_check_col is empty
merged_rows = []

for _, row in df.iterrows():
    if pd.isna(row[merge_check_col]) or row[merge_check_col].strip() == "":
        if merged_rows:
            prev_row = merged_rows[-1]
            for col in df.columns:
                prev_text = str(prev_row[col]).strip()
                new_text = str(row[col]).strip()
                if new_text and new_text.lower() != "nan":
                    prev_row[col] = (prev_text + " " + new_text).strip()
    else:
        merged_rows.append(row.to_dict())

# Convert back to DataFrame and save
df_cleaned = pd.DataFrame(merged_rows)
# Export to CSV

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test.csv"

df_cleaned.to_csv(output_path, index=False)

