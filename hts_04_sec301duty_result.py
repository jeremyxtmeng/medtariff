# Created: Sept 06, 2025
# Updated:
# China's Section 301 Duties

from pathlib import Path
import fitz
import pandas as pd
import re
import numpy as np


#--------------loading expiration dates derived from shadings--------
shading_path="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_HTScode_processed_02_sec301.csv"

df_shading= pd.read_csv(shading_path, usecols=['file_name','expiration_indicator','L0_note'])
df_shading=df_shading[df_shading['expiration_indicator']=="Y"]


#--------------loading extract of notes--------
master_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\HTS_schedule_ch98_99_note_20.csv"

master=pd.read_csv(master_path)
master=master.copy()

df = master[master['HTS'].astype("string").str.len() > 2]

df["HTS"] = (df["HTS"].astype("string")
                        .str.replace(r"[\[\]']", "", regex=True)
                        .str.replace(r"\s+", "", regex=True))

df_combined = (df.groupby('L1', dropna=False)['HTS']
         .apply(lambda s: ','.join(s.dropna().astype('string')))
         .reset_index(name='HTS'))


df_combined["HTS_cleaned"] = (df_combined["HTS"].astype("string")
                     .apply(lambda s: (
                         ",".join(sorted({x.strip() for x in str(s).split(",") if x and x.strip()}))
                         if pd.notna(s) else pd.NA
                     )))

df_combined["HTS_cleaned2"] ="check"+ df_combined["HTS_cleaned"]  

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test_final.csv"

df_combined.to_csv(output_path, index=False)



df2=df.groupby(['L1'])['HTS'].sum().reset_index()

df3=df2.drop_duplicates(subset=['L1','HTS'])

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test_hs_updated.csv"

df3.to_csv(output_path, index=False)



df_compiler = master[master['compiler_note'].notna()][['L1', 'file_name','compiler_note']]
df_compiler = df_compiler[df_compiler['L1'].notna()]
mask = df_compiler['compiler_note'].fillna('').str.startswith('Compiler')

# Filter the DataFrame to keep only the rows where the mask is True.
df_filtered = df_compiler[mask]

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test_compiler.csv"

df_filtered.to_csv(output_path, index=False)

df_compiler2=df_compiler.groupby(['L1', 'file_name'])['compiler_note'].sum().reset_index()


df_note=df['L1'].unique()

# & (master['file_name']=="Revision 2%2019%99.pdf")

aa=master[(master['L1']=="vvv")  ]


# cleaning exclusion list
hsex_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test_hs.csv"
df_excl= pd.read_csv(hsex_path, usecols=['L1','file_name','hs_excl'], dtype={'L1': str, 'file_name': str, 'hs_excl': str})

def make_hs_cleaned(df, src='hs_excl', dst='hs_cleaned'):
    s = df[src]
    # split → explode → strip ALL whitespace inside tokens → drop blanks
    exploded = (s[s.notna()]
                .str.split(',')
                .explode()
                .str.replace(r'\s+', '', regex=True))
    exploded = exploded[exploded.ne('')]

    # de-duplicate per row while preserving first appearance order
    dedup = exploded.groupby(level=0).apply(lambda x: ','.join(pd.unique(x)))

    # write back, keeping NaN where original was missing
    df[dst] = dedup.reindex(df.index)
    return df

# usage
a = make_hs_cleaned(df_excl)
a['hs_cleaned']="check"+a['hs_cleaned']

output_path ="C:\\Users\\xiang\\Documents\\backup_T580\\Videos\\Videos\\Dropbox\\tariff_medgoods_2025\\"\
    "Data\\raw\\tariff\\test_hs_cleaned.csv"

a.to_csv(output_path, index=False)
