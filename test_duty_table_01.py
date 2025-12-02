# pip install duckdb pandas

import duckdb
import pandas as pd
from datetime import date

# 1) Connect (creates a file-backed DB named simple_history.duckdb)
con = duckdb.connect("simple_history.duckdb")

# 2) Create a minimal table for country membership intervals
con.execute("""
DROP TABLE IF EXISTS country_membership;
CREATE TABLE country_membership (
  country     TEXT NOT NULL,
  valid_from  DATE NOT NULL,
  valid_to    DATE            -- NULL = open-ended; interval is [valid_from, valid_to)
);
""")

# 3) Example data: 5 countries with adds/deletes already processed into intervals
data = [
    # country, valid_from,   valid_to (None means still valid)
    ("USA", "2000-01-01", None),
    ("MEX", "2000-01-01", "2003-01-01"),
    ("CHN", "2001-07-01", None),
    ("IND", "2002-01-01", "2004-01-01"),
    ("BRA", "2000-01-01", "2000-06-01"),
]
df = pd.DataFrame(data, columns=["country","valid_from","valid_to"])
df["valid_from"] = pd.to_datetime(df["valid_from"]).dt.date
df["valid_to"]   = pd.to_datetime(df["valid_to"]).dt.date

con.register("df_mem", df)
con.execute("INSERT INTO country_membership SELECT * FROM df_mem;")

# 4) Build a (year, country) table by expanding intervals to years.
#    We consider a country present in a year if its interval intersects that year's span:
#    [year-01-01, (year+1)-01-01)
con.execute("""
DROP TABLE IF EXISTS year_country;
WITH bounds AS (
  SELECT
    CAST(MIN(EXTRACT(YEAR FROM valid_from)) AS INTEGER) AS y0,
    CAST(MAX(EXTRACT(YEAR FROM COALESCE(valid_to - INTERVAL 1 DAY, CURRENT_DATE))) AS INTEGER) AS y1
  FROM country_membership
),
years AS (
  -- produce all integer years from y0 to y1 inclusive
  SELECT y AS year
  FROM bounds, range((SELECT y0 FROM bounds), (SELECT y1 FROM bounds) + 1) AS t(y)
),
year_windows AS (
  SELECT
    year,
    CAST(strftime(CAST(year AS VARCHAR) || '-01-01', '%Y-%m-%d') AS DATE)                AS y_start,
    CAST(strftime(CAST((year + 1) AS VARCHAR) || '-01-01', '%Y-%m-%d') AS DATE)          AS y_end
  FROM years
)
CREATE TABLE year_country AS
SELECT
  yw.year::INTEGER AS year,
  cm.country       AS country
FROM year_windows yw
JOIN country_membership cm
  ON cm.valid_from < yw.y_end
 AND (cm.valid_to IS NULL OR cm.valid_to > yw.y_start)
ORDER BY year, country;
""")

# 5) Read the result (year, country)
result = con.execute("SELECT * FROM year_country;").fetchdf()
print(result)
