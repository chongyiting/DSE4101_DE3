import pandas as pd
import numpy as np

INPUT_FILE = "position_full.csv"
OUTPUT_FILE = "position_usa_adb_only.csv"

df = pd.read_csv(INPUT_FILE)

df["country"] = df["country"].astype(str).str.strip().str.upper()
df["source"] = df["source"].astype(str).str.strip().str.upper()


filtered = df[
    (df["country"] == "USA") &
    (df["source"] == "ADB")
].copy()

filtered.to_csv(OUTPUT_FILE, index=False)

POSITION_FILE = "position_usa_adb_only.csv"
README_FILE = "readme_positiondata_2025Oct.xlsx"
ORBIS_FILE = "orbis_sp_us_nacerev2.xlsx"
OUTPUT_FILE = "company_upstream_downstream_adb35_2014_2024.csv"
START_YEAR = 2014
END_YEAR = 2024

position = pd.read_csv(POSITION_FILE)
sectors = pd.read_excel(README_FILE, sheet_name="sectors", engine="openpyxl")
orbis = pd.read_excel(ORBIS_FILE, sheet_name="Results", engine="openpyxl")

sectors = sectors.rename(
    columns={
        "Unnamed: 4": "broad_sector_code",
        "Unnamed: 8": "broad_sector_label_raw",
    }
)

sectors["sect"] = pd.to_numeric(sectors["sect"], errors="coerce")
sectors["source"] = sectors["source"].astype(str).str.strip().str.lower()

adb_sector_lookup = (
    sectors.loc[
        sectors["source"].eq("adb"),
        ["sect", "sect_name", "broad_sector_code"]
    ]
    .drop_duplicates()
    .rename(
        columns={
            "sect": "adb_sector_code",
            "sect_name": "adb_sector_name",
        }
    )
)

position["sect"] = pd.to_numeric(position["sect"], errors="coerce")
position["country"] = position["country"].astype(str).str.strip().str.upper()
position["source"] = position["source"].astype(str).str.strip().str.lower()
position["t"] = pd.to_numeric(position["t"], errors="coerce")

position_us = position.loc[
    (position["country"] == "USA")
    & (position["source"] == "adb")
    & (position["sect"].notna())
    & (position["t"] >= START_YEAR)
    & (position["t"] <= END_YEAR),
    ["sect", "t", "upstreamness", "downstreamness"]
].copy()

position_us = position_us.rename(
    columns={
        "sect": "adb_sector_code",
        "t": "year",
        "upstreamness": "upstream_value",
        "downstreamness": "downstream_value",
    }
)

position_us = position_us.merge(
    adb_sector_lookup,
    on="adb_sector_code",
    how="left"
)

ORBS_COMPANY_COL = "Company name Latin alphabet"
ORBS_COUNTRY_COL = "Country ISO code"
ORBS_NACE4_COL = "NACE Rev. 2, core code (4 digits)"
ORBS_TICKER_COL = "Ticker symbol"

orbis[ORBS_COUNTRY_COL] = orbis[ORBS_COUNTRY_COL].astype(str).str.strip().str.upper()

orbis_us = orbis.loc[
    orbis[ORBS_COUNTRY_COL].eq("US"),
    [ORBS_COMPANY_COL, ORBS_NACE4_COL, ORBS_TICKER_COL]
].copy()

orbis_us = orbis_us.rename(
    columns={
        ORBS_COMPANY_COL: "company_name",
        ORBS_NACE4_COL: "nace_code_4d",
        ORBS_TICKER_COL: "ticker",
    }
)
orbis_us["nace_code_4d"] = (
    orbis_us["nace_code_4d"]
    .astype(str)
    .str.extract(r"(\d+)", expand=False)
    .str.zfill(4)
)

orbis_us["nace_code_2d"] = (
    orbis_us["nace_code_4d"]
    .str[:2]
)

nace2_to_adb = {
    "01": 1, "02": 1, "03": 1,

    "05": 2, "06": 2, "07": 2, "08": 2, "09": 2,

    "10": 3, "11": 3, "12": 3,
    "13": 4, "14": 4,
    "15": 5,
    "16": 6,
    "17": 7, "18": 7,
    "19": 8,
    "20": 9, "21": 9,
    "22": 10,
    "23": 11,
    "24": 12, "25": 12,
    "28": 13,
    "26": 14, "27": 14,
    "29": 15, "30": 15,
    "31": 16, "32": 16, "33": 16,

    "35": 17, "36": 17, "37": 17, "38": 17, "39": 17,
    "41": 18, "42": 18, "43": 18,

    "45": 19,
    "46": 20,
    "47": 21,

    "55": 22, "56": 22,
    "49": 23,
    "50": 24,
    "51": 25,
    "52": 26, "53": 26, "79": 26,

    "58": 27, "59": 27, "60": 27, "61": 27,
    "64": 28, "65": 28, "66": 28,
    "68": 29,

    "62": 30, "63": 30,
    "69": 30, "70": 30, "71": 30, "72": 30, "73": 30, "74": 30,
    "77": 30, "78": 30, "80": 30, "81": 30, "82": 30,

    "84": 31,
    "85": 32,
    "86": 33, "87": 33, "88": 33,
    "90": 34, "91": 34, "92": 34, "93": 34, "94": 34, "95": 34, "96": 34, "99": 34,
    "97": 35, "98": 35
}
orbis_us["adb_sector_code"] = orbis_us["nace_code_2d"].map(nace2_to_adb)
orbis_us["company_name_upper"] = orbis_us["company_name"].astype(str).str.upper()

extractive_mask = (
    orbis_us["adb_sector_code"].isna()
    & orbis_us["company_name_upper"].str.contains(
        "APA|COTERRA|DEVON|DIAMONDBACK|EOG|EXPAND ENERGY|OCCIDENTAL|PETROLEUM|RESOURCES|HALLIBURTON|FREEPORT",
        na=False
    )
)
orbis_us.loc[extractive_mask, "adb_sector_code"] = 2

materials_mask = (
    orbis_us["adb_sector_code"].isna()
    & orbis_us["company_name_upper"].str.contains(
        "MARTIN MARIETTA|VULCAN",
        na=False
    )
)
orbis_us.loc[materials_mask, "adb_sector_code"] = 11

orbis_us = orbis_us.merge(
    adb_sector_lookup,
    on="adb_sector_code",
    how="left"
)

orbis_us = orbis_us[
    ["company_name", "ticker", "nace_code_4d", "nace_code_2d", "adb_sector_code", "adb_sector_name"]
].drop_duplicates().reset_index(drop=True)

final = orbis_us.merge(
    position_us[
        ["adb_sector_code", "adb_sector_name", "year", "upstream_value", "downstream_value"]
    ],
    on=["adb_sector_code", "adb_sector_name"],
    how="left"
)

final = final[
    [
        "company_name",
        "ticker",
        "nace_code_4d",
        "nace_code_2d",
        "adb_sector_code",
        "adb_sector_name",
        "year",
        "upstream_value",
        "downstream_value",
    ]
].sort_values(["company_name", "year"]).reset_index(drop=True)

print("Unique companies:", final["company_name"].nunique())
print("Mapped ADB sectors:", final["adb_sector_code"].nunique())
print("Unmapped companies:", final["adb_sector_code"].isna().sum())

missing_years = final.groupby("company_name")["year"].nunique().lt(11).sum()
print("Companies with incomplete year coverage:", missing_years)

final.to_csv(OUTPUT_FILE, index=False)
print("File saved:", OUTPUT_FILE)