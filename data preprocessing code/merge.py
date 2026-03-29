import pandas as pd
file_path = r"C:\Users\mdama\OneDrive\Desktop\DSE4101\data\company_upstream_downstream_adb35_2014_2024.csv"
output_path = r"C:\Users\mdama\OneDrive\Desktop\DSE4101\data\company_upstream_downstream_adb35_2014_2024_wide.csv"
df = pd.read_csv(file_path)
pivot_df = df.pivot_table(
    index=[
        "company_name",
        "ticker",
        "nace_code_4d",
        "nace_code_2d",
        "adb_sector_code",
        "adb_sector_name",
    ],
    columns="year",
    values=["upstream_value", "downstream_value"],
    aggfunc="first"
)

pivot_df.columns = [
    f"{metric.replace('_value', '')}({int(year)})"
    for metric, year in pivot_df.columns
]

pivot_df = pivot_df.reset_index()

fixed_cols = [
    "company_name",
    "ticker",
    "nace_code_4d",
    "nace_code_2d",
    "adb_sector_code",
    "adb_sector_name",
]

year_cols = []
years = sorted(df["year"].dropna().unique())

for y in years:
    y = int(y)
    if f"upstream({y})" in pivot_df.columns:
        year_cols.append(f"upstream({y})")
    if f"downstream({y})" in pivot_df.columns:
        year_cols.append(f"downstream({y})")

pivot_df = pivot_df[fixed_cols + year_cols]
pivot_df.to_csv(output_path, index=False)
excel_path = "final_merged_company_data.xlsx"
csv_path = "company_upstream_downstream_adb35_2014_2024_wide.csv"
output_path = "final_merged_company_data_with_upstream_downstream.xlsx"
excel_file = pd.ExcelFile(excel_path)
sheet_names = excel_file.sheet_names
first_sheet_name = sheet_names[0]
first_sheet_df = pd.read_excel(excel_path, sheet_name=first_sheet_name)
second_sheet_df = None
second_sheet_name = None
if len(sheet_names) > 1:
    second_sheet_name = sheet_names[1]
    second_sheet_df = pd.read_excel(excel_path, sheet_name=second_sheet_name)
csv_df = pd.read_csv(csv_path)
csv_df = csv_df.drop(columns=["company_name"], errors="ignore")
merged_first_sheet = first_sheet_df.merge(
    csv_df,
    on="ticker",
    how="left"
)
with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
    merged_first_sheet.to_excel(writer, sheet_name=first_sheet_name, index=False)
    if second_sheet_df is not None:
        second_sheet_df.to_excel(writer, sheet_name=second_sheet_name, index=False)