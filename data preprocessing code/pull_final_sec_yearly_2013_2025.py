#!/usr/bin/env python3
"""

Core variables
--------------
- assets
- assets_current
- cash_and_equivalents
- liabilities_current
- stockholders_equity
- revenue
- operating_income_loss
- net_income_loss
- operating_cash_flow

Defaults
--------
- start_year = 2013
- end_year = 2025

Example
-------
python pull_final_sec_yearly_2013_2025.py input.xlsx \
  --output_dir out_dir \
  --user_agent "Your Name your_email@example.com" \
  --manual_overrides_csv manual_name_overrides.csv
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
import time
from pathlib import Path
from typing import Dict, List, Optional

import pandas as pd
import requests

SEC_TICKERS_URL = "https://www.sec.gov/files/company_tickers_exchange.json"

# Final 12 variables matching the earlier final SEC file
VARIABLE_TAGS: Dict[str, List[str]] = {
    "assets": ["Assets"],
    "assets_current": ["AssetsCurrent"],
    "cash_and_equivalents": ["CashAndCashEquivalentsAtCarryingValue"],
    "liabilities_current": ["LiabilitiesCurrent"],
    "stockholders_equity": ["StockholdersEquity"],
    "revenue": [
        "RevenueFromContractWithCustomerExcludingAssessedTax",
        "Revenues",
    ],
    "operating_income_loss": ["OperatingIncomeLoss"],
    "net_income_loss": ["NetIncomeLoss"],
    "operating_cash_flow": ["NetCashProvidedByUsedInOperatingActivities"],
}

VALID_FORMS = {"10-K", "10-K/A", "20-F", "20-F/A", "40-F", "40-F/A"}


def normalize_company_name(name: str) -> str:
    if pd.isna(name):
        return ""
    s = str(name).upper().strip()
    s = s.replace("&", " AND ")
    s = re.sub(r"[.,/()'’\-]", " ", s)
    s = re.sub(r"\b(THE)\b", " ", s)
    s = re.sub(
        r"\b(INC|INCORPORATED|CORP|CORPORATION|CO|COMPANY|COMPANIES|COS|HOLDINGS|HOLDING|GROUP|PLC|LTD|LIMITED|N V|NV|S A|SA|DE)\b",
        " ",
        s,
    )
    s = re.sub(r"\s+", " ", s).strip()
    return s


def load_input_companies(input_xlsx: Path) -> pd.DataFrame:
    df = pd.read_excel(input_xlsx, sheet_name="Results")
    required_cols = {"Company name Latin alphabet", "Country ISO code"}
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Missing required columns in input workbook: {sorted(missing)}")

    df = df[["Company name Latin alphabet", "Country ISO code"]].copy()
    df = df.rename(
        columns={
            "Company name Latin alphabet": "orbis_company_name",
            "Country ISO code": "country_iso_code",
        }
    )
    df = df.dropna(subset=["orbis_company_name"])
    df["country_iso_code"] = df["country_iso_code"].astype(str).str.upper().str.strip()
    df = df[df["country_iso_code"] == "US"].copy()
    df = df.drop_duplicates(subset=["orbis_company_name"]).reset_index(drop=True)
    return df


def load_sec_ticker_map(user_agent: str) -> pd.DataFrame:
    headers = {"User-Agent": user_agent}
    r = requests.get(SEC_TICKERS_URL, headers=headers, timeout=60)
    r.raise_for_status()
    data = r.json()

    if isinstance(data, dict) and "data" in data and "fields" in data:
        sec_df = pd.DataFrame(data["data"], columns=data["fields"])
    else:
        sec_df = pd.DataFrame(data)

    if not {"cik", "name", "ticker"}.issubset(sec_df.columns):
        raise ValueError("SEC ticker file is missing expected columns.")

    sec_df = sec_df.rename(columns={"name": "sec_company_name"})
    sec_df["ticker"] = sec_df["ticker"].astype(str).str.upper().str.strip()
    sec_df["cik"] = sec_df["cik"].astype(int).astype(str)
    sec_df["cik_padded"] = sec_df["cik"].str.zfill(10)
    sec_df["normalized_sec_name"] = sec_df["sec_company_name"].map(normalize_company_name)
    return sec_df


def load_manual_overrides(path: Optional[Path]) -> pd.DataFrame:
    if path is None:
        return pd.DataFrame(columns=["orbis_company_name", "ticker", "cik"])
    df = pd.read_csv(path)
    required = {"orbis_company_name", "ticker", "cik"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Manual overrides CSV missing required columns: {sorted(missing)}")
    df = df.copy()
    df["orbis_company_name"] = df["orbis_company_name"].astype(str)
    df["ticker"] = df["ticker"].astype(str).str.upper().str.strip()
    df["cik"] = df["cik"].astype(str).str.strip()
    return df


def auto_match_names(
    firms_df: pd.DataFrame,
    sec_df: pd.DataFrame,
    min_auto_match_score: int = 92,
) -> pd.DataFrame:
    sec_name_to_rows = {}
    for idx, nm in sec_df["normalized_sec_name"].items():
        sec_name_to_rows.setdefault(nm, []).append(idx)

    normalized_sec_names = list(sec_name_to_rows.keys())
    out_rows = []

    for _, row in firms_df.iterrows():
        orbis_name = row["orbis_company_name"]
        norm_orbis = normalize_company_name(orbis_name)

        if norm_orbis in sec_name_to_rows:
            sec_row = sec_df.loc[sec_name_to_rows[norm_orbis][0]]
            out_rows.append(
                {
                    "orbis_company_name": orbis_name,
                    "normalized_orbis_name": norm_orbis,
                    "matched_sec_name": sec_row["sec_company_name"],
                    "ticker": sec_row["ticker"],
                    "cik": sec_row["cik"],
                    "match_score": 100,
                    "match_method": "exact_normalized",
                    "review_flag": "auto_ok",
                }
            )
            continue

        matches = difflib.get_close_matches(norm_orbis, normalized_sec_names, n=1, cutoff=0.0)
        if matches:
            best = matches[0]
            score = int(round(difflib.SequenceMatcher(None, norm_orbis, best).ratio() * 100))
            sec_row = sec_df.loc[sec_name_to_rows[best][0]]
            out_rows.append(
                {
                    "orbis_company_name": orbis_name,
                    "normalized_orbis_name": norm_orbis,
                    "matched_sec_name": sec_row["sec_company_name"],
                    "ticker": sec_row["ticker"],
                    "cik": sec_row["cik"],
                    "match_score": score,
                    "match_method": "fuzzy",
                    "review_flag": "auto_ok" if score >= min_auto_match_score else "review",
                }
            )
        else:
            out_rows.append(
                {
                    "orbis_company_name": orbis_name,
                    "normalized_orbis_name": norm_orbis,
                    "matched_sec_name": None,
                    "ticker": None,
                    "cik": None,
                    "match_score": None,
                    "match_method": "unmatched",
                    "review_flag": "unmatched",
                }
            )

    return pd.DataFrame(out_rows)


def apply_manual_overrides(mapping_df: pd.DataFrame, overrides_df: pd.DataFrame, sec_df: pd.DataFrame) -> pd.DataFrame:
    if overrides_df.empty:
        return mapping_df

    sec_lookup = sec_df.set_index("ticker")[["sec_company_name", "cik"]].to_dict("index")
    mapping_df = mapping_df.copy()

    for _, ov in overrides_df.iterrows():
        orbis_name = ov["orbis_company_name"]
        ticker = ov["ticker"]
        cik = str(ov["cik"])
        sec_name = sec_lookup.get(ticker, {}).get("sec_company_name")

        mask = mapping_df["orbis_company_name"] == orbis_name
        if mask.any():
            mapping_df.loc[mask, "ticker"] = ticker
            mapping_df.loc[mask, "cik"] = cik
            mapping_df.loc[mask, "matched_sec_name"] = sec_name
            mapping_df.loc[mask, "match_score"] = 999
            mapping_df.loc[mask, "match_method"] = "manual_override"
            mapping_df.loc[mask, "review_flag"] = "manual_ok"
        else:
            mapping_df = pd.concat(
                [
                    mapping_df,
                    pd.DataFrame(
                        [
                            {
                                "orbis_company_name": orbis_name,
                                "normalized_orbis_name": normalize_company_name(orbis_name),
                                "matched_sec_name": sec_name,
                                "ticker": ticker,
                                "cik": cik,
                                "match_score": 999,
                                "match_method": "manual_override",
                                "review_flag": "manual_ok",
                            }
                        ]
                    ),
                ],
                ignore_index=True,
            )

    return mapping_df


def get_companyfacts(cik_padded: str, user_agent: str) -> dict:
    url = f"https://data.sec.gov/api/xbrl/companyfacts/CIK{cik_padded}.json"
    headers = {"User-Agent": user_agent}
    r = requests.get(url, headers=headers, timeout=90)
    r.raise_for_status()
    return r.json()


def pick_unit_bucket(concept_entry: dict):
    units = concept_entry.get("units", {})
    if not units:
        return None

    preferred_order = ["USD", "USD/shares", "shares"]
    for unit in preferred_order:
        if unit in units:
            return units[unit]

    first_key = next(iter(units.keys()), None)
    return units.get(first_key) if first_key else None


def filter_annual_facts(facts: list, start_year: int, end_year: int) -> pd.DataFrame:
    if not facts:
        return pd.DataFrame()

    df = pd.DataFrame(facts)
    needed = {"end", "val", "form", "filed"}
    if not needed.issubset(df.columns):
        return pd.DataFrame()

    df = df.copy()
    df["end"] = pd.to_datetime(df["end"], errors="coerce")
    df["filed"] = pd.to_datetime(df["filed"], errors="coerce")
    df["fy"] = pd.to_numeric(df.get("fy"), errors="coerce")
    df["fp"] = df.get("fp")
    df["form"] = df["form"].astype(str)

    df = df[df["form"].isin(VALID_FORMS)].copy()
    df = df[df["end"].notna() & df["filed"].notna()].copy()

    # use period end year as the annual bucket, matching the earlier final output logic
    df["fiscal_year"] = df["end"].dt.year
    df = df[(df["fiscal_year"] >= start_year) & (df["fiscal_year"] <= end_year)].copy()

    # prefer full-year rows when available
    if "fp" in df.columns:
        fy_df = df[df["fp"].astype(str).str.upper() == "FY"].copy()
        if not fy_df.empty:
            df = fy_df

    # prefer non-amended forms, then latest filed
    df["is_amendment"] = df["form"].str.contains(r"/A|A$", regex=True, na=False)
    df = df.sort_values(
        by=["fiscal_year", "is_amendment", "filed", "end"],
        ascending=[True, True, False, False],
    )
    df = df.drop_duplicates(subset=["fiscal_year"], keep="first")
    return df


def extract_variable_series(companyfacts: dict, tag_names: List[str], start_year: int, end_year: int) -> pd.DataFrame:
    us_gaap = companyfacts.get("facts", {}).get("us-gaap", {})

    for tag in tag_names:
        concept_entry = us_gaap.get(tag)
        if not concept_entry:
            continue
        unit_bucket = pick_unit_bucket(concept_entry)
        if not unit_bucket:
            continue
        annual_df = filter_annual_facts(unit_bucket, start_year, end_year)
        if annual_df.empty:
            continue
        annual_df = annual_df[["fiscal_year", "end", "filed", "val", "form"]].copy()
        annual_df["source_tag"] = tag
        return annual_df

    return pd.DataFrame()


def pull_one_company(
    cik: str,
    ticker: str,
    company_name: str,
    user_agent: str,
    start_year: int,
    end_year: int,
):
    cik_padded = str(cik).zfill(10)
    companyfacts = get_companyfacts(cik_padded, user_agent)

    yearly_rows = []
    raw_rows = []

    for final_var, tag_names in VARIABLE_TAGS.items():
        extracted = extract_variable_series(companyfacts, tag_names, start_year, end_year)
        if extracted.empty:
            continue

        for _, r in extracted.iterrows():
            raw_rows.append(
                {
                    "ticker": ticker,
                    "cik": cik,
                    "company_name": company_name,
                    "variable_name": final_var,
                    "source_tag": r["source_tag"],
                    "fiscal_year": int(r["fiscal_year"]),
                    "period_end": r["end"].date().isoformat(),
                    "filed_date": r["filed"].date().isoformat(),
                    "form": r["form"],
                    "value": r["val"],
                }
            )

        var_df = extracted[["fiscal_year", "val"]].rename(columns={"val": final_var})
        yearly_rows.append(var_df)

    if not yearly_rows:
        return pd.DataFrame(), pd.DataFrame()

    from functools import reduce

    yearly_df = reduce(
        lambda left, right: pd.merge(left, right, on="fiscal_year", how="outer"),
        yearly_rows,
    )
    yearly_df["ticker"] = ticker
    yearly_df["cik"] = cik
    yearly_df["company_name"] = company_name

    id_cols = ["ticker", "cik", "company_name", "fiscal_year"]
    other_cols = [c for c in yearly_df.columns if c not in id_cols]
    yearly_df = yearly_df[id_cols + other_cols].sort_values(["ticker", "fiscal_year"])

    raw_df = pd.DataFrame(raw_rows)
    return yearly_df, raw_df


def make_coverage_summary(long_df: pd.DataFrame) -> pd.DataFrame:
    value_cols = [c for c in long_df.columns if c not in {"ticker", "cik", "company_name", "fiscal_year"}]
    rows = []
    total_firms = long_df["ticker"].nunique() if not long_df.empty else 0

    for col in value_cols:
        non_null = int(long_df[col].notna().sum())
        firms_with_any = int(long_df.loc[long_df[col].notna(), "ticker"].nunique())
        rows.append(
            {
                "variable": col,
                "non_null_observations": non_null,
                "firms_with_any_data": firms_with_any,
                "share_of_firms_with_any_data": firms_with_any / total_firms if total_firms else None,
            }
        )

    return pd.DataFrame(rows).sort_values(
        ["firms_with_any_data", "non_null_observations"],
        ascending=False,
    )


def make_wide_descending(long_df: pd.DataFrame) -> pd.DataFrame:
    id_cols = ["ticker", "cik", "company_name"]
    value_cols = [c for c in long_df.columns if c not in id_cols + ["fiscal_year"]]

    wide = long_df.set_index(id_cols + ["fiscal_year"])[value_cols].unstack("fiscal_year")

    years_desc = sorted(long_df["fiscal_year"].dropna().unique(), reverse=True)

    ordered_cols = []
    for var in value_cols:
        for year in years_desc:
            col = (var, year)
            if col in wide.columns:
                ordered_cols.append(col)

    wide = wide[ordered_cols]
    wide.columns = [f"{var}_{year}" for var, year in wide.columns]
    wide = wide.reset_index()
    return wide


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_xlsx", help="Path to Orbis/Tammy Excel workbook")
    parser.add_argument("--output_dir", default="sec_yearly_output_final")
    parser.add_argument("--user_agent", required=True, help="SEC-compliant User-Agent, e.g. 'Your Name your@email.com'")
    parser.add_argument("--start_year", type=int, default=2013)
    parser.add_argument("--end_year", type=int, default=2025)
    parser.add_argument("--manual_overrides_csv", default=None)
    parser.add_argument("--mapping_only", action="store_true")
    parser.add_argument("--min_auto_match_score", type=int, default=92)
    parser.add_argument("--sleep_seconds", type=float, default=0.15)
    return parser.parse_args()


def main():
    args = parse_args()

    input_xlsx = Path(args.input_xlsx)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    firms_df = load_input_companies(input_xlsx)
    sec_df = load_sec_ticker_map(args.user_agent)

    mapping_df = auto_match_names(firms_df, sec_df, min_auto_match_score=args.min_auto_match_score)
    overrides_df = load_manual_overrides(Path(args.manual_overrides_csv)) if args.manual_overrides_csv else pd.DataFrame()
    mapping_df = apply_manual_overrides(mapping_df, overrides_df, sec_df)

    mapping_path = output_dir / "sec_name_mapping_review.csv"
    mapping_df.to_csv(mapping_path, index=False)

    unmatched_df = mapping_df[
        mapping_df["ticker"].isna()
        | mapping_df["cik"].isna()
        | mapping_df["review_flag"].isin(["review", "unmatched"])
    ]
    unmatched_df.to_csv(output_dir / "sec_unmatched_companies.csv", index=False)

    print(f"Wrote mapping review: {mapping_path}")
    print(f"US firms in input: {len(firms_df)}")
    print(f"Matched/overridden firms: {mapping_df['ticker'].notna().sum()}")

    if args.mapping_only:
        print("Mapping-only mode complete.")
        return

    pull_df = mapping_df[mapping_df["ticker"].notna() & mapping_df["cik"].notna()].copy()

    long_panels = []
    raw_panels = []
    failures = []

    for _, row in pull_df.iterrows():
        orbis_name = row["orbis_company_name"]
        ticker = str(row["ticker"]).upper()
        cik = str(row["cik"])
        try:
            yearly_df, raw_df = pull_one_company(
                cik=cik,
                ticker=ticker,
                company_name=orbis_name,
                user_agent=args.user_agent,
                start_year=args.start_year,
                end_year=args.end_year,
            )
            if not yearly_df.empty:
                long_panels.append(yearly_df)
            if not raw_df.empty:
                raw_panels.append(raw_df)
            print(f"[OK] {ticker} - {orbis_name}")
        except Exception as e:
            failures.append(
                {
                    "orbis_company_name": orbis_name,
                    "ticker": ticker,
                    "cik": cik,
                    "error": str(e),
                }
            )
            print(f"[FAIL] {ticker} - {orbis_name}: {e}", file=sys.stderr)
        time.sleep(args.sleep_seconds)

    if failures:
        pd.DataFrame(failures).to_csv(output_dir / "sec_pull_failures.csv", index=False)

    if not long_panels:
        print("No yearly panels were created.")
        return

    long_df = pd.concat(long_panels, ignore_index=True)
    # one row per ticker-year
    long_df = long_df.sort_values(["ticker", "fiscal_year"]).drop_duplicates(
        subset=["ticker", "fiscal_year"], keep="first"
    )

    raw_df = pd.concat(raw_panels, ignore_index=True) if raw_panels else pd.DataFrame()
    coverage_df = make_coverage_summary(long_df)
    wide_df = make_wide_descending(long_df)

    year_range = f"{args.start_year}_{args.end_year}"
    long_csv = output_dir / f"final_sec_{args.start_year}-{args.end_year}_yearly_long.csv"
    wide_csv = output_dir / f"final_sec_{args.start_year}-{args.end_year}_yearly.csv"
    wide_xlsx = output_dir / f"final_sec_{args.start_year}-{args.end_year}_yearly.xlsx"
    raw_csv = output_dir / f"final_sec_{args.start_year}-{args.end_year}_raw_long.csv"
    coverage_csv = output_dir / f"sec_yearly_coverage_summary_{args.start_year}_{args.end_year}.csv"

    long_df.to_csv(long_csv, index=False)
    wide_df.to_csv(wide_csv, index=False)
    coverage_df.to_csv(coverage_csv, index=False)
    if not raw_df.empty:
        raw_df.to_csv(raw_csv, index=False)

    # Excel output matching the earlier final attached file style
    sheet_name = f"sec_yearly_core_panel_{args.start_year}_{args.end_year}"
    with pd.ExcelWriter(wide_xlsx, engine="openpyxl") as writer:
        wide_df.to_excel(writer, sheet_name=sheet_name[:31], index=False)

    print(f"Wrote long panel: {long_csv}")
    print(f"Wrote wide CSV: {wide_csv}")
    print(f"Wrote wide Excel: {wide_xlsx}")
    print(f"Wrote coverage summary: {coverage_csv}")
    if not raw_df.empty:
        print(f"Wrote raw source rows: {raw_csv}")


if __name__ == "__main__":
    main()
