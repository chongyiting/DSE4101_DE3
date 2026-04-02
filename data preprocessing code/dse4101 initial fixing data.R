library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Load company data

company <- read_excel("C:/Users/zhoum/Downloads/current_fixed_company_data.xlsx")  
na_counts <- company %>%
  summarise(across(everything(), ~ sum(is.na(.))))

print(na_counts)
# Load disaster data
disaster <- read_csv("C:/Users/zhoum/Downloads/climate_data_clean_v1.csv")



keywords <- c("liabilities", "inventory_net", "long_term_debt_noncurrent")

cols_to_exclude <- colnames(company)[sapply(colnames(company), function(x) 
  any(sapply(keywords, grepl, x, ignore.case = TRUE)))
]

print("Columns to exclude:")
print(cols_to_exclude)

company_filtered <- company[, !(colnames(company) %in% cols_to_exclude)]

print("Remaining columns:")
print(colnames(company_filtered))

us_states <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
  "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
  "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawaii", "ID" = "Idaho",
  "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas",
  "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
  "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi",
  "MO" = "Missouri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada",
  "NH" = "New Hampshire", "NJ" = "New Jersey", "NM" = "New Mexico", "NY" = "New York",
  "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio", "OK" = "Oklahoma",
  "OR" = "Oregon", "PA" = "Pennsylvania","PR"="Puerto Rico", "RI" = "Rhode Island", "SC" = "South Carolina",
  "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah",
  "VT" = "Vermont", "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia",
  "WI" = "Wisconsin", "WY" = "Wyoming", "DC" = "District of Columbia"
)

can_provinces <- c(
  "AB" = "Alberta", "BC" = "British Columbia", "MB" = "Manitoba",
  "NB" = "New Brunswick", "NL" = "Newfoundland and Labrador", "NS" = "Nova Scotia",
  "NT" = "Northwest Territories", "NU" = "Nunavut", "ON" = "Ontario",
  "PE" = "Prince Edward Island", "QC" = "Quebec", "SK" = "Saskatchewan",
  "YT" = "Yukon"
)

us_can_mapping <- c(us_states, can_provinces)

state_map <- data.frame(
  Abbrev = names(us_can_mapping),
  FullName = as.character(us_can_mapping),
  stringsAsFactors = FALSE
)


company_clean <- company_filtered %>%
  rename(Branches = `BRANCH - State or province (in US or Canada)`) 


  company_clean <- separate_rows(company_clean,Branches, sep = ",\\s*") 
  company_clean$Branches 
  company_clean<- company_clean %>% 
    left_join(state_map, by = c("Branches" = "Abbrev"))    
  company_clean <- company_clean %>%
    rename(States=FullName)
  
  
  company_clean <- company_clean %>%
    filter(!is.na(States))

  # Pick metrics that follow "metricname_year" pattern
  assets_cols <- company_clean %>%
    select(ticker, company_name, States, starts_with("assets_")) %>%
    select(-starts_with("assets_current_"))
  
  company_long_assets <- assets_cols %>%
    pivot_longer(
      cols = -c(ticker, company_name, States),
      names_to = c("metric", "year"),
      names_sep = "_",
      values_to = "value"
    ) 
  
  
  assets_current_cols <- company_clean %>%
    select(ticker, company_name, States, starts_with("assets_current"))
  
  company_long_assets_current <- assets_current_cols %>%
    pivot_longer(
      cols = -c(ticker, company_name, States),
      names_to = c("metric", "year"),
      names_pattern = "(assets_current)_(\\d{4})",  # everything before last _ = metric, last 4 digits = year
      values_to = "value"
    ) 
  # Select only the columns  listed, plus identifiers
  financial_cols <- company_clean %>%
    select(ticker, company_name, States, 
           starts_with("cash_and_equivalents"),
           starts_with("stockholders_equity"),
           starts_with("revenue"),
           starts_with("operating_income_loss"),
           starts_with("net_income_loss"),
           starts_with("operating_cash_flow"))
  
  # Pivot into long format
  company_long_financials <- financial_cols %>%
    pivot_longer(
      cols = -c(ticker, company_name, States),
      names_to = c("metric", "year"),
      names_pattern = "(.*)_(\\d{4})",
      values_to = "value"
    ) 
  
  employee_financial_cols <- company_clean %>%
    select(ticker, company_name, States,
           starts_with("Number of employees"),
           starts_with("Net Income"),
           starts_with("Net inventory"),
           starts_with("Cash flow / Operating revenue"),
           starts_with("EBITDA margin"),
           starts_with("Liquidity ratio"),
           starts_with("Investments"),
           starts_with("Net debt"),
           starts_with("Total cash & short-term investments"))
  
  employee_financial_cols <- employee_financial_cols %>%
    mutate(across(contains("Number of employees"),
                  ~ as.numeric(gsub(",", "", .))))
  
  employee_financial_cols <- employee_financial_cols %>%
    mutate(
      across(
        -c(ticker, company_name, States),
        ~ as.numeric(gsub(",", "", .))
      )
    )
  
  # Pivot to long format
  company_long_employee_financials <- employee_financial_cols %>%
    pivot_longer(
      cols = -c(ticker, company_name, States),
      names_to = c("metric", "year"),
      names_pattern = "^(.*?)\\r?\\n.*?(\\d{4})$",
      values_to = "value"
      
    ) 
  
  up_down_cols <- company_clean %>%
    select(
      ticker, company_name, States,
      starts_with("upstream"),
      starts_with("downstream")
    )
  
  company_long_updown <- up_down_cols %>%
    pivot_longer(
      cols = -c(ticker, company_name, States),
      names_to = c("direction", "year"),
      names_pattern = "(upstream|downstream)\\((\\d{4})\\)",
      values_to = "value"
    ) 
  
  
  
  company_updown_wide <- company_long_updown %>%
    pivot_wider(
      names_from = direction,
      values_from = value
    )
  
  all_metrics_long <- bind_rows(
    company_long_assets,
    company_long_assets_current,
    company_long_financials,
    company_long_employee_financials
  )
  
  
company_all_long <- all_metrics_long %>%
    left_join(company_updown_wide,
              by = c("ticker", "company_name", "States", "year"))%>%
     mutate(year=as.integer(year))
  
  
disaster_clean <- disaster %>%
   separate_rows(affected_states, sep = ",\\s*")


disaster_clean <- disaster_clean %>%
  mutate(disaster_year = lubridate::year(start_date))

disaster_clean <- disaster_clean %>%
  mutate(disaster_year = as.integer(disaster_year))

disaster_clean_filtered <- disaster_clean %>%
  filter(disaster_year >= 2014)

company_disaster <- disaster_clean_filtered %>%
  left_join(company_all_long, by = c("disaster_year" = "year","affected_states" = "States"))


#merged <- disaster_clean %>%
  #left_join(company_clean, by = c("affected_states" = "States"))

sapply(company_disaster, class)


company_disaster_clean <- company_disaster %>%
  unnest(cols = c(upstream, downstream))


company_disaster_wide <- company_disaster_clean %>%
  pivot_wider(
    id_cols = c(ticker, company_name,event_name,disaster_subtype,magnitude_kph,total_damage_adjusted,start_date,end_date, disaster_year,affected_states,upstream,downstream),
    names_from = metric,
    values_from = value
  )
company_disaster_wide_unique <- company_disaster_wide %>%
  group_by(ticker, company_name,event_name, disaster_year) %>%
  slice(1) %>%  # keep only the first row per company-disaster
  ungroup()
write.csv(company_disaster_wide_unique, "company_disaster_post2014.csv", row.names = FALSE)
