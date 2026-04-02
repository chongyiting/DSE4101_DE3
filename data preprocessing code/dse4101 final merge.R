library(dplyr)
library(tidyr)
library(readr)
library(readxl)

library(lubridate)


rm(list=ls())
disasters <- read_csv("C:/Users/zhoum/Documents/company_disaster_post2014.csv")
recovery <- read_csv("C:/Users/zhoum/Downloads/storm_company_recovery_days.csv")


disaster_recovery <- disasters %>%
                        left_join(recovery,by = c("ticker","event_name"))

disaster_recovery <- filter (disaster_recovery,disaster_year>=2015)

disaster_recovery$start_date <- as.Date(disaster_recovery$start_date, format = "%Y/%m/%d")
disaster_recovery$end_date   <- as.Date(disaster_recovery$end_date, format = "%Y/%m/%d")

disaster_recovery$start_month <- month(disaster_recovery$start_date)              
disaster_recovery$end_month   <- month(disaster_recovery$end_date)

disaster_recovery$duration <- disaster_recovery$end_date-disaster_recovery$start_date
disaster_recovery <- disaster_recovery %>%
  relocate(start_month, end_month, duration, .after = disaster_year)



disaster_recovery <- disaster_recovery %>%
  mutate(
    rec_1d   = as.integer(recovery_days != -1 & recovery_days <= 1),
    rec_7d   = as.integer(recovery_days != -1 & recovery_days <= 7),
    rec_30d  = as.integer(recovery_days != -1 & recovery_days <= 30),
    rec_180d = as.integer(recovery_days != -1 & recovery_days <= 180)
  )
disaster_recovery$duration <- as.numeric(
  gsub(" days", "", disaster_recovery$duration)
)


real_GDP <- read.csv ("C:/Users/zhoum/Downloads/GDPCA.csv")

real_GDP<- real_GDP %>%
  mutate(year = as.integer(year(as.Date(observation_date,format = "%d/%m/%Y"))))%>%
  select(year, GDPCA) 
  
disaster_recovery <- disaster_recovery %>%
  left_join(real_GDP, by = c("disaster_year" = "year"))

inflation <- read.csv("C:/Users/zhoum/Downloads/inflation.csv")


inflation<- inflation %>%
  mutate(year = as.integer(year(as.Date(observation_date,format = "%d/%m/%Y"))),inflation=FPCPITOTLZGUSA)%>%
  select(year, inflation)                     

disaster_recovery <- disaster_recovery %>%
  left_join(inflation, by = c("disaster_year" = "year"))


northeast <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", 
               "Rhode Island", "Connecticut", "New York", "New Jersey", "Pennsylvania")

midwest <- c("Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin",
             "Minnesota", "Iowa", "Missouri", "North Dakota", 
             "South Dakota", "Nebraska", "Kansas")

south <- c("Delaware", "Maryland", "District of Columbia", "Virginia", "West Virginia",
           "North Carolina", "South Carolina", "Georgia", "Florida",
           "Kentucky", "Tennessee", "Mississippi", "Alabama", "Oklahoma", "Texas", "Arkansas", "Louisiana")

west <- c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Utah", "Nevada",
          "Washington", "Oregon", "California", "Alaska", "Hawaii")
library(dplyr)

disaster_recovery <- disaster_recovery %>%
  mutate(region = case_when(
    affected_states %in% northeast ~ "Northeast",
    affected_states %in% midwest   ~ "Midwest",
    affected_states %in% south     ~ "South",
    affected_states %in% west      ~ "West",
    TRUE                  ~ "Other" 
  ))
disaster_recovery <- disaster_recovery %>%
  relocate(region, .after = affected_states)


disaster_recovery <- disaster_recovery %>%
  filter(!is.na(pre_shock_price))



disaster_recovery <- disaster_recovery %>%
 select(-Investments, -magnitude_kph)




na_count <- disaster_recovery %>%
  summarise(across(everything(), ~mean(is.na(.))*100))

getwd()
write.csv(disaster_recovery, 
          file = "predictor_disaster_data.csv", 
          row.names = FALSE)



