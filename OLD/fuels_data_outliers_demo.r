library(dplyr)

# 1. Calculate yearly thresholds and flag all outliers
df_flagged <- df_geom %>%
  group_by(year) %>%
  mutate(
    Q1 = quantile(prezzo_Benzina, 0.25, na.rm = TRUE),
    Q3 = quantile(prezzo_Benzina, 0.75, na.rm = TRUE),
    IQR_val = Q3 - Q1,
    outlier_status = case_when(
      prezzo_Benzina > (Q3 + 1.5 * IQR_val) ~ "High",
      prezzo_Benzina < (Q1 - 1.5 * IQR_val) ~ "Low",
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()

# 2. Identify nonstructural (transient) outliers 
# Defined here as municipalities that were outliers for only 1 or 2 years
transient_comuni <- df_flagged %>%
  filter(outlier_status != "Normal") %>%
  group_by(comune) %>%
  summarise(years_as_outlier = n()) %>%
  filter(years_as_outlier <= 2) %>%
  pull(comune)

# 3. Extract the specific outlier years and their corresponding historical event data
transient_analysis <- df_flagged %>%
  filter(comune %in% transient_comuni & outlier_status != "Normal") %>%
  select(comune, NUTS2_Name, year, outlier_status, prezzo_Benzina, covid_prov, guerra_ukr) %>%
  arrange(year, desc(prezzo_Benzina))

# 4. Summarize the macro-relationship between events and outlier appearances
event_impact_summary <- transient_analysis %>%
  group_by(year, outlier_status) %>%
  summarise(
    new_outliers_count = n(),
    avg_covid_reg_days = mean(covid_reg, na.rm = TRUE),
    avg_ukraine_war_days = mean(guerra_ukr, na.rm = TRUE),
    .groups = "drop"
  )

# Display the granular municipal data
head(transient_analysis, 10)

# Display the aggregated event summary
print(event_impact_summary)