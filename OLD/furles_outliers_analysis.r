library(dplyr)
library(ggplot2)
library(sf)
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(shiny)
library(tmap)
library(plotly)
library(dplyr)
library(DT)
library(bslib)
library(rlang)

# ---------------- PARAMS ----------------
base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"
setwd(base_path)
nuts_munic_codes_file <- file.path(base_path, "micro_dashboard/NUTS_Municipal_codes.xlsx")
nuts_shp_files <- c(
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS0.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS1.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS2.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS3.shp')
)
rdata_path <- file.path(base_path, "RData")
draw_samples <- FALSE


# ---------------- HELPERS ----------------
standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}


# ---------------- PREP DATA 2: FUEL PRICES ----------------

shape_names <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3")
shapes_df_list <- lapply(nuts_shp_files, function(x) st_read(x, quiet = TRUE))
names(shapes_df_list) <- shape_names
shapes_df_list <- lapply(shapes_df_list, function(x) {
  x %>% filter(str_detect(.[[1]], "IT"))
})

shapes_df_list <- lapply(shapes_df_list, function(df) {
  names(df)[1] <- substr(names(df)[1], 1, 5)
  df
})

nuts_munic_codes <- read_excel(nuts_munic_codes_file) %>% 
  standardize_names() %>% 
  rename(
    PRO_COM_T = codice_comune_alfanumerico,
    NUTS3_Code = codice_nuts3_2024
  ) %>% 
  select(PRO_COM_T, comune, NUTS3_Code) %>% 
  mutate(
    NUTS2_Code = str_sub(NUTS3_Code, 1, 4),
    NUTS1_Code = str_sub(NUTS3_Code, 1, 3),
    NUTS0_Code = str_sub(NUTS3_Code, 1, 2)
  ) %>%
  mutate(comune = case_when(
    comune == "Murisengo Monferrato" ~ "Murisengo",
    comune == "Castegnero Nanto" ~ "Castegnero",
    comune == "Tripi - Abakainon" ~ "Tripi",
    TRUE ~ comune
  ))


nuts_names <- nuts_munic_codes %>% 
  rename(
    NUTS3 = NUTS3_Code
  ) %>% 
  left_join(
    shapes_df_list[[4]] %>% st_drop_geometry()
  )


comuni_nogeom <- readRDS(file.path(base_path, "RData/TO_CLEAN/comuni_nogeom.RDS"))

comuni_nogeom <- comuni_nogeom %>%
  as.data.frame() %>% 
  mutate(
    pro_com_t = str_pad(as.character(pro_com), width = 6, side = "left", pad = "0")
  ) %>% 
  select(-(38:45))

comuni_region <- comuni_nogeom %>% 
  select(pro_com_t, comune, regione, macro_area4, ripartizione_istat5) %>% 
  filter(!duplicated(pro_com_t))

comuni_sum <- comuni_nogeom %>% 
  group_by(pro_com_t, anno) %>% 
  reframe(
    across(3:7, median, na.rm = TRUE),
    across(25:33, sum, na.rm = TRUE),
    n_pumps = mean(n_impianti_tot)
  )

fuel_prices_summed <- left_join(comuni_sum, comuni_region, by = "pro_com_t") %>% 
  rename(
    PRO_COM_T = pro_com_t,
    year = anno
  )

fuel_prices_summed <- fuel_prices_summed %>% 
  left_join(
    nuts_names %>% 
      select(
        PRO_COM_T, 
        NUTS3_Name,
      )
  ) %>% 
  rename(
    NUTS2_Name = regione,
    NUTS1_Name = ripartizione_istat5
  ) %>% 
  select(-macro_area4)






##################


fuel_prices_summed %>% 
  arrange(
    desc(prezzo_Benzina)
  ) %>% 
  View()


fuel_prices_selected <- fuel_prices_summed %>% 
  select(
    PRO_COM_T, year, prezzo_Benzina, prezzo_Gasolio, prezzo_Metano, prezzo_GPL, n_pumps,
    covid_emergenza, covid_reg, covid_prov, guerra_ukr, comune, NUTS2_Name, NUTS1_Name, NUTS3_Name
  )



df <- fuel_prices_selected


comuni <- st_read(
  file.path(base_path, "Limiti01012025/Com01012025/Com01012025_WGS84.shp"),
  quiet = TRUE
)


df_geom <- df %>% 
  left_join(
    comuni
  ) %>% st_as_sf()


##################


# 1. Identify outliers each year
df_processed <- df_geom %>%
  group_by(year) %>%
  mutate(
    Q1 = quantile(prezzo_Benzina, 0.25, na.rm = TRUE),
    Q3 = quantile(prezzo_Benzina, 0.75, na.rm = TRUE),
    IQR_val = Q3 - Q1,
    is_outlier = prezzo_Benzina > (Q3 + 1.5 * IQR_val) | prezzo_Benzina < (Q1 - 1.5 * IQR_val)
  ) %>%
  ungroup()

# 2. Identify structural and nonstructural outliers
outlier_classification <- df_processed %>%
  filter(is_outlier) %>%
  group_by(comune) %>%
  summarise(years_as_outlier = n(), .groups = "drop") %>%
  mutate(
    outlier_type = case_when(
      years_as_outlier >= 5 ~ "Structural",
      TRUE ~ "Non-Structural"
    )
  )

# 3. Min/Max distributions and labeling logic
# Isolate all municipalities that have been an outlier at least once
outlier_comuni <- outlier_classification$comune

# Calculate min and max price across all years for these municipalities
comune_minmax <- df_processed %>%
  filter(comune %in% outlier_comuni) %>%
  group_by(comune) %>%
  summarise(
    min_price = min(prezzo_Benzina, na.rm = TRUE),
    max_price = max(prezzo_Benzina, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate medians of the min and max distributions
median_min_dist <- median(comune_minmax$min_price, na.rm = TRUE)
median_max_dist <- median(comune_minmax$max_price, na.rm = TRUE)

# Apply labeling logic
comune_labels <- comune_minmax %>%
  mutate(
    label = case_when(
      max_price > median_max_dist & min_price > median_min_dist ~ "High",
      min_price < median_min_dist & max_price < median_max_dist ~ "Low",
      min_price < median_min_dist & max_price > median_max_dist ~ "Volatile",
      TRUE ~ "Flat" # Accounts for min > median AND max < median
    )
  )

# 4. Plot time series
# Merge labels back to the main dataframe
ts_data <- df_processed %>%
  st_drop_geometry() %>% 
  inner_join(comune_labels %>% select(comune, label), by = "comune") # %>%
  # filter(label %in% c("High", "Low", "Mixed"))

plot_ts <- ggplot(ts_data, aes(x = year, y = prezzo_Benzina, group = comune, color = label)) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  theme_minimal() +
  labs(
    title = "Time Series of Labeled Outlier Municipalities",
    x = "Year",
    y = "Petrol Price",
    color = "Outlier Label"
  )

plot_ts2 <- ggplot(ts_data, aes(x = year, y = prezzo_Benzina, group = comune, color = label)) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  theme_minimal() +
  labs(
    title = "Time Series of Labeled Outlier Municipalities",
    x = "Year",
    y = "Petrol Price",
    color = "Outlier Label"
  ) +
  facet_wrap(~label)

# 5. Plot outlier points on map
# Extract one geometry row per municipality for the map

geoms <- df_geom %>% 
  select(PRO_COM_T, geometry)

map_data <- df_geom %>%
  st_drop_geometry() %>% 
  filter(comune %in% ts_data$comune) %>%
  group_by(comune) %>%
  slice(1) %>% 
  ungroup() %>%
  inner_join(comune_labels %>% select(comune, label), by = "comune") # %>%
  # filter(label %in% c("High", "Low", "Mixed", "Flat"))



plot_map <- ggplot() +
  geom_sf(data = map_data, aes(color = label, geometry = geometry), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of Classified Outliers",
    color = "Outlier Label"
  )

# Output plots
print(plot_ts)
print(plot_ts2)
print(plot_map)






#### Analyze number of pumps per municipality by label

map_data %>% 
  group_by(label) %>% 
  summarize(
    mean(n_pumps),
    sd(n_pumps),
    n()
  )

map_data %>% 
  ggplot() +
  geom_boxplot(
    aes(label, n_pumps)
  )

#### Count municipalities by label -- identify geographical patterns
map_data %>% 
  group_by(label, NUTS1_Name) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(NUTS1_Name) %>% 
  mutate(
    tot = sum(n),
    prop = n / tot
  ) %>% View()

library(ggplot2)

summarized_data_nuts1 <- map_data %>% 
  group_by(label, NUTS1_Name) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(NUTS1_Name) %>% 
  mutate(
    tot = sum(n),
    prop = n / tot
  ) %>%
  ungroup()

# Stacked bar chart using absolute counts (n)
ggplot(summarized_data_nuts1, aes(x = NUTS1_Name, y = n, fill = label)) + 
  scale_fill_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  geom_col()

# Stacked bar chart using proportions (prop)
ggplot(summarized_data_nuts1, aes(x = NUTS1_Name, y = prop, fill = label)) + 
  scale_fill_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  geom_col()



summarized_data_nuts2 <- map_data %>% 
  group_by(label, NUTS2_Name) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(NUTS2_Name) %>% 
  mutate(
    tot = sum(n),
    prop = n / tot
  ) %>%
  ungroup()

# Stacked bar chart using absolute counts (n)
ggplot(summarized_data_nuts2, aes(x = NUTS2_Name, y = n, fill = label)) +
  geom_col() + 
  scale_fill_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

# Stacked bar chart using proportions (prop)
ggplot(summarized_data_nuts2, aes(x = NUTS2_Name, y = prop, fill = label)) +
  geom_col() + 
  scale_fill_manual(values = c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")) +
  theme(
    axis.text.x = element_text(angle = 90)
  )



