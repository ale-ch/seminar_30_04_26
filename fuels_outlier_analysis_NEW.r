# ==============================================================================
# 0. SETUP & LIBRARIES
# ==============================================================================
library(tidyverse) # Includes dplyr, ggplot2, stringr, purrr, tidyr, lubridate
library(sf)
library(readxl)
library(shiny)
library(tmap)
library(plotly)
library(DT)
library(bslib)

# ==============================================================================
# 1. PARAMETERS & PATHS
# ==============================================================================
base_path <- "/Volumes/T7 Shield/FRES/DB_Comunale"
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

# Define consistent color palette
label_colors <- c("High" = "firebrick", "Low" = "steelblue", "Volatile" = "purple", "Flat" = "green")

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}

# ==============================================================================
# 3. DATA PREPARATION: SHAPEFILES & METADATA
# ==============================================================================
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
    NUTS0_Code = str_sub(NUTS3_Code, 1, 2),
    comune = case_when(
      comune == "Murisengo Monferrato" ~ "Murisengo",
      comune == "Castegnero Nanto" ~ "Castegnero",
      comune == "Tripi - Abakainon" ~ "Tripi",
      TRUE ~ comune
    )
  )

nuts_names <- nuts_munic_codes %>% 
  rename(NUTS3 = NUTS3_Code) %>% 
  left_join(shapes_df_list[[4]] %>% st_drop_geometry(), by = "NUTS3")

# ==============================================================================
# 4. DATA PREPARATION: FUEL PRICES
# ==============================================================================
comuni_nogeom <- readRDS(file.path(base_path, "RData/TO_CLEAN/comuni_nogeom.RDS")) %>%
  as.data.frame() %>% 
  mutate(pro_com_t = str_pad(as.character(pro_com), width = 6, side = "left", pad = "0")) %>% 
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

df <- left_join(comuni_sum, comuni_region, by = "pro_com_t") %>% 
  rename(PRO_COM_T = pro_com_t, year = anno) %>% 
  left_join(nuts_names %>% select(PRO_COM_T, NUTS3_Name), by = "PRO_COM_T") %>% 
  rename(NUTS2_Name = regione, NUTS1_Name = ripartizione_istat5) %>% 
  select(
    PRO_COM_T, year, prezzo_Benzina, prezzo_Gasolio, prezzo_Metano, prezzo_GPL, n_pumps,
    covid_emergenza, covid_reg, covid_prov, guerra_ukr, comune, NUTS2_Name, NUTS1_Name, NUTS3_Name
  )

# Add Geometry
comuni <- st_read(file.path(base_path, "Limiti01012025/Com01012025/Com01012025_WGS84.shp"), quiet = TRUE)
df_geom <- df %>% left_join(comuni) %>% st_as_sf()

# ==============================================================================
# 5. OUTLIER ANALYSIS & CLASSIFICATION
# ==============================================================================
# 5.1 Identify yearly outliers
df_processed <- df_geom %>%
  group_by(year) %>%
  mutate(
    Q1 = quantile(prezzo_Benzina, 0.25, na.rm = TRUE),
    Q3 = quantile(prezzo_Benzina, 0.75, na.rm = TRUE),
    IQR_val = Q3 - Q1,
    is_outlier = prezzo_Benzina > (Q3 + 1.5 * IQR_val) | prezzo_Benzina < (Q1 - 1.5 * IQR_val)
  ) %>%
  ungroup()

# 5.2 Identify structural vs non-structural outliers
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

# 5.3 Min/Max distributions and labeling logic
outlier_comuni <- outlier_classification$comune

comune_minmax <- df_processed %>%
  filter(comune %in% outlier_comuni) %>%
  group_by(comune) %>%
  summarise(
    min_price = min(prezzo_Benzina, na.rm = TRUE),
    max_price = max(prezzo_Benzina, na.rm = TRUE),
    .groups = "drop"
  )

median_min_dist <- median(comune_minmax$min_price, na.rm = TRUE)
median_max_dist <- median(comune_minmax$max_price, na.rm = TRUE)

comune_labels <- comune_minmax %>%
  mutate(
    label = case_when(
      max_price > median_max_dist & min_price > median_min_dist ~ "High",
      min_price < median_min_dist & max_price < median_max_dist ~ "Low",
      min_price < median_min_dist & max_price > median_max_dist ~ "Volatile",
      TRUE ~ "Flat" 
    )
  )

# Merge labels back for visualizations
ts_data <- df_processed %>%
  st_drop_geometry() %>% 
  inner_join(comune_labels %>% select(comune, label), by = "comune")

# ==============================================================================
# 6. TIME SERIES VISUALIZATIONS
# ==============================================================================
plot_ts <- ggplot(ts_data, aes(x = year, y = prezzo_Benzina, group = comune, color = label)) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = label_colors) +
  theme_minimal() +
  labs(title = "Time Series of Outlier Municipalities", x = "Year", y = "Petrol Price", color = "Outlier Label") #  + 
  # geom_smooth(method='loess')

# plot_ts2 <- plot_ts + facet_wrap(~label)

plot_ts2 <- plot_ts + 
  facet_wrap(~label) + 
  geom_smooth(aes(group = 1), method = 'loess', se = FALSE, formula = y ~ x, color = "black", linewidth = 1.2)  + 
  theme(
    axis.text.x = element_text(angle = 60)
  )

print(plot_ts)
print(plot_ts2)

# ==============================================================================
# 7. SPATIAL VISUALIZATION
# ==============================================================================
# Filter df_geom directly to preserve geometry for the map
map_data <- df_geom %>%
  st_drop_geometry() %>% 
  filter(comune %in% ts_data$comune) %>%
  group_by(comune) %>%
  slice(1) %>% 
  ungroup() %>%
  inner_join(comune_labels %>% select(comune, label), by = "comune")

plot_map <- ggplot() +
  geom_sf(data = map_data, aes(color = label, geometry = geometry), size = 2, alpha = 0.8) +
  scale_color_manual(values = label_colors) +
  theme_minimal() +
  labs(
    title = "Spatial Distribution of Outliers",
    color = "Outlier Label"
  )


print(plot_map)



# 1. Prepare the spatial data properly (DO NOT drop geometry)
sf_use_s2(FALSE) # Prevents spherical geometry errors when calculating centroids

map_points <- df_geom %>%
  filter(comune %in% ts_data$comune) %>%
  group_by(comune) %>%
  slice(1) %>% 
  ungroup() %>%
  st_join(comune_labels %>% select(comune, label), by = "comune") %>%
  st_centroid() # Converts the municipal polygons into single points (dots)

# 2. Plot with tmap in interactive view mode
tmap_mode("view") # Turns on interactive map tiles

tm_shape(map_points) +
  tm_dots(
    col = "label", 
    palette = label_colors, 
    size = 0.5, 
    alpha = 0.8,
    title = "Outlier Label",
    popup.vars = c("Municipality" = "comune.x", "Region" = "NUTS2_Name")
  )



# ==============================================================================
# 8. SUMMARY STATISTICS & CHARTS
# ==============================================================================
# Analyze number of pumps per municipality by label
pump_stats <- map_data %>% 
  st_drop_geometry() %>%
  group_by(label) %>% 
  summarize(
    mean_pumps = mean(n_pumps, na.rm = TRUE),
    sd_pumps = sd(n_pumps, na.rm = TRUE),
    count = n()
  )
print(pump_stats)

ggplot(map_data, aes(x = label, y = n_pumps, fill = label)) +
  geom_boxplot() +
  scale_fill_manual(values = label_colors) +
  theme_minimal() +
  labs(title = "Number of Pumps by Outlier Label", y = "# Pumps")

# Summarize data by NUTS1
summarized_data_nuts1 <- map_data %>% 
  st_drop_geometry() %>%
  count(label, NUTS1_Name) %>% 
  group_by(NUTS1_Name) %>% 
  mutate(tot = sum(n), prop = n / tot) %>%
  ungroup()

# Stacked bar chart (NUTS1 absolute & proportions)
ggplot(summarized_data_nuts1, aes(x = NUTS1_Name, y = n, fill = label)) + 
  geom_col() +
  scale_fill_manual(values = label_colors) +
  theme_minimal()  +
  labs(title = "Territorial composition of outlier municipalities (count, NUTS 1)", y = "# Municipalities", x = "Area")

ggplot(summarized_data_nuts1, aes(x = NUTS1_Name, y = prop, fill = label)) + 
  geom_col() +
  scale_fill_manual(values = label_colors) +
  theme_minimal() +
  labs(title = "Territorial composition of outlier municipalities (proportion, NUTS 1)", y = "# Municipalities", x = "Area")

# Summarize data by NUTS2
summarized_data_nuts2 <- map_data %>% 
  st_drop_geometry() %>%
  count(label, NUTS2_Name) %>% 
  group_by(NUTS2_Name) %>% 
  mutate(tot = sum(n), prop = n / tot) %>%
  ungroup()

# Stacked bar chart (NUTS2 absolute & proportions)
ggplot(summarized_data_nuts2, aes(x = NUTS2_Name, y = n, fill = label)) +
  geom_col() + 
  scale_fill_manual(values = label_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Territorial composition of outlier municipalities (proportion, NUTS 2)", y = "# Municipalities", x = "Region")

ggplot(summarized_data_nuts2, aes(x = NUTS2_Name, y = prop, fill = label)) +
  geom_col() + 
  scale_fill_manual(values = label_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Territorial composition of outlier municipalities (proportion, NUTS 2)", y = "# Municipalities", x = "Region")