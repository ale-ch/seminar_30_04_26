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


# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your dataframe is loaded as 'df'
# df <- read.csv("your_dataset.csv")

# ---------------------------------------------------------
# Plot 1: National Average Fuel Prices Over Time
# Highlights macroeconomic trends and crisis spikes
# ---------------------------------------------------------
df_yearly_avg <- df %>%
  group_by(year) %>%
  summarise(
    Benzina = mean(prezzo_Benzina, na.rm = TRUE),
    Gasolio = mean(prezzo_Gasolio, na.rm = TRUE),
    Metano  = mean(prezzo_Metano, na.rm = TRUE),
    GPL     = mean(prezzo_GPL, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -year, names_to = "Fuel_Type", values_to = "Average_Price")

plot_time_series <- ggplot(df_yearly_avg, aes(x = year, y = Average_Price, color = Fuel_Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "National Average Fuel Prices Over Time",
    subtitle = "Tracking price evolution across Italy",
    x = "Year",
    y = "Price",
    color = "Fuel Type"
  ) +
  theme(legend.position = "bottom")

# ---------------------------------------------------------
# Plot 2: Regional Price Disparities (Petrol)
# Shows spatial inequalities across NUTS2 regions
# ---------------------------------------------------------
plot_regional <- ggplot(df, aes(x = reorder(NUTS2_Name, prezzo_Benzina, FUN = median, na.rm = TRUE), 
                                y = prezzo_Benzina)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.shape = NA) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Regional Distribution of Petrol Prices",
    subtitle = "Variation across Italian NUTS2 regions (All Years)",
    x = "Region (NUTS2)",
    y = "Petrol Price"
  )

# ---------------------------------------------------------
# Plot 3: Impact of Regional Restrictions
# Examines relationship between restriction days and prices
# ---------------------------------------------------------
plot_restrictions <- ggplot(df, aes(x = covid_reg, y = prezzo_Benzina, color = NUTS1_Name)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  facet_wrap(~NUTS1_Name) +
  theme_minimal() +
  labs(
    title = "Petrol Prices vs. Regional Covid Restriction Days",
    subtitle = "Faceted by Macro-Region (NUTS1)",
    x = "Days of Regional Restrictions (covid_reg)",
    y = "Petrol Price"
  ) +
  theme(legend.position = "none")

# ---------------------------------------------------------
# Print plots to the viewer
# ---------------------------------------------------------
print(plot_time_series)
print(plot_regional)
print(plot_restrictions)










sampled_codes <- sample(unique(df_geom$PRO_COM_T), 1000, replace = FALSE)

tm_shape(
  df_geom %>% 
    filter(
      year == 2022,
      PRO_COM_T %in% sampled_codes
      )
  ) + 
  tm_polygons(
    "prezzo_Benzina",
    palette = "viridis")


comuni_coords <- read_csv("https://raw.githubusercontent.com/opendatasicilia/comuni-italiani/refs/heads/main/dati/coordinate.csv")
comuni_coords <- comuni_coords %>%   
  mutate(
    PRO_COM_T = str_pad(as.character(pro_com_t), width = 6, side = "left", pad = "0")
  ) %>% 
  select(-pro_com_t)

df_coords <- df %>% 
  left_join(comuni_coords)





clust <- kmeans(
  df_coords %>% 
    filter(year == 2022) %>% 
    select(
      lat, long, prezzo_Benzina
    ),
  
  20
)

clusters <- clust$cluster



df_coords_2022 <- df_coords %>% 
  filter(year == 2022) %>% 
  mutate(
    cluster = clusters
  )

df_coords_2022 %>% 
  group_by(cluster) %>% 
  reframe(
    n()
  )


df_coords_2022 %>% 
  group_by(cluster) %>% 
  reframe(
    mean_prezzo_benzina = mean(prezzo_Benzina),
    sd_prezzo_benzina = sd(prezzo_Benzina),
    min_prezzo_Benzina = min(prezzo_Benzina),
    max_prezzo_Benzina = max(prezzo_Benzina)
  ) %>% 
  arrange(desc(max_prezzo_Benzina))



tm_shape(
  df_coords_2022 %>% left_join(comuni) %>% st_as_sf()
) + 
  tm_polygons(
    "cluster",
    palette = "viridis")


tm_shape(
  df_coords_2022 %>% left_join(comuni) %>% filter(cluster == 4) %>% st_as_sf()
) + 
  tm_polygons(
    "cluster",
    palette = "viridis")

tm_shape(
  df_coords_2022 %>% left_join(comuni) %>% filter(cluster == 5) %>% st_as_sf()
) + 
  tm_polygons(
    "cluster",
    palette = "viridis")





df_coords_2022 %>% left_join(comuni) %>% filter(cluster == 4) %>% View()

df_coords_2022 %>% left_join(comuni) %>% filter(cluster == 5) %>% View()

df_coords_2022 %>%  
  filter(
    lat <= 4000, long <= 8000
  ) %>% 
  mutate(
    price_outliers = case_when(
      prezzo_Benzina >= sort(bp$out)[which(sort(bp$out) > 2)][1] ~ 1,
      prezzo_Benzina < sort(bp$out)[which(sort(bp$out) > 2)][1] ~ 0,
    )
  ) %>% 
  ggplot() + 
  geom_point(aes(x = long, y = lat, color = cluster)) + 
  geom_point(aes(x=long, y=lat, color = price_outliers))
  


df_coords_2022 %>%  
  filter(
    lat <= 4000, long <= 8000
  ) %>% 
  mutate(
    price_outliers = case_when(
      prezzo_Benzina >= sort(bp$out)[which(sort(bp$out) > 2)][1] ~ 1,
      TRUE ~ 0 # Using TRUE as a default is cleaner than repeating the condition
    )
  ) %>% 
  ggplot(aes(x = long, y = lat)) + 
  # Base layer: All points colored by their cluster
  geom_point(aes(color = prezzo_Benzina), alpha = 0.6) + 
  
  # Highlight layer: Subset only the outliers and plot them distinctly
  geom_point(
    data = ~ subset(., price_outliers == 1), 
    shape = 21,         # Shape 21 allows both fill and outline color
    fill = "transparent", 
    color = "red",      # Red outline
    size = 4,           # Larger size to circle the point
    stroke = 1.2        # Thicker border
  ) +
  theme_minimal() +
  labs(
    color = "Cluster",
    title = paste("Clusters with Highlighted Price Outliers (Gas Price >=", sort(bp$out)[which(sort(bp$out) > 2)][1], "EUR)")
  )

df_coords_2022 %>%  
  filter(
    lat <= 4000, long <= 8000
  ) %>% 
  mutate(
    price_outliers = case_when(
      prezzo_Benzina >= sort(bp$out)[which(sort(bp$out) > 2)][1] ~ 1,
      TRUE ~ 0 # Using TRUE as a default is cleaner than repeating the condition
    )
  ) %>% 
  ggplot(aes(x = long, y = lat)) + 
  # Base layer: All points colored by their cluster
  geom_point(aes(color = as.factor(cluster)), alpha = 0.6) + 
  
  # Highlight layer: Subset only the outliers and plot them distinctly
  geom_point(
    data = ~ subset(., price_outliers == 1), 
    shape = 21,         # Shape 21 allows both fill and outline color
    fill = "transparent", 
    color = "red",      # Red outline
    size = 4,           # Larger size to circle the point
    stroke = 1.2        # Thicker border
  ) +
  theme_minimal() +
  labs(
    color = "Cluster",
    title = paste("Clusters with Highlighted Price Outliers (Gas Price >=", sort(bp$out)[which(sort(bp$out) > 2)][1], "EUR)")
  )


df_coords_2022 %>%  
  filter(
    lat <= 4000, long <= 8000
  ) %>% 
  mutate(
    price_outliers = case_when(
      prezzo_Benzina >= sort(bp$out)[which(sort(bp$out) > 2)][1] ~ 1,
      TRUE ~ 0 # Using TRUE as a default is cleaner than repeating the condition
    )
  ) %>% 
  ggplot(aes(x = long, y = lat)) + 
  # Base layer: All points colored by their cluster
  geom_point(aes(color = guerra_ukr), alpha = 0.6) + 
  
  # Highlight layer: Subset only the outliers and plot them distinctly
  geom_point(
    data = ~ subset(., price_outliers == 1), 
    shape = 21,         # Shape 21 allows both fill and outline color
    fill = "transparent", 
    color = "red",      # Red outline
    size = 4,           # Larger size to circle the point
    stroke = 1.2        # Thicker border
  ) +
  theme_minimal() +
  labs(
    color = "Cluster",
    title = paste("Clusters with Highlighted Price Outliers (Gas Price >=", sort(bp$out)[which(sort(bp$out) > 2)][1], "EUR)")
  )

