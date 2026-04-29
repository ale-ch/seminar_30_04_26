library(dplyr)
library(sf)
library(leaflet)

# 1. Define static filters
target_year <- 2022
target_fuel <- "prezzo_Benzina"

# 2. Filter data and calculate boxplot statistics
df_geom <- sf::st_transform(df_geom, crs = 4326)

df_filtered <- df_geom %>%
  filter(year == target_year) %>%
  select(comune, price = !!sym(target_fuel), geometry) %>%
  filter(!is.na(price))

q_stats <- quantile(df_filtered$price, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
iqr_val <- q_stats[3] - q_stats[1]
lower_bound <- q_stats[1] - 1.5 * iqr_val
upper_bound <- q_stats[3] + 1.5 * iqr_val

# 3. Assign clusters
df_filtered <- df_filtered %>%
  mutate(
    cluster = case_when(
      price < lower_bound ~ "Low Outlier",
      price >= lower_bound & price < q_stats[1] ~ "Q1",
      price >= q_stats[1] & price < q_stats[2] ~ "Q2",
      price >= q_stats[2] & price < q_stats[3] ~ "Q3",
      price >= q_stats[3] & price <= upper_bound ~ "Q4",
      price > upper_bound ~ "High Outlier"
    ),
    cluster = factor(cluster, levels = c("Low Outlier", "Q1", "Q2", "Q3", "Q4", "High Outlier"))
  )

# 4. Define colors
cluster_colors <- colorFactor(
  palette = c("green", "#cccccc", "#999999", "#666666", "#333333", "red"),
  domain = c("Low Outlier", "Q1", "Q2", "Q3", "Q4", "High Outlier"),
  ordered = TRUE
)

# 5. Generate interactive map
leaflet(df_filtered) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~cluster_colors(cluster),
    weight = 0.5,
    opacity = 1,
    color = "white",
    fillOpacity = ~ifelse(cluster %in% c("Low Outlier", "High Outlier"), 0.9, 0.6),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = ~paste0(comune, ": €", round(price, 3), " (", cluster, ")")
  ) %>%
  addLegend(
    position = "bottomright",
    pal = cluster_colors,
    values = ~cluster,
    title = paste("Price Clusters:", target_fuel, "-", target_year),
    opacity = 1
  )
