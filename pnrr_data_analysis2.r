# ==============================================================================
# PNRR FUNDING ANALYSIS SCRIPT
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(purrr)
library(tidyr)
library(lubridate)

standardize_names <- function(df) {
  df %>%
    rename_with(~ .x %>%
                  str_to_lower() %>%
                  str_replace_all("[^a-z0-9]+", "_") %>%
                  str_remove_all("^_+|_+$"))
}

base_path  <- "/Volumes/T7 Shield/FRES/DB_Comunale"

municipal_data_merged <- readRDS(file.path(base_path, "RData/Merged/municipal_data_merged.RDS"))

nuts_munic_codes_file <- file.path(base_path, "micro_dashboard/NUTS_Municipal_codes.xlsx")

nuts_shp_files <- c(
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS0.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS1.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS2.shp'),
  file.path(base_path, '../macro_dashboard/data/Geometrie/Shapefile_NUTS3.shp')
)

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
  )) %>% 
  left_join(
    municipal_data_merged %>% 
      select(COMUNE, PRO_COM_T) %>% 
      rename(comune = COMUNE) %>% 
      st_drop_geometry() %>% 
      unique(), 
    by = "comune") %>% 
  select(3:7) %>% 
  rename(PRO_COM_T = PRO_COM_T.y)

# 

# ---------------- PREP DATA 3: PNNR ----------------
load(file.path(base_path, "RData/TO_CLEAN/PNNR.RData")) # Loads Data4

df_pnnr <- Data4 %>% 
  select(1:12, 17:28, Data.Inizio.Progetto.Prevista, Data.Fine.Progetto.Prevista)

df_renamed <- df_pnnr[, 13:27] %>% 
  mutate(across(1:11, as.numeric))

df_renamed <- standardize_names(df_renamed)

df2 <- df_pnnr[, 1:12] %>% 
  st_drop_geometry() %>% 
  bind_cols(df_renamed) %>% 
  st_as_sf() %>%
  mutate(
    date_start = dmy(data_inizio_progetto_prevista),
    date_end = dmy(data_fine_progetto_prevista),
    year_start = year(date_start),
    year_end = year(date_end)
  )

################################

meta_cols <- c(1:13, 165:172)
pnnr_cols <- 173:182
fuels_cols <- 183:197

#pnnr_data <- municipal_data_merged %>% 
#  select(all_of(meta_cols), all_of(pnnr_cols))
#load(file.path(base_path, "RData/TO_CLEAN/PNNR.RData"))
#Data4$denominazione
#length(unique(Data4$denominazione))
#length(unique(pnnr_data$PRO_COM_T))

###########


df_summarized <- df2 %>% 
  st_drop_geometry() %>% 
  mutate(
    across(13:24, as.numeric),
    PRO_COM_T = str_pad(as.character(PRO_COM_T), width = 6, side = "left", pad = "0")
  ) %>% 
  group_by(year_start, denominazione) %>% 
  reframe(
    PRO_COM_T,
    n_projects = n(),
    across(12:23, sum, na.rm=FALSE)
  ) %>% 
  ungroup()




pnrr_summarized_nuts <- left_join(df_summarized, nuts_munic_codes, by = "PRO_COM_T") %>% 
  rename(
    NUTS3 = NUTS3_Code,
    NUTS2 = NUTS2_Code,
    NUTS1 = NUTS1_Code,
    NUTS0 = NUTS0_Code,
  ) %>% 
  left_join(shapes_df_list[["NUTS0"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS1"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS2"]] %>% st_drop_geometry()) %>%
  left_join(shapes_df_list[["NUTS3"]] %>% st_drop_geometry())


pnrr_summarized_nuts <- pnrr_summarized_nuts %>% 
  distinct(year_start, denominazione, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 4. EXPLORATORY DATA VIEWS (Optional: run line-by-line as needed)
# ------------------------------------------------------------------------------
# NUTS1 Split
# pnrr_summarized_nuts %>% mutate(pnrr_start = case_when(year_start >= 2021 ~ 1, year_start < 2021 ~ 0)) %>% group_by(NUTS1_Name, pnrr_start) %>% reframe(tot_projects_pnrr = sum(n_projects), tot_pnrr_funding_split = sum(finanziamento_pnrr)) %>% na.omit() %>% ungroup() %>% group_by(NUTS1_Name) %>% reframe(NUTS1_Name, pnrr_start, tot_projects = sum(tot_projects_pnrr), tot_projects_pnrr, tot_pnrr_funding_split, tot_pnrr_funding = sum(tot_pnrr_funding_split)) %>% ungroup() %>% mutate(share_projects = round((tot_projects_pnrr / tot_projects) * 100, 2), share_pnrr_funding = round((tot_pnrr_funding_split / tot_pnrr_funding) * 100, 2)) %>% view()

# NUTS2 Split
# pnrr_summarized_nuts %>% mutate(pnrr_start = case_when(year_start >= 2021 ~ 1, year_start < 2021 ~ 0)) %>% group_by(NUTS2_Name, pnrr_start) %>% reframe(tot_projects_pnrr = sum(n_projects), tot_pnrr_funding_split = sum(finanziamento_pnrr)) %>% na.omit() %>% ungroup() %>% group_by(NUTS2_Name) %>% reframe(NUTS2_Name, pnrr_start, tot_projects = sum(tot_projects_pnrr), tot_projects_pnrr, tot_pnrr_funding_split, tot_pnrr_funding = sum(tot_pnrr_funding_split)) %>% ungroup() %>% mutate(share_projects = round((tot_projects_pnrr / tot_projects) * 100, 2), share_pnrr_funding = round((tot_pnrr_funding_split / tot_pnrr_funding) * 100, 2)) %>% view()

# NUTS3 Split
# pnrr_summarized_nuts %>% mutate(pnrr_start = case_when(year_start >= 2021 ~ 1, year_start < 2021 ~ 0)) %>% group_by(NUTS3_Name, pnrr_start) %>% reframe(tot_projects_pnrr = sum(n_projects), tot_pnrr_funding_split = sum(finanziamento_pnrr)) %>% na.omit() %>% ungroup() %>% group_by(NUTS3_Name) %>% reframe(pnrr_start, tot_projects = sum(tot_projects_pnrr), tot_projects_pnrr, tot_pnrr_funding_split, tot_pnrr_funding = sum(tot_pnrr_funding_split)) %>% ungroup() %>% mutate(share_projects = round((tot_projects_pnrr / tot_projects) * 100, 2), share_pnrr_funding = round((tot_pnrr_funding_split / tot_pnrr_funding) * 100, 2)) %>% view()

# Municipal Split
# pnrr_summarized_nuts %>% mutate(pnrr_start = case_when(year_start >= 2021 ~ 1, year_start < 2021 ~ 0)) %>% group_by(denominazione, pnrr_start) %>% reframe(tot_projects_pnrr = sum(n_projects), tot_pnrr_funding_split = sum(finanziamento_pnrr)) %>% na.omit() %>% ungroup() %>% group_by(denominazione) %>% reframe(pnrr_start, tot_projects = sum(tot_projects_pnrr), tot_projects_pnrr, tot_pnrr_funding_split, tot_pnrr_funding = sum(tot_pnrr_funding_split)) %>% ungroup() %>% mutate(share_projects = round((tot_projects_pnrr / tot_projects) * 100, 2), share_pnrr_funding = round((tot_pnrr_funding_split / tot_pnrr_funding) * 100, 2)) %>% view()


# ------------------------------------------------------------------------------
# 5. REGIONAL ANALYSES & PLOTS
# ------------------------------------------------------------------------------

# 5.1 Macro-Regional Split (Policy Compliance)
macro_regional_split <- pnrr_summarized_nuts %>%
  group_by(NUTS1_Name) %>%
  summarise(total_pnrr = sum(finanziamento_pnrr, na.rm = TRUE)) %>%
  mutate(perc_total = (total_pnrr / sum(total_pnrr)) * 100) %>%
  arrange(desc(perc_total))

plot_macro <- ggplot(macro_regional_split, aes(x = reorder(NUTS1_Name, perc_total), y = perc_total)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 40, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "PNRR Funding Distribution by Macro-Region",
       x = "Macro-Region (NUTS1)", 
       y = "Percentage of Total PNRR Funds (%)") +
  theme_minimal()

# 5.2 Private Multiplier Effect by Region (NUTS2)
private_multiplier <- pnrr_summarized_nuts %>%
  group_by(NUTS2_Name) %>%
  summarise(
    total_pnrr = sum(finanziamento_pnrr, na.rm = TRUE),
    total_privato = sum(finanziamento_privato, na.rm = TRUE)
  ) %>%
  mutate(multiplier_ratio = total_privato / total_pnrr) %>%
  arrange(desc(multiplier_ratio))

plot_priv_mult <- ggplot(private_multiplier, aes(x = reorder(NUTS2_Name, multiplier_ratio), y = multiplier_ratio)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Private Funding Multiplier by Region",
       subtitle = "Ratio of Private Funds to PNRR Funds",
       x = "Region (NUTS2)", y = "Multiplier Ratio") +
  theme_minimal()

# 5.3 Local Fiscal Capacity by Region (NUTS2)
local_fiscal_capacity <- pnrr_summarized_nuts %>%
  mutate(
    local_funding = rowSums(across(c(finanziamento_comune, finanziamento_regione, finanziamento_provincia)), na.rm = TRUE),
    local_dependency_ratio = local_funding / finanziamento_totale
  ) %>%
  group_by(NUTS2_Name) %>%
  summarise(avg_local_capacity = mean(local_dependency_ratio, na.rm = TRUE)) %>%
  arrange(desc(avg_local_capacity))

plot_local_cap <- ggplot(local_fiscal_capacity, aes(x = reorder(NUTS2_Name, avg_local_capacity), y = avg_local_capacity)) +
  geom_col(fill = "darkorange") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(title = "Local Fiscal Capacity by Region",
       subtitle = "Average % of local funding (Commune, Province, Region) in total project cost",
       x = "Region (NUTS2)", y = "Local Dependency Ratio") +
  theme_minimal()

# 5.4 Risk and Execution Feasibility (Funding Gaps NUTS2)
funding_gaps <- pnrr_summarized_nuts %>%
  group_by(NUTS2_Name) %>%
  summarise(
    total_da_reperire = sum(finanziamento_da_reperire, na.rm = TRUE),
    total_finanziamento = sum(finanziamento_totale, na.rm = TRUE)
  ) %>%
  mutate(gap_percentage = (total_da_reperire / total_finanziamento) * 100) %>%
  arrange(desc(gap_percentage))

plot_gaps <- ggplot(funding_gaps, aes(x = reorder(NUTS2_Name, gap_percentage), y = gap_percentage)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Funding Gaps by Region",
       subtitle = "Percentage of total funding still 'da reperire' (to be found)",
       x = "Region (NUTS2)", y = "Gap Percentage (%)") +
  theme_minimal()

# 5.5 Scale and Fragmentation (NUTS3)
scale_fragmentation <- pnrr_summarized_nuts %>%
  group_by(NUTS3_Name) %>%
  summarise(
    total_funding = sum(finanziamento_totale, na.rm = TRUE),
    total_projects = sum(n_projects, na.rm = TRUE)
  ) %>%
  mutate(avg_funding_per_project = total_funding / total_projects) %>%
  arrange(avg_funding_per_project)

top_bottom_scale <- scale_fragmentation %>%
  filter(!is.na(NUTS3_Name)) %>%
  arrange(avg_funding_per_project) %>%
  slice(c(1:10, (n() - 9):n())) %>%
  mutate(Type = ifelse(row_number() <= 10, "Highly Fragmented (Micro)", "Large Scale (Macro)"))

plot_scale <- ggplot(top_bottom_scale, aes(x = reorder(NUTS3_Name, avg_funding_per_project), y = avg_funding_per_project, fill = Type)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values = c("Highly Fragmented (Micro)" = "#E69F00", "Large Scale (Macro)" = "#56B4E9")) +
  labs(title = "Average Funding per Project by Province (NUTS3)",
       subtitle = "Comparing the 10 most fragmented vs. 10 largest-scale provinces",
       x = "Province (NUTS3)", y = "Average Funding (€)", fill = "Project Scale") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 5.6 Temporal Evolution
temporal_evolution <- pnrr_summarized_nuts %>%
  mutate(period = ifelse(year_start >= 2021, "Post-PNRR (>= 2021)", "Pre-PNRR (< 2021)")) %>%
  group_by(period) %>%
  summarise(
    total_pnrr = sum(finanziamento_pnrr, na.rm = TRUE),
    total_other = sum(finanziamento_totale_pubblico, na.rm = TRUE) - sum(finanziamento_pnrr, na.rm = TRUE)
  ) %>%
  mutate(
    perc_pnrr = (total_pnrr / (total_pnrr + total_other)) * 100,
    perc_other = (total_other / (total_pnrr + total_other)) * 100
  )

temporal_long <- temporal_evolution %>%
  select(period, perc_pnrr, perc_other) %>%
  pivot_longer(cols = c(perc_pnrr, perc_other), 
               names_to = "funding_type", 
               values_to = "percentage") %>%
  mutate(funding_type = ifelse(funding_type == "perc_pnrr", "PNRR Funds", "Other Public Funds"))

plot_temporal <- ggplot(temporal_long, aes(x = period, y = percentage, fill = funding_type)) +
  geom_col(position = "stack", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  labs(title = "Shift in Public Funding Composition",
       subtitle = "Comparing project structures Pre vs. Post PNRR Launch",
       x = "Project Start Period", y = "Percentage of Total Public Funding (%)", fill = "Funding Source") +
  scale_fill_manual(values = c("PNRR Funds" = "#1f77b4", "Other Public Funds" = "#7f7f7f")) +
  theme_minimal()


# ------------------------------------------------------------------------------
# 6. MUNICIPAL ANALYSES & PLOTS
# ------------------------------------------------------------------------------

# 6.1 Private Multiplier (Municipal)
muni_private_multiplier <- pnrr_summarized_nuts %>%
  group_by(denominazione) %>%
  summarise(
    total_pnrr = sum(finanziamento_pnrr, na.rm = TRUE),
    total_privato = sum(finanziamento_privato, na.rm = TRUE)
  ) %>%
  filter(total_pnrr > 100000) %>% 
  mutate(multiplier_ratio = total_privato / total_pnrr) %>%
  slice_max(multiplier_ratio, n = 15)

plot_muni_priv_mult <- ggplot(muni_private_multiplier, aes(x = reorder(denominazione, multiplier_ratio), y = multiplier_ratio)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 15 Municipalities: Private Funding Multiplier",
       subtitle = "Ratio of Private Funds to PNRR Funds (Filtered > €100k PNRR)",
       x = "Municipality", y = "Multiplier Ratio") +
  theme_minimal()

# 6.2 Local Fiscal Capacity (Municipal)
muni_local_capacity <- pnrr_summarized_nuts %>%
  mutate(
    local_funding = rowSums(across(c(finanziamento_comune, finanziamento_regione, finanziamento_provincia)), na.rm = TRUE),
    local_dependency_ratio = local_funding / finanziamento_totale
  ) %>%
  group_by(denominazione) %>%
  summarise(
    total_funding = sum(finanziamento_totale, na.rm = TRUE),
    avg_local_capacity = mean(local_dependency_ratio, na.rm = TRUE)
  ) %>%
  filter(total_funding > 100000) %>%
  slice_max(avg_local_capacity, n = 15)

plot_muni_local_cap <- ggplot(muni_local_capacity, aes(x = reorder(denominazione, avg_local_capacity), y = avg_local_capacity)) +
  geom_col(fill = "darkorange") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(title = "Top 15 Municipalities: Local Fiscal Capacity",
       subtitle = "Highest average % of local funding in total project cost",
       x = "Municipality", y = "Local Dependency Ratio") +
  theme_minimal()

# 6.3 Risk and Execution Feasibility (Municipal)
muni_funding_gaps <- pnrr_summarized_nuts %>%
  group_by(denominazione) %>%
  summarise(
    total_da_reperire = sum(finanziamento_da_reperire, na.rm = TRUE),
    total_finanziamento = sum(finanziamento_totale, na.rm = TRUE)
  ) %>%
  filter(total_finanziamento > 100000) %>%
  mutate(gap_percentage = (total_da_reperire / total_finanziamento) * 100) %>%
  slice_max(gap_percentage, n = 15)

plot_muni_gaps <- ggplot(muni_funding_gaps, aes(x = reorder(denominazione, gap_percentage), y = gap_percentage)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Top 15 Municipalities: Funding Gaps",
       subtitle = "Highest percentage of total funding still 'da reperire' (to be found)",
       x = "Municipality", y = "Gap Percentage (%)") +
  theme_minimal()

# 6.4 Scale and Fragmentation (Municipal)
muni_scale_fragmentation <- pnrr_summarized_nuts %>%
  group_by(denominazione) %>%
  summarise(
    total_funding = sum(finanziamento_totale, na.rm = TRUE),
    total_projects = sum(n_projects, na.rm = TRUE)
  ) %>%
  filter(total_projects >= 5) %>% 
  mutate(avg_funding_per_project = total_funding / total_projects)

muni_top_bottom_scale <- muni_scale_fragmentation %>%
  filter(!is.na(denominazione)) %>%
  arrange(avg_funding_per_project) %>%
  slice(c(1:10, (n() - 9):n())) %>%
  mutate(Type = ifelse(row_number() <= 10, "Highly Fragmented (Micro)", "Large Scale (Macro)"))

plot_muni_scale <- ggplot(muni_top_bottom_scale, aes(x = reorder(denominazione, avg_funding_per_project), y = avg_funding_per_project, fill = Type)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values = c("Highly Fragmented (Micro)" = "#E69F00", "Large Scale (Macro)" = "#56B4E9")) +
  labs(title = "Average Funding per Project by Municipality",
       subtitle = "10 most fragmented vs. 10 largest-scale (min. 5 projects)",
       x = "Municipality", y = "Average Funding (€)", fill = "Project Scale") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ------------------------------------------------------------------------------
# PRINT PLOTS (Run to visualize in RStudio)
# ------------------------------------------------------------------------------
plot_macro
plot_priv_mult
plot_local_cap
# plot_gaps
plot_scale
plot_temporal
plot_muni_priv_mult
plot_muni_local_cap
# plot_muni_gaps
plot_muni_scale
