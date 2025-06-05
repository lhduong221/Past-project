# Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(maps)
library(sf)
library(gridExtra)
library(ggthemes)
library(readxl)


# Main Data import ----
getwd()
data <- read.csv("data/e050.csv")
summary(data)
str(data)




# Prepare Main data ----  
## Extract Approval Year
data$Approval.Year <- format(as.Date(data$EO50.Approval.Date, format = "%m/%d/%Y"), "%Y")
## Get NY state data
data_ny <- data[data$Business.State == "New York",]
summary(data_ny)
str(data_ny)
## NYC data 
data_nyc <- data_ny[data_ny$Borough != "",]
data_nyc$EO50.Approval.Date <- mdy_hms(data_nyc$EO50.Approval.Date)
str(data_nyc)
unique(data_nyc$Borough)


# NYC map data by Boroughs & centroid ----
## New York City boroughs border data
nybb <- read.csv("data/nybb.csv")
## Convert map data into sf objects and add CRS (cordinate reference system)
nybb_df <- st_as_sf(nybb, wkt = "the_geom", crs = 4326)
## Get centroid coordinates
nybb_df$centroid <- st_centroid(nybb_df$the_geom)
nybb_df$centroid_coords <- st_coordinates(nybb_df$centroid)


# DATA preparing for PLOT 1 ----
## Number of approvals in each Borough in NY city by Year
bor_counts_by_year <- data_nyc |>
  group_by(Borough, Approval.Year) |>
  summarise(count = n()) |>
  pivot_wider(names_from = "Approval.Year", names_prefix = "Year", values_from = "count")
bor_counts_by_year[is.na(bor_counts_by_year)] <- 0 
bor_counts_by_year$Borough <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
## Merge nybb_df with bor_counts_by_year to create data for plot 1
data_plot_1 <- merge(
  nybb_df, bor_counts_by_year, 
  by.x = "BoroName",
  by.y = "Borough"
)


# PLOT 1:New York City EO50 Approval by Borough and by Year  ----
plot_1 <- ggplot(data_plot_1) +
  geom_sf(fill = "white", color = "black") +
  geom_segment(
    aes(
      x = centroid_coords[, 1] - 0.015, xend = centroid_coords[, 1] - 0.015,
      y = centroid_coords[, 2], yend = centroid_coords[, 2] + Year2021 / 1e3,
      color = "2021"
    ),
    size = 3
  ) +
  geom_segment(
    aes(
      x = centroid_coords[, 1] - 0.005, xend = centroid_coords[, 1] - 0.005,
      y = centroid_coords[, 2], yend = centroid_coords[, 2] + Year2022 / 1e3,
      color = "2022"
    ),
    size = 3
  ) +
  geom_segment(
    aes(
      x = centroid_coords[, 1] + 0.005, xend = centroid_coords[, 1] + 0.005,
      y = centroid_coords[, 2], yend = centroid_coords[, 2] + Year2023 / 1e3,
      color = "2023"
    ),
    size = 3
  ) +
  geom_segment(
    aes(
      x = centroid_coords[, 1] + 0.015, xend = centroid_coords[, 1] + 0.015,
      y = centroid_coords[, 2], yend = centroid_coords[, 2] + Year2024 / 1e3,
      color = "2024"
    ),
    size = 3
  ) +
  geom_text(
    aes(x = centroid_coords[, 1], y = centroid_coords[, 2] - 0.005, label = BoroName),
    size = 4, color = "black", fontface = "bold"
  ) +
  labs(
    title = "New York City EO50 Approval",
    subtitle = "by Borough and by Year",
    x = "Latitude", y = "Longtitude"
  ) +
  scale_color_manual(
    values = c("2021" = "#234E6D", "2022" = "#F8AA01", "2023" = "#F86814", "2024" = "#B99B33"),
    name = "Approval Year",
    guide = guide_legend(title.position = "top")
  ) +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom") 


# NYC map data by NTA ----
## NTA data
neigh_map <- read.csv("data/2020_Neighborhood_Tabulation_Areas__NTAs__20241201.csv")
## Convert map data into sf objects and add CRS (cordinate reference system)
neigh_map <- st_as_sf(neigh_map, wkt = "the_geom", crs = 4326)


# DATA preparing for PLOT 2 ----
## Cleaning EO50 data: Remove rows with empty NTA value (46 values)
data_nyc_2 <- data_nyc[data_nyc$Neighborhood.Tabulation.Area..NTA...2020. != "",]
## Count approvals by NTA
data_nta <- data_nyc_2 |>
  group_by(Neighborhood.Tabulation.Area..NTA...2020.) |>
  summarise(count =n())
## Merge data into neighbor map
data_plot_2.1 <- merge(
  neigh_map, data_nta,
  by.x = "NTA2020",
  by.y = "Neighborhood.Tabulation.Area..NTA...2020.",
  all = TRUE
)
#merged_df_nta$count[is.na(merged_df_nta$count)] <- 0
## Convert data into sf objects and add CRS (cordinate reference system)
#data_plot_2.1 <- st_as_sf(merged_df_nta, wkt = "the_geom", crs = 4326)
#print(st_geometry_type(data_plot2))

## Population Distribution
### Population Distribution data
pop <- read_excel("data/Dem_1822_NTA.xlsx")
pop_by_nta <- pop |>
  group_by(GeoID) |>
  summarise("Working_population" = Pop_1E - PopU181E - Pop65pl1E)
### Mapping data
data_plot_2.2 <- merge(neigh_map, pop_by_nta,
                       by.x = "NTA2020",
                       by.y = "GeoID",
                       all = TRUE)
data_plot_2.2$Working_population[data_plot_2.2$Working_population == 0] <- NA




# PLOT 2: EO50 Approvals Diffusion vs. Population Distribution ----
## PLOT 2.1: EO50 Approvals Diffusion
plot_2.1 <- ggplot(data_plot_2.1) +
  geom_sf(aes(fill = count), color = "gray20") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_fivethirtyeight() +
  labs(
    title = "New York EO50 Approvals Diffusion",
    subtitle = "by Neighborhood Tabulation Area",
    fill = "Number of Approvals",
    x = "Latitude",
    y = "Longtitude"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.2, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom"
  ) +
  coord_sf()


## PLOT 2.2
plot_2.2 <- ggplot(data_plot_2.2) +
  geom_sf(aes(fill = Working_population), color = "gray20") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_fivethirtyeight(base_family = "sans") +
  labs(
    title = "New York Population",
    subtitle = "by Neighborhood Tabulation Area",
    fill = "Population",
    x = "Latitude",
    y = "Longtitude"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.2, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom"
  )
  coord_sf()

plot_2 <-grid.arrange(plot_2.1, plot_2.2, ncol = 2)
plot_1
plot_2



  
