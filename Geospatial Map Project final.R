library(readxl)
library(ggplot2)
library(dplyr)
library(ggmap)
library(sf)
library(geosphere)  

# path to Excel file
file_path <- "/Users/jessodell/Documents/R/MAINA_health_centers_SSA.xlsx"

# Read Excel file
health_data <- read_excel(file_path)

# Filter data for the Lake Zone regions + exclude dispensaries
lake_zone_regions <- c("Kagera", "Kigoma", "Mara", "Mwanza", "Shinyanga", "Tabora", "Geita", "Simiyu")
lake_zone_data <- health_data %>%
  filter(Admin1 %in% lake_zone_regions & `Facility type` != "Dispensary")

# Convert data to sf object
lake_zone_sf <- st_as_sf(lake_zone_data, coords = c("Long", "Lat"), crs = 4326)

# Bugando Medical Center (BMC) coordinates
bmc_coords <- data.frame(
  name = "Bugando Medical Center",
  Latitude = -2.5165,
  Longitude = 32.8956
)
bmc_sf <- st_as_sf(bmc_coords, coords = c("Longitude", "Latitude"), crs = 4326)

# Calculate distances (straight-line distance)
lake_zone_data <- lake_zone_data %>%
  rowwise() %>%
  mutate(distance_to_BMC_km = distHaversine(c(Long, Lat), c(bmc_coords$Longitude, bmc_coords$Latitude)) / 1000)

# Load shapefile
regions_sf <- st_read("/Users/jessodell/Documents/regions/")

# Filter data for the Lake Zone regions 
regions_sf_filtered <- regions_sf %>%
  filter(Region_Nam %in% c("Kagera", "Kigoma", "Mara", "Mwanza", "Shinyanga", "Tabora", "Geita", "Simiyu"))
print(head(regions_sf_filtered))

# Load population data Excel
population_data <- read_excel("/Users/jessodell/Documents/R/Copy of Tanzania.xlsx")
print(head(population_data))

#Filter population data year 2024 + region
filtered_population_data <- population_data %>%
  filter(YR == 2024 & AREA_NAME %in% c("KAGERA", "KIGOMA", "MARA", "MWANZA", "SHINYANGA", "TABORA", "GEITA", "SIMIYU")) %>%
  select(Region = AREA_NAME, POP_DENS)

print(head(filtered_population_data))

#Region_Nam in regions_sf is in uppercase for merge 
regions_sf_filtered <- regions_sf_filtered %>%
  mutate(Region_Nam = toupper(trimws(Region_Nam)))

print(head(regions_sf_filtered))
# Merge filtered population data with regions_sf based on the Region names
regions_sf_filtered <- regions_sf_filtered %>% 
  left_join(filtered_population_data, by = c("Region_Nam" = "Region"))

print(head(regions_sf_filtered))

# map with regions shaded by population
ggplot() +
  geom_sf(data = regions_sf_filtered, aes(fill = POP_DENS), color = "black", size = 0.2) +
  geom_sf(data = lake_zone_sf, aes(color = `Facility type`), size = 0.5) +
  geom_point(data = bmc_coords, aes(x = Longitude, y = Latitude), color = "purple", size = 2, shape = 18) +
  geom_sf_text(data = regions_sf_filtered, aes(label = Region_Nam), size = 3, color = "black") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Health Care Facilities and Population in Lake Zone, Tanzania", fill = "Population")

# Save R code
saveRDS(list(libraries = c("readxl", "ggplot2", "dplyr", "ggmap", "sf", "geosphere"),
             data_path = file_path,
             lake_zone_regions = lake_zone_regions,
             bmc_coords = bmc_coords,
             code = " "),
        file = "Lake_Zone_Map_Final.RDS")
