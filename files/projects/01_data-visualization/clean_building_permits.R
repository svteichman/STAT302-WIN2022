library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(sf)
# load data
# downloaded 10/15/2021 from https://data.seattle.gov/Permitting/Building-Permits/76t5-zqzr/data
building_permits <- read_csv("files/projects/01_data-visualization/Building_Permits.csv")

# remove data without issued dates
building_permits <- building_permits %>%
  filter(!is.na(IssuedDate))

# convert dates into a processable format
issued_date <- as.Date(
  as.POSIXlt(building_permits$IssuedDate,
             format = "%m/%d/%Y %H:%M",
             tz=Sys.timezone())
  )

# pull out the months and years
building_permits$IssuedYear <- year(issued_date) + 2000
building_permits$IssuedMth <- month(issued_date) 

# restrict to last five years and remove unnecessary variables
building_permits <- building_permits %>%
  filter(IssuedYear >= 2017 & IssuedYear <= 2021) %>%
  select(-AppliedDate, -ExpiresDate, -CompletedDate, -RelatedMup, -Link, -Location1) 

# remove rows with missing location
building_permits <- building_permits %>%
  filter(!is.na(Longitude))

building_permits_sf <- st_as_sf(building_permits,
                                coords = c("Longitude", "Latitude"),
                                crs = 4326)

# load some geographic boundaries
# downloaded 10/15/2021 from https://data-seattlecitygis.opendata.arcgis.com/datasets/community-reporting-areas
# and https://data-seattlecitygis.opendata.arcgis.com/datasets/council-districts
community_reporting_areas <- 
  st_read("files/projects/01_data-visualization/Community_Reporting_Areas/") %>%
  select(GEN_ALIAS, NEIGHDIST) %>%
  rename(CommunityReportingArea = GEN_ALIAS,
         NeighborhoodDistrict = NEIGHDIST)
council_districts <- st_read("files/projects/01_data-visualization/Council_Districts/") %>%
  select(C_DISTRICT) %>%
  rename(CouncilDistrict = C_DISTRICT)

# match permit to council districts and community reporting areas
building_permits_final <- building_permits_sf %>% 
  st_join(council_districts) %>%
  st_join(community_reporting_areas) %>%
  st_set_geometry(NULL)

# save cleaned data
write_csv(building_permits_final, "files/projects/01_data-visualization/Building_Permits_Clean.csv")
                                