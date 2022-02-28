library(tidyverse) 
library(mice)

# read in data
environ_df <- read_csv("files/projects/02_dimension_reduction/original_data/epi2020results20200604.csv")
country_df <- read_csv("files/projects/02_dimension_reduction/original_data/epi2020countryattributes20200604.csv")

# filter data to remove rankings and changes 
environ_df <- environ_df %>% 
  select(-contains("change"), -contains("rnk"), -code)

# remove ".new" from column names 
names(environ_df) <- str_remove(names(environ_df), ".new")

# add "cat" to end to category indicators 
cat_ind <- c("EPI", "HLT", "ECO", "AIR", "H2O", "HMT", "WMG", "BDH", 
             "ECS", "FSH", "CCH", "APE", "AGR", "WRS")
names(environ_df) <- ifelse(names(environ_df) %in% cat_ind, 
                            str_c(names(environ_df), "_cat"), 
                            names(environ_df))

# look at missing values by column (variable)
colMeans(is.na(environ_df)) 
# look at missig values by row (individuals)
rowMeans(is.na(environ_df))

# here I would decide to remove any variables or observations with too much missingness 

# remove all fisheries indicators because of high missingness 
environ_df <- environ_df %>%
  select(-FSH_cat, -FSS, -RMS, -FGT)

# impute other indicators using mice package
mice_res <- mice(environ_df)
# grab the 1st version of the imputed dataset (we could also use the 2nd, 3rd, etc.)
clean_env_df <- complete(mice_res, 1)

# in country_df, only keep region and country
country_sm <- country_df %>%
  select(region, country, ldc, emmrkt)

# add region to clean_env_df 
clean_env_df <- left_join(clean_env_df, country_sm, by = "country")

# combine with another data source 
gdp_df <- read_csv("files/projects/02_dimension_reduction/original_data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3469501.csv",
                   skip = 3, col_names = TRUE)
gdp_df <- gdp_df %>%
  select(`Country Name`, `2020`)
names(gdp_df) <- c("country", "gdp")
clean_env_df <- left_join(clean_env_df, gdp_df)

pca_df <- clean_env_df %>%
  select(-iso, -region, -contains("cat"), 
         -region, -ldc, -emmrkt, -gdp)

write_csv(pca_df, "files/projects/02_dimension_reduction/pca_data.csv")

other_df <- clean_env_df %>% 
  select(country, region, ldc, emmrkt, gdp, EPI_cat) 
names(other_df) <- str_remove(names(other_df), "_cat")

write_csv(other_df, "files/projects/02_dimension_reduction/additional_data.csv")



