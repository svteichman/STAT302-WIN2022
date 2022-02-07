library(tidyverse) 
library(mice)

# read in data
environ_df <- read_csv("files/projects/02_dimension_reduction/epi2020results20200604.csv")
country_df <- read_csv("files/projects/02_dimension_reduction/epi2020countryattributes20200604.csv")

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

# look at missing values by column 
colMeans(is.na(environ_df))

# remove all fisheries indicators because of high missingness 
environ_df <- environ_df %>%
  select(-FSH_cat, -FSS, -RMS, -FGT)

# impute other indicators using mice package
mice_res <- mice(environ_df)
clean_env_df <- complete(mice_res, 1)

# in country_df, only keep region and country
country_sm <- country_df %>%
  select(region, country, sids, ldc, lldc)

# add region to clean_env_df 
clean_env_df <- left_join(clean_env_df, country_sm, by = "country")

# combine with another data source 
gdp_df <- read_csv("files/projects/02_dimension_reduction/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3469501.csv",
                   skip = 3, col_names = TRUE)
gdp_df <- gdp_df %>%
  select(`Country Name`, `2020`)
names(gdp_df) <- c("country", "gdp")
clean_env_df <- left_join(clean_env_df, gdp_df)

tourism_df <- read_csv("files/projects/02_dimension_reduction/API_ST.INT.ARVL_DS2_en_csv_v2_3469485.csv",
                       skip = 3, col_names = TRUE) %>%
  select(`Country Name`, `2019`)
names(tourism_df) <- c("country", "tourism")
clean_env_df <- left_join(clean_env_df, tourism_df)


pca_df <- clean_env_df %>%
  select(-iso, -region, -contains("cat"), 
         -region, -sids, -ldc, -gdp, -lldc, -tourism)

write.csv(pca_df, "files/projects/02_dimension_reduction/pca_data.csv")

other_df <- clean_env_df %>% 
  select(iso, country, contains("cat"), region, gdp, ldc) 
names(other_df) <- str_remove(names(other_df), "_cat")

write.csv(other_df, "files/projects/02_dimension_reduction/additional_data.csv")

# -------- do PCA
pca_df <- clean_env_df %>%
  select(-iso, -country, -region, -contains("cat"), 
         -region, -sids, -ldc, -gdp, -lldc, -tourism)
pca_res <- princomp(pca_df, cor = TRUE)
plot_df <- data.frame(pc1 = pca_res$scores[,1],
                      pc2 = pca_res$scores[,2],
                      pc3 = pca_res$scores[,3],
                      epi_score = clean_env_df$EPI_cat,
                      region = clean_env_df$region,
                      log_gdp = log(clean_env_df$gdp),
                      island = clean_env_df$sids,
                      less_devel = clean_env_df$ldc,
                      land_lock = clean_env_df$lldc,
                      log_tourism = log(clean_env_df$tourism))
ggplot(plot_df, aes(x = pc1, y = pc2, color = region)) + 
  geom_point()
ggplot(plot_df, aes(x = pc1, y = epi_score, color = region)) + 
  geom_point()
ggplot(plot_df, aes(x = pc1, y = pc2, color = epi_score)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "yellow")
ggplot(plot_df, aes(x = pc1, y = pc2, color = log_gdp)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "yellow")
ggplot(plot_df, aes(x = pc1, y = pc2, color = as.factor(island))) + 
  geom_point()
ggplot(plot_df, aes(x = pc1, y = pc2, color = as.factor(less_devel))) + 
  geom_point()
ggplot(plot_df, aes(x = pc1, y = pc2, color = as.factor(land_lock))) + 
  geom_point()
ggplot(plot_df, aes(x = pc2, y = pc3, color = region)) + 
  geom_point()
ggplot(plot_df, aes(x = pc2, y = pc3, color = log_gdp)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "yellow")
ggplot(plot_df, aes(x = pc1, y = pc2, color = log_tourism)) + 
  geom_point() + 
  scale_color_gradient(low = "blue", high = "yellow")

plot(plot_df$pc2, clean_env_df$BDH_cat)
cor(plot_df$pc2, clean_env_df$BDH_cat)


mod1 <- lm(plot_df$log_gdp ~ plot_df$epi_score)
summary(mod1)

mod2 <- lm(plot_df$log_gdp ~ plot_df$pc1)
summary(mod2)

biplot(pca_res, col = c("white", "black"))
loadings_df <- data.frame(pc1_load = pca_res$loadings[,1],
                          pc2_load = pca_res$loadings[,2],
                          var = rownames(pca_res$loadings),
                          cat = c("AIR","AIR","AIR","H2O","H2O",
                                  "HMT","WMG","BDH","BDH","BDH",
                                  "BDH","BDH","BDH","BDH","ECS",
                                  "ECS","ECS","CCH","CCH","CCH",
                                  "CCH","CCH","CCH","CCH","CCH",
                                  "APE","APE","AGR","WRS"))
ggplot(loadings_df, aes(x = pc1_load, y = pc2_load, label = var,
                        color = cat)) + 
  geom_text() 


