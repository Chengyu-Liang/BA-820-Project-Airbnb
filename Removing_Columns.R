library(readr)
library(tidyverse)
library(dummies)
library(fastDummies)
library(factoextra)
library(skimr)
library(corrplot)
library(purrr)
library(cluster)

reviews <- read_csv("reviews.csv")

listings <- read_csv("listings.csv")

listings2 <- listings %>% 
  select(-experiences_offered,
         -scrape_id,
         -last_scraped,
         -host_name, 
         -host_is_superhost, 
         -host_total_listings_count,
         -host_verifications,
         -host_has_profile_pic,
         -host_identity_verified,
         -neighbourhood,
         -neighbourhood_group_cleansed,
         -city,
         -state,
         -market,
         -zipcode,
         -smart_location,
         -country,
         -country_code,
         -is_location_exact,
         -bed_type,
         -weekly_price,
         -monthly_price,
         -calendar_updated,
         -minimum_nights,
         -maximum_nights,
         -has_availability,
         -availability_30,
         -availability_60,
         -availability_90,
         -availability_365,
         -calendar_last_scraped,
         -requires_license,
         -license,
         -jurisdiction_names,
         -instant_bookable,
         -require_guest_profile_picture,
         -require_guest_phone_verification,
         -calculated_host_listings_count,
         -listing_url,
         -thumbnail_url,
         -medium_url,
         -picture_url,
         -xl_picture_url,
         -host_url,
         -host_thumbnail_url,
         -host_picture_url)

saveRDS(listings2,file="Airbnb_listings.rds")

airbnb_listings <- readRDS("Airbnb_listings.rds")

airbnb_listings$price = as.numeric(gsub("[\\$,]", "", airbnb_listings$price))

airbnb_listings$security_deposit = as.numeric(gsub("[\\$,]", "", airbnb_listings$security_deposit))

airbnb_listings$cleaning_fee = as.numeric(gsub("[\\$,]", "", airbnb_listings$cleaning_fee))

airbnb_listings$extra_people = as.numeric(gsub("[\\$,]", "", airbnb_listings$extra_people))

listings_num = airbnb_listings %>% 
  select_if(is.numeric) %>% 
  select(-host_id, -id)

listings_num[is.na(listings_num)] = 0

summary(listings_num)



airbnb_dummy <- airbnb_listings %>% 
  select(room_type, property_type, cancellation_policy, host_response_time)

airbnb_dummy[is.na(airbnb_dummy)] = 0

airbnb_dummy <-dummy_cols(airbnb_dummy, remove_first_dummy = T) %>% 
  select(-1:-4)

listings_uml <- cbind(scale(listings_num), airbnb_dummy)

listings_uml_cor <- cor(listings_uml)

corrplot(listings_uml_cor, 
         method = "color", 
         diag=F, 
         addCoef.col = "black")

p = prcomp(listings_uml, center=T, scale = T)

fviz_screeplot(p, addlabels=T, ylim =c(0,100))

get_eigenvalue(p)

fviz_pca_contrib(p, choice="var")
fviz_pca_contrib(p, choice="var", axes = 2)

s_pcs = predict(p, newdata = listings_uml)

s_pcs = s_pcs[, 1:30]
