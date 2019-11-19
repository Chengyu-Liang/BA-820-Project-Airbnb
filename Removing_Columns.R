library(readr)
library(tidyverse)

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










