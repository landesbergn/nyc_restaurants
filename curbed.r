# scrape data from https://www.curbed.com/article/nyc-businesses-closed-2020-pandemic.html

library(rvest) # for web scraping
library(dplyr) # for data tidying
library(stringr) # for working with strings
library(tidytext) # analyze text data!
library(tidyr) # for data organization
library(purrr) # for iteration
library(googlesheets4) # for writing to googlesheets
library(googleway)
library(ggmap)
library(RCurl)

curbed_url <- "https://www.curbed.com/article/nyc-businesses-closed-2020-pandemic.html"

curbed_raw <- curbed_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "clay-paragraph", " " ))]') %>%
  html_text() %>%
  trimws() %>%
  tibble(listing = .) %>%
  slice(13:512)

curbed_clean <- curbed_raw %>%
  # get rid of stuff in brackets and parenthases
  mutate(
    listing =  gsub("\\[.*?\\]", "", listing),
    listing = gsub("\\(.*?\\)", "", listing)
  ) %>%
  # split into components. this throws some warnings because some rows
  # have > 3 components (row 318 - BarBaconBar, Union Square, Hell’s Kitchen, 2014)
  # have < 3 components (row 12 - Nat Sherman TownhouseTobacco Shop Midtown, 1930)
  separate(listing, c("name", "neighborhood", "year_opened"), sep = ",") %>%
  # remove whitespace
  mutate_if(
    is.character, str_trim
  ) %>%
  mutate(
    # remove non-numeric characters from year_opened data
    year_opened = trimws(str_remove_all(year_opened, "[^0-9]")),
    # fix > 3 component cases
    year_opened = case_when(
      str_detect(neighborhood, "^[0-9]+$") ~ neighborhood,
      TRUE ~ year_opened
    ),
    # add the years back in, manually
    year_opened = case_when(
      grepl("BarBacon", name) ~ "2014",
      grepl("Ice & Vice", name) ~ "2015",
      grepl("Juicy Spot Cafe", name) ~ "2015",
      TRUE ~ year_opened
    ),
    # fix < 3 component cases
    neighborhood = case_when(
      neighborhood == year_opened ~ word(name, -1),
      TRUE ~ neighborhood
    ),
    name = case_when(
      grepl("TownhouseTobacco Shop Midtown", name) ~ "Nat Sherman TownhouseTobacco Shop",
      grepl("Diner. Chelsea", name) ~ "The Rail Line DinerDiner",
      TRUE ~ name
    )
  )

curbed_final <- curbed_clean %>%
  mutate(
    category = case_when(
      grepl("Restaurant", name) ~ "Restaurant",
      grepl("Vegetarian Restaurant", name) ~ "Vegetarian Restaurant",
      grepl("Department Store", name) ~ "Department Store",
      grepl("Hookah Bar", name) ~ "Hookah Bar",
      grepl("Bar", name) ~ "Bar",
      grepl("Coffee Shop", name) ~ "Coffee Shop",
      grepl("Salon", name) ~ "Salon",
      grepl("Retailer", name) ~ "Retailer",
      grepl("Corner Store", name) ~ "Corner Store",
      grepl("Hotel", name) ~ "Hotel",
      grepl("Tobacco Shop", name) ~ "Hotel",
      grepl("Nightclub", name) ~ "Nightclub",
      grepl("Deli", name) ~ "Deli",
      grepl("Delicatessen", name) ~ "Delicatessen",
      grepl("Dry Cleaner", name) ~ "Dry Cleaner",
      grepl("Venue", name) ~ "Venue",
      grepl("School", name) ~ "School",
      grepl("Pizzeria", name) ~ "Pizzeria",
      grepl("Antiques Store", name) ~ "Antiques Store",
      grepl("Fitness Studio", name) ~ "Fitness Studio",
      grepl("Pizzeria", name) ~ "Pizzeria",
      grepl("Bakery", name) ~ "Bakery",
      grepl("Pub", name) ~ "Pub",
      grepl("Shop", name) ~ "Shop",
      grepl("Clothing Shop", name) ~ "Clothing Shop",
      grepl("Vintage Store", name) ~ "Vintage Store",
      grepl("Rehearsal Space", name) ~ "Rehearsal Space",
      grepl("Shoe Repair", name) ~ "Shoe Repair",
      grepl("Art Gallery", name) ~ "Art Gallery",
      grepl("Diner", name) ~ "Diner",
      grepl("Shipping Service", name) ~ "Shipping Service",
      grepl("Café", name) ~ "Café",
      grepl("Speakeasy", name) ~ "Speakeasy",
      grepl("Clothing Store", name) ~ "Clothing Store",
      grepl("Theater", name) ~ "Theater",
      grepl("Shoe Store", name) ~ "Shoe Store",
      grepl("Event Space", name) ~ "Event Space",
      grepl("Plumber", name) ~ "Plumber",
      grepl("Lighting Store", name) ~ "Lighting Store",
      grepl("Jazz Club", name) ~ "Jazz Club",
      grepl("Coffee", name) ~ "Coffee Shop",
      grepl("Grocery Store", name) ~ "Grocery Store",
      grepl("Comedy Club", name) ~ "Comedy Club",
      grepl("Meditation Center", name) ~ "Meditation Center",
      grepl("Specialty Store", name) ~ "Specialty Store",
      grepl("Hookah Lounge", name) ~ "Hookah Lounge",
      grepl("Plant Store", name) ~ "Plant Store",
      grepl("Gym", name) ~ "Gym",
      grepl("Studio", name) ~ "Studio",
      grepl("Martial-Arts", name) ~ "Martial-Arts",
      grepl("Jewelry Store", name) ~ "Jewelry Store",
      grepl("Arcade", name) ~ "Arcade",
      grepl("Yoga", name) ~ "Yoga Studio",
      grepl("Laundry", name) ~ "Laundry",
      grepl("Spa", name) ~ "Spa",
      grepl("Fitness", name) ~ "Fitness",
      grepl("Electronics Repair", name) ~ "Electronics Repair",
      grepl("Design Store", name) ~ "Design Store",
      grepl("Herbalist", name) ~ "Herbalist",
      grepl("Museum", name) ~ "Museum",
      grepl("Maternity Classes", name) ~ "Maternity Classes",
      grepl("Beer Hall", name) ~ "Beer Hall",
      grepl("Tattoo Parlor", name) ~ "Tattoo Parlor",
      grepl("Health-Food Store", name) ~ "Health-Food Store",
      grepl("Grooming", name) ~ "Grooming",
      grepl("Bodega", name) ~ "Bodega",
      grepl("Tea House", name) ~ "Tea House",
      grepl("Bookstore", name) ~ "Bookstore",
      grepl("General Store", name) ~ "General Store",
      grepl("Dojo", name) ~ "Dojo",
      grepl("Minor League Baseball Team", name) ~ "Minor League Baseball Team",
      grepl("Natural-Wellness Store", name) ~ "Natural-Wellness Store",
      grepl("Secondhand Store", name) ~ "Secondhand Store",
      TRUE ~ "NA"
    )
  ) %>%
  filter(
    category %in% c("Restaurant", "Bar", "Café", "Diner",
                    "Beer Hall", "Coffee Shop", "Deli", "Pub",
                    "Bakery", "Pizzaria", "Delicatessen")
    ) %>%
  mutate(
    name = str_remove_all(name, category)
  )

url_google_place_search <- function(search_query_url, key_url) {
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  # construct search request for place id
  url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                  "json", "?input=", search_query_url,
                                  "&inputtype=textquery","&fields=place_id",
                                  "&key=", "AIzaSyBzXOTbuKPa-jrU5rtgG5XrhnSmbWn-V5Y")
  return(url_place_search_call)
}

# curbed_mini <- curbed_final %>% head()

# curbed_places <-
#   curbed_mini %>%
#   rowwise() %>%
#   mutate(
#     place_data <- googleway::google_places(paste(name, neighborhood))$results$name[1]
#   )
