# scrape data from https://www.curbed.com/article/nyc-businesses-closed-2020-pandemic.html

library(rvest) # for web scraping
library(dplyr) # for data tidying
library(stringr) # for working with strings
library(tidytext) # analyze text data!
library(ggplot2) # make nice plots
library(scales) # make plots even nicer
library(tidyr) # for data organization
library(purrr) # for iteration

curbed_url <- "https://www.curbed.com/article/nyc-businesses-closed-2020-pandemic.html"

curbed_raw <- curbed_url %>%
  read_html() %>%
  # html_nodes(".clay-paragraph:nth-child(15) :nth-child(16)") %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "clay-paragraph", " " ))]') %>%
  html_text() %>%
  trimws() %>%
  tibble(listing = .) %>%
  slice(13:512)

curbed_raw %>%
  separate(listing, c("name", "neighborhood", "year"), sep = ",") %>%
  mutate(
      year = str_remove(year, "[^0-9]")
  ) %>%
  distinct(year)

