# scrape data from https://www.curbed.com/article/nyc-businesses-closed-2020-pandemic.html

library(rvest) # for web scraping
library(dplyr) # for data tidying
library(stringr) # for working with strings
library(tidytext) # analyze text data!
library(tidyr) # for data organization

infatuation_url <- "https://www.theinfatuation.com/new-york/features/nyc-restaurant-closings"

infatuation <- infatuation_url %>%
  read_html() %>%
  html_nodes("#main li , .post__content__section-header span") %>%
  html_text() %>%
  tibble(name = .) %>%
  mutate(
    neighborhood = case_when(
      grepl("([A-Z]){4}", name) ~ str_to_sentence(name)
    )
  ) %>%
  fill(neighborhood) %>%
  filter(tolower(name) != tolower(neighborhood))

