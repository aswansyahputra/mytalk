# Load packages -----------------------------------------------------------
library(tidyverse)
library(rvest)
library(janitor)

# Define function to scrape OP chars --------------------------------------
scrape_op_char <- function(char_url) {
  char_url %>% 
    read_html() %>% 
    {
      tibble(
        label = html_nodes(., "h3.pi-data-label") %>%
          html_text(),
        value = html_nodes(.,"div.pi-data-value") %>% 
          html_text()
      )
    } %>% 
    mutate(
      label = make_clean_names(label),
      label = recode(label,
                     japanese_name_2 = "devilfruit_en",
                     english_name	= "devilfruit_jp",
                     meaning = "devilfruit_meaning",
                     type = "devilfruit_type",
                     japanese_name_3 = "devilfruit_en_2",
                     english_name_2	= "devilfruit_jp_2",
                     meaning_2 = "devilfruit_meaning_2",
                     type_2 = "devilfruit_type_2"),
      value = str_remove_all(value, "\\[\\d+\\]")
    )
}


# List all OP characters ---------------------------------------------------

op_chars_list <- 
  "https://onepiece.fandom.com/wiki/List_of_Canon_Characters" %>% 
  read_html() %>% 
  html_node("table.wikitable:nth-child(6)") %>% 
  html_table(fill = TRUE, trim = TRUE) %>% 
  as_tibble(.name_repair = make_clean_names) %>% 
  select(-x, -x_2)
op_chars_list

# Get url for every OP char -----------------------------------------------

op_char_urls_raw <- 
  "https://onepiece.fandom.com/wiki/List_of_Canon_Characters" %>% 
  read_html() %>% 
  html_nodes("table.wikitable:nth-child(6) > tr > td > a") %>% 
  {
    tibble(
      name = html_attr(.,"title"),
      url = str_c("https://onepiece.fandom.com", html_attr(.,"href"))
    )
  }

op_char_urls <-
  op_char_urls_raw %>% 
  semi_join(op_chars_list) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  deframe()
op_char_urls


# Scrape all op characters ------------------------------------------------

op_chars_raw <-
  map_dfr(op_char_urls, scrape_op_char, .id = "name")

# Prepare tidy dataset ----------------------------------------------------

op_characters <-
  op_chars_raw %>% 
  mutate(label = factor(label, levels = unique(label))) %>% 
  spread(label, value) %>% 
  mutate_all(~na_if(.x, "N/A")) %>% 
  unite("affiliations", matches("aff"), sep = ";") %>% 
  mutate(affiliations = str_remove_all(affiliations, "(?:NA;NA|;NA)")) %>% 
  mutate_all(~na_if(.x, "")) %>% 
  inner_join(op_chars_list) %>% 
  distinct(name, .keep_all = TRUE) %>% 
  mutate(affiliations = str_remove_all(affiliations, "\\(.+\\)")) %>% 
  separate_rows(affiliations, sep = "(?:;|,)") %>% 
  mutate_if(is.character, ~str_trim(.x)) %>% 
  select(name, debut_manga = chapter, debut_anime = episode, year, everything(), -debut) %>% 
  select_if(~mean(is.na(.x)) < 0.7)

op_characters
