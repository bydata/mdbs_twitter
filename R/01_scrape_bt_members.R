library(tidyverse)
library(rvest)
library(tictoc)

# a request is sent to this URL when switching to list view 
url_base <- "https://www.bundestag.de"
url_biolist <- "https://www.bundestag.de/ajax/filterlist/de/abgeordnete/biografien/525246-525246?limit=9999&view=BTBiographyList"

# download the page and extract <a> elements which contain name, party, and bio link for each member
page <- read_html(url_biolist)
biolinks <- html_nodes(page, css = "li > a")

mdb_biolinks <- list(
  name = html_attr(biolinks, "title"),
  party = html_nodes(biolinks, css = ".bt-teaser-person-text p") %>% 
    html_text() %>% 
    str_remove("\\\n ") %>% 
    str_trim(),
  biolink = html_attr(biolinks, "href")
)

as_tibble(mdb_biolinks)


#' Downloads and parses the biography of a given MP
#' 
#' @description Download and parse the biography of a given MP from bundestag.de.
#' @param name Name of the MP
#' @param party Party affiliation of the MP
#' @param biolink Relative link to the MP's biography on bundestag.de
#' @param sleep Time in seconds to halt the function execution. 
#'   This is useful to avoid being blocked by the content management system for causing too many requests.
#'   Default is 0
scrape_bio <- function(name, party, biolink, sleep = 0) {
  # gcache_url <- "https://webcache.googleusercontent.com/search?q=cache:"
  # url <- str_c(gcache_url, url_base, biolink)
  url <- str_c(url_base, biolink)
  page <- read_html(url)
  linklist <- html_node(page, css = "h5 + ul.bt-linkliste")
  
  mdb <- list(
    name = name,
    party = party,
    beruf = html_node(page, css = ".bt-biografie-beruf") %>% 
      html_text() %>% 
      str_remove_all("\\\n") %>% 
      str_trim(),
    bio = html_nodes(page, css = "#ptv1 > .bt-collapse-padding-bottom p") %>% 
      html_text()  %>% 
      str_remove_all("\\\n") %>% 
      str_trim(),
    typ = html_node(page, css = "div.bt-biografie.row > div:nth-child(2) > div:nth-child(1) > div > h4") %>% 
      html_text(),
    bundesland = html_node(page, css = "div.col-xs-12.col-sm-6.bt-standard-content > h5") %>% 
      html_text(),
    wahlkreis = html_node(page, css = "div.col-xs-12.col-sm-6.bt-standard-content > ul > li > a") %>% 
      html_attr("title"),
    linklist = list(
      "website" = html_node(linklist, css = "li:nth-child(1) > a") %>% 
        html_attr("href"),
      "facebook" = html_node(linklist, css = "li > a[title='Facebook']") %>% 
        html_attr("href"),
      "twitter" = html_node(linklist, css = "li > a[title='Twitter']") %>% 
        html_attr("href"),
      "instagram" = html_node(linklist, css = "li > a[title='Instagram']") %>% 
        html_attr("href")
    )
  )
  mdb[["geboren"]] <- str_match(str_c(mdb[["bio"]], 
                                      collapse = " "), 
                                "Geboren am (\\d{1,2}\\. [a-zA-Z]+ \\d{4})(?:\\s|&nbsp;)in (.+?)[;.]")[, 2:3]
  mdb[["verstorben"]] <- str_match(str_c(mdb[["bio"]], 
                                         collapse = " "), 
                                   "ist am (\\d{1,2}\\. [a-zA-Z]+ \\d{4}) verstorben")[, 2]
  
  # short break to make sure we're not considered Russian hackers
  Sys.sleep(sleep)
  
  mdb  
}

# create safely and possibly version of the scraping function
scrape_bio_possibly <- possibly(scrape_bio, otherwise = NULL)
scrape_bio_safely <- safely(scrape_bio, otherwise = NULL)

# mdb_bios <- pmap(pluck(mdb_biolinks, 1), scrape_bio_possibly)
# mdb_bios <- pmap(pluck(mdb_biolinks, 1), scrape_bio_safely)


# scrape the MP bios from bundestag.de
tic()
mdb_bios <- pmap(mdb_biolinks, scrape_bio_safely, sleep = 50/1000)
toc()

mdb_bios <- mdb_bios %>% set_names(mdb_biolinks$name)

# check how many bios are missing
table(map_dbl(pluck(mdb_bios, "result"), length))

# which MPs are missing?
mdb_bios %>% keep(~length(.x$result) == 0)

current_time_str <- Sys.time() %>% format("%Y%m%d-%H%M%S")
filepath <- file.path("output", str_c("mdb_bios_", current_time_str, ".RData"))
write_rds(mdb_bios, filepath)


## Cleanup datasets ==============================================================

results <- mdb_bios %>% 
  discard(~length(.x$result) == 0) %>% 
  pluck("result")


foo <- tibble(
  name = unlist(pluck(results, "name")),
  party = unlist(pluck(results, "party")),
  linklist = pluck(results, "linklist"),
  bio = pluck(results, "bio"),
  geboren = pluck(results, "geboren")
)

foo %>% 
  unnest_wider(linklist) %>% 
  mutate(geburtsdatum = unlist(map(geboren, 1)),
         geburtsort = unlist(map(geboren, 2)),
         twitter_handle = str_remove(twitter, "https?://(www\\.)?twitter.com/") %>% 
           str_extract("[^?/]+"))

foo2 <- names(results[[1]]) 



twitter_handles <- results %>% 
  pluck("linklist") %>% pluck("twitter") %>% 
  discard(is.na) %>% 
  unlist() %>% 
  str_remove("https?://(www\\.)?twitter.com/") %>% 
  str_extract("[^?/]+")
