#'---
#' title: "rapid overview of SRs of cognition and fluoride exposure"
#' date: "`r Sys.Date()`"
#'---

#' ## fluoride

#+ echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE

library(pacman)
p_load(tidyverse, myScrapers, gt, knitr, textrank, flextable )

key <- Sys.getenv("ncbi_key")

f <- myScrapers::get_ss_data(search = "fluoride AND systematic review", n = 100)

f$data %>%
  select(title, abstract, contains("Id")) %>%
  #mutate(summary = map(abstract, myScrapers::text_summariser)) %>%
  #select(title, summary, contains("Id")) %>%
  flextable()

f$data[11,]

#browseURL(f$data$url[11])

#' ## retrieve web text

[img]
myScrapers::get_page_text("https://ehp.niehs.nih.gov/doi/10.1289/isee.2020.virtual.O-SY-1323") %>% .[15] 
