#'---
#' title: "Neurotoxicity associated with high fluoride levels"
#' date: "`r Sys.Date()`"
#' always_allow_html: true
#' ---

#+ echo = FALSE, message = FALSE, warning = FALSE, cache = TRUE

knitr::opts_chunk$set(echo = FALSE)


remotes::install_github('julianflowers/myScrapers')


here::here("fluoride-work")
devtools::install_github("julianflowers/myScrapers")

library(myScrapers)
library(fulltext)
library(tidypmc)
library(tidyverse)
library(jsonlite)

library(myScrapers)

library(kableExtra)


#' ## fluoride systematic reviews

fl_sr <- get_ss_data(search = "fluoride AND systematic review", n = 100)

fl_cit_imp <- fl_sr$data %>%
  select(paperId, title, influentialCitationCount, externalIds.PubMedCentral, externalIds.DOI) %>%
  arrange(-influentialCitationCount) 

fl_cit_imp %>%
  pluck("paperId")

fluoride_neurotox <- tidypmc::pmc_xml(id = paste0("PMC", fl_cit_imp[1, 4]))

text <- tidypmc::pmc_text(fluoride_neurotox)
meta <- tidypmc::pmc_metadata(fluoride_neurotox)
tables <- tidypmc::pmc_table(fluoride_neurotox)

tables$`Table 1` %>%
  gt::gt()

tables$`Table 2` %>%
  gt::gt()

text %>%
  gt::gt()

#' ## get citations and refs using semantic scholar graph api

get_ss_graph <- function(id = NULL){
  
  uri <- "https://api.semanticscholar.org/graph/v1/paper/"
  citations <- glue::glue(uri, {id}, "/citations?fields=title,authors,venue,year,externalIds")
  references <- glue::glue(uri, {id}, "/references?fields=title,authors,venue,year,externalIds")
  cits <- fromJSON(citations, simplifyDataFrame = TRUE)
  refs <- fromJSON(references, simplifyDataFrame = TRUE)
  out <- list(citations = cits, references = refs)
}

###

test <- get_ss_graph(id = fl_cit_imp$paperId[1])
citing_papers <- test$citations$data$citingPaper %>%
  filter(str_detect(title, "fluoride")) %>% arrange(-year) %>%
  unnest("authors") %>%
  select(-authorId) %>%
  group_by(paperId, title, venue, year, externalIds) %>%
  summarise(authors = paste(name, collapse = ", ")) 

citing_papers %>%
  reactable::reactable(filterable = TRUE, sortable = TRUE, searchable = TRUE, groupBy = "venue")

#' ### get papers
get_ss_paper <- function(id = NULL){
  
  uri <- "https://api.semanticscholar.org/graph/v1/paper/"
  paper <- glue::glue(uri,{id}, "?fields=externalIds,abstract,citationCount")
  paper <- jsonlite::fromJSON(paper, simplifyDataFrame = TRUE)
}

paper <- get_ss_paper("0733f5a113d4b6c59c6682534e2c5f3e56f1caa3")
paper1 <- get_ss_paper("4511e038d09c98996d0af8439f9049c5a52e123c")

paper

fluoride_neurotox_1 <- tidypmc::pmc_xml(id = paste0("PMC", paper1$externalIds$PubMedCentral))
meta1 <- tidypmc::pmc_metadata(fluoride_neurotox_1)
meta1
tables1 <- tidypmc::pmc_table(fluoride_neurotox_1)

tables1

text1 <- tidypmc::pmc_text(fluoride_neurotox_1)

text1 %>%
  reactable::reactable(filterable = TRUE)
