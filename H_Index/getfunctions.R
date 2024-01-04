
# Creates Queries #######################################

query_func = function(name1,
                      name2=NA,
                      affiliation1,
                      affiliation2 = NA,
                      affiliation3 = NA){

  if (name2==""){
    name2=NA
  }
  if (affiliation2==""){
    affiliation2=NA
  }
  if (affiliation3==""){
    affiliation3=NA
  }

 query = tibble(
  name1 = name1,
  name2 = name2,
  affiliation1 = affiliation1,
  affiliation2 = affiliation2,
  affiliation3 = affiliation3
) %>%
  pivot_longer(
    name1:affiliation3,
    names_to = "label",
    values_to = "input"
  ) %>%
  mutate(tag = if_else(grepl("name", label),"[Author]","[Affiliation]"),
         ind = if_else(is.na(input),0,1)
  ) %>%
  filter(ind == 1) %>%
  mutate(separator = if_else(lead(tag) == tag, " OR ", " AND "),
         separator = if_else(is.na(separator),"",separator),
         containerL = if_else(separator == " OR ","(",""),
         containerR = if_else(lag(separator) == " OR ",")",""),
         containerR = if_else(is.na(containerR),"",containerR)) %>%
  dplyr::summarize(query = paste0(containerL,input,tag,containerR, separator,collapse = "")) %>% pull(query)

query

}


# Gets Publications #######################################

get_pubmed <- function(recs, hiredate){

  # parses meta information
  parsed <- XML::xmlTreeParse(recs, useInternalNodes = TRUE)

  specific_date = lubridate::mdy(hiredate)
  two_years_prior_date <- specific_date %m-% years(2)

  cbind(
  titles = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation//ArticleTitle')),
  pubdates = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/PubmedData/History/PubMedPubDate[@PubStatus="pubmed"]')),
  PUBid = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation/PMID')) %>% tibble() %>% rename("PUBid"="text" )

  ) %>% tibble() %>%
    mutate(date = paste(pubdates.Month,pubdates.Day,pubdates.Year, sep = "/") %>% lubridate::mdy()
    ) %>%
    filter(date >= two_years_prior_date & date < specific_date) %>%
    rename("titles" = "text") %>%
    select(PUBid, titles, date)
  }

# Get Article link ###################################
get_article_url <- function(title) {
  # Construct the Google Scholar URL
   paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C33&q=",
                         URLencode(title))
}

# Get cited xml ##############

get_cited <- function(title){

  article_link = get_article_url(
    title = title
  )
  responce = scholar::get_scholar_resp(attempts_left = 1, url =article_link )
  page = rvest::read_html(responce)
  cite_string = rvest::html_node(page, xpath = '//*[@id="gs_res_ccl_mid"]/div/div[contains(@class, "gs_ri")]/div[contains(@class, "gs_fl")]//a[contains(text(), "Cited by")]')%>%
    rvest::html_text()

  as.numeric(gsub("\\D", "", cite_string))

}

# Get citations from pubmed ################

pub_citefunc <- function(id){


  cite_link = paste0("https://pubmed.ncbi.nlm.nih.gov/?linkname=pubmed_pubmed_citedin&from_uid=",id)
  session = polite::bow(cite_link, force = T)
  page = polite::scrape(session, query=list(t="semi-soft", per_page=1))

  count = (rvest::html_node(page, xpath = '/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]/h3/span') |>
             rvest::html_text() |>
             as.numeric())-1

  count
  }

# Get full data #######################

article_data <- function(recs,
                         hiredate){

  tab= get_pubmed(recs = recs,
             hiredate = hiredate)
  cores <- parallel::makeCluster(parallel::detectCores())

  ids = tab %>% pull(PUBid)

  citations= parallel::parLapply(cores,ids,pub_citefunc) %>% tibble() %>%
    unnest() %>%
    rename("citations" = ".") %>%
    mutate(citations= replace_na(citations,0)) %>%
    pull(citations)

  tab %>% mutate(citations = citations)


}


# Getting unique co authors ###################

coauthor_func <- function(row,recs){

  rec = recs[row]
  parsed = XML::xmlTreeParse(rec, useInternalNodes = TRUE)

  tibble(forename = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation//AuthorList/Author/ForeName')) ,
         Lastname = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation//AuthorList/Author/LastName')) ) %>%
    unnest(cols = c(forename, Lastname)) %>%
    rename(  "forename" = "text",  "lastname" = "text1" )

}

get_coauthor_count<- function(ids, recs){

  recs = recs

coauthor_table = tibble(
  rows = c(1:length(ids))
) %>%
  mutate(outcome = map(rows,coauthor_func)) %>%
  unnest(cols = c(outcome)) %>%
  separate(forename, into = c("firstname","x"),sep = " ") %>%
  select(-rows, -x) %>%
  unique() %>%
  arrange(firstname) %>%
  summarize(n = n()-1) %>%
  pull(n)

}
