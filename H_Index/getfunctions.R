
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

get_pubmed <- function(parsed){




  cbind(
  titles = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation//ArticleTitle')),
  pubdates = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/PubmedData/History/PubMedPubDate[@PubStatus="pubmed"]')),
  PUBid = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation/PMID')) %>% tibble() %>% rename("PUBid"="text" )

  ) %>% tibble() %>%
    mutate(date = paste(pubdates.Month,pubdates.Day,pubdates.Year, sep = "/") %>% lubridate::mdy()
    ) %>%

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

  session = polite::bow(cite_link, delay = 5, times = 5)

 # Sys.sleep(5)

  page = polite::scrape(session, query = list(t = "semi-soft", per_page = 1))

  #close(page)
  if(!is.null(page)){
    count =  as.numeric(
      rvest::html_text(
        rvest::html_node(page, xpath = "//*[@id='search-results']//h3/span") )
     ) - 1
  }else{
    count = NA
  }

count

}

# Get full data #######################

article_data <- function(parsed){

  tab = get_pubmed(parsed) #%>%
  cores = parallel::makeCluster(parallel::detectCores())
  PUBids = tab %>% pull(PUBid)

  #mutate(citations = map(PUBid,pub_citefunc))# %>%
  citations= parallel::parLapply(cores,PUBids,pub_citefunc) %>% tibble() %>%
    unnest(cols = c(.)) %>%
   #unnest(cols = c(citations)) %>%
    rename("citations" = ".") %>%
    mutate(citations= replace_na(citations,0))  %>%
    pull(citations)

 tab %>% mutate(citations = citations)


}

# Getting unique co authors ###################


get_single_coauthor = function(id){

  rec = entrez_fetch("pubmed", id, rettype = "xml")
  parse = XML::xmlTreeParse(rec, useInternalNodes = TRUE)

  names = cbind(forename = XML::xmlToDataFrame(nodes = XML::getNodeSet(parse, '//PubmedArticle/MedlineCitation//AuthorList/Author/ForeName')) ,
                Lastname = XML::xmlToDataFrame(nodes = XML::getNodeSet(parse, '//PubmedArticle/MedlineCitation//AuthorList/Author/LastName')))
  colnames(names) = c("forename","lastname")

  date = pubdates = XML::xmlToDataFrame(nodes = XML::getNodeSet(parse, '//PubmedArticle/PubmedData/History/PubMedPubDate[@PubStatus="pubmed"]'))
  tibble(names, date)

  }
# Get co author
get_coauthor_count<- function(ids,two_years_prior_date,specific_date){

  name_tab =  tibble(
    id = ids
  ) %>%
    mutate(coauthors = map(id,get_single_coauthor)) %>%
    unnest(coauthors) %>%
    mutate(date = paste(Month,Day,Year, sep = "/") %>% lubridate::mdy()
    ) %>%
    select(id, forename, lastname, date) %>%
    filter(date >= two_years_prior_date & date < specific_date) %>%
    select(forename, lastname) %>%
    separate(forename, into = c("firstname","x"),sep = " ") %>%
    separate(firstname, into = c("firstname","x"),sep = "-") %>%
    select( -x) %>%
    arrange(lastname) %>%
    unique()

  n = name_tab %>%
    summarize(n = n()-1) %>%
    pull(n)

  list(name_tab,n)
}


# Batch function --------------

query_list = function(data){

  data %>%
    mutate(query = purrr::pmap(.,query_func))

}

query_data<- function(query){

  ids =  entrez_search("pubmed", query, retmax = 10000)$ids
  recs = entrez_fetch("pubmed", ids, rettype = "xml")
  parsed = XML::xmlTreeParse(recs, useInternalNodes = TRUE)

  article_data(parsed)

}

batch_metrics<- function(data){


    query_list(data = data) %>%
    unnest(col = c(query)) %>%
    mutate(results = map(query, query_data))



}
