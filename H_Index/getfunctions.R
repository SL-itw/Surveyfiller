
# Creates Queries #######################################

query_func = function(name1,
                      name2=NA,
                      affiliation1,
                      affiliation2 = NA,
                      affiliation3 = NA){
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

get_pubmed<- function(name1,
                      hiredate,
                      affiliation1,
                      name2= NA,
                      affiliation2 = NA,
                      affiliation3 = NA
                      ){

  if (name2==""){
  name2=NA
  }
  if (affiliation2==""){
    affiliation2=NA
  }
  if (affiliation3==""){
    affiliation3=NA
  }

 query = query_func(name1 = name1,
                    name2 = name2,
                    affiliation1 = affiliation1,
                    affiliation2 = affiliation2,
                    affiliation3 = affiliation3)


  # queries author article ID's by name
  ids <- entrez_search("pubmed", query, retmax = 10000)$ids

  # pulls records by ID's
  rec<-  entrez_fetch("pubmed", ids, rettype = "xml")

  # parses meta information
  parsed <- XML::xmlTreeParse(rec, useInternalNodes = TRUE)

  specific_date = lubridate::mdy(hiredate)
  two_years_prior_date <- specific_date %m-% years(2)

  cbind(
  titles = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/MedlineCitation//ArticleTitle')),
  pubdates = XML::xmlToDataFrame(nodes = XML::getNodeSet(parsed, '//PubmedArticle/PubmedData/History/PubMedPubDate[@PubStatus="pubmed"]'))

  ) %>% tibble() %>%
    mutate(date = paste(pubdates.Month,pubdates.Day,pubdates.Year, sep = "/") %>% lubridate::mdy()
    ) %>%
    filter(date >= two_years_prior_date & date < specific_date) %>%
    rename("titles" = "text") %>%
    select(titles, date)
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
  responce = scholar::get_scholar_resp(attempts_left = 1,url =article_link )
  page = rvest::read_html(responce)
  cite_string = rvest::html_node(page, xpath = '//*[@id="gs_res_ccl_mid"]/div/div[contains(@class, "gs_ri")]/div[contains(@class, "gs_fl")]//a[contains(text(), "Cited by")]')%>%
    rvest::html_text()

  as.numeric(gsub("\\D", "", cite_string))

}


# Get full data #######################

article_data <- function(name1,
                         hiredate,
                         affiliation1,
                         name2= NA,
                         affiliation2 = NA,
                         affiliation3 = NA){

  get_pubmed(name1 = name1,
             hiredate = hiredate,
             affiliation1 = affiliation1,
             name2 = name2,
             affiliation2 = affiliation2,
             affiliation3 = affiliation3
            ) %>%
    mutate(citations = map(titles,get_cited)) %>%
    unnest(citations)

}

# Get H-index ##############

h_index <- function(name1,
                    hiredate,
                    affiliation1,
                    name2= NA,
                    affiliation2 = NA,
                    affiliation3 = NA){


  article_data(name1 = name1,
               hiredate = hiredate,
               affiliation1 = affiliation1,
               name2 = name2,
               affiliation2 = affiliation2,
               affiliation3 = affiliation3) %>%
    mutate(n = length(titles),
           ind = if_else(citations >= n, 1,0)) %>%
    summarize(h_index = sum(ind, na.rm = T)) %>%
    pull(h_index)
}


