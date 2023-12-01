
# Creates Queries #######################################

query_func = function(name1,
                      name2,
                      affiliation1,
                      affiliation2,
                      affiliation3){
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
  summarize(query = paste0(containerL,input,tag,containerR, separator,collapse = "")) %>% pull(query)

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




