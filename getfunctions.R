
get_pubmed<- function(name, hiredate){

  # Formats author name
  author_name = name
  query <- paste0(author_name, "[Author]")

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
