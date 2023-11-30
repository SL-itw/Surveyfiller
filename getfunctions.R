


########################################

# Gets publications
get_pubmed<- function(name,
                      hiredate,
                         affiliation1 = NA,
                        affiliation2 = NA,
                       affiliation3 = NA
                      ){

  # Formats author name
  author_name = name
  if(is.na(affiliation1) & is.na(affiliation2) & is.na(affiliation3)){
    query <- paste0(author_name, "[Author]")
  }else if(is.na(affiliation2) & is.na(affiliation3)){
    query <- paste0(author_name, "[Author]", " AND ", affiliation1,"[Affiliation]")
  }else if(is.na(affiliation3)){
    query <- paste0(author_name, "[Author]", " AND ","(", affiliation1,"[Affiliation]"," OR ", affiliation2,"[Affiliation]",")")
  }else{
    query <- paste0(author_name, "[Author]", " AND ","(", affiliation1,"[Affiliation]"," OR ", affiliation2,"[Affiliation]"," OR ", affiliation3,"[Affiliation]",")")
  }


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


#################################


