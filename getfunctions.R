

get_publications <- function(author_name, max_results = 10) {
  # Query PubMed for publications by the author
  query <- paste0(author_name, "[Author]")
  ids <- entrez_search("pubmed", query, retmax = max_results)$ids
  # Fetch publication details
  if (length(ids) > 0) {
    records <- entrez_fetch("pubmed", ids, rettype = "xml")$pubmedarticle
    # Extract relevant information (customize based on your needs)
    publications <- lapply(records, function(record) {
      data.frame(
        title = xpathSApply(record, "//ArticleTitle", xmlValue),
        journal = xpathSApply(record, "//Title", xmlValue),
        year = xpathSApply(record, "//PubDate/Year", xmlValue),
        pmid = xpathSApply(record, "//PMID", xmlValue)
      )
    }) %>% bind_rows()
    return(publications)
  } else {
    warning("No publications found.")
    return(NULL)
  }
}
