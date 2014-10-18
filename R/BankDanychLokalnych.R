# Access to Banku Danych Lokalnych
# through the API in http://mojepanstwo.pl/api/!/bdl/!/bdl/!/bdl/series
getBDLtree <- function(debug = 0) {
  library(rjson)
  url <- 'http://api.mojepanstwo.pl:80/bdl/tree'
  document <- fromJSON(file=url, method='C')
  
  dgs <- lapply(document, function(d) {
    if (debug >= 1) cat(d$name, "\n")
    ggs <- lapply(d$groups, function(g) {
      if (debug >= 2) cat("     ", g$name, "\n")
      sgs <- lapply(g$subgroups, function(s) {
        c(subgroup = s$name, id = s$`_id`)
      })
      data.frame(group = g$name, do.call(what = rbind, sgs))
    })
    data.frame(document = d$name, do.call(what = rbind, ggs))
  })
  do.call(what = rbind, dgs)
}

getBDLsearch <- function(query = "", debug = 0) {
  library(rjson)
  library(htmltools)
  url <- paste0('http://api.mojepanstwo.pl:80/bdl/search?q=', query)
  document <- fromJSON(file=htmlEscape(url), method='C')
  
  dgs <- lapply(document, function(d) {
    if (debug >= 1) cat(d$hl, "\n")
    data.frame(id = d$id, dataset = d$dataset, object_id = d$object_id, 
      tytul = d$data$tytul,
      urlid = d$`_id`)
  })
  do.call(what = rbind, dgs)
}

