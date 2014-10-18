# Access to Banku Danych Lokalnych
# through the API in http://mojepanstwo.pl/api/!/bdl/!/bdl/!/bdl/series
getBDLtree <- function(debug = 0, raw = FALSE) {
  library(rjson)
  url <- 'http://api.mojepanstwo.pl:80/bdl/tree'
  document <- fromJSON(file=url, method='C')
  if (raw) return(document)
  
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

getBDLsearch <- function(query = "", debug = 0, raw = FALSE) {
  library(rjson)
  library(htmltools)
  url <- paste0('http://api.mojepanstwo.pl:80/bdl/search?q=', query)
  document <- fromJSON(file=htmlEscape(url), method='C')
  if (raw) return(document)
  
  dgs <- lapply(document, function(d) {
    if (debug >= 1) cat(d$hl, "\n")
    data.frame(id = d$id, dataset = d$dataset, object_id = d$object_id, 
      tytul = d$data$tytul,
      urlid = d$`_id`)
  })
  do.call(what = rbind, dgs)
}


getBDLseries <- function(metric_id = "", 
                         slice = NULL,
                         time_range = NULL,
                         wojewodztwo_id = NULL,
                         powiat_id = NULL,
                         gmina_id = NULL,
                         meta = NULL,
                         debug = 0, raw = FALSE) {
  library(rjson)
  library(htmltools)
  
  url <- paste0('http://api.mojepanstwo.pl:80/bdl/series?metric_id=', metric_id)
  if (!is.null(slice)) url <- paste0(url, "&slice=", htmlEscape(slice))
  if (!is.null(time_range)) url <- paste0(url, "&time_range=", htmlEscape(time_range))
  if (!is.null(wojewodztwo_id)) url <- paste0(url, "&wojewodztwo_id=", htmlEscape(wojewodztwo_id))
  if (!is.null(powiat_id)) url <- paste0(url, "&powiat_id=", htmlEscape(powiat_id))
  if (!is.null(gmina_id)) url <- paste0(url, "&gmina_id=", htmlEscape(gmina_id))
  if (!is.null(meta)) url <- paste0(url, "&meta=", htmlEscape(meta))
  
  document <- fromJSON(file=htmlEscape(url), method='C')
  if (raw) return(document)
  
  met <- t(sapply(document$series, function(s) s$slice))
  fullmet <- do.call(what = rbind, lapply(1:ncol(met), function(d) {
    tmp <- do.call(what = rbind, document$meta$dimensions[[d]]$options)
    rownames(tmp) <- tmp[,1]
    met[,d] <<- unlist(tmp[met[,d],"value"])
    data.frame(dim=d, tmp)
  }) )
    
  dgs <- lapply(seq_along(document$series), function(sn) {
    s <- document$series[[sn]]
    tmp <- data.frame(do.call(what = rbind, args = s$series), units = s$units)
    for (i in ncol(met):1)
      tmp <- data.frame(dimension = met[sn,i], tmp)
    tmp
  })
  do.call(what = rbind, dgs)
}
