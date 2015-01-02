get_millwardbrown <- function(url) {
  doc <- fromJSON(file=url, method = "C")
  return(doc)
}

get_millwardbrown_detail <- function(x) {
  detailed_url <- paste0('http://wybory.millwardbrown.com/partie-polityczne-parlament-krajowy/',x,'.json')
  doc <- fromJSON(file=detailed_url, method = "C")
#  download.file(detailed_url,destfile = basename(detailed_url))
#  doc <- jsonlite::fromJSON(basename(detailed_url)) #jsonlite::
  return(doc)
}

getMillwardBrown <- function() {
  main_url <- 'http://wybory.millwardbrown.com/partie-polityczne-parlament-krajowy.json'
  
  ## dane z wykresu
  dane <- get_millwardbrown(main_url)
  
  ## dodatkowe informacje
  ids <- sapply(dane$polls, function(x) x$id)
  pojedyncze <- lapply(ids,get_millwardbrown_detail)
  
  # to the data frame
  namess <- sapply(dane$data, function(x) x$name)
  inds <- which(sapply(namess, length) > 0)
  partie <- unlist(namess[inds])
  lista <- lapply(inds, function(ind) {
#    tmp <- dane$data[[id]]$data
    tmp <- as.data.frame(t(as.data.frame(dane$data[[ind]]$data)))
    rownames(tmp) <- NULL
    colnames(tmp) <- c("data", "poparcie")
    tmp$partia <- namess[ind]
    tmp
  })
  do.call(rbind, lista)
}
