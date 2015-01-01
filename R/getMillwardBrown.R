get_millwardbrown <- function(url) {
  download.file(url,destfile = basename(url))
  doc <- jsonlite::fromJSON(basename(url)) # 
  return(doc)
}

get_millwardbrown_detail <- function(x) {
  detailed_url <- paste0('http://wybory.millwardbrown.com/partie-polityczne-parlament-krajowy/',x,'.json')
  download.file(detailed_url,destfile = basename(detailed_url))
  doc <- jsonlite::fromJSON(basename(detailed_url)) #jsonlite::
  return(doc)
}

getMillwardBrown <- function() {
  main_url <- 'http://wybory.millwardbrown.com/partie-polityczne-parlament-krajowy.json'
  
  ## dane z wykresu
  dane <- get_millwardbrown(main_url)
  
  ## dodatkowe informacje
  pojedyncze <- lapply(dane$polls$id,get_millwardbrown_detail)
  
  # to the data frame
  inds <- which(!is.na(dane$data$name))
  partie <- dane$data$name[inds]
  lista <- lapply(inds, function(ind) {
    tmp <- dane$data$data[[ind]]
    colnames(tmp) <- c("data", "poparcie")
    data.frame(tmp, partia = dane$data$name[ind])
  })
  do.call(rbind, lista)
}
