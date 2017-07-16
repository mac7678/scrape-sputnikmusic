library(XML)
library(dplyr)
####----- Functions for Scraping -----####

findSputURL_soundoffpage <- function(sputurl){ # find soundoffpage
  links <- getHTMLLinks(sputurl)
  if(!is.na(links[grep('/soundoff',links)[1]])){
    links <- links[grep('/soundoff',links)[1]]
    links <- paste0('http://www.sputnikmusic.com',links)
  }else{
    links <- sputurl
  }
  return(links)
}
findSputURL_band <- function(sputurl){ # find band page
  links <- getHTMLLinks(sputurl)
  if(!is.na(links[grep('bands/',links)[1]])){
    links <- links[grep('bands/',links)[1]]
    links <- paste0('http://www.sputnikmusic.com/',links)
  }else{
    links <- sputurl
  }
  
  return(links)
}

find_album <- function(tmp){
  newtmp <- list()
  if(any(names(tmp)=='font') & any(names(tmp)=='.attrs')){
    if(regexpr('/album/',tmp$.attrs)>0){
      newtmp <- tmp
    }else{
      newtmp <- NULL
    }
  }else{
    newtmp <- NULL
  }
  return(newtmp)
}

sputdate <- function(dates)
{if(!is.character(dates)) stop('Not readable')
  s<-c()
  for (i in 1:length(dates))
  {
    tmpdate <- unlist(strsplit(dates[i], ' '))
    if (length(tmpdate)==1){
      s[i] <- NA}
    else
    {
      mo <- grep(strsplit(tmpdate, ' ')[1],month.name)
      da <- as.numeric(substr(tmpdate[2],1,nchar(tmpdate[2])-2))
      ye <- paste0('20',tmpdate[3])
      s[i] <- paste(ye,mo,da,sep = "/")
    }
  }
  s <- as.Date(s,"%Y/%m/%d")
  return(s)
}
scrape_band <- function(band_page){
  links <- xpathSApply(band_page, "//b")
  band <- xmlToList(links[[1]]) # get band name
  return(band)
}
scrape_album <- function(band_page,link){
  links <- xpathSApply(band_page, "//a")
  tmp <- lapply(links,xmlToList)
  newtmp <- lapply(tmp,find_album)
  newtmp <- Filter(function(x) !is.null(x),lapply(tmp,find_album))
  newtmp <- newtmp[[as.integer(which(unlist(lapply(newtmp,function(x,y=link) x$.attrs==y))))]] #search through album links to match current album link of user page
  if(length(names(newtmp$font))<2){
    album <- ''
  }else{
    album <- newtmp$font$text # album name
  }
  return(album)
}
scrape_tags <- function(band_page,genre){ # get genre tags from band_page
  links <- getHTMLLinks(band_page)
  gtags <- unlist(lapply(grep('/genre/',links,value = TRUE), function(x) tail(unlist(strsplit(x,'/')),1)))
  if(is.null(gtags)) gtags <- NA
  check.for.sup <- function(gtag){
    if(gtag %in% genre$superhtml){
      return(genre$Genre[which(genre$superhtml==gtag)[1]])
    }else if(gtag %in% genre$htmllink){
      return(genre$SubGenre[genre$htmllink==gtag])
    }else{
      return(NA)
    }
  }
  gtags <- unlist(lapply(gtags, check.for.sup))
  gtags <- data.frame('Genre1'= gtags[1],'Genre2'= gtags[2],'Genre3'= gtags[3])
  return(gtags)
}
scrape_soundoff <- function(obj,link){ # function to scrape data 
  if(!any(class(obj) %in% c("HTMLInternalDocument", "HTMLInternalDocument",
                           "XMLInternalDocument",  "XMLAbstractDocument")) && is.character(obj)){
    link <- obj
    if(!grepl('/soundoff.php',obj)) stop('Character string provided is not a soundoff page')
    obj <- htmlParse(obj)
  }
  if(!any(class(obj) %in% c("HTMLInternalDocument", "HTMLInternalDocument",
                            "XMLInternalDocument",  "XMLAbstractDocument")) && !is.character(obj)) {
    stop(paste0('input is not an html object or a character.',
                'if calling this function directly,',
                ' ensure that your input is a character ',
                'string that is the name of sputnikmusic soundoff page.'))
  }
  user_links <- grep('/user/',getHTMLLinks(obj),value = TRUE)
  links <- getHTMLLinks(obj)
  if(length(grep('/best/albums/',links))>1){ # if theirs more than one link to best albums
    # ... that means the release year is in the second link
    release.year <- as.numeric(tail(unlist(strsplit(tail(grep('/best/albums/',links,value = TRUE),1),'/')),1))
  }else{
    release.year <- as.numeric(tail(unlist(strsplit(unlist(lapply(xpathSApply(obj, "//b"),xmlToList)[[2]]),'/')),1))
  }
  dat<-readHTMLTable(obj,which = 1)
  if(any(grepl('http:/',user_links))) user_links <- user_links[-grep('http:/',user_links)]
  dat$V2 <- as.character(dat$V2)
  names(dat)[1] <- 'Rating'
  dat <- dat[!is.na(dat$V2),]
  tmpr <- as.character(dat$V3)
  tmpr <- tmpr[grep(pattern = 'Rating',tmpr)]
  tmpr <- as.numeric(substr(tmpr, 10, nchar(tmpr)-1))
  dat <- dat[c(1,2)]
  dat$Rating <- as.numeric(substr(dat$Rating,1,3))
  dat <- dat[!is.na(dat$Rating),]
  dat <- dat[2:nrow(dat),]
  # for (i in 1:length(dat$V2))
  split_rating <- function(dat){
    tmpstr <- strsplit(x = dat, split = ' | ',fixed = TRUE)
    out <- data.frame(user = tmpstr[[1]][1],date = tmpstr[[1]][2])
    return(out)
  }
  dat <- data.frame(dat,bind_rows(lapply(dat$V2,split_rating)))
  user_links <- user_links[dat$Rating>0]
  dat <- dat[dat$Rating>0,]
  dat <- dat[, c(1,3,4)]
  dat$date <- sputdate(dat$date)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  dat$user <- trim.trailing(dat$user)
  dat$userlinks <- substr(user_links,7,nchar(user_links))
  dat$albumlink <- link
  dat <- data.frame(release.year,dat)
  return(dat)
}

scrapesput <- function(link,sput = 'http://www.sputnikmusic.com/'){ # preload dat with band/album/ and if previously found: genre tags
  genre <- readRDS('genre.rds')
  is_soundoff <- ifelse(grepl('/soundoff.php',link),1,0) # detect if soundoff page
  if(grepl(sput,link)) link <- paste0('/',tail(unlist(strsplit(link,sput)),1)) # grab href link
  if(!grepl(sput,link)) link_url <- paste0(sput,link) else link_url <- link # attach website to link
  html_page <- htmlParse(findSputURL_soundoffpage(link_url)) # find soundoff page if it's an album link
  # if(is.na(html_page)) return(NA)
  bandlink <- findSputURL_band(link_url) # find band page
  if(grepl('//bands//0/',bandlink)) return(data.frame()) 
  band_page <- htmlParse(bandlink) # create an html object from bandlink
  if(!link %in% getHTMLLinks(band_page) && !is_soundoff){
    return(data.frame())
  }else{
    dat <- scrape_soundoff(html_page,link)
    if(!is_soundoff){
      album <- scrape_album(band_page,link)
      band <- scrape_band(band_page)
      tags <- scrape_tags(band_page,genre)
      dat <- data.frame(band,album,dat,tags) # if an album link page, returns genre as well as album information
    }
    free(html_page)
    writeLines(sprintf('%s COMPLETED',link))
    free(band_page)
    return(dat)
  }
}
####----- End Functions -----#####

####----- EXAMPLE -----####
# scrape an an album from sputnikmusic.com
# -- 'maudlin of the Well,   My Fruit Psychobells... A Seed Combustible'---
# links need to be from the band page (i.e. '/album/' page)...
# or from a soundoff page, but data returned from a soundoff page will miss band, album, and genre information

sputurl <- 'http://www.sputnikmusic.com/album/14363/maudlin-of-the-Well-My-Fruit-Psychobells...-A-Seed-Combustible/'
dat <- scrapesput(sputurl)
###########################