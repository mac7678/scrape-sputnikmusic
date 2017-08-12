library(XML)
library(dplyr)
library(purrr)
## sputfunctions -----------

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
scrape_soundoff <- function(obj,link,alldat=list(),getuser_metrics = TRUE){ # function to scrape data 
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
  if(getuser_metrics){
    user_stats <- bind_rows(lapply(user_links,userstat,alldat = alldat))
  } else {
    user_stats <- data.frame()
  }
  dat <- dat[dat$Rating>0,]
  dat <- dat[, c(1,3,4)]
  sputdate <- function(dates){
    if(!is.character(dates)) stop('Not readable')
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
  dat$date <- sputdate(dat$date)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  dat$user <- trim.trailing(dat$user)
  dat$userlinks <- substr(user_links,7,nchar(user_links))
  dat$albumlink <- link
  dat <- data.frame(release.year,dat)
  if(getuser_metrics) dat <- data.frame(dat,user_stats)
  dat <- dat[order(dat$date,decreasing = TRUE),]
  return(dat)
}

userstat <- function(links,alldat=list()){
  if(substr(links,7,nchar(links)) %in% unique(alldat$userlinks)){
    thd <- alldat[substr(links,7,nchar(links)) == alldat$userlinks,c("ratings","reviews","lists", "comments")]
    thd <- thd[1,]
  }else{
    tmp <- tryCatch(readHTMLTable(paste0('http://www.sputnikmusic.com',links),which=1), error=function(e) NULL)
    thd <- c()
    if(is.null(tmp)) {
      thd$ratings <- NaN
      thd$reviews <- NaN
      thd$lists <- NaN
      thd$comments <- NaN
    }else{
      coms <- unlist(strsplit(unlist(strsplit(as.character(tmp$V1[2]),split = 'Review Comments'))[2],'\t'))[1]
      coms <- gsub(',',replacement = '',x = coms)
      coms <- as.numeric(substr(coms,2,nchar(coms)))
      tmp <- as.character(tmp$V2[grep('main profile', tmp$V2)])
      nums <-as.numeric(unlist(regmatches(tmp, gregexpr("[[:digit:]]+", tmp))))
      thd$ratings <- nums[1]
      thd$reviews <- nums[2]
      thd$lists <- nums[3]
      thd$comments <- coms
    }
  }
  return(as.data.frame(thd))
}
scrapesput <- function(link,
                       sput = 'http://www.sputnikmusic.com/',
                       alldat=list(),getuser_metrics = TRUE)
{ # preload dat with band/album/ and if previously found: genre tags
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
    dat <- scrape_soundoff(html_page,link,getuser_metrics = getuser_metrics,alldat = alldat)
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

round2 = function(x, n) { # from https://stackoverflow.com/questions/12688717/round-up-from-5-in-r
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

sput_rig <- function(album, points = 1L){
  if(!is.integer(points) && points < 1) {
    stop("'points' is not an integer")
  } else {
    points <- round(points)/10
  }
  dat <- scrapesput(album, getuser_metrics = FALSE)
  dat <- map_df(dat, ~ if(is.factor(.x)) as.character(.x) else .x)
  rigged <- list()
  rigged$Rating <- dat$Rating
  rigged$mean <- round2(mean(dat$Rating),1)
  if(rigged$mean + points >5 || rigged$mean-points < 1){
    stop("'points' set too high")
  }
  rigged$extended_mean <- round2(mean(dat$Rating),4)
  rigged$num_ratings <- nrow(dat)
  ratings <- rigged$Rating
  # find number of ratings needed to add to decrease
  
  rig_down_add <- function(ratings, points){
    cur_mean <-  round2(mean(ratings),1)
    adder <- 0
    while(cur_mean < round2(mean(ratings),1) + points){
      adder <- adder + 1
      ratings <- c(ratings,1)
    }
    return(adder)
  }
  rig_up_add <- function(ratings, points){
    cur_mean <-  round2(mean(ratings),1)
    adder <- 0
    while(cur_mean + points > round2(mean(ratings),1)){
      adder <- adder + 1
      ratings <- c(ratings,5)
    }
    return(adder)
  }
  rig_down_hack <- function(ratings, points){
    cur_mean <-  round2(mean(ratings),1)
    ratings <- sort(ratings,decreasing = TRUE)
    for(i in seq_along(ratings)){
      ratings[i] <- 1
      if(!cur_mean < round2(mean(ratings),1) + points) break
    }
    return(i)
  }
  rig_up_hack <- function(ratings, points){
    cur_mean <-  round2(mean(ratings),1)
    ratings <- sort(ratings)
    for(i in seq_along(ratings)){
      ratings[i] <- 5
      if(!cur_mean + points > round2(mean(ratings),1)) break
    }
    return(i)
  }
  # rig rating up by adding ratings
  rigged$up_add <- rig_up_add(ratings,points)
  # rig rating down by adding ratings
  rigged$down_add <- rig_down_add(ratings,points)
  # rig rating up by hacking ratings
  rigged$up_hack <- rig_up_hack(ratings,points)
  # rig rating down by hacking ratings
  rigged$down_hack <- rig_down_hack(ratings,points)
  writeLines(sprintf(paste0(" \nBand: %s, Album: %s\n",
                            "Current Average Rating: %s (full: %s), %s Ratings\n",
                            "Number of 5's needed to increase rating by %s points: %s\n",
                            "Number of 1's needed to decrease rating by %s points: %s\n",
                            "Number of accounts to needed be hacked to increase rating by %s points: %s\n",
                            "Number of accounts to needed be hacked to decrease rating by %s points: %s\n"),
                     dat$band[1],dat$album[1],
                     rigged$mean,rigged$extended_mean, rigged$num_ratings,
                     points,rigged$up_add,
                     points,rigged$down_add,
                     points,rigged$up_hack,
                     points,rigged$down_hack))
  return(list(dat,rigged))
}

## Code to Rig the ratings -----

# link of album to rig
album <- 'http://www.sputnikmusic.com/album/67689/Universum-Mortuus-Machina/'

dat <- sput_rig(album, points = 1) # points is increment of rating ...
# i.e. going from 4.0 to 4.1 is a "points" increase of 1
