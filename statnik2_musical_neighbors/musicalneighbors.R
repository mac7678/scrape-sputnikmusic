# Musical Neighbors
library(tidyverse)
library(XML)
library(RCurl)

####------ Functions ------#####
strsplit_last <- Vectorize(function(x) tail(unlist(strsplit(x,'/')),1),USE.NAMES = FALSE)
htmlParse_https <- function(link){
  # Read page
  page <- getURL(link)
  obj <- htmlParse(page)
}
find_user <- function(user){ # find users rating page
  sput <- 'https://www.sputnikmusic.com/user/'
  site_links <- getHTMLLinks(htmlParse_https(paste0(sput,user)))
  userid <- tail(unlist(strsplit(grep('/uservote.php',site_links,value = TRUE),'=')),1)
  ratepage <- paste0('https://www.sputnikmusic.com/uservote.php?sort=1&memberid=',userid)
  return(ratepage)
}
get_ratings <- function(siteobj){ # get the ratings from the page
  b_tag <- xpathSApply(siteobj,'//b')
  convert_xml <- Vectorize(function(x) xmlToList(x))
  ratings <- as.numeric(convert_xml(b_tag)[5:length(b_tag)])
  return(ratings)
}
get_albums <- function(siteobj){ # get album names from the page
  links <- getHTMLLinks(siteobj)
  albums <- strsplit_last(grep('^/album/',links,value = TRUE))
  return(albums)
}
scrape_user <- function(user){ # get album and rating information from a user
  if(!is.character(user)) stop("*user* has to be a character string")
  user_site <- find_user(user) %>% str_replace('^http:', 'https:')
  # find the easiest table to load and then get links from
  siteobj <- htmlParse_https(user_site)
  dat <- list(user = user, Rating = get_ratings(siteobj),album = get_albums(siteobj))
  if(length(dat$album)==0 || all(is.na(dat$Rating))) return(list()) else return(as.data.frame(dat,stringsAsFactors = FALSE))
  free(siteobj)
}
prepare_sim_matrix <- function(tmp,user){ # make large user x rating matrix
  albums <- sort(unique(tmp$album))
  user_mat <- matrix(0,length(unique(tmp$user)),length(albums))
  colnames(user_mat) <- albums
  rownames(user_mat) <- unique(c(user,tmp$user))
  for(i in 1:nrow(user_mat)){
    user_mat[i,tmp$album[tmp$user==rownames(user_mat)[i]]] <- tmp$Rating[tmp$user==rownames(user_mat)[i]]
  }
  return(user_mat)
}
cos.sim=function(ma, mb){ # cosine similarity courtesy of https://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}
calc_neighbors <- function(user,neighbors,n_toprint=10,dat=c()){
  if(length(neighbors) < n_toprint) n_toprint <- length(neighbors)
  if(length(dat)==0) dat <- bind_rows(lapply(c(user,neighbors),scrape_user))
  sim_mat <- prepare_sim_matrix(dat,user)
  neighbor_scores <- cos.sim(sim_mat,sim_mat)[,1]
  neighbor_scores <- sort(neighbor_scores[!names(neighbor_scores)==user],decreasing = TRUE)
  print_neighbors(neighbor_scores,user,n_toprint)
  return(list(neighbor_scores=neighbor_scores,all_ratings = dat))
}
print_neighbors <- function(neighbor_scores,user,n_toprint=10){
  writeLines(paste0('Top ', n_toprint,' Musical Neighbors for ',user))
  for(i in 1:n_toprint){
    writeLines(sprintf('Rank %d, %s, score %.3f',
                       i,names(neighbor_scores)[i],neighbor_scores[i]))
  }
}
####------ End Functions ------#####

neighbors <- c('macman76','mx','Jom','Willie','Deviant.','AtomicWaste',
               'SowingSeason','Atari','manosg','Jacquibim','Rowan5215',
               'robertsona','TalonsOfFire','Gameofmetal','DaveyBoy',
               'Dave+de+Sylvia','Arcade','SgtPepper','Irving',
               'StrangerofSorts','insomniac15','Trebor.','klap',
               'theacademy','JohnnyOnTheSpot','Voivod','Xenophanes',
               'anatelier','butcherboy','DoofusWainwright','JamieTwort',
               'MercuryToHell','Winesburgohio','Frippertronics','Archelirion',
               'Astral+Abortis','beachdude','Insurrection','TheSpirit',
               'Conmaniac','danielcardoso','wtferrothorn','hesperus','Mort.',
               'hogan900','Calc','LandDiving','Xenorazr','Mongi123','ExplosiveOranges',
               'cryptologous','linguist2011','ScuroFantasma','DrGonzo1937',
               'Bartender','Damrod','mynameischan','plane','Electric+City',
               'Knott-','Athom','StreetlightRock','morrissey','br3ad_man',
               'AngelofDeath','GnarlyShillelagh','joshuatree','conradtao',
               'VheissuCrisis','paradox1216','Liberi+Fatali','greg84',
               'Hyperion1001','Jade','Ponton','Iluvatar','JViney','thebhoy',
               'Kiran','Crysis','Robert+Crumb','Metalstyles','Minus+The+Flair',
               'Med57','Mikesn','Iai','204409','robin','IsItLuck%3F','kingsoby1',
               'FlawedPerfection','Tyler','Brostep')

user <- 'macman76' # user to calculate similarity for
neighbors <- neighbors[!user == neighbors]
dat <- calc_neighbors(user,neighbors) # function to calculate similarity between user and neighbors
print_neighbors(dat$neighbor_scores,user)
