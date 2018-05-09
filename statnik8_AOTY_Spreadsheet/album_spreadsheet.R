library(tidyverse)
library(googlesheets)
library(knitr)
library(reshape2)

# functions -----
cos.sim <- function(ma, mb){ 
  # cosine similarity courtesy of https://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}
get_lower_tri<-function(cormat){
  # from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# load all data -----

##Uncomment the following to take latest version of the 2017 AOTY spreadsheet
## for repeatability sake, i've provided a snapshot of the csvs

# year_obj <- gs_url(paste0('https://docs.google.com/',
#                           'spreadsheets/d/',
#                           '1-uY5_BeeKH_e2-XROri85qpV8s_5hMEEoFoxigci61I/edit#gid=0')) # link to aggregated year end list
# 
# year_links <- year_obj %>% 
#   gs_read_cellfeed(literal = FALSE, col_names = FALSE, range = cell_rows(1)) %>%
#   select(value, input_value) %>%
#   filter(grepl('HYPERLINK', input_value)) # table of publications included
# year_dat <- year_obj %>% 
#   gs_read() %>%
#   select(which(!grepl('^X[0-9]',names(.)))) %>%
#   mutate(ALBUM= str_to_lower(ALBUM))# read data
#####################################################
aoty_dir <- './statnik8_AOTY_Spreadsheet/'
year_links <- read_csv(str_c(aoty_dir, 'AOTY_ListsInfo_2017.csv'))
year_dat <- read_csv(str_c(aoty_dir, 'AOTY_2017.csv'))

user_year_dat <- read_csv(str_c(aoty_dir, 'sput_year_end_lists_2017.csv')) %>% 
  mutate(Band_Album = paste(band, 
                            album, sep = ' - ') %>% 
           str_to_lower()) %>%
  filter(staff == FALSE) %>%
  select(Band_Album, rank)

staff_year_dat <- read_csv(str_c(aoty_dir, 'sput_year_end_lists_2017.csv')) %>% 
  mutate(Band_Album = paste(band, 
                            album, sep = ' - ') %>% 
           str_to_lower()) %>%
  filter(staff == TRUE) %>%
  select(Band_Album, rank)

rating_dat <- read_csv(str_c(aoty_dir, '2017_AllRatings.csv')) %>%
  mutate(Band_Album = paste(Band, 
                            Album, sep = ' - ') %>% 
           str_to_lower()) %>%
  mutate(rank = min_rank(desc(l95))) 

stop_albs <- tibble(Band_Album = c('converge - i can tell you about pain',
                                   'city of caterpillar - driving spain up a wall')) # ep's found 
                                                                                     # on the top 50 of the sputusers list
# data manipulation/joining -----
all_dat <- year_dat %>% 
  full_join(staff_year_dat %>%
              rename(STAFF=rank),
            by = c('ALBUM' = 'Band_Album')) %>% # join staff list to aoty list
  full_join(user_year_dat %>%
              rename(SPUTUSERS=rank),
            by = c('ALBUM' = 'Band_Album')) %>% # join user list to aoty list
  full_join(rating_dat %>%
              select(Band_Album, rank) %>%
              rename(SPUTRATINGS = rank) %>%
              anti_join(stop_albs) %>%
              .[1:50,] %>%
              mutate(SPUTRATINGS = 1:50),
            by = c('ALBUM' = 'Band_Album')) %>% # join user ratings list to aoty list
  mutate(CONSENSUS = min_rank(`CONS SCORE`) %>%
           ifelse(. <= 50, ., NA)) # add consensus score as its own column

year_links <- year_links %>% # add to the list of links, the sputnik lists, and consensus ranking
  bind_rows(
    read_csv('value, input_value
             STAFF,https://www.sputnikmusic.com/blog/2017/12/20/staffs-top-50-albums-of-2017-50-31/    
             SPUTUSERS,https://www.sputnikmusic.com/blog/2018/01/09/users-top-50-albums-of-2017-50-31/
             SPUTRATINGS,https://www.sputnikmusic.com/blog/2018/01/02/2017-reflections-of-a-sputnik-world/
             CONSENSUS,https://docs.google.com/spreadsheets/d/1-uY5_BeeKH_e2-XROri85qpV8s_5hMEEoFoxigci61I/edit?usp=sharing')) %>% # add names of new rank colums into year_links
  mutate(input_value = input_value %>% 
           str_replace_all('"|=HYPERLINK[(]','') %>%
           str_split(',', simplify = TRUE) %>% 
           .[, 1]) %>%
  arrange(input_value) %>%
  set_names(c('AOTY_Column', 'URL_link'))

## new rankings -----
new_ranks <- all_dat %>%
  select(c(1,which(names(.) %in% year_links$AOTY_Column))) %>% # select columns with ranks, and albums
  select(ALBUM, STAFF, SPUTUSERS, SPUTRATINGS, names(.) %>% sort(), -CONSENSUS) %>% # reorder them
  gather(Lists, new_rank, STAFF:WIRE) %>% # wide to long
  mutate_all(~ifelse(is.na(.x), 75, .x)) # replace NA values with 75
new_ranks <- new_ranks %>% 
  group_by(ALBUM) %>%
  summarise(new_rank = mean(new_rank)) %>% # average ranks for each album (i.e. make consensus score)
  ungroup() %>%
  mutate(new_rank = min_rank(new_rank)) %>% # average the ranks
  arrange(new_rank) %>%
  full_join(all_dat %>%
              select(ALBUM, `CONS SCORE`) %>%
              mutate(orig_rank = min_rank(`CONS SCORE`)) %>%
              select(ALBUM, orig_rank)) %>% # join new ranks with the original ranks from the AOTY list
  filter(new_rank <= 50) %>% # keep just the top 50 new ranked albums
  arrange(new_rank) %>%
  mutate(change = orig_rank-new_rank,
         change = as.character(change) %>%
           map_chr(~ifelse(grepl('^-|^0', .x), .x, paste0('+',.x)))) %>%
  select(new_rank, orig_rank, change, ALBUM) 

new_ranks %>% 
  knitr::kable('html', align = 'l')

## similarity plot ----
ranks_mat <- all_dat %>%
  select(which(names(.) %in% year_links$AOTY_Column)) %>% # select columns with ranks
  select(CONSENSUS, STAFF, SPUTUSERS, SPUTRATINGS, names(.) %>% sort()) %>%
  mutate_all(~ifelse(is.na(.x), 0, .x)) # zero out NA values

cos_mat <- cos.sim(ranks_mat %>% as.matrix() %>% t(),
                   ranks_mat %>% as.matrix() %>% t()) %>% # make cosine matrix
  get_lower_tri() %>% # replace upper triangle with NA's
  melt() %>%
  set_names(c('List1','List2','Cosine_sim'))


# plot the similarity matrix
cos_plot <- ggplot(data = cos_mat, aes(List1, List2, fill = Cosine_sim))+
  geom_tile(color = "white",na.rm = TRUE) +
  scale_fill_gradient2(limit = c(0,.55),
                       name = 'Similarity') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, 
                                   size = 10, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  coord_fixed() + 
  ggtitle('Cosine Similarity Matrix Plot') 
print(cos_plot)
cos_plot +
  ggsave(str_c(aoty_dir, 'cos_sim.png'), height = 10, width = 8.6)

cos_mat %>% 
  arrange(-Cosine_sim) %>%
  filter(Cosine_sim < 1) %>%
  .[1:20, ] %>%
  knitr::kable('html', digits = 3, align = 'l') 
