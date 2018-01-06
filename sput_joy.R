library(tidyverse)
library(ggjoy)
library(wesanderson)
library(broom)

# functions -----
sput_wavg <- function(dat, weights=c(1,3,2,1)){
  dat <- dat %>% 
    group_by(Band_Album) %>%
    summarise(w.mean = weighted.mean(c(weighted.mean(Rating,sqrt(ratings)),
                                       weighted.mean(Rating,sqrt(reviews)),
                                       weighted.mean(Rating,sqrt(lists)),
                                       weighted.mean(Rating,sqrt(comments))),
                                     weights)
    ) %>%
    ungroup()
  return(dat)
}
boot_albs <- function(dat, weights = c(1,3,2,1)){
  dat <- dat[sample(1:nrow(dat), replace = TRUE), ] %>%
    sput_wavg(weights)
  return(dat)
}
# Script ----
dat <- read_csv('2017_UserRatings.csv') %>%
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x))
dat_order <- read_csv('2017_UserRatings_order.csv')
joy_dir <- './joyplots/'
if(!dir.exists(joy_dir)) dir.create(joy_dir)

set.seed(01052018)
out <- list()
boots <- 1000
for(i in unique(dat_order$Band_Album)){
  out[[i]] <- map_df(1:boots, ~ dat %>%
                       filter(Band_Album == i) %>%
                       boot_albs())
  writeLines(paste0(i, ' COMPLETED'))
}
out <- out %>%
  bind_rows() %>% 
  mutate(Band_Album = factor(Band_Album, levels = dat_order$Band_Album, ordered = TRUE))

# standard ggjoy plot
base_plot <- ggplot(out, aes(x = w.mean, y = Band_Album)) + 
  geom_joy() +
  theme(axis.title.y=element_blank()) +
  ggtitle('JoyPlot')
base_plot +
  ggsave(paste0(joy_dir, 'standard_joyplot.png'),height = 4, width = 8)

# wessy anderson plot
wes_plot <- ggplot(out, aes(x = w.mean, y = Band_Album, fill = Band_Album)) + 
  geom_joy() +
  theme_minimal() +
  scale_fill_cyclical(values = c(wes_palette("Royal1"), 
                                 wes_palette("Royal2"),
                                 wes_palette("Rushmore"),
                                 wes_palette("Moonrise3"))) +
  theme(panel.background = element_rect(wes_palette("GrandBudapest2")[2])) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Wes Anderson JoyPlot')
wes_plot +
  ggsave(paste0(joy_dir, 'wes_joyplot.png'),height = 4, width = 8)

# code modified from
# https://github.com/alexwhan/alexwhan.github.io/blob/master/_source/2016-03-24-joy-division-plot.Rmd -----

j3 <- out %>% 
  mutate(Band_Album = Band_Album %>% as.integer(),
         B_A = rev(as.numeric(Band_Album)),
         w.mean = scale(w.mean)[,1]) %>% 
  arrange(Band_Album) %>%
  rename(x = w.mean) %>%
  group_by(Band_Album, B_A) %>% 
  do(tidy(density(.$x, n = 100))) %>% 
  group_by() %>% 
  mutate(ymin = B_A * (max(y) / 7), #This constant controls how much overlap between groups there is
         ymax = y + ymin)


j4 <- j3 %>% 
  group_by(Band_Album, B_A) %>% 
  do(data.frame(approx(.$x, .$ymax, xout = seq(min(j3$x), max(j3$x), length.out = 250)))) %>% 
  mutate(y = ifelse(is.na(y), j3$ymin[j3$Band_Album == Band_Album][1], y),
         ymin = j3$ymin[j3$Band_Album == Band_Album][1],
         ymaxN = y + rnorm(n(), 0.001, 0.005)) %>% 
  arrange(x) %>% 
  mutate(ymaxN = ifelse(row_number() %in% c(1, n()), ymin + min(ymaxN - ymin), ymaxN))

j4$ymaxS <- smooth(j4$ymaxN, kind = "S", endrule = "copy", do.ends = FALSE)
p <- ggplot()
for (i in rev(unique(j4$Band_Album))) {
  p <- p + geom_ribbon(data = j4[j4$B_A == i,], 
                       aes(x = x, ymin = ymin + min(j4$ymaxN - j4$ymin), ymax = ymaxS, group = B_A), 
                       colour = "#F0F0F0", fill = "black") +
    geom_hline(yintercept = j4$ymin[j4$B_A == i][1] + min(j4$ymaxN - j4$ymin), colour = "#000000")
}
p <- p + 
  coord_fixed(5) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
p +
  ggsave(paste0(joy_dir, 'faithful_joyplot.png'),height = 9, width = 4)

## BONUS, Unknown Pleasure's JoyPlot ----
joy_dat <- read_csv('Joy-Division-Unknown-Pleasures.csv') %>%
  mutate(ratings = map_dbl(ratings, ~ifelse(.x %>% is.na(), 1L, .x)) %>% as.integer) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.x), 0, .x))

set.seed(556666777)
weights <- tibble(names = as.character(1:50),
                  w1 = runif(50),
                  w2 = runif(50),
                  w3 = runif(50),
                  w4 = runif(50))
joy <- list()
boots <- 1000
for(i in 1:nrow(weights)){
  tmp_weights <- weights[i, 2:5 ] %>% as_vector()
  joy[[i]] <- map_df(1:boots, ~ joy_dat %>%
                                 boot_albs(tmp_weights)) %>%
    mutate(Band_Album = weights$names[i])
  print(paste0(i, ' COMPLETED'))
}
joy <- joy %>%
  bind_rows()
# it's copypasted, blah i know
j3 <- joy %>% 
  mutate(Band_Album = Band_Album %>% as.integer(),
         B_A = rev(as.numeric(Band_Album)),
         w.mean = scale(w.mean)[,1]) %>% 
  arrange(Band_Album) %>%
  rename(x = w.mean) %>%
  group_by(Band_Album, B_A) %>% 
  do(tidy(density(.$x, n = 100))) %>% 
  group_by() %>% 
  mutate(ymin = B_A * (max(y) / 7), #This constant controls how much overlap between groups there is
         ymax = y + ymin)


j4 <- j3 %>% 
  group_by(Band_Album, B_A) %>% 
  do(data.frame(approx(.$x, .$ymax, xout = seq(min(j3$x), max(j3$x), length.out = 250)))) %>% 
  mutate(y = ifelse(is.na(y), j3$ymin[j3$Band_Album == Band_Album][1], y),
         ymin = j3$ymin[j3$Band_Album == Band_Album][1],
         ymaxN = y + rnorm(n(), 0.001, 0.005)) %>% 
  arrange(x) %>% 
  mutate(ymaxN = ifelse(row_number() %in% c(1, n()), ymin + min(ymaxN - ymin), ymaxN))

j4$ymaxS <- smooth(j4$ymaxN, kind = "S", endrule = "copy", do.ends = FALSE)
p <- ggplot()
for (i in rev(unique(j4$Band_Album))) {
  p <- p + geom_ribbon(data = j4[j4$B_A == i,], 
                       aes(x = x, ymin = ymin + min(j4$ymaxN - j4$ymin), ymax = ymaxS, group = B_A), 
                       colour = "#F0F0F0", fill = "black") +
    geom_hline(yintercept = j4$ymin[j4$B_A == i][1] + min(j4$ymaxN - j4$ymin), colour = "#000000")
}
p <- p + 
  coord_fixed(4) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
p +
  ggsave(paste0(joy_dir, 'joyplot_joyplot.png'),height = 9, width = 4)
