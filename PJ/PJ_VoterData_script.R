library(tidyverse)
library(scales)

## files/io ----------
pj <- read_csv('./PJ/PJ_vote.csv') %>%
  mutate(poll = 'P&J')
sput <- read_csv('./PJ/Sput_vote.csv') %>%
  mutate(poll = 'Sput')

## exclude rule breakers ----
pj_stop <- pj %>%
  group_by(Voter_id) %>%
  summarise(n = n(),
            sum = sum(Album_Points)) %>%
  filter(n != 10 | sum != 100)
pj <- pj %>%
  anti_join(pj_stop)

sput_stop <- sput %>%
  group_by(Voter_id) %>%
  summarise(n = n(),
            sum = sum(Album_Points)) %>%
  filter(n != 10 | sum != 100)
sput <- sput %>%
  anti_join(sput_stop)
## histograms ----

hist_dat <- pj %>%
  bind_rows(sput) %>%
  mutate_at(vars(Album_Points), as.factor) %>%
  group_by(Album_Points, poll) %>%
  count() %>%
  ungroup()
hist_dat <- hist_dat %>%
  group_by(poll) %>%
  summarise(sum = sum(n)) %>%
  left_join(hist_dat) %>%
  mutate(n = n/sum)

hist_plt <- hist_dat %>%
  ggplot(aes(Album_Points, n, fill = poll)) +
  facet_wrap(~poll, nrow = 2) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .05),
                     labels = scales::percent) +
  ylab("Percent Frequency") +
  ggtitle('Histogram of Allotted Album Points')
hist_plt +
  ggsave('./PJ/Sput_PJ_PointsHist.jpg', height = 7.5, width = 10)

## categories ----
cat_dat <- pj %>%
  bind_rows(sput) %>%
  group_by(Voter_id, poll) %>%
  summarise(all10s = all(Album_Points == 10),
            base5 = !all(Album_Points == 10) &&
              all((Album_Points %% 5) ==0),
            no_base5 = all(!(Album_Points %% 5) ==0),
            all_unique = length(unique(Album_Points))==10) %>%
  ungroup() %>%
  group_by(poll) %>%
  summarise_all(mean) %>%
  gather(Type, Percent, all10s:all_unique) %>%
  mutate(Type = factor(Type,
                       levels = c('all10s','base5','all_unique','no_base5'),
                       ordered = TRUE))
cat_plt <- cat_dat %>%
  ggplot(aes(Type, Percent, fill = poll)) +
  facet_wrap(~poll) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(breaks = seq(0, 1, .05),
                     labels = scales::percent) +
  ylab("Percent Frequency") +
  ggtitle('Bar Plot of Voter Types')
cat_plt +
  ggsave('./PJ/Sput_PJ_CategoryHist.jpg', height = 7.5, width = 10)
