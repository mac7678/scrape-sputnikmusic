library(tidyverse)
library(optimx)
# library()

## Functions ------
loss_fun <- function(X, y, par){  
  yhat <- apply(X, 1, 
                function(x) weighted.mean(x, par, na.rm = TRUE))
  mse <- mean((yhat - y)^2, na.rm = TRUE) # rmse
  mse <- mse + 100*length(which(par <= 0.001 | par >= 1)) # negative/>1 penalty
  mse <- abs(sum(par)-1)*100 + mse # not sum to 1 penalty
  if(max(par)/min(par) > 15 | max(par)/min(par) < 2) mse <- mse + 100 # high/low ratio penalty
  return(mse) 
}

cv_split <- function(x, k){
  x <- sample(x)
  starts <- c(seq(1, 
                  length(x), 
                  length(x)/k) %>% 
                round(0), 
              max(x)+1) %>%
    unique()
  splits <- list()
  for(i in 1:k){
    splits[[i]] <- x[starts[i]:(starts[i+1]-1)]
  }
  out <- list()
  for(i in 1:k){
    out[[i]] <- list(splits[[i]], splits[-i] %>% unlist())
  }
  return(out)
}
opt_train_test <- function(X_train, y_train, X_test, y_test, weights){
  opt <- optimx(weights,fn=loss_fun, X = X_train, y = y_train, 
                method =  c('BFGS', 'CG', 
                            'L-BFGS-B',
                            'spg', 
                            'newuoa'))
  weights_list <- map(1:nrow(opt), 
                      ~ opt[.x, 1:length(weights)] %>%
                        unlist() %>% as.vector())
  opt$test_RMSE <- map_dbl(weights_list, ~loss_fun(X_test, y_test, .x))
  opt$weights <- weights_list
  opt$opt_type <- rownames(opt)
  opt <- opt %>% 
    as_tibble() %>%
    select(-str_which(names(.),'^p[0-9]')) %>%
    rename(train_RMSE = value) %>%
    select(opt_type, weights, train_RMSE, test_RMSE, everything())
  return(opt)
}
main_opt_weights <- function(X, y, weights, k){
  ind <- cv_split(1:nrow(X), k)
  split_X <- map(ind, ~list(X_test = X[.x[[1]], ], 
                            X_train = X[.x[[2]], ]))
  split_y <- map(ind, ~list(y_test = y[.x[[1]]], 
                            y_train = y[.x[[2]]]))
  out <- map2(split_X, split_y,
              ~opt_train_test(.x$X_train, .y$y_train, 
                              .x$X_test, .y$y_test, 
                              weights))
  out <- map_df(1:k, ~out[[.x]] %>%
                  mutate(fold = .x))
  return(out)
}

## Files/IO ----------
fil <- './metacritic/publications_matrix.csv'
dat <- read_csv(fil)


## Script ---------
X <- dat[,2:length(dat)] %>% as.matrix() # publications scores
y <- dat$total_meta_score # metascore
weights <- rep(1/ncol(X),ncol(X)) # initialized weights
k <- 3 # folds of the crossvalditaion
set.seed(7143432) # guess the significance of this, if you walk through the garden
opt_dat <- main_opt_weights(X, y, weights, k)
opt_dat_sum <- opt_dat %>% # average the k-folds 
  select(-fold) %>%
  group_by(opt_type) %>%
  summarise_all(~ ifelse(is.list(.x), do.call(rbind, .x) %>% 
                           apply(2, function(x) mean(x)) %>%
                           list(),
                         mean(.x)))

top_opt <- opt_dat_sum %>% # extract top 3 optimizations
  arrange(test_RMSE) %>%
  .[1:3, ] %>%
  mutate(weights = map(weights, ~ .x/max(.x))) %>% # scale weights to max of weights
  unnest(weights) %>%
  mutate(publication = rep(colnames(X), k))

top_opt <- map_df(unique(top_opt$opt_type), # prepare for the ggplot
                  ~ top_opt %>% 
                    filter(opt_type == .x) %>%
                    mutate(rank = min_rank(desc(weights))) %>%
                    mutate(sput_val = .$weights[.$publication == 'Sputnikmusic']))
plt <- ggplot(top_opt, aes(x=weights, fill = opt_type))+ 
  geom_vline(aes(xintercept = sput_val), colour="black") +
  geom_histogram(binwidth=0.05,colour = "black") +
  facet_wrap(~opt_type) +
  ggtitle('Histogram of Publication Weights')
plt +
  ggsave('./metacritic/weights_hist.jpg',height = 5, width = 10)
             