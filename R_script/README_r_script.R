## ----libraries, echo=TRUE, message=FALSE, warning=FALSE------------------
library("ggplot2")
library("grid")
library("dplyr")
library("tools")
library("tidyverse")
library("acs")
library("reshape2")
library("readr")
library("tidytext")
library("scales")
library("ggplot2")
library("ggrepel")
library("broom")
library("fitdistrplus")
library("rstan")
library("knitr")
library("rmarkdown")

## ----get_data, comment = '', cache=TRUE----------------------------------
#api.key.install("YOUR KEY HERE!")
## population data
stategeo <- geo.make(state = "*")
popfetch <- acs.fetch(geography = stategeo, 
                      endyear = 2014,
                      span = 5, 
                      table.number = "B01003",
                      col.names = "pretty")
## song lyrics
lyrics_url <- "https://raw.githubusercontent.com/walkerkq/musiclyrics/master/billboard_lyrics_1964-2015.csv"
save_lyrics_loc <- "~/Documents/R_Local/Put_a_prior_on_it-Blog_post/data/billboard_lyrics_1964-2015.csv"
# download.file(lyrics_url, save_lyrics_loc)
song_lyrics <- read_csv(save_lyrics_loc)
cities_dat_loc <- "~/Documents/R_Local/Put_a_prior_on_it-Blog_post/data/cities_over_100k_pop.csv"
state_abbrev_loc <- "~/Documents/R_Local/Put_a_prior_on_it-Blog_post/data/state_abbrev.csv"
state_abbrev <- read_csv(state_abbrev_loc)

## ----population_data,comment = ''----------------------------------------
# extract desired info from acs data
pop_df <- tbl_df(melt(estimate(popfetch))) %>%
  mutate(name = as.character(Var1),
         state_name = tolower(Var1),
         pop2014 = value) %>%
  dplyr::select(name, state_name, pop2014) %>%
  filter(state_name != "puerto rico") %>%
  left_join(state_abbrev)

# clean in city names
cities <- read_csv(cities_dat_loc) %>%
  mutate(city = gsub("\x96", "-", city),
         city_name = tolower(city),
         state_name = tolower(state))

# extract and tidy lyrics from songs data
tidy_lyrics <- bind_rows(song_lyrics %>% 
               unnest_tokens(lyric, Lyrics),
               song_lyrics %>% 
               unnest_tokens(lyric, Lyrics, 
               token = "ngrams", n = 2))

## ----data_summarize, comment=''------------------------------------------
# join and retain songs whether or not they are in the lyrics
tidy_lyrics_state_zeros <- right_join(tidy_lyrics, pop_df,
                                by = c("lyric" = "state_name")) %>%
  distinct(Song, Artist, lyric, .keep_all = TRUE) %>% 
  mutate(cnt = ifelse(is.na(Source), 0, 1)) %>%
  filter(lyric != "district of columbia") %>%
  dplyr::rename(state_name = lyric)

tidy_lyrics_state <- filter(tidy_lyrics_state_zeros, cnt > 0)

## count the states up
zero_rate <- 0.0000001 # beta hates zeros

# group, summarise, and calculate rate per 100k population
state_counts_zeros <- tidy_lyrics_state_zeros %>% 
  group_by(state_name) %>% 
  dplyr::summarise(n = sum(cnt))  %>% # sum(cnt)
  left_join(pop_df, by = c("state_name" = "state_name")) %>%
  mutate(rate = (n / (pop2014 / 100000)) + zero_rate) %>%
  arrange(desc(n))

# create another data set with 
state_counts <- filter(state_counts_zeros, rate > zero_rate)
print(dplyr::select(state_counts, state_name, n ,pop2014, rate))

## ----cities, comment=''--------------------------------------------------
### Cities
## join cities together - inner_join b/c I don't care about cities with zero mentions (right_join otherwise)
tidy_lyrics_city <- inner_join(tidy_lyrics, cities,
                               by = c("lyric" = "city_name")) %>%
  distinct(Song, Artist, lyric, .keep_all = TRUE) %>%
  filter(!city %in% c("Surprise", "Hollywood", 
                      "District of Columbia", "Jackson",
                    "Independence")) %>%
  mutate(cnt = ifelse(is.na(Source), 0, 1)) %>%
  dplyr::rename(city_name = lyric)

# count cities mentions. No need for a rate; not of use now
city_counts <- tidy_lyrics_city %>% 
  group_by(city_name) %>% 
  dplyr::summarise(n = sum(cnt)) %>%
  arrange(desc(n))
print(city_counts)

# count of states that host the cities mentioned
city_state_counts <- tidy_lyrics_city %>% 
  group_by(state_name) %>% 
  dplyr::summarise(n = sum(cnt)) %>%
  arrange(desc(n))
print(city_state_counts)

## ----city_state_join, comment=''-----------------------------------------
state_city_counts_zeros <- left_join(state_counts_zeros,
                                     city_state_counts,
                                     by = "state_name") %>%
  dplyr::rename(n_state = n.x, n_city = n.y) %>%
  mutate(n_city = ifelse(is.na(n_city), 0, n_city),
         n_city_state = n_state + n_city,
         city_state_rate = (n_city_state / (pop2014 / 100000)) + zero_rate)

# same as above, but no states with zero mentioned by city or state
state_city_counts <- filter(state_city_counts_zeros, n_city_state > zero_rate)

## ----fun_facts, comment=''-----------------------------------------------
# Boston = most mentioned city without its state
all_the_cities <- filter(state_city_counts, !state_name %in% state_counts$state_name) %>%
  dplyr::select(name) %>%
  mutate_if(is.factor, as.character) %>%
  left_join(tidy_lyrics_city, by = c("name" = "state")) %>%
  dplyr::select(name, Song, Artist, city)

kable(all_the_cities)

## ----fun_facts2, comment=''----------------------------------------------
n_states_mentioned <- tidy_lyrics_state %>%
  group_by(Artist, Song) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(5)
kable(n_states_mentioned)

# Top song is...
filter(tidy_lyrics_state, Song == as.character(n_states_mentioned[1,1])) %>%
  dplyr::select(Song, Artist, Year, state_name)

## ----fun_facts3, comment=''----------------------------------------------
most_repeated_in_song <- right_join(tidy_lyrics, pop_df,
                                by = c("lyric" = "state_name")) %>%
  group_by(Song, Artist, lyric) %>%   
  dplyr::summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(row_number() <= 10)

kable(most_repeated_in_song)

## ----fitdistr, comment=''------------------------------------------------
## beta boot for comparison
beta_fit <- fitdist(state_counts_zeros$rate,"beta") # best logLik
summary(beta_fit)
exp_fit <- fitdist(state_counts_zeros$rate,"exp")
summary(exp_fit)
lnorm_fit <- fitdist(state_counts_zeros$rate,"lnorm")
summary(lnorm_fit)

## ----stan_optim, message=FALSE, warning=FALSE, comment='', include=TRUE, results="hide", cache = TRUE----
opt_chr1 <- "
data {
  int<lower=0> N;
  real x[N];
}
parameters {
  real<lower = 0> alpha0;
  real<lower = 0> beta0;
}
model {
  alpha0 ~ normal(0, 1);
  beta0 ~ normal(0, 10);
  //target += beta_lpdf(x | alpha0, beta0); // same as below
  x ~ beta(alpha0, beta0);
}
"
# initialize parameter values (based on knowledge or fitdist results)
init_list <- list(alpha0 = 0.1, beta0 = 1)
# compile model (~ 10 to 15 seconds)
opt_mod1 <- stan_model(model_code = opt_chr1)
# optimize data given model
opt1 <- optimizing(object = opt_mod1, as_vector = FALSE,
                   data = list(x = state_counts_zeros$rate,
                               N = length(state_counts_zeros$rate)),
                   hessian = TRUE,
                   draws = 2500)

## ----optim_results, comment=''-------------------------------------------
# view results
opt1$par 
opt1$value #compare to LogLikelihood of summary(beta_fit)

## ----param_plot, comment='', fig.align="center"--------------------------
ggplot(data.frame(opt1$theta_tilde), aes(x = alpha0, y = beta0)) +
  geom_point(color = "skyblue3", alpha = 0.35) +
  geom_density2d(aes(colour =..level..)) + 
  scale_colour_gradient(low="gray80",high="firebrick") + 
  scale_x_continuous(breaks = seq(0,0.3,0.025)) +
  scale_y_continuous(breaks = seq(0,7,0.5)) +
  theme_bw() +
    labs(x = "alpha0",
       y = "beta0",
       title = "Distribution of Alpha and Beta Shape Parameters",
       subtitle = "2500 samples from MLE optimized beta model posterior") +
 theme(
    panel.border = element_rect(colour = "gray90"),
    axis.text.x = element_text(size = 8, family = "Trebuchet MS"),
    axis.text.y = element_text(size = 8, family = "Trebuchet MS"),
    axis.title = element_text(size = 10, family = "Trebuchet MS"),
    plot.caption = element_text(size = 7, hjust=0, margin=margin(t=5), 
                                family = "Trebuchet MS"),
    plot.title=element_text(family="TrebuchetMS-Bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
    )

## ---- stan_fit, comment='', cache=TRUE-----------------------------------
model_string1_pred <- "
data {
  int<lower=1> N;
  vector[N] x;
  int<lower=1> M;
  vector[M] new_success;
  vector[M] new_attempts;
}
parameters {
  real<lower=0> alpha0;
  real<lower=0> beta0;
}
model {
  alpha0 ~ normal(0, 1);
  beta0 ~ normal(0, 10);
  x ~ beta(alpha0, beta0);
} generated quantities {
  vector[M] x_tilde; 
  for (n in 1:M)
    x_tilde[n] = beta_rng((new_success[n] + alpha0),
                (new_attempts[n] - new_success[n]  + beta0));
}
"

## ----sta_fit_data, message=FALSE, warning=FALSE, comment='', include=TRUE, results="hide"----
new_success = state_counts_zeros$n
new_attempts = (state_counts_zeros$pop2014)/100000
model_dat1_pred <- list(x = state_counts_zeros$rate, 
                   N = length(state_counts_zeros$rate),
                   new_success = new_success,
                   new_attempts = new_attempts,
                   M = length(new_success))
fit1_pred <- stan(model_code = model_string1_pred, 
                  data = model_dat1_pred,
                  iter = 10000, chains = 4,  warmup=2500)

## ----print_dat1_model, comment=''----------------------------------------
fit1_pred_summary <- data.frame(summary(fit1_pred)[["summary"]]) %>%
  rownames_to_column() %>%
  mutate(Parameter = c("alpha0", "beta0",
                  as.character(state_counts_zeros$name), "lp__")) %>%
  dplyr::select(Parameter, mean, sd, X2.5., X97.5., n_eff, Rhat) %>%
  dplyr::rename(Mean = mean,
                SD = sd,
                `2.5%` = X2.5.,
                `97.5%` = X97.5.)
kable(fit1_pred_summary, digits = 3)

## ----state_estimate, message=FALSE, warning=FALSE, comment='', fig.height=6, fig.width=6, fig.align="center"----
state_estimates <- rstan::extract(fit1_pred, pars = "x_tilde") %>%
  data.frame() %>%
  rename_(.dots=setNames(names(.),state_counts_zeros$state_name)) %>%
  gather() %>%
  dplyr::rename(state_name = key) %>%
  group_by(state_name) %>%
  dplyr::summarise(q025 = quantile(value, probs = 0.025),
                   q5 = quantile(value, probs = 0.5),
                   q975 = quantile(value, probs = 0.975),
                   mean = mean(value)) %>%
  left_join(.,state_counts_zeros)

## ----post_pred_state_plot, comment=''------------------------------------
### could melt and add q025,q5,q975 by color/shape
### could also predict across range of rates and show areas

ggplot(state_estimates, aes(rate, mean)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = 2) +
  geom_point(size = 4, aes(color = n)) +
  geom_text_repel(aes(label = abbrev), stat = "identity",
                  point.padding = unit(0.5, "lines"),
                  max.iter = 5000) +
  scale_color_gradient(low = "midnightblue", high = "pink",
                       name="Number\nof songs") +
  labs(title = "States in Song Lyrics with Empirical Bayes",
       subtitle = "States like Montana and Hawaii (high rates, few mentions) are shifted the most",
       x = "Measured rate of mentions per 100k population",
       y = "Mean predicted rate per 100k population",
       caption = "plot design by @juliasilge") +
  theme_minimal(base_family = "Trebuchet MS") +
  theme(plot.title=element_text(family="Trebuchet MS"))

## ----state_estim_plot, comment='', fig.height=8, fig.width=7, fig.align="center"----
state_estimates %>% 
  arrange(desc(mean)) %>% 
  mutate(state_name = factor(name, levels = rev(unique(name)))) %>%
  dplyr::select(state_name, 'Measured rate' = rate, 
         'Bayesian estimate' = mean, q025, q975) %>% 
  gather(type, rate, `Measured rate`, `Bayesian estimate`) %>%
  ggplot(aes(rate, state_name, color = type)) +
  geom_errorbarh(aes(xmin = q025, xmax = q975), color = "gray50") +
  geom_point(size = 3) +
  xlim(0, NA) +
   labs(x = "Rate of mentions per 100k population",
       y = NULL, title = "Measured Rates, Bayesian Estimates (HMC), and 95% Credible Intervals",
       subtitle = "Mention rate for states sorted by descending posterior mean",
       caption = "plot design by @juliasilge") +
  theme_minimal(base_family = "Trebuchet MS") +
  theme(plot.title=element_text(family="Trebuchet MS", face = "bold")) +
  theme(legend.title=element_blank())

## ----state_city_fit, comment=''------------------------------------------
new_success_SC = state_city_counts_zeros$n_city_state
new_attempts_SC = (state_city_counts_zeros$pop2014)/100000
model_SC_pred <- list(x = state_city_counts_zeros$city_state_rate, 
                   N = length(state_city_counts_zeros$city_state_rate),
                   new_success = new_success_SC,
                   new_attempts = new_attempts_SC,
                   M = length(new_success_SC))

## ----fit_SC_model, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE, comment='', results="hide"----
fit_SC_pred <- stan(model_code = model_string1_pred, 
                  data = model_SC_pred,
                  iter = 10000, chains = 4,  warmup=2500)

## ----print_model_SC_pred, comment=''-------------------------------------
fit_SC_pred_summary <- data.frame(summary(fit_SC_pred)[["summary"]]) %>%
  rownames_to_column() %>%
  mutate(Parameter = c("alpha0", "beta0",
                  as.character(state_counts_zeros$name), "lp__")) %>%
  dplyr::select(Parameter, mean, sd, X2.5., X97.5., n_eff, Rhat) %>%
  dplyr::rename(Mean = mean,
                SD = sd,
                `2.5%` = X2.5.,
                `97.5%` = X97.5.)
kable(fit_SC_pred_summary,  digits = 3)

## ----state_city_estimates, comment=''------------------------------------
state_city_estimates <- rstan::extract(fit_SC_pred, pars = "x_tilde") %>%
  data.frame() %>%
  rename_(.dots=setNames(names(.),state_city_counts_zeros$state_name)) %>%
  gather() %>%
  dplyr::rename(state_name = key) %>%
  group_by(state_name) %>%
  dplyr::summarise(q025 = quantile(value, probs = 0.025),
                   q5 = quantile(value, probs = 0.5),
                   q975 = quantile(value, probs = 0.975),
                   mean = mean(value)) %>%
  left_join(.,state_city_counts_zeros)

## ----state_city_dot_plot, comment='', fig.height=6, fig.width=6, fig.align="center"----
### could melt and add q025,q5,q975 by color/shape
### could also predict across range of rates and show areas
ggplot(state_city_estimates, aes(city_state_rate, mean)) +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = 2) +
  geom_point(size = 4, aes(color = n_city_state)) +
  geom_text_repel(aes(label = abbrev), stat = "identity",
                  point.padding = unit(0.5, "lines"),
                  max.iter = 5000) +
  scale_color_gradient(low = "midnightblue", high = "pink",
                       name="Number\nof songs") +
  labs(title = "States & Cities in Song Lyrics Modeled with Bayes (HMC)",
       subtitle = "States like Nebraska and Hawaii (high rates, few mentions) are shifted the most",
       x = "Measured rate of mentions per 100k population",
       y = "Mean predicted rate per 100k population",
       caption = "plot design by @juliasilge") +
  theme_minimal(base_family = "Trebuchet MS") +
  theme(plot.title=element_text(family="Trebuchet MS"))

## ----state_city_range_plot, comment='', fig.height=8, fig.width=7, fig.align="center"----
### range estiamtes plot
state_city_estimates %>% 
  arrange(desc(mean)) %>% 
  mutate(state_name = factor(name, levels = rev(unique(name)))) %>%
  dplyr::select(state_name, 'Measured rate' = city_state_rate, 
         'Bayesian estimate' = mean, q025, q975) %>% 
  gather(type, city_state_rate, `Measured rate`, `Bayesian estimate`) %>%
  ggplot(aes(city_state_rate, state_name, color = type)) +
  geom_errorbarh(aes(xmin = q025, xmax = q975), color = "gray50") +
  geom_point(size = 3) +
  xlim(0, NA) +
  labs(x = "Rate of mentions per 100k population",
       y = NULL, title = "Measured Rates, Bayesian Estimates (HMC), and 95% Credible Intervals",
       subtitle = "Mention rate for states & cities sorted by descending posterior mean",
       caption = "plot design by @juliasilge") +
  theme_minimal(base_family = "Trebuchet MS") +
  theme(plot.title=element_text(family="Trebuchet MS", face = "bold")) +
  theme(legend.title=element_blank())

## ----model_RMSE, comment=''----------------------------------------------
city_state_error <- state_city_estimates %>%
  mutate(rate_error = mean - city_state_rate,
         pred_mentions = round(mean * (pop2014/100000),1),
         mention_error = n_city_state - pred_mentions,
         CI_width = q975 - q025) %>%
  dplyr::summarise(RMSE_rate = sqrt(mean(rate_error^2)),
            MAE_rate = mean(abs(rate_error)),
            mean_CI = mean(CI_width),
            RMSE_mentions = sqrt(mean(mention_error^2)),
            MAE_mentions = mean(abs(mention_error))) %>%
  as.numeric()

state_error <- state_estimates %>%
  mutate(rate_error = mean - rate,
         pred_mentions = round(mean * (pop2014/100000),1),
         mention_error = n - pred_mentions,
         CI_width = q975 - q025) %>%
  dplyr::summarise(RMSE_rate = sqrt(mean(rate_error^2)),
            MAE_rate = mean(abs(rate_error)),
            median_CI = median(CI_width),
            RMSE_mentions = sqrt(mean(mention_error^2)),
            MAE_mentions = mean(abs(mention_error))) %>%
  as.numeric()

#print
model_rmse <- data.frame(model = c("States Only", "City and States"),
           RMSE_rate =  c(state_error[1], city_state_error[1]),
           MAE_rate =  c(state_error[2], city_state_error[2]),
           Median_CI =  c(state_error[3], city_state_error[3]),
           RMSE_mentions =  c(state_error[4], city_state_error[4]),
           MAE_mentions =  c(state_error[5], city_state_error[5]))
kable(model_rmse, digits = 3)

## ----purl, comment=''----------------------------------------------------
rmd_loc <- "/Users/mattharris/Documents/R_Local/Put_a_prior_on_it-Blog_post/"
purl(input  = paste0(rmd_loc, "README.Rmd"), 
     output = paste0(rmd_loc, "R_script/README_r_script.R"))

## ----session_info, comment=''--------------------------------------------
sessionInfo()

