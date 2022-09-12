library(tidyverse)
library(ashr)

dat <- read_csv("data/standard2015NP.csv") %>% add_column(Year = 2015) %>%
  bind_rows(read_csv("data/standard2016NP.csv") %>% add_column(Year = 2016)) %>%
  bind_rows(read_csv("data/standard2017NP.csv") %>% add_column(Year = 2017)) %>%
  bind_rows(read_csv("data/standard2018NP.csv") %>% add_column(Year = 2018)) %>%
  bind_rows(read_csv("data/standard2019NP.csv") %>% add_column(Year = 2019)) %>%
  select(playerid, Name, Year, PA, BB) %>%
  filter(BB > 0, BB < PA)
  
last_yr <- dat %>% group_by(playerid) %>% summarize(LastYear = max(Year))

dat <- dat %>%
  left_join(last_yr, by = "playerid") %>%
  mutate(Year = LastYear - Year)

dat <- dat %>%
  gather(Stat, Count, -playerid, -Name, -Year, -LastYear) %>%
  unite(tmp, Stat, Year) %>%
  spread(tmp, Count)

dat <- dat %>%
  mutate_all(~replace_na(., 0)) %>%
  filter(PA_1 > 0)

set.seed(666)
test.dat <- dat %>% sample_frac(0.2)
train.dat <- dat %>% setdiff(test.dat)

binom.llik <- function(PA, BB, preds) {
  return(sum(BB * log(preds) + (PA - BB) * log(1 - preds)))
}
rmse <- function(PA, BB, preds) {
  return(sqrt(mean((preds - BB / PA)^2)))
}
mae <- function(PA, BB, preds) {
  return(mean(abs(preds - BB / PA)))
}

# Naive predictions: just use previous year's walk rate.
binom.llik(test.dat$PA_0, test.dat$BB_0, test.dat$BB_1 / test.dat$PA_1) # -12711

BBrate <- function(df, wt, g = NULL, fixg = FALSE) {
  df <- df %>% 
    mutate(BB = round(wt * BB_1 + wt^2 * BB_2 + wt^3 * BB_3 + wt^4 * BB_4),
           PA = round(wt * PA_1 + wt^2 * PA_2 + wt^3 * PA_3 + wt^4 * BB_4))
  ash.res <- ash(rep(0, nrow(df)), 1, 
                 lik = lik_binom(df$BB, df$PA, link = "identity"),
                 gridmult = 2^0.25, 
                 g = g,
                 fixg = fixg)
  df <- df %>%
    select(-BB, -PA) %>%
    mutate(actual = BB_0 / PA_0,
           ashr.preds = ash.res$result$PosteriorMean) 
  return(list(df = df, ash.res = ash.res))
}

ashr.llik <- function(wt) {
  preds <- BBrate(train.dat, wt)$df$ashr.preds
  return(binom.llik(train.dat$PA_0, train.dat$BB_0, preds))
}

grid <- seq(0.25, 1, by = .01)
all.ashr.res <- sapply(grid, ashr.llik)

ggplot(tibble(wt = grid, llik = all.ashr.res), aes(x = wt, y = llik)) + 
  geom_point() + geom_smooth()

# Eyeball the correct rate.
train.BB <- BBrate(train.dat, 0.45)
train.dat <- train.BB$df
test.BB <- BBrate(test.dat, 0.45, g = train.BB$ash.res$fitted_g, fixg = TRUE)
test.dat <- test.BB$df

binom.llik(test.dat$PA_0, test.dat$BB_0, test.dat$ashr.preds) # -12671

# Compare with linear regression.
lm.3yr <- lm(I(BB_1 / PA_1) ~ I(BB_2 / PA_2) + I(BB_3 / PA_3) + I(BB_4 / PA_4),
             data = train.dat)
lm.2yr <- lm(I(BB_1 / PA_1) ~ I(BB_2 / PA_2) + I(BB_3 / PA_3), data = train.dat)
lm.1yr <- lm(I(BB_1 / PA_1) ~ I(BB_2 / PA_2), data = train.dat)
test.dat <- test.dat %>%
  mutate(preds.3yr = predict(lm.3yr, newdata = test.dat),
         preds.2yr = predict(lm.2yr, newdata = test.dat),
         preds.1yr = predict(lm.1yr, newdata = test.dat)) %>%
  mutate(lm.preds = case_when(!is.nan(preds.3yr) ~ preds.3yr,
                           !is.nan(preds.2yr) ~ preds.2yr,
                           !is.nan(preds.1yr) ~ preds.1yr,
                           TRUE ~ BB_1 / PA_1)) %>%
  select(-preds.3yr, -preds.2yr, -preds.1yr)

binom.llik(test.dat$PA_0, test.dat$BB_0, test.dat$lm.preds) # -12701

# What about Marcel, Steamer?
mean.BBRate <- sum(train.dat$BB_1) / sum(train.dat$PA_1)
test.dat <- test.dat %>%
  mutate(marcel.preds = (5 * BB_1 + 4 * BB_2 + 3 * BB_3 + 1200 * mean.BBRate) /
           (5 * PA_1 + 4 * PA_2 + 3 * PA_3 + 1200))
  
steamer <- read_csv("data/SteamerProjBatters2019.csv") %>%
  filter(!(str_starts(playerid, "sa"))) %>%
  transmute(playerid = as.numeric(playerid),
            steamer.preds = BB / PA)

test.dat <- test.dat %>%
  left_join(steamer, by = "playerid")

# A lot of zeroes...
test.subset <- test.dat %>% filter(steamer.preds > 0)
binom.llik(test.subset$PA_0, test.subset$BB_0, test.subset$BB_1 / test.subset$PA_1) # -10987
binom.llik(test.subset$PA_0, test.subset$BB_0, test.subset$lm.preds) # -10994
binom.llik(test.subset$PA_0, test.subset$BB_0, test.subset$marcel.preds) # -10960
binom.llik(test.subset$PA_0, test.subset$BB_0, test.subset$ashr.preds) # -10961
binom.llik(test.subset$PA_0, test.subset$BB_0, test.subset$steamer.preds) # -10945

rmse(test.subset$PA_0, test.subset$BB_0, test.subset$BB_1 / test.subset$PA_1) # 0.0285
rmse(test.subset$PA_0, test.subset$BB_0, test.subset$lm.preds) # 0.0266
rmse(test.subset$PA_0, test.subset$BB_0, test.subset$marcel.preds) # 0.0241
rmse(test.subset$PA_0, test.subset$BB_0, test.subset$ashr.preds) # 0.0240
rmse(test.subset$PA_0, test.subset$BB_0, test.subset$steamer.preds) # 0.0223

mae(test.subset$PA_0, test.subset$BB_0, test.subset$BB_1 / test.subset$PA_1) # 0.0223
mae(test.subset$PA_0, test.subset$BB_0, test.subset$lm.preds) # 0.0203
mae(test.subset$PA_0, test.subset$BB_0, test.subset$marcel.preds) # 0.0193
mae(test.subset$PA_0, test.subset$BB_0, test.subset$ashr.preds) # 0.0190
mae(test.subset$PA_0, test.subset$BB_0, test.subset$steamer.preds) # 0.0176


# Is there structure in the residuals? Look at advanced metrics.
fan18 <- read_csv("data/fangraphs-2018.csv") %>%
  rename_all(~ str_replace_all(., "%", "Rate")) %>%
  rename_all(~ str_remove_all(., "[- ]")) %>%
  mutate(BBRate = BB/ PA, SORate = SO / PA) %>%
  select(playerid, Age, BBRate, SORate, OSwingRate:SwStrRate) %>%
  mutate_at(vars(OSwingRate:SwStrRate), ~ as.numeric(str_remove_all(., "%")))

train.dat <- train.dat %>%
  left_join(fan18, by = "playerid")

train.dat <- train.dat %>%
  mutate(ashr.resid = (ashr.preds - actual) * sqrt(PA_0) / sqrt(ashr.preds * (1 - ashr.preds)),
         lm.resid = (lm.preds - actual) * sqrt(PA_0) / sqrt(lm.preds * (1 - lm.preds)),
         steamer.resid = (steamer.preds - actual) * sqrt(PA_0) / sqrt(steamer.preds * (1 - steamer.preds)))
train.subset <- train.dat %>%
  filter(steamer.preds > 0) %>%
  remove_missing()

corrplot::corrplot(cor(select(train.subset, Age, BBRate:steamer.resid)))

ggplot(train.dat, aes(x = steamer.preds, y = ashr.preds)) + geom_point()

train.dat <- train.dat %>%
  mutate(ashr.resid = (ashr.preds - actual) * sqrt(PA_18) / sqrt(ashr.preds * (1 - ashr.preds)), 
         steamer.resid = (steamer.preds - actual) * sqrt(PA_18) / sqrt(steamer.preds * (1 - steamer.preds)))

ggplot(train.dat, aes(x = steamer.resid, y = ashr.resid, col = PA_18)) + geom_point() + geom_abline(slope = 1)

