library(tidyverse)
library(lme4)

# Scrape data -----

# season_begin <- as.Date("2017-04-02")
# season_end <- as.Date("2017-11-01")
#
# dat <- tibble()
# start_date <- season_begin
# while (start_date <= season_end) {
#   cat("Start Date:", format(start_date, "%m-%d-%y"), "\n")
#   dat <- dat %>%
#     bind_rows(scrape_statcast_savant_batter_all(start_date = start_date,
#                                                 end_date = start_date + 6))
#   start_date <- start_date + 7
# }
# saveRDS(dat, "mlb2017data.rds")

dat <- readRDS("data/mlb2017data.rds") %>%
  bind_rows(readRDS("data/mlb2018data.rds")) %>%
  bind_rows(readRDS("data/mlb2019data.rds"))


# Remove outliers -----

# Remove batters who pitch and pitchers who bat. A player is defined as a pitcher
#   if he throws more pitches than he faces.
p.cts <- dat %>%
  group_by(pitcher) %>%
  summarize(n = n())
b.cts <- dat %>%
  group_by(batter) %>%
  summarize(n = n())
pitchers <- p.cts %>%
  left_join(b.cts, by = c("pitcher" = "batter"), suffix = c(".p", ".b")) %>%
  filter(is.na(n.b) | n.p > n.b) %>%
  pull(pitcher)

dat <- dat %>% 
  filter(pitcher %in% pitchers & !(batter %in% pitchers))


bb <- dat %>%
  filter(!is.na(launch_speed)) %>%
  mutate(year = format.Date(game_date, "%Y"),
         batter = player_name) %>%
  mutate_at(vars(batter, pitcher, p_throws, stand, year), as.factor) %>%
  select(batter, pitcher, launch_speed, launch_angle, p_throws, stand, year)

bb <- dat %>% filter(!is.na(launch_speed))

# Seems like they impute angle and speed for many data points. Remove these.
launch <- bb %>%
  group_by(launch_angle, launch_speed) %>%
  summarize(n = n())
bb <- bb %>%
  left_join(launch, by = c("launch_angle", "launch_speed")) %>%
  filter(n <= 11) %>%
  select(-n)

# What range of launch speeds and angles do we actually care about?
launch <- bb %>%
  mutate(launch_angle = round(launch_angle),
         launch_speed = round(launch_speed)) %>%
  group_by(launch_angle, launch_speed) %>%
  summarize(hr = sum(events == "home_run", na.rm = TRUE) / n(),
            single = sum(events == "single", na.rm = TRUE) / n(),
            double = sum(events == "double", na.rm = TRUE) / n(),
            triple = sum(events == "triple", na.rm = TRUE) / n(),
            hit = sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE) / n(),
            n = n()) %>%
  mutate(max.chance = pmax(hr, single, double, triple),
         region = case_when(hit < 0.5 ~ "out",
                            max.chance == hr ~ "hr",
                            max.chance == single ~ "single",
                            TRUE ~ "double"))

ggplot(filter(launch, n > 10),
       aes(x = launch_angle, y = launch_speed, fill = region, alpha = hit)) +
  geom_tile()

bb <- bb %>%
  filter(!is.na(woba_value) & woba_denom > 0) %>%
  filter(launch_angle >= -30, launch_angle <= 50) %>%
  filter(launch_speed >= 60) %>%
  mutate(launch_speed = pmin(launch_speed, 112)) %>%
  mutate_at(vars(launch_angle, launch_speed), round)

# Estimated woba is crap, I'll need to do that myself.
xwoba_table <- bb %>%
  group_by(launch_angle, launch_speed) %>%
  summarize(n = n(), woba = sum(woba_value) / sum(woba_denom))

dist <- outer(xwoba_table$launch_angle, xwoba_table$launch_angle, FUN = `-`)^2 + 
  outer(xwoba_table$launch_speed, xwoba_table$launch_speed, FUN = `-`)^2
smooth.par <- 2.5
wts <- xwoba_table$n * exp(-dist / (2 * smooth.par^2))

xwoba_table$xwoba <- colSums(wts * xwoba_table$woba) / colSums(wts)

ggplot(xwoba_table, aes(x = launch_angle, y = launch_speed, fill = xwoba)) + 
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red")

bb <- bb %>%
  left_join(select(xwoba_table, -n, -woba), 
            by = c("launch_speed", "launch_angle"))

bb <- bb %>%
  mutate(year = format(game_date, "%Y"))

ggplot(bb, aes(x = xwoba)) + geom_histogram(binwidth = 0.05)
# Define "crush" as xwoba >= 0.9. Interested in crush rate and crush xwoba.
# Define "chip" as 0.6 <= xwoba < 0.9. Get chip rate.

summs <- bb %>%
  group_by(player_name, year) %>%
  summarize(n = n(),
            crush_rate = mean(xwoba >= 0.9),
            crush_xwoba = mean(xwoba * (xwoba >= 0.9)),
            chip_rate = mean(xwoba >= 0.6 & xwoba < 0.9),
            chip_xwoba = mean(xwoba * (xwoba >= 0.6 & xwoba < 0.9))) %>%
  filter(n >= 30)

# No correlation in chip_xwoba!
chip_df <- summs %>%
  select(player_name, year, chip_xwoba) %>%
  spread(key = year, value = chip_xwoba)
ggplot(power_df, aes(x = `2017`, y = `2018`)) + geom_point() + geom_smooth(method = "lm")
ggplot(power_df, aes(x = `2018`, y = `2019`)) + geom_point() + geom_smooth(method = "lm")

crush_df <- summs %>%
  select(player_name, year, crush_xwoba) %>%
  spread(key = year, value = crush_xwoba) %>%
  ungroup()

oneyr_df <- bind_rows(crush_df %>% select(2:3) %>% rename(prev = `2017`, curr = `2018`),
                      crush_df %>% select(3:4) %>% rename(prev = `2018`, curr = `2019`))
oneyr_df <- oneyr_df %>% remove_missing(na.rm = TRUE)
ggplot(oneyr_df, aes(x = prev, y = curr)) + geom_point() + geom_smooth(method = "lm")

oneyr_lm <- lm(curr ~ prev, data = oneyr_df)
twoyr_lm <- lm(`2019` ~ `2017` + `2018`, data = crush_df)

ggplot(filter(bb, 
              year == 2019,
              player_name %in% c("Mike Trout", "Pete Alonso", "Tim Anderson")), 
       aes(x = xwoba, col = player_name)) + 
  geom_density()

preds_from_2018 <- predict(oneyr_lm, newdata = tibble(prev = crush_df$`2018`))
preds_from_2019 <- predict(oneyr_lm, newdata = tibble(prev = crush_df$`2019`))
oneyr_preds <- ifelse(is.na(preds_from_2019), preds_from_2018, preds_from_2019)
twoyr_preds <- predict(twoyr_lm, newdata = crush_df %>% 
                         rename(`2017` = `2018`, `2018` = `2019`, `2016` = `2017`))
preds <- ifelse(is.na(twoyr_preds), oneyr_preds, twoyr_preds)

crush_df <- crush_df %>%
  mutate(preds = preds)

marginal_la <- bb %>%
  group_by(launch_angle) %>%
  summarize(la_xwoba = mean(xwoba)) 
ggplot(marginal_la, aes(x = launch_angle, y = la_xwoba)) + geom_line()

marginal_ls <- bb %>%
  group_by(launch_speed) %>%
  summarize(ls_xwoba = mean(xwoba)) 
ggplot(marginal_ls, aes(x = launch_speed, y = ls_xwoba)) + geom_line()

bb_marg <- bb %>%
  mutate(year = format(game_date, "%Y")) %>%
  select(player_name, year, launch_angle, launch_speed, xwoba) %>%
  left_join(marginal_la, by = "launch_angle") %>%
  left_join(marginal_ls, by = "launch_speed") %>%
  group_by(player_name, year) %>%
  summarize(n = n(), 
            xwobacon = mean(xwoba), 
            la_xwoba = mean(la_xwoba), 
            ls_xwoba = mean(ls_xwoba)) %>%
  mutate(power = xwobacon - la_xwoba) %>%
  filter(n >= 50) %>%
  group_by(year) %>%
  mutate(power = round(50 + 10 * scale(power)),
         qoc = round(50 + 10 * scale(la_xwoba)))

power_df <- bb_marg %>%
  select(player_name, year, power) %>%
  spread(key = year, value = power)
ggplot(power_df, aes(x = `2017`, y = `2018`)) + geom_point() + geom_smooth(method = "lm")
ggplot(power_df, aes(x = `2018`, y = `2019`)) + geom_point() + geom_smooth(method = "lm")


qoc_df <- bb_marg %>%
  select(player_name, year, qoc) %>%
  spread(key = year, value = qoc)
ggplot(qoc_df, aes(x = `2017`, y = `2018`)) + geom_point() + geom_smooth(method = "lm")
ggplot(qoc_df, aes(x = `2018`, y = `2019`)) + geom_point() + geom_smooth(method = "lm")

ggplot(bb_marg %>% select(-n) %>% gather(key = "stat", value = "value", -player_name, -year), 
       aes(x = value, col = stat)) + 
  geom_density()

ggplot(bb_marg, aes(x = ls_xwoba, y = xwobacon)) + geom_point() + geom_abline(slope = 1)
ggplot(bb_marg, aes(x = la_xwoba, y = xwobacon)) + geom_point() + geom_abline(slope = 1)

stats_df <- bb_marg %>% 
  group_by(player_name) %>% 
  summarize(years = n(), 
            xwobacon_chg = max(xwobacon) - min(xwobacon),
            ls_chg = max(ls_xwoba) - min(ls_xwoba),
            la_chg = max(la_xwoba) - min(la_xwoba)) 

ggplot(stats_df %>%
         filter(years == 3) %>%
         select(-years) %>%
         gather(key = "stat", value = "value", -player_name), 
       aes(x = stat, y = value)) + 
  geom_violin()

# I want each batter's power as a function of launch angle, relative to other batters. I
#   do a kernel-type quantile esimate by looking at all balls within 2.5 degrees.
bb_quantiles <- bb %>%
  select(player_name, launch_angle, launch_speed) %>%
  filter(launch_angle >= -32.5 & launch_angle <= 47.5) %>%
  mutate(quantile = NA, ID = row_number())

for (la in seq(-30, 45, by = 0.1)) {
  la_df <- bb_quantiles %>% 
    filter(launch_angle >= la - 2.5 & launch_angle <= la + 2.5) %>%
    mutate(quantile = rank(launch_speed) / (length(launch_speed) + 1)) %>%
    filter(abs(launch_angle - la) < 0.01) %>%
    select(ID, quantile)
  bb_quantiles <- bb_quantiles %>% 
    left_join(la_df, by = "ID", suffix = c("", "_new")) %>%
    mutate(quantile = ifelse(is.na(quantile), quantile_new, quantile)) %>%
    select(-quantile_new)
}

bb_quantiles <- bb_quantiles %>%
  filter(launch_angle >= -30 & launch_angle <= 45) %>%
  mutate(z = qnorm(quantile)) 

ggplot(filter(bb_quantiles, player_name == "Giancarlo Stanton"), 
       aes(x = launch_angle, y = z)) +
  geom_point() + geom_smooth()

ggplot(filter(bb_quantiles, player_name == "Mike Trout"), 
       aes(x = launch_angle, y = z)) +
  geom_point() + geom_smooth()

ggplot(filter(bb_quantiles, player_name == "Eric Hosmer"), 
       aes(x = launch_angle, y = z)) +
  geom_point() + geom_smooth()

bb_quantiles <- bb_quantiles %>%
  select(-quantile_new) %>%
  filter(launch_angle >= -30 & launch_angle <= 45)

ggplot(filter(bb_quantiles, player_name == "Joey Votto"),
       aes(x = launch_angle, y = quantile))

tmp2 <- df %>% filter(launch_angle == la)
  

summ_stats <- bb %>% 
  mutate(launch_angle = round(launch_angle)) %>% 
  group_by(launch_angle) %>%
  summarize(med = median(launch_speed),
            lq = quantile(launch_speed, 0.1),
            uq = quantile(launch_speed, 0.9)) %>%
  gather(key = "quantile", value = "value", -launch_angle)
ggplot(summ_stats, aes(x = launch_angle, y = value, col = quantile)) + 
  geom_line()

tmp <- bb %>% mutate(launch_angle = round(launch_angle)) %>% group_by(launch_angle) %>% summarize(n = n())

bbb <- 


bb <- bb %>%
  filter(description != "foul") %>%
  arrange(launch_angle)
n.bb <- nrow(bb)
bb <- bb %>%
  mutate(la.z = qnorm(seq(1/n.bb, 1 - 1/n.bb, length.out = n.bb)))

bb.small <- bb %>%
  filter(game_date > "2019-01-01" & launch_angle > 0 & launch_angle < 30) %>%
  filter(!(pitch_type %in% c("EP", "FO", "KN"))) %>%
  select(player_name, pitcher, launch_angle, launch_speed, pfx_x:plate_z, effective_speed, pitch_type) %>%
  rename(batter = player_name) %>%
  remove_missing() %>%
  mutate_at(vars(pfx_x, pfx_z, plate_x, plate_z, effective_speed), scale) %>%
  arrange(batter)

bb.small$batter <- factor(bb.small$batter)
bb.small$pitcher <- factor(bb.small$pitcher)
bb.gam <- gam(launch_speed ~ s(pfx_x) + s(pfx_z) + s(plate_x) + s(plate_z) +
                s(effective_speed) + s(launch_angle) + s(batter, bs = "re") +
                s(pitcher, bs = "re"), data = bb.small)
plot(bb.gam)
summary(bb.gam)

bat.ranef <- bb.gam$coefficients[str_starts(names(bb.gam$coefficients), "s\\(batter\\)")]
names(bat.ranef) <- levels(bb.small$batter)

pit.ranef <- bb.gam$coefficients[str_starts(names(bb.gam$coefficients), "s\\(pitcher\\)")]

zz <- baseballr::playername_lookup(1)
pit.names <- chadwick_player_lu_table[match(levels(bb.small$pitcher),
                                            chadwick_player_lu_table$key_mlbam),]
pit.names <- paste(pit.names$name_first, pit.names$name_last)
pit.ranef <- tibble(pitcher = pit.names, coef = pit.ranef)

bb.lm <- lm(launch_speed ~ pfx_x:pitch_type + pfx_z:pitch_type + 
              plate_x + I(plate_x^2) + plate_z + I(plate_z^2) +
              effective_speed:pitch_type, data = bb.small)
summary(bb.lm)

bb.lm2 <- lmer(launch_speed ~ pfx_x:pitch_type + pfx_z:pitch_type + 
               plate_x:pitch_type + I(plate_x^2):pitch_type + plate_z:pitch_type + I(plate_z^2):pitch_type +
               effective_speed:pitch_type + launch_angle:pitch_type + (1 | batter), 
               data = bb.small, verbose = 1)
summary(bb.lm2)
anova(bb.lm, bb.lm2)

bat.ranef <- ranef(bb.lm2)$batter

ggplot(sample_n(bb.small, 1000), aes(x = la.z, y = launch_angle)) + geom_line()

bb.summ <- bb.small %>%
  group_by(batter) %>%
  summarize(n = n(),
            angle_mean = mean(la.z),
            angle_sd = sd(la.z),
            angle_kurt = mean(((la.z - angle_mean) / angle_sd)^3))

bb.small <- bb.small %>%
  group_by(pitch_type) %>%
  mutate(effective_speed = effective_speed - mean(effective_speed)) %>%
  ungroup()
bb.pt <- bb.small %>% filter(pitch_type == "SL")
lmer.fit <- lmer(la.z ~ -1 + poly(plate_x, 2) + poly(plate_z, 2) +
                   poly(pfx_x, 1) + poly(pfx_z, 2) + effective_speed +
                   (1 | batter) + (1 | pitcher), 
                 data = bb.pt, verbose = 1)
summary(lmer.fit)

nopitch <- lmer(la.z ~ -1 + poly(plate_x, 2) + poly(plate_z, 2) +
                  poly(pfx_x, 1) + poly(pfx_z, 2) + effective_speed +
                  (1 | batter), 
                data = bb.pt, verbose = 1)
anova(lmer.fit, nopitch)

SL.pitch <- ranef(lmer.fit)$pitcher
## FF: remove pfx_x; pitcher.chisq = 343.97
## SL: remove 2nd-order pfx_z; chisq = 42.78

bat.ranef <- ranef(lmer.fit)$batter
pit.ranef <- ranef(lmer.fit)$pitcher


la.mean <- mean(bb$launch_angle)
la.sd <- sd(bb$launch_angle)
la.kurt <- mean(((bb$launch_angle - la.mean) / la.sd)^3)

bb.player <- bb %>%
  filter(game_date > "2019-01-01") %>%
  mutate(batter = player_name) %>%
  select(batter, pitcher, launch_angle, launch_speed) %>%
  group_by(batter) %>%
  summarize(n = n(),
            angle_mean = mean(launch_angle),
            angle_sd = sd(launch_angle),
            angle_kurt = mean(((launch_angle - angle_mean) / angle_sd)^3)) %>%
  filter(n >= 10)

hitprob <- launch %>% 
  group_by(launch_angle) %>% 
  summarize(hprob = sum(hit * n) / sum(n))
ggplot(hitprob, aes(x = launch_angle, y = hprob)) + geom_line()

opt.angle <- hitprob$launch_angle[which.max(hitprob$hprob)]
ggplot(bb, aes(x = launch_angle)) + geom_density()

# EB estimate of densities:

batters <- bb.player$batter

all.dens <- matrix(nrow = length(batters), ncol = 181)
all.s2 <- matrix(nrow = length(batters), ncol = 181)

nsamp <- 1000
for (i in 1:length(batters)) {
  next.la <- bb %>% filter(player_name == batters[i]) %>% pull(launch_angle)
  all.dens[i, ] <- density(next.la, from = -90, to = 90, n = 181)$y
  
  boot.dens <- matrix(nrow = nsamp, ncol = 181)
  set.seed(666)
  for (j in 1:nsamp) {
    boot.la <- sample(next.la, replace = TRUE)
    boot.dens[j, ] <- density(boot.la, from = -90, to = 90, n = 181)$y
  }
  all.s2[i, ] <- apply(boot.dens, 2, var)
}

mean.dens <- apply(all.dens, 2, mean)
s2hat <- apply(all.dens, 2, var)
tau <- s2hat - colSums(all.sd^2) / nrow(all.sd)

eb.dens <- t(t(all.dens) * (tau / (tau + t(all.s2))) + mean.dens * (t(all.s2) / (tau + t(all.s2))))

library(mgcv)
quick.gam <- gam(
  formula = launch_speed ~ 1 + s(launch_angle),
  data = bb
)
plot(quick.gam, ylim = c(-30, 10))
hist(bb$launch_angle)

bb$gam.resid <- fitted(quick.gam) - bb$launch_speed

lmm.full <- lmer(
  formula = gam.resid ~ -1 + year + (1 | pitcher) + (1 | batter),
  data = bb,
  verbose = 1
)
summary(lmm.full)

bb$relaunch <- fitted(lmm.full) - bb$gam.resid + bb$launch_speed

next.gam <- gam(
  formula = relaunch ~ 1 + s(launch_angle),
  data = bb
)
plot(next.gam)

bb$gam.resid <- fitted(next.gam) - bb$launch_speed

next.lmm <- update(lmm.full, data = bb)
summary(next.lmm)

bb$relaunch <- fitted(next.lmm) - bb$gam.resid + bb$launch_speed

lmm.nopitch <- update(
  lmm.full,
  formula = launch_speed ~ -1 + year + (1 | batter)
)

anova(lmm.full, lmm.nopitch)

bb <- bb %>% filter(year == "2019") %>% mutate_at(vars(batter, pitcher), droplevels)

tmp <- bb %>% filter(batter == "Mike Trout")
tmp.gam <- gam(
  formula = launch_speed ~ 1 + s(launch_angle),
  data = tmp
)
plot(tmp.gam, ylim = c(-30, 10))


glm.full <- glmer(
  formula = cbind(HR, 1 - HR) ~ -1 + year + (1 | park) + (1 | pitcher) + (1 | batter),
  data = flies,
  family = binomial,
  nAGQ = 0,
  verbose = 1,
  control = glmerControl(optimizer = "nloptwrap")
)

glm.nopitch <- update(
  glm.full,
  formula = cbind(HR, 1 - HR) ~ -1 + year + (1 | park) + (1 | batter)
)

anova(glm.full, glm.nopitch)


launch <- bb %>%
  mutate(launch_angle = round(launch_angle),
         launch_speed = round(launch_speed)) %>%
  group_by(launch_angle, launch_speed) %>%
  summarize(hr = sum(events == "home_run", na.rm = TRUE) / n(),
            single = sum(events == "single", na.rm = TRUE) / n(),
            double = sum(events == "double", na.rm = TRUE) / n(),
            triple = sum(events == "triple", na.rm = TRUE) / n(),
            hit = sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE) / n(),
            n = n()) %>%
  mutate(max.chance = pmax(hr, single, double, triple),
         region = case_when(hit < 0.5 ~ "out",
                            max.chance == hr ~ "hr",
                            max.chance == single ~ "single",
                            TRUE ~ "double"))

ggplot(filter(launch, n > 10), aes(x = launch_angle, y = launch_speed, fill = region, alpha = hit)) + 
  geom_tile() 

ggplot(filter(bb, player_name %in% c("Mike Trout", "Justin Turner", "Joey Votto") 
              & game_date < "2018-01-01"),
       aes(x = launch_angle, col = player_name)) + geom_density()

ggplot(filter(bb, player_name == "Mike Trout" & game_date >= "2019-01-01"), 
       aes(x = launch_angle, y = launch_speed, col = plate_z > 3)) + geom_point()