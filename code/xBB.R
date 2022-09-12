# Idea: conditional on location, a pitch will be called a ball with a certain
#   probability. Also, xWOBA will differ by location. We should be able to tell
#   how much WOBA is gained or lost at each pitch as a result of plate discipline.
# The probability of making contact depends on the pitch location. To make the plate
#   discipline metrics strictly comparable, I'll assume the same distribution across 
#   batters, but I'll make allowances for differences between left- and right-handed
#   hitters.

all_pitches <- readRDS("output/mlb_eventvals.rds")

baseline_woba <- readRDS("output/count_woba.rds") %>%
  filter(balls == 0, strikes == 0) %>%
  select(-balls, -strikes) %>%
  rename(baseline_wOBA = wOBA)

eye_woba <- all_pitches %>%
  mutate(take_value = (1 - p_strike) * ball_value + p_strike * strike_value,
         swing_value = (1 - p_contact) * strike_value + 
           p_contact * p_foul * foul_value +
           p_contact * (1 - p_foul - p_barrel) * nonbrl_value + 
           p_contact * p_barrel * barrel_value,
         avg_player_wOBA = (1 - p_swing) * take_value + p_swing * swing_value,
         eye_wOBA = (1 - swing) * take_value + swing * swing_value) %>%
  group_by(game_year, player_name) %>%
  summarize(PA = length(unique(PA_ID)),
            avg_player_wOBA = sum(avg_player_wOBA) / PA,
            eye_wOBA = sum(eye_wOBA) / PA) %>%
  ungroup() %>%
  left_join(baseline_woba, by = "game_year") %>%
  mutate(avg_player_wOBA = baseline_wOBA + avg_player_wOBA,
         eye_wOBA = baseline_wOBA + eye_wOBA,
         eyeAA = eye_wOBA - avg_player_wOBA)

contact_woba <- all_pitches %>%
  mutate(take_value = (1 - p_strike) * ball_value + p_strike * strike_value,
         contact_value = p_foul * foul_value +
           (1 - p_foul - p_barrel) * nonbrl_value + 
           p_barrel * barrel_value,
         contact_wOBA = (1 - swing) * take_value + 
           swing * (1 - contact) * strike_value +
           swing * contact * contact_value) %>%
  group_by(game_year, player_name) %>%
  summarize(contact_wOBA = sum(contact_wOBA) / length(unique(PA_ID))) %>%
  ungroup() 

power_woba <- all_pitches %>%
  mutate(take_value = (1 - p_strike) * ball_value + p_strike * strike_value,
         power_wOBA = (1 - swing) * take_value + 
           swing * (1 - contact) * strike_value +
           swing * contact * foul * foul_value +
           swing * contact * barrel * barrel_value +
           swing * contact * (1 - foul) * (1 - barrel) * nonbrl_value) %>%
  group_by(game_year, player_name) %>%
  summarize(power_wOBA = sum(power_wOBA) / length(unique(PA_ID))) %>%
  ungroup() 

cwoba <- eye_woba %>%
  left_join(contact_woba, by = c("game_year", "player_name")) %>%
  left_join(power_woba, by = c("game_year", "player_name")) %>%
  mutate(contact_wOBA = contact_wOBA + baseline_wOBA,
         contactAA = contact_wOBA - eye_wOBA,
         power_wOBA = power_wOBA + baseline_wOBA,
         powerAA = power_wOBA - contact_wOBA)

tmp <- cwoba %>% 
  filter(game_year == 2018, PA > 50) %>%
  mutate_if(is.numeric, ~round(., digits = 3)) %>%
  mutate(luckAA = avg_player_wOBA - baseline_wOBA) %>%
  mutate(luckAA = luckAA - mean(luckAA),
         skillAA = eyeAA + contactAA + powerAA,
         xcwOBA = baseline_wOBA + eyeAA + contactAA + powerAA + luckAA) %>%
  select(player_name, eyeAA, contactAA, powerAA, skillAA, luckAA, xcwOBA)

eye <- cwoba %>%
  filter(game_year > 2017, PA > 200) %>%
  select(player_name, game_year, powerAA) %>%
  spread(game_year, powerAA) %>%
  remove_missing()
summary(lm(`2019` ~ `2018`, data = eye))

# R2: eye: 0.61; contact: 0.67; power: 0.56

cwOBA <- all_pitches %>%
  mutate(y_take = (1 - p_strike) * (1 - swing) * ball_value +
           p_strike * (1 - swing) * strike_value,
         mean_take = (1 - p_strike) * (1 - p_swing) * ball_value +
           p_strike * (1 - p_swing) * strike_value,
         y_ss = swing * (1 - contact) * strike_value,
         mean_ss = p_swing * (1 - p_contact) * strike_value,
         y_contact = foul * foul_value +
           swing * (1 - foul) * (1 - barrel) * nonbrl_value,
         mean_contact = p_swing * p_contact * p_foul * foul_value +
           p_swing * p_contact * (1 - p_foul - p_barrel) * nonbrl_value,
         y_barrel = barrel * barrel_value,
         mean_barrel = p_swing * p_contact * p_barrel * barrel_value) %>%
  mutate(take_wOBA = y_take - mean_take,
         ss_wOBA = y_ss - mean_ss,
         contact_wOBA = y_contact - mean_contact,
         barrel_wOBA = y_barrel - mean_barrel) %>%
  mutate(base_wOBA = mean_take + mean_ss + mean_contact + mean_barrel,
         plus_wOBA = take_wOBA + ss_wOBA + contact_wOBA + barrel_wOBA)

cwOBA <- cwOBA %>%
  group_by(player_name, game_year) %>%
  summarize_at(vars(plus_wOBA, take_wOBA, ss_wOBA, contact_wOBA, barrel_wOBA),
               ~ sum(.) / length(unique(PA_ID))) %>%
  ungroup()

player_res <- wOBA %>%
  left_join(cwOBA, by = c("game_year", "player_name")) %>%
  left_join(avg_wOBA, by = "game_year") %>%
  mutate(wOBA = wOBA - avg_wOBA) %>%
  select(-avg_wOBA) %>%
  filter(PA >= 200) %>%
  mutate_if(is_numeric, ~ round(., digits = 3))

luck <- tmp %>%
  group_by(player_name, game_year) %>%
  summarize(n = n(), sumx = sum(x_value), luck = sum(x_value) / n()) %>%
  filter(n > 1000)

all_pitches <- all_pitches %>%
  mutate(swing_value = (1 - p_contact) * strike_value + p_contact * p_foul * foul_value +
           p_contact * (1 - p_foul) * HIP_value,
         take_value = p_strike * strike_value + (1 - p_strike) * ball_value)
all_pitches <- all_pitches %>%
  mutate(avg_PDwOBA = p_swing * swing_value + (1 - p_swing) * take_value,
         PDwOBAaa = swing * swing_value + (1 - swing) * take_value - avg_PDwOBA)

# Contact metric

all_pitches <- all_pitches %>%
  mutate(contact_value = p_foul * foul_value + (1 - p_foul) * HIP_value)
# Don't ever penalize for making contact.
all_pitches <- all_pitches %>%
  mutate(contact_value = ifelse(strike_value > contact_value, strike_value, contact_value))
all_pitches <- all_pitches %>%
  mutate(avg_CONwOBA = p_contact * contact_value + (1 - p_contact) * strike_value,
         CONwOBAaa = contact * contact_value + (1 - contact) * strike_value - avg_CONwOBA)

wOBA <- all_pitches %>%
  filter(game_year == 2019, balls == 0, strikes == 0) %>%
  group_by(player_name) %>%
  summarize(n_PA = n(), wOBA = sum(pa_woba_value) / sum(pa_woba_denom))

PDwOBA <- all_pitches %>%
  filter(game_year == 2019) %>%
  group_by(player_name) %>%
  summarize(PDwOBAaa = sum(PDwOBAaa) / length(unique(PA_ID)))

CONwOBA <- all_pitches %>%
  filter(game_year == 2019, swing == 1) %>%
  group_by(player_name) %>%
  summarize(CONwOBAaa = sum(CONwOBAaa) / length(unique(PA_ID)))

# use xwoba here (b/c of park effects)
POWwOBA <- all_pitches %>%
  filter(game_year == 2019, contact == 1, foul == 0, pa_woba_denom > 0) %>%
  group_by(player_name) %>%
  summarize(POWwOBAaa = sum(pa_woba_value - wOBACON) / length(unique(PA_ID)))

res <- wOBA %>%
  left_join(PDwOBA, by = "player_name") %>%
  left_join(CONwOBA, by = "player_name") %>%
  left_join(POWwOBA, by = "player_name") %>%
  mutate_at(vars(PDwOBAaa, CONwOBAaa, POWwOBAaa), 
            ~round(scale(., center = TRUE, scale = TRUE) * 10 + 50))  %>%
  mutate(wOBA = round(wOBA, digits = 3)) %>%
  filter(n_PA >= 200) 

lm_res <- lm(wOBA ~ PDwOBAaa + CONwOBAaa + POWwOBAaa, data = res)
summary(lm_res)

res <- res %>%
  mutate(xwOBA = round(fitted(lm_res), 3))

# Due to all of the approximations involved, the results need to be recentered.
all_pitches <- all_pitches %>%
  group_by(balls, strikes) %>%
  mutate_at(vars(PDwOBAaa, CONwOBAaa) ~ scale(., center = TRUE, scale = FALSE)) %>%
  ungroup()

# When is it important to make contact?

plot_df <- all_pitches %>%
  group_by(balls, strikes, plate_x, plate_z) %>%
  summarize(contact_benefit = mean(contact_value - strike_value)) %>%
  ungroup()
ggplot(plot_df, aes(x = plate_x, y = plate_z, fill = contact_benefit)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "dark green") +
  facet_wrap(~balls + strikes, nrow = 4, ncol = 3)


