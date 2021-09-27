####************************** SIMULATIONS **************************#### 
# source files
source("functions.R")

# local
wd = paste0("/users/abilinsk/data/abilinsk/Schools/MS_24_Sep_Sens_0", version)
setwd(wd)

# number of  simulations
n_tot = 1

# screening sensitivity
df_SENS1 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .7,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5,
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# community notification rate
df_SENS2 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case", "A/B (2)"), teacher_susp = c(.9),
                   prob = c(1,5,10,25,50)*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# length of infectious period
df_SENS3 = make_df(attack = c(.01, .02), days_inf = 10,
                   n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case", "A/B (2)"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5,
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# frequency of screening
df_SENS4 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("2x_week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, 
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# day of screening
df_SENS5 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, 
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   test_start_day = c(1:7),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# day of screening
df_SENS6 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, 
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   vax_eff = c(.5, .7, .9),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# day of screening
df_SENS7 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, 
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   child_vax = c(0, .3, .6, .9),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

# day of screening
df_SENS8 = make_df(attack = c(.02, .04), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = 1, child_susp = 1, child_vax = .5, 
                   high_school = F,
                   p_asymp_adult = .2, p_asymp_child = .4,
                   p_subclin_adult = .2, p_subclin_child = .4,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   no_test_vacc = c(T,F),
                   n_other_adults = 30, n_class = 7, 
                   test_quarantine = test_q, notify = notify_val)

df_ELEM = rbind(df_SENS1, df_SENS2, df_SENS3, df_SENS4, df_SENS5, df_SENS6, df_SENS7, df_SENS8) %>% mutate(i = row_number()) %>% 
  dplyr::select(-test_quarantine) %>% expand_grid(test_quarantine = c(T, F)) %>%
  filter(!test_quarantine | (quarantine.length==7 & scenario == "Base case")) %>%
  filter(test_quarantine | quarantine.length==10) %>%
  mutate(quarantine.grace = ifelse(test_quarantine, 0, 3), isolate = ifelse(test_quarantine, test_q_isolate, 1))

#df_ELEM = bind_rows(df_ELEM) %>% mutate(i = row_number()) %>% filter(!test_quarantine | (quarantine.length==7 & scenario == "Base case")) %>%
#mutate(quarantine.grace = 0, isolate = ifelse(test_quarantine, 0, 1)) %>% mutate(child_susp = 0.5)

print(dim(df_ELEM))

# don't double run
#nums = data.frame(results = 0)
#if(length(list.files())>0) nums = data.frame(id = list.files()) %>% separate(id, into = c("results", "date"), sep = "_") %>% mutate(results = sub("results", "", results), results = as.numeric(results))
#df_ELEM = df_ELEM %>% filter(!i%in%nums$results)
#print(dim(df_ELEM))

# make class
set.seed(3232)
class = make_school(synthpop = synthpop_MS, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
tic()
run_parallel(df_ELEM, synthpop_MS, class = class)
toc()