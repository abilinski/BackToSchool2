####************************** SIMULATIONS **************************#### 

# source files
source("functions.R")

# local
wd = paste0("/users/abilinsk/data/abilinsk/Schools2/", level, "_1_Dec_Sens")
setwd(wd)

# fixed parameters
sens.attack = c(0.02, 0.04)
sens.prob = 25*3/100000

# screening sensitivity
df_SENS1 = make_df(test_sens = c(.7, .9),
                   attack = sens.attack, 
                   child_prob = sens.prob, 
                   adult_prob = sens.prob) 

# community notification rate
df_SENS2 = make_df(child_prob = c(1,5,10,25,50)*3/100000, 
                   adult_prob = c(1,5,10,25,50)*3/100000,
                   attack = sens.attack) 

# length of infectious period
df_SENS3 = make_df(attack = c(.01, .02), days_inf = 10,
                   child_prob = sens.prob, 
                   adult_prob = sens.prob)

# frequency of screening
df_SENS4 = make_df(attack = sens.attack, test = T,
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   test_days = c("2x_week"))

# day of screening
df_SENS5 = make_df(attack = sens.attack, test = T,
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   test_start_day = c(1:7)) 

# vaccine efficacy
df_SENS6 = make_df(attack = sens.attack, 
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   vax_eff = c(.5, .7, .9), child_vax = s.vax.sens)

# vax coverage
df_SENS7 = make_df(attack = sens.attack, 
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   child_vax = c(0, .3, .6, .9))

# don't test unvaccinated
df_SENS8 = make_df(attack = sens.attack, 
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   no_test_vacc = c(T, F), child_vax = s.vax.sens) 

# rapid test sensitivity
df_SENS9 = make_df(attack = sens.attack, 
                   child_prob = sens.prob, 
                   adult_prob = sens.prob,
                   rapid_test_sens = c(.6, .8))

# bind them
df_ELEM = rbind(df_SENS1, df_SENS2, df_SENS3, df_SENS4, df_SENS5, df_SENS6, df_SENS7, df_SENS8, df_SENS9) %>% 
  mutate(i = row_number()) %>% 
  
  # expand to TTS & non-TTS
  dplyr::select(-test_quarantine) %>% expand_grid(test_quarantine = c(T, F)) %>%
  
  # 10 day quarantine without TTS; 7 days for TTS
  filter((test_quarantine & quarantine.length==7) | (!test_quarantine & quarantine.length==10)) %>%
  
  # set additional parameters
  mutate(isolate = ifelse(test_quarantine, test_q_isolate, 1), 
         quarantine.grace = ifelse(test_quarantine, 0, 3))

# check dimension
print(dim(df_ELEM))

# make class
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
tic()
run_parallel(df_ELEM, synthpop, class = class)
toc()
