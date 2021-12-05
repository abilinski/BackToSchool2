####************************** SIMULATIONS **************************#### 

# source files
source("functions.R")
source("abm6.R")

# local
wd = paste0("/users/abilinsk/data/abilinsk/Schools/", level, "_1_Dec_", version)
setwd(wd)

setwd("/users/alyssabilinski/Desktop/Test/")

# 5-day
df_ELEM = make_df(scenario = c("Base case"),
                  test_days = c("week", "2x_week"), 
                  test_frac = c(.7, .9),
                  child_vax = c(s.child_vax, s.child_vax.vary),
                  test_quarantine = test_q)
# Surveillance
df_ELEM_SURV = make_df(scenario = c("Base case"), 
                       test_frac = c(0.1, 0.2, .9),
                       attack = c(0.02, 0.04),
                       child_vax = c(s.child_vax, s.child_vax.vary),
                       surveillance = T,
		                   test_quarantine = test_q) %>%
  filter(!(test_frac==0.9 & attack!=0.04))
df_ELEM_SURV2 = df_ELEM_SURV %>% mutate(test = F) %>% filter(attack==0.04 & test_frac==0.9)

# Extra base case to smooth noise
df_ELEM_ADD = make_df(scenario = c("Base case"), 
                      child_prob = c(1)*3/100000, adult_prob = c(1)*3/100000,
                      n_tot = s.n_tot*2, 
                      test_days = c("week", "2x_week"),
                      test_frac = c(.7, .9),
                      child_vax = c(s.child_vax, s.child_vax.vary),
                      test_quarantine = test_q)
# A/B
df_ELEM1 = make_df(scenario = c("A/B (2)"), 
                   attack = c(.02, .04), 
                   test = F,
                   child_vax = c(s.child_vax, s.child_vax.vary),
                   test_quarantine = test_q)

# Remote
df_ELEM2 = make_df(scenario = c("Remote"), 
                   attack = c(.02, .04),
                   test = F,
                   child_vax = c(s.child_vax, s.child_vax.vary),
                   quarantine.length = 10, 
                   test_quarantine = test_q, notify = F) 

# combine data frames

# only include extras for the elementary level
if(level=="Elementary"){
  df_ELEM_temp = rbind(df_ELEM, df_ELEM_SURV, df_ELEM_SURV2, df_ELEM_ADD, df_ELEM1, df_ELEM2)
} else {df_ELEM_temp = rbind(df_ELEM, df_ELEM_SURV, df_ELEM_SURV2, df_ELEM1, df_ELEM2)}

# modify variables
df_ELEM = df_ELEM_temp %>% 
  mutate(i = row_number()) %>% 
  
  # 10 day quarantine without TTS; 7 days for TTS
  filter((test_quarantine & quarantine.length==7) | (!test_quarantine & quarantine.length==10)) %>%
  
  # set additional parameters
  mutate(isolate = ifelse(test_quarantine, test_q_isolate, 1), 
         
         # drop quarantine grace period with test quarantine
         quarantine.grace = ifelse(test_quarantine, 0, 3),
         
         # set vaccine efficacy from main arguments
         vax_eff = vax_eff_val, 
         
         # set rapid test sensitivity based on version
         rapid_test_sens = ifelse(version==4, .6, .8))

# check dimension while on cluster
print(dim(df_ELEM))

# make class
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
tic()
g = run_parallel(df_ELEM[1,] %>% mutate(test_quarantine = F, adult_prob = 300/100000, child_prob = 300/100000), synthpop, class = class)
toc()
