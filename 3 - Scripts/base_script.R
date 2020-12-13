#***************************** RUN BASE MODELS *****************************####
# Note that I'm continuously updating this file
# But it should give an idea of how to run things

#### SETUP ####
# install packages
library(tidyverse)
library(tictoc)
library(igraph)
library(devtools)
library(foreach) 
library(doMC) 
wd = ""
setwd("/home/rstudio/BackToSchool/4 - Output/Base HS")
setwd("~/Dropbox/Schools/Public code/4 - Output/Base Elem")

# set up function
sims = function(df, i){
  out = mult_runs(N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i], 
                  run_specials_now = df$run_specials_now[i], start_mult = df$start_mult[i], high_school = df$high_school[i],
                  attack = df$attack[i], child_susp = df$child_susp[i], time = df$time[i], synthpop = synthpop,
                  rel_trans = df$rel_trans[i], n_other_adults = df$n_other_adults[i], n_class = df$n_class[i],
                  class = NA, notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], 
                  start_type = df$start_type[i], child_trans = df$child_trans[i], type = df$type[i],
                  days_inf = df$days_inf[i], disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i], 
                  total_days = df$total_days[i],  teacher_susp = df$teacher_susp[i], mult_asymp = df$mult_asymp[i], 
                  isolate = df$isolate[i], test_sens = df$test_sens[i], test_frac = df$test_frac[i],
                  test_days = df$test_days[i], test_type = df$test_type[i], rel_trans_HH = .04/df$attack[i], n_HH = df$n_HH[i]) %>%
    mutate(id = df$scenario[i]) %>% bind_cols(df[i,])
  
  save(out, file = paste0("results", i, ".RData"))
  rm(out); gc()
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop){  
  
  doMC::registerDoMC(cores = detectCores())
  
  detectCores()
  foreach::getDoParWorkers()
  
  (time_foreach <- system.time(
    foreach(i=1:nrow(df)) %dopar% sims(df, i)
  )[3])
}

#### MAKE DATA FRAME OF PARAMETERS ####

make_df = function(attack = c(.01, .02, .03), disperse_transmission = c(F),
                   teacher_susp = c(1, .5), start_type = c("mix", "teacher", "child"), 
                   notify = c(F, T), test = c(F,T), n_tot = 1, dedens = 1, n_start = 1, 
                   mult_asymp = 1, isolate = 1, days_inf = 5, time = 30, n_contacts = 20,
                   n_staff_contacts = 10, n_other_adults = 60, test_sens = .9, test_frac = .9, 
                   rel_trans = 1/8, n_HH = 2, test_days = "week", test_type = "all", start_mult = 0,
                   scenario = c("Base case", "Limit contacts", "Reduced class size","A/B (2)"),
                   high_school = T, child_susp = 1, child_trans = 1, n_class = 63) {

  # make a grid
  df = expand.grid(attack, disperse_transmission, teacher_susp, start_type, notify, test, n_tot,
                   dedens, n_start, mult_asymp, isolate, days_inf, time, n_contacts, n_staff_contacts, n_other_adults,
                   test_sens, test_frac, rel_trans, n_HH, test_days, test_type, start_mult,
                   scenario, high_school, child_susp, child_trans, n_class) 
  
    names(df) = c("attack", "disperse_transmission", "teacher_susp", "start_type", "notify", "test", "n_tot",
                "dedens", "n_start", "mult_asymp", "isolate", "days_inf", "time", "n_contacts", "n_staff_contacts", "n_other_adults",
                "test_sens", "test_frac", "rel_trans", "n_HH", "test_days", "test_type", "start_mult", 
                "scenario", "high_school", "child_susp", "child_trans", "n_class")
  
  df = df %>%  filter(!(test & !notify)) %>%
    mutate(run_specials_now = ifelse(scenario=="Base case" & !high_school, T, F),
                     n_class = ifelse(scenario == "Reduced class size", n_class/2, n_class),
                     n_contacts = ifelse(scenario!="Base case", n_contacts/2, n_contacts),
                     n_staff_contacts = ifelse(scenario!="Base case", n_staff_contacts/2, n_staff_contacts),
                     type = ifelse(grepl("On/off", scenario), "On/off", "base"),
                     type = ifelse(grepl("A/B", scenario), "A/B", type),
                     total_days = ifelse(grepl("1", scenario), 1, 5),
                     total_days = ifelse(grepl("2", scenario), 2, total_days))
  return(df)
}

set.seed(634532)

# HIGH SCHOOL
setwd(paste0(wd, "Base Elem"))
df_HS = make_df(n_tot = 2000, n_class = 16, high_school = T)
run_parallel(df_HS, synthpop_HS)


#### ELEMENTARY SCHOOL ####
setwd(paste0(wd, "Base Elem"))
df_ELEM = make_df(n_tot = 2000,
                  child_trans = .5, child_susp = .5, high_school = F,
                  n_other_adults = 30, n_class = 5, n_HH = 0)
run_parallel(df_ELEM, synthpop)

