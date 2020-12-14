#***************************** RUN BASE MODELS *****************************####
# Note that I'm continuously updating this file
# But it should give an idea of how to run things

#### SETUP ####
# install packages
library(tidyverse)
library(tictoc)
library(igraph)
library(foreach) 
library(doMC) 
wd = "~/Dropbox/Schools/Public code/4 - Output/"
wd = "/home/rstudio/BackToSchool2/4 - Output/"
setwd(wd)

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
                  p_asymp_adult = df$p_asymp_adult[i], p_asymp_child = df$p_asymp_child[i], 
                  p_subclin_adult = df$p_subclin_adult[i], p_subclin_child = df$p_subclin_child[i],
                  test_days = df$test_days[i], test_type = df$test_type[i], rel_trans_HH = .04/df$attack[i], n_HH = df$n_HH[i]) %>%
    mutate(id = df$scenario[i], sim = df$sim[i]) %>% bind_cols(df[i,])
  
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
                   p_asymp_adult = .4, p_asymp_child = .8,
                   p_subclin_adult = 0, p_subclin_child = 0,
                   n_staff_contact = 10, n_other_adults = 60, 
                   test_sens = .9, test_frac = .9, 
                   rel_trans = 1/8, n_HH = 2, test_days = "week", test_type = "all", start_mult = 0,
                   scenario = c("Base case", "Limit contacts", "Reduced class size","A/B (2)"),
                   high_school = T, child_susp = 1, child_trans = 1, n_class = 63) {
  
  # make a grid
  df = expand.grid(attack, disperse_transmission, teacher_susp, start_type, notify, test, 1,
                   dedens, n_start, mult_asymp, isolate, days_inf, time, n_contacts, n_staff_contact, n_other_adults,
                   test_sens, test_frac, rel_trans, n_HH, test_days, test_type, start_mult,
                   scenario, high_school, child_susp, child_trans, n_class, p_asymp_adult, p_asymp_child,
                   p_subclin_adult, p_subclin_child) 
  
  names(df) = c("attack", "disperse_transmission", "teacher_susp", "start_type", "notify", "test", "n_tot",
                "dedens", "n_start", "mult_asymp", "isolate", "days_inf", "time", "n_contacts", "n_staff_contact", "n_other_adults",
                "test_sens", "test_frac", "rel_trans", "n_HH", "test_days", "test_type", "start_mult", 
                "scenario", "high_school", "child_susp", "child_trans", "n_class", "p_asymp_adult", "p_asymp_child",
                "p_subclin_adult", "p_subclin_child")
  
  df = df %>%  filter(!(test & !notify)) %>%
    mutate(run_specials_now = ifelse(scenario=="Base case" & !high_school, T, F),
           n_class = ifelse(scenario == "Reduced class size", n_class*2, n_class),
           n_contacts = ifelse(scenario!="Base case", n_contacts/2, n_contacts),
           n_staff_contact = ifelse(scenario!="Base case", n_staff_contact/2, n_staff_contact),
           type = ifelse(grepl("On/off", scenario), "On/off", "base"),
           type = ifelse(grepl("A/B", scenario), "A/B", type),
           total_days = ifelse(grepl("1", scenario), 1, 5),
           total_days = ifelse(grepl("2", scenario), 2, total_days),
           sim = row_number())
  
  # repeat according to simulation count
  df <- df[rep(row.names(df), n_tot),] %>% mutate(i = row_number())
  
  return(df)
}

set.seed(4324)

#### HIGH SCHOOL BASE ####
setwd(paste0(wd, "Base HS"))
df_HS = make_df(n_tot = 100, n_class = 16, high_school = T, n_HH = 2)
run_parallel(df_HS[1,], synthpop_HS)

df_HS = make_df(n_tot = 10, n_class = 16, high_school = T, attack = .03,
                rel_trans = 1/8, n_HH = 2,
                start_type = "mix", notify = F, test = F, scenario = "Base case")
run_parallel(df_HS[1,], synthpop_HS)

run_parallel(df_HS[1,], synthMD_HS)

#### ELEMENTARY SCHOOL BASE ####
setwd(paste0(wd, "Base Elem"))
df_ELEM = make_df(n_tot = 2000,
                  child_trans = .5, child_susp = .5, high_school = F,
                  n_other_adults = 30, n_class = 5, n_HH = 1)
run_parallel(df_ELEM, synthpop)

#### HIGH SCHOOL DYNAMIC ####

#### ELEMENTARY SCHOOL DYNAMIC ####
setwd(paste0(wd, "Elem Dynamic"))
df_ELEM = make_df(n_tot = 1, start_type = "cont", tacher_susp = 1,
                  scenario = c("Base case","A/B (2)", "Remote"),
                  prob = 300/100000,#c(1, seq(10,100, by = 10))/100000*3,
                  time = 56, 
                  child_trans = .5, child_susp = .5, high_school = F,
                  n_other_adults = 30, n_class = 5, n_HH = 1)

df_ELEM = make_df(n_tot = 20, start_type = "cont", attack = .02,
                  scenario = c("Base case", "Remote"), teacher_susp = 1,
                  prob = c(20/100000), #c(1, seq(10,100, by = 10))/100000*3,
                  time = 56, notify = F, test = F,
                  child_trans = .5, child_susp = .5, high_school = F,
                  n_other_adults = 30, n_class = 5)

run_parallel(df_ELEM, synthpop)
out1 = out %>% mutate(Rt2 = ifelse(is.na(Rt), 0, Rt))
out = out %>% mutate(Rt2 = ifelse(is.na(Rt), 0, Rt))
colMeans(out1[,1:10], na.rm = T) - colMeans(out[,1:10], na.rm = T)

mean(out1$Rt2) - mean(out$Rt2)

#### SUPPLEMENTS ####

n_supp = 1000
n_supp = 1

#### ELEMENTARY SCHOOL ####

#### EQUAL TRANS, 5 seconds
setwd(paste0(wd, "Elem_supp1"))
df_ELEM_supp1 = make_df(n_tot = n_supp,
                  child_trans = 1, child_susp = .5, high_school = F,
                  teacher_susp = 1, start_type = "mix",
                  n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp1, synthpop)

#### ALTERNATIVE SCHEDULES, 5 seconds
setwd(paste0(wd, "Elem_supp2"))
df_ELEM_supp2 = make_df(n_tot = n_supp,
                        child_trans = .5, child_susp = .5, high_school = F,
                        teacher_susp = 1, start_type = "mix",
                        scenario = c("A/B (5)", "A/B (1)", "On/off (2)", "On/off (1)"),
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp2, synthpop)

#### OVERDISPERSION, 5 seconds
setwd(paste0(wd, "Elem_supp3"))
df_ELEM_supp3 = make_df(n_tot = n_supp,
                        child_trans = .5, child_susp = .5, high_school = F,
                        teacher_susp = 1, start_type = "mix",
                        disperse_transmission = T,
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp3, synthpop)

#### HOUSEHOLDS, 30 seconds
setwd(paste0(wd, "Elem_supp4"))
df_ELEM_supp4 = make_df(n_tot = n_supp,
                        child_trans = .5, child_susp = .5, high_school = F,
                        teacher_susp = 1, start_type = "mix",
                        n_HH = seq(0, 10, by = 2),
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp4, synthpop)

#### PATHWAY THROUGH ASYMPTOMATICS, 7 seconds
setwd(paste0(wd, "Elem_supp5"))
df_ELEM_supp5 = make_df(n_tot = n_supp,
                        child_trans = 1, child_susp = .5, high_school = F,
                        teacher_susp = 1, start_type = "mix",
                        p_asymp_adult = .2, p_asymp_child = .4,
                        p_subclin_adult = .2, p_subclin_child = .4,
                        mult_asymp = .5,
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp5, synthpop)

# HIGH SCHOOL SUPPLEMENTAL ANALYSES

# SUSCEPTIBLE - 8 seconds
setwd(paste0(wd, "HS_supp1"))
df_HS_supp1 = make_df(n_tot = n_supp, teacher_susp = 1,
                      start_type = "mix", child_susp = 0.5,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp1, synthpop_HS)

# SCENARIOS - 7 seconds
setwd(paste0(wd, "HS_supp2"))
df_HS_supp2 = make_df(n_tot = n_supp, teacher_susp = 1,
                      start_type = "mix", child_susp = 1,
                      scenario = c("A/B (1)", "On/off (2)", "On/off (1)"),
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp2, synthpop_HS)

# DISPERSION - 10 seconds
setwd(paste0(wd, "HS_supp3"))
df_HS_supp3 = make_df(n_tot = n_supp, teacher_susp = 1,
                      start_type = "mix", child_susp = 1,
                      disperse_transmission = T,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp3, synthpop_HS)

# Households - 10 seconds
setwd(paste0(wd, "HS_supp4"))
df_HS_supp4 = make_df(n_tot = n_supp, teacher_susp = 1,
                      start_type = "mix", child_susp = 1,
                      n_HH = seq(0,10, by = 2),
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp4, synthpop_HS)

# Through asymptomatic - 10 seconds
setwd(paste0(wd, "HS_supp5"))
df_HS_supp5 = make_df(n_tot = n_supp, teacher_susp = 1,
                      start_type = "mix", child_susp = 1,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp5, synthpop_HS)

