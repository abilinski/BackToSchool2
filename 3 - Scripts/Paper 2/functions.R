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
library(BackToSchool)
library(ddpcr)

####************************** FUNCTIONS TO VARY PARAM SETS **************************#### 

#### RUN SINGLE ITERATION ####
sims = function(df, i, synthpop, class = NA){
  #set.seed(i)
  out = mult_runs(version = 2, N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i], 
                  run_specials_now = df$run_specials_now[i], start_mult = df$start_mult[i], high_school = df$high_school[i],
                  attack = df$attack[i], child_susp = df$child_susp[i], time = df$time[i], synthpop = synthpop,
                  rel_trans = df$rel_trans[i], n_other_adults = df$n_other_adults[i], n_class = df$n_class[i],
                  notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], num_adults = 1,
                  start_type = df$start_type[i], child_trans = df$child_trans[i], type = df$type[i],
                  days_inf = df$days_inf[i], disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i], 
                  total_days = df$total_days[i],  teacher_susp = df$teacher_susp[i], mult_asymp = df$mult_asymp[i], 
                  isolate = df$isolate[i], test_sens = df$test_sens[i], test_frac = df$test_frac[i],
                  p_asymp_adult = df$p_asymp_adult[i], p_asymp_child = df$p_asymp_child[i], 
                  p_subclin_adult = df$p_subclin_adult[i], p_subclin_child = df$p_subclin_child[i],
                  child_prob = df$child_prob[i], adult_prob = df$adult_prob[i], class = class,
                  test_days = df$test_days[i], test_type = df$test_type[i], rel_trans_HH = .04/df$attack[i], n_HH = df$n_HH[i],
                  quarantine.length = df$quarantine.length[i], turnaround.time = df$turnaround.time[i], vax_eff = df$vax_eff[i],
                  test_start_day = df$test_start_day[i], family_susp = df$family_susp[i], test_quarantine = df$test_quarantine[i], surveillance = df$surveillance[i],
                  child_vax = df$child_vax[i], no_test_vacc = df$no_test_vacc[i]) %>%
    mutate(id = df$scenario[i], i = i, sim = df$sim[i])

  out = out %>% bind_cols(df[i,])
  save(out, file = paste0("results", i, "_", Sys.time(), ".RData"))
  print(i)
  rm(out); gc()
  #return(out)
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop, class = NA){  
  
  doMC::registerDoMC(cores = detectCores())
  
  detectCores()
  foreach::getDoParWorkers()

  foreach(i=1:nrow(df), .combine = rbind) %dopar% sims(df, i, synthpop, class = NA)

}

#### MAKE DATA FRAME OF PARAMETERS ####
make_df = function(attack = c(.02), disperse_transmission = c(F),
                   teacher_susp = c(1, .5), start_type = c("mix", "teacher", "child"), 
                   notify = c(F, T), test = c(F,T), n_tot = 1, dedens = 1, n_start = 1, 
                   mult_asymp = 1, isolate = 1, days_inf = 5, time = 30, n_contacts = 20,
                   p_asymp_adult = .4, p_asymp_child = .8,
                   p_subclin_adult = 0, p_subclin_child = 0,
                   n_staff_contact = 10, n_other_adults = 60, 
                   test_sens = .9, test_frac = .9, family_susp = .7, child_vax = .5, 
                   rel_trans = 1/8, n_HH = 2, test_days = "week", test_type = "all", start_mult = 0,
                   scenario = c("Base case", "Limit contacts", "Reduced class size","A/B (2)"),
                   high_school = T, child_susp = 1, child_trans = 1, n_class = 63, prob = 0, disperse = T, 
                   quarantine.length = 10, turnaround.time = 1, test_start_day = 1, test_quarantine = F, vax_eff = 0.8, surveillance = F, no_test_vacc = F) {
  
  # make a grid
  df = expand.grid(attack, disperse_transmission, teacher_susp, start_type, notify, test, 1,
                   dedens, n_start, mult_asymp, isolate, days_inf, time, n_contacts, n_staff_contact, n_other_adults,
                   test_sens, test_frac, rel_trans, n_HH, test_days, test_type, start_mult,
                   scenario, high_school, child_susp, child_trans, n_class, p_asymp_adult, p_asymp_child,
                   p_subclin_adult, p_subclin_child, prob, quarantine.length, turnaround.time, test_start_day, family_susp, test_quarantine, vax_eff, surveillance, 
                   child_vax, no_test_vacc) 
  
  names(df) = c("attack", "disperse_transmission", "teacher_susp", "start_type", "notify", "test", "n_tot",
                "dedens", "n_start", "mult_asymp", "isolate", "days_inf", "time", "n_contacts", "n_staff_contact", "n_other_adults",
                "test_sens", "test_frac", "rel_trans", "n_HH", "test_days", "test_type", "start_mult", 
                "scenario", "high_school", "child_susp", "child_trans", "n_class", "p_asymp_adult", "p_asymp_child",
                "p_subclin_adult", "p_subclin_child", "prob", "quarantine.length", "turnaround.time", "test_start_day", "family_susp",
                "test_quarantine", "vax_eff", "surveillance", "child_vax", "no_test_vacc")
  
  df = df %>% # filter(!(test & !notify)) %>%
   # filter(teacher_susp==1 | (notify == T & test == F)) %>%
    filter((test_days=="week" & test_sens == .9 & test_frac == .9 & test_type == "all") | test) %>%
    mutate(run_specials_now = ifelse(scenario=="Base case" & !high_school, T, F),
           n_class = ifelse(scenario == "Reduced class size", n_class*2, n_class),
           n_contacts = ifelse(scenario!="Base case", n_contacts/2, n_contacts),
           n_staff_contact = ifelse(scenario!="Base case", n_staff_contact/2, n_staff_contact),
           type = ifelse(grepl("On/off", scenario), "On/off", "base"),
           type = ifelse(grepl("A/B", scenario), "A/B", type),
           type = ifelse(grepl("Remote", scenario), "Remote", type),
           total_days = ifelse(grepl("1", scenario), 1, 5),
           total_days = ifelse(grepl("2", scenario), 2, total_days),
           child_prob = prob,
           adult_prob = prob*2,
           sim = row_number()) %>% filter(type!="Remote" | (!notify & !test))
  
  # repeat according to simulation count
  if(disperse) df <- df[rep(row.names(df), n_tot),] %>% mutate(i = row_number())
  
  return(df)
}
 
####************************** FUNCTIONS TO INPUT ARGUMENTS **************************#### 

## pull data
## Pull command arguments
args=(commandArgs(TRUE))
# args is now a list of character vectors
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if (length (args) == 0) {
  print("No arguments supplied.")
  
} else {
  for (i in 1:length(args)) {
    eval (parse (text = args[[i]] ))
  }
  
  test_q = as.logical(test_q)
  notify_val = as.numeric(notify_val)
  test_q_isolate = as.logical(test_q_isolate)
  vax_eff_val = as.logical(vax_eff_val)
  version = as.character(version)
  commandArgs <- function(...) c(test_q, notify_val, test_q_isolate, version)
}

