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
  test_q_isolate = as.logical(test_q_isolate)
  vax_eff_val = as.numeric(vax_eff_val)
  version = as.character(version)
  level = as.character(level)
  commandArgs <- function(...) c(test_q, notify_val, test_q_isolate, version)
}

#***************************** SET UP PARAMTERS *****************************#
# baseline simulation parameters
s.n_tot = 1
s.attack = c(.02)
s.disperse_transmission = F
s.start_type = c("cont") 
s.dedens = 1
s.n_start = 1
s.mult_asymp = 0.5
s.days_inf = 5; s.time = 30
s.n_contacts = 20; s.n_staff_contact = 10
s.p_asymp_adult = .2; s.p_subclin_adult = .2
s.test_sens = .9; s.test_frac = .9; s.test_days = "week"; s.test_type = "all"
s.adult_prob = c(1,5,10,25,50,100)*3/100000; s.child_prob = c(1,5,10,25,50,100)*3/100000
s.scenario = "Base case"
s.family_susp = .7; s.teacher_susp = .9; s.vax_eff = 0.8
s.surveillance = F; s.no_test_vacc = F; s.rapid_test_sens = .8
s.notify = T; s.test = c(F,T); s.isolate = 1
s.quarantine.length = c(7,10); s.quarantine.grace = 3
s.rel_trans = 1/8; s.rel_trans_adult = 2; s.rel_trans_CC = 2; s.n_HH = 2; s.num_adults = 2
s.turnaround.time = 2; s.test_start_day = 1; s.test_quarantine = F
s.time_seed_inf = NA; s.teacher_trans = 1; s.seed_asymp = F; s.nper = 8
s.start_mult = 0; s.includeFamily = T; s.overdisp_off = F; s.include_weekends = T 

# elementary specific
if(level == "Elementary"){
  s.mult_asymp_child = 1; s.rel_trans_HH_symp_child = 2
  s.p_asymp_child = .4; s.p_subclin_child = .4
  s.child_vax = 0; s.high_school = F
  s.child_susp = .5; s.child_trans = .5
  s.n_class = 5; s.n_other_adults = 30
  s.vax.sens = .3 # vaccination level for sensitivity analysis when vaccination is needed
  s.child_vax.vary = .3 # varied vaccination level for second base case
}

# elementary specific
if(level == "Middle"){
  s.mult_asymp_child = .5; s.rel_trans_HH_symp_child = 1
  s.p_asymp_child = .4; s.p_subclin_child = .4
  s.child_vax = .5; s.high_school = F
  s.child_susp = 1; s.child_trans = 1
  s.n_class = 7; s.n_other_adults = 30
  s.vax.sens = .5 # vaccination level for sensitivity analysis when vaccination is needed
  s.child_vax.vary = .3 # varied vaccination level for second base case
}


####************* MAKE DATA FRAME OF PARAMETER SETS TO RUN *******************####
make_df = function(disperse = T, # how to distribute runs
                   attack = s.attack, disperse_transmission = s.disperse_transmission,
                   teacher_susp = s.teacher_susp, start_type = s.start_type, 
                   notify = s.notify, test = s.test, n_tot = s.n_tot, 
                   dedens = s.dedens, n_start = s.n_start, 
                   mult_asymp = s.mult_asymp, isolate = s.isolate, days_inf = s.days_inf, 
                   time = s.time, n_contacts = s.n_contacts,
                   p_asymp_adult = s.p_asymp_adult, p_asymp_child = s.p_asymp_child,
                   p_subclin_adult = s.p_subclin_adult, p_subclin_child = s.p_subclin_child,
                   n_staff_contact = s.n_staff_contact, n_other_adults = s.n_other_adults, 
                   test_sens = s.test_sens, test_frac = s.test_frac, family_susp = s.family_susp, 
                   child_vax = s.child_vax, 
                   rel_trans = s.rel_trans, n_HH = s.n_HH, test_days = s.test_days, 
                   test_type = s.test_type, start_mult = s.start_mult,
                   scenario = s.scenario, high_school = s.high_school, 
                   child_susp = s.child_susp, child_trans = s.child_trans, n_class = s.n_class, 
                   quarantine.length = s.quarantine.length, quarantine.grace = s.quarantine.grace,
                   turnaround.time = s.turnaround.time, test_start_day = s.test_start_day, 
                   test_quarantine = s.test_quarantine, vax_eff = s.vax_eff, 
                   surveillance = s.surveillance, no_test_vacc = s.no_test_vacc, 
                   rapid_test_sens = s.rapid_test_sens, rel_trans_HH_symp_child = s.rel_trans_HH_symp_child, 
                   mult_asymp_child = s.mult_asymp_child, time_seed_inf = s.time_seed_inf, 
                   teacher_trans = s.teacher_trans, seed_asymp = s.seed_asymp, 
                   overdisp_off = s.overdisp_off, include_weekends = s.include_weekends, rel_trans_CC = s.rel_trans_CC,
                   rel_trans_adult = s.rel_trans_adult, num_adults = s.num_adults, 
                   nper = s.nper, includeFamily = s.includeFamily, adult_prob = s.adult_prob, child_prob = s.child_prob) {
  
  # make a grid
  df = expand_grid(attack, disperse_transmission, teacher_susp, start_type, notify, test, n_tot,
                   dedens, n_start, mult_asymp, isolate, days_inf, time, n_contacts, n_staff_contact, n_other_adults,
                   test_sens, test_frac, rel_trans, n_HH, test_days, test_type, start_mult,
                   scenario, high_school, child_susp, child_trans, n_class, p_asymp_adult, p_asymp_child,
                   p_subclin_adult, p_subclin_child, quarantine.length, turnaround.time, test_start_day, family_susp, test_quarantine, vax_eff, surveillance, 
                   child_vax, no_test_vacc, rapid_test_sens, rel_trans_HH_symp_child, mult_asymp_child, time_seed_inf, teacher_trans, quarantine.grace, 
                   seed_asymp, overdisp_off, include_weekends, rel_trans_CC, rel_trans_adult, num_adults, nper, includeFamily,
                   adult_prob, child_prob) 
  
  df = df %>%
    # set up additional variables
    mutate(
      # set up specials
      run_specials_now = ifelse(!high_school, T, F),
      
      # allow for reduce class size
      n_class = ifelse(scenario == "Reduced class size", n_class*2, n_class),
      
      # reduce random contacts in alternative schedules
      n_contacts = ifelse(scenario!="Base case", n_contacts/2, n_contacts),
      n_staff_contact = ifelse(scenario!="Base case", n_staff_contact/2, n_staff_contact),
      
      # set schedule type
      type = ifelse(grepl("On/off", scenario), "On/off", "base"),
      type = ifelse(grepl("A/B", scenario), "A/B", type),
      type = ifelse(grepl("Remote", scenario), "Remote", type),
      
      # set number of days
      total_days = ifelse(grepl("1", scenario), 1, 5),
      total_days = ifelse(grepl("2", scenario), 2, total_days),
      sim = row_number()) %>% 
    
    # remove duplicated scenarios
    # don't vary testing params if you're not testing
    filter((test_days=="week" & test_sens == .9 & test_frac == .9 & test_type == "all") | test) %>%
    # don't vary notifications and testing if remote
    filter(type!="Remote" | (!notify & !test)) %>%
    # adjust incidence for vaccination
    mutate(child_prob=child_prob/(1-child_vax*vax_eff),
           adult_prob=adult_prob/(1-family_susp*vax_eff))
  
  # repeat according to simulation count
  if(disperse) df <- df[rep(row.names(df), n_tot),] %>% mutate(i = row_number())
  
  return(df)
}

####************************** RUN PARALLELIZED CODE **************************#### 

#### RUN SINGLE ITERATION ####
sims = function(df, i, synthpop, class = NA){
  #set.seed(i)
  out = mult_runs(version = 2, N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i], 
                  run_specials_now = df$run_specials_now[i], start_mult = df$start_mult[i], high_school = df$high_school[i],
                  attack = df$attack[i], child_susp = df$child_susp[i], time = df$time[i], synthpop = synthpop,
                  rel_trans = df$rel_trans[i], n_other_adults = df$n_other_adults[i], n_class = df$n_class[i],
                  notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], num_adults = df$num_adults[i],
                  start_type = df$start_type[i], child_trans = df$child_trans[i], type = df$type[i],
                  days_inf = df$days_inf[i], disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i], 
                  total_days = df$total_days[i],  teacher_susp = df$teacher_susp[i], mult_asymp = df$mult_asymp[i], 
                  isolate = df$isolate[i], test_sens = df$test_sens[i], test_frac = df$test_frac[i],
                  p_asymp_adult = df$p_asymp_adult[i], p_asymp_child = df$p_asymp_child[i], 
                  p_subclin_adult = df$p_subclin_adult[i], p_subclin_child = df$p_subclin_child[i],
                  child_prob = df$child_prob[i], adult_prob = df$adult_prob[i], class = class,
                  test_days = df$test_days[i], test_type = df$test_type[i], rel_trans_HH = .14/df$attack[i], n_HH = df$n_HH[i],
                  quarantine.length = df$quarantine.length[i], quarantine.grace = df$quarantine.grace[i],
                  turnaround.time = df$turnaround.time[i], vax_eff = df$vax_eff[i],
                  test_start_day = df$test_start_day[i], family_susp = df$family_susp[i], test_quarantine = df$test_quarantine[i], surveillance = df$surveillance[i],
                  child_vax = df$child_vax[i], no_test_vacc = df$no_test_vacc[i], rapid_test_sens = df$rapid_test_sens[i], 
                  rel_trans_HH_symp_child = df$rel_trans_HH_symp_child[i], mult_asymp_child = df$mult_asymp_child[i],
                  time_seed_inf = df$time_seed_inf[i], teacher_trans = df$teacher_trans[i], seed_asymp = df$seed_asymp[i], 
                  overdisp_off = df$overdisp_off[i], include_weekends = df$include_weekends[i], rel_trans_CC = df$rel_trans_CC[i],
                  rel_trans_adult = df$rel_trans_adult[i], nper = df$nper[i], includeFamily = df$includeFamily[i]) %>%
    mutate(id = df$scenario[i], i = i, sim = df$sim[i])

  out = out %>% bind_cols(df[i,])
  save(out, file = paste0("results", i, "_", Sys.time(), ".RData"))
  print(i)
  #rm(out); gc()
  return(out)
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop, class = NA){  
  
  doMC::registerDoMC(cores = detectCores())
  
  detectCores()
  foreach::getDoParWorkers()

  
  foreach(i=1:nrow(df), .errorhandling = "pass", .combine = rbind) %dopar% {
    each_filename <- paste("RESULT_", "_", i, ".RData", sep = "")
    out <- tryCatch(
      withCallingHandlers(sims(df, i, synthpop, class = class),
                          error = function(e){
                            stack <- sys.calls()
                            save(stack, file = paste("./Desktop/Test/Errors/", paste("error", i, sep = "_"), ".RData", sep = ""))
                            
                            return(NULL)}),
      error = function(c){
        message(paste("Error occurred in row", i))
        message("Original error message:")
        message(c)
        return(NULL)
      })
  }
  
}


