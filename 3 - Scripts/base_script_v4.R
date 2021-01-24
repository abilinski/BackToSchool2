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

# local
wd = "~/Dropbox/Schools/4 - Output/"

# cluster
wd = "/home/rstudio/"
setwd(wd)

####************************** FUNCTIONS TO VARY PARAM SETS **************************#### 

#### RUN SINGLE ITERATION ####
sims = function(df, i, synthpop, class = NA){
  #set.seed(i)
  out = mult_runs(N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i], 
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
                  quarantine.length = df$quarantine.length[i], turnaround.time = df$turnaround.time[i]) %>%
    mutate(id = df$scenario[i], i = i, sim = df$sim[i]) %>% left_join(df[i,], "i")
  
  save(out, file = paste0("results", i, "_", Sys.time(), ".RData"))
  rm(out); gc()
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop, class = NA){  
  
  doMC::registerDoMC(cores = detectCores())
  
  detectCores()
  foreach::getDoParWorkers()
  
  (time_foreach <- system.time(
    foreach(i=1:nrow(df)) %dopar% sims(df, i, synthpop, class = NA)
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
                   high_school = T, child_susp = 1, child_trans = 1, n_class = 63, prob = 0, disperse = T, 
                   quarantine.length = 10, turnaround.time = 1) {
  
  # make a grid
  df = expand.grid(attack, disperse_transmission, teacher_susp, start_type, notify, test, 1,
                   dedens, n_start, mult_asymp, isolate, days_inf, time, n_contacts, n_staff_contact, n_other_adults,
                   test_sens, test_frac, rel_trans, n_HH, test_days, test_type, start_mult,
                   scenario, high_school, child_susp, child_trans, n_class, p_asymp_adult, p_asymp_child,
                   p_subclin_adult, p_subclin_child, prob, quarantine.length, turnaround.time) 
  
  names(df) = c("attack", "disperse_transmission", "teacher_susp", "start_type", "notify", "test", "n_tot",
                "dedens", "n_start", "mult_asymp", "isolate", "days_inf", "time", "n_contacts", "n_staff_contact", "n_other_adults",
                "test_sens", "test_frac", "rel_trans", "n_HH", "test_days", "test_type", "start_mult", 
                "scenario", "high_school", "child_susp", "child_trans", "n_class", "p_asymp_adult", "p_asymp_child",
                "p_subclin_adult", "p_subclin_child", "prob", "quarantine.length", "turnaround.time")
  
  df = df %>%  filter(!(test & !notify)) %>%
    filter(teacher_susp==1 | (notify == T & test == F)) %>%
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
           child_prob = prob/2,
           adult_prob = prob,
           sim = row_number()) %>% filter(type!="Remote" | (!notify & !test))
  
  # repeat according to simulation count
  if(disperse) df <- df[rep(row.names(df), n_tot),] %>% mutate(i = row_number())
  
  return(df)
}

####************************** SIMULATIONS **************************#### 
#### BASE CASES ####

#### ELEMENTARY SCHOOL BASE ####
setwd(paste0(wd, "Base Elem"))

# remove files
file.remove(list.files())
df_ELEM = make_df(n_tot = 2000, start_type = c("mix", "teacher"), teacher_susp = c(.33,1),
                  child_trans = .5, child_susp = .5, high_school = F, 
                  p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  n_other_adults = 30, n_class = 5, n_HH = 2)
run_parallel(df_ELEM, synthpop)

#### HIGH SCHOOL BASE ####
# set working directory
setwd(paste0(wd, "Base HS"))

# remove files
file.remove(list.files())

# choose parameter set
df_HS = make_df(n_tot = 2000, n_class = 16, high_school = T, teacher_susp = c(.33,1),
                p_asymp_adult = .2, p_asymp_child = .4,
                p_subclin_adult = .2, p_subclin_child = .4,
                mult_asymp = .5,
                n_HH = 2, start_type = "mix") 

# run code
run_parallel(df_HS, synthpop_HS)

#### SUPPLEMENTS ####

n_supp = 2000
tic()
#### ELEMENTARY SCHOOL ####

#### EQUAL TRANS, 5 seconds
setwd(paste0(wd, "Elem_supp1"))
file.remove(list.files())
df_ELEM_supp1 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = 1, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = .4, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .4,
                        start_type = "mix",
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp1, synthpop)

#### ALTERNATIVE SCHEDULES, 5 seconds
setwd(paste0(wd, "Elem_supp2"))
file.remove(list.files())
df_ELEM_supp2 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "mix",
                        scenario = c("A/B (5)", "A/B (1)", "On/off (2)", "On/off (1)"),
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp2, synthpop)

#### OVERDISPERSION, 5 seconds
setwd(paste0(wd, "Elem_supp3"))
file.remove(list.files())
df_ELEM_supp3 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "mix",
                        disperse_transmission = T,
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp3, synthpop)


#### START TYPE TEACHER
setwd(paste0(wd, "Elem_supp5"))
file.remove(list.files())
df_ELEM_supp5 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "teacher",
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp5, synthpop)

# HIGH SCHOOL SUPPLEMENTAL ANALYSES

# SUSCEPTIBLE - 8 seconds
setwd(paste0(wd, "HS_supp1"))
file.remove(list.files())
df_HS_supp1 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 0.5,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp1, synthpop_HS)

# SCENARIOS - 7 seconds
setwd(paste0(wd, "HS_supp2"))
file.remove(list.files())
df_HS_supp2 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 1,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      scenario = c("A/B (1)", "On/off (2)", "On/off (1)"),
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp2, synthpop_HS)

# DISPERSION - 10 seconds
setwd(paste0(wd, "HS_supp3"))
file.remove(list.files())
df_HS_supp3 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 1,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      disperse_transmission = T,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp3, synthpop_HS)
toc()

#### DYNAMIC ADDITIONS ####

#### ELEMENTARY SCHOOL DYNAMIC ####
setwd(paste0(wd, "Dynamic Elem"))
file.remove(list.files())
df_ELEM = make_df(n_tot = 2000, start_type = "cont", n_HH = 2,
                  scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                  prob = c(1,10,25,50,100)*3/100000, time = 60,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5,
                  n_other_adults = 30, n_class = 5) 
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])
run_parallel(df_ELEM, synthpop, class = class)

setwd(paste0(wd, "Dynamic Elem Sens"))
file.remove(list.files())
df_ELEM = make_df(n_tot = 1000, start_type = "cont", n_HH = c(0, 5, 10),
                  scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                  prob = c(1,10,25,50,100)*3/100000, time = 60,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5,
                  n_other_adults = 30, n_class = 5) 
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])
run_parallel(df_ELEM, synthpop, class = class)

setwd(paste0(wd, "Elem Test"))
file.remove(list.files())
df_ELEM = make_df(attack = c(.01, .02), 
                  n_tot = 1000, start_type = "cont", n_HH = 2,
                  test_days = c("week", "2x_week"), test_type = c("all", "staff"),
                  test_frac = c(.5, .7, .9),
                  scenario = c("Base case"), teacher_susp = c(.33,1),
                  prob = c(1,10,25,50,100)*3/100000, time = 30,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5, quarantine.length = c(7, 10),
                  turnaround.time = c(1,2),
                  n_other_adults = 30, n_class = 5) 


df_ELEM1 = make_df(attack = c(.01, .02), notify = T, test = F,
                  n_tot = 1000, start_type = "cont", n_HH = 2,
                  test_days = c("week", "2x_week"), test_type = c("all", "staff"),
                  test_frac = c(.5, .7, .9),
                  scenario = c("A/B (2)"), teacher_susp = c(.33,1),
                  prob = c(1,10,25,50,100)*3/100000, time = 30,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5, quarantine.length = 10, turnaround.time = 2,
                  n_other_adults = 30, n_class = 5) 

df_ELEM2 = make_df(attack = c(.01, .02), notify = F, test = F,
                   n_tot = 1000, start_type = "cont", n_HH = 2,
                   test_days = c("week", "2x_week"), test_type = c("all", "staff"),
                   test_frac = c(.5, .7, .9),
                   scenario = c("Remote"), teacher_susp = c(.33,1),
                   prob = c(1,10,25,50,100)*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5,
                   n_other_adults = 30, n_class = 5) 


set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])
run_parallel(df_ELEM, synthpop, class = class)
run_parallel(df_ELEM1, synthpop, class = class)
run_parallel(df_ELEM2, synthpop, class = class)

#### HIGH SCHOOL DYNAMIC ####
setwd(paste0(wd, "Dynamic High"))
file.remove(list.files())
df_HS = make_df(n_tot = 250, n_class = 16, high_school = T, n_HH = 2,
                start_type = "cont",
                scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                prob = c(1,10,25,50,100)*3/100000, time = 60)
set.seed(3232)
class = make_school(synthpop = synthpop_HS, n_other_adults = df_HS$n_other_adults[1], 
                    includeFamily = T, n_class = df_HS$n_class[1])
run_parallel(df_HS, synthpop_HS, class = class)

setwd(paste0(wd, "Dynamic High Sens"))
file.remove(list.files())
df_HS = make_df(n_tot = 100, n_class = 16, high_school = T, n_HH = c(0, 5, 10),
                start_type = "cont",
                scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                prob = c(1,10,25,50,100)*3/100000, time = 60)
set.seed(3232)
class = make_school(synthpop = synthpop_HS, n_other_adults = df_HS$n_other_adults[1], 
                    includeFamily = T, n_class = df_HS$n_class[1])
run_parallel(df_HS, synthpop_HS, class = class)



