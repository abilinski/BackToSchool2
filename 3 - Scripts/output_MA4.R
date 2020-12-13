#***************************** RUN BASE MODELS *****************************####
# Note that I'm continuously updating this file
# But it should give an idea of how to run things

#### SETUP ####
# install packages
library(tidyverse)
library(tictoc)
library(igraph)
library(devtools)

setwd("/home/rstudio/BackToSchool/output/MA_elem_inc")

#### RUN OUTPUT ####
# Note that I've been changing the format around quite a bit on this.

# set up output
n_tot = 1000
attack = c(0.01, 0.02)
child_trans = c(.5)
isolate = c(F, T)
test = c(F,T)
notify = c(F, T)
disperse_transmission = c(F)
teacher_susp = c(1)
dedens = 1
n_start = 1
test_days = c("week") # "2x_week", "day")
n_HH = c(0, 3)
j = 1:2
div = c(.5, 1, 2)
type = c("all") # "students", "staff"
df = expand.grid(attack, notify, test, dedens, teacher_susp, child_trans, isolate, disperse_transmission, n_start, type, n_HH, test_days, j, div) %>%
  rename("attack" = 1, "notify" = 2, "test" = 3, "dedens" = 4, "teacher_susp" = 5,
         "child_trans" = 6, "isolate" = 7, "disperse_transmission" = 8, "n_start" = 9, "test_type" = 10, "n_HH" = 11, test_days = 12, "j" = 13, "div" = 14) %>% 
  filter(!(test & !notify)) %>%
  filter(test | test_type=="all") %>%
  filter(test | test_days=="week") %>%
  filter(!(!isolate & (notify | test))) %>% mutate(id = row_number())
keep = data.frame()
time = 120

# set up function
sims = function(df, i){
  
  if(df$j[i] == 1){
    temp = mult_runs(N = n_tot, n_contacts = 10, run_specials_now = T, start_type = "cont", test_days = df$test_days[i],
                     attack = df$attack[i], child_susp = 1/2, time = time, n_staff_contact = 10, test_type = df$test_type[i],
                     class = class, notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], n_HH = df$n_HH[i], rel_trans = 1/6,
                     days_inf = 5, disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i], 
                     teacher_susp = df$teacher_susp[i], child_trans = df$child_trans[i], isolate = df$isolate[i], child_prob = .047*df$div[i], adult_prob = .016*df$div[i]) %>%
      mutate(id = "Base case") %>% mutate(sim = i) 
  } else if(df$j[i] == 2){
    
    temp = mult_runs(N = n_tot, n_contacts = 10, run_specials_now = T,time = time, n_staff_contact = 10, test_days = df$test_days[i],
                     attack = df$attack[i], child_susp = 1/2, type = "A/B", total_days = 2, test_type = df$test_type[i], 
                     class = class, notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], n_HH = df$n_HH[i], start_type = "cont",
                     days_inf = 5, disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i],  rel_trans = 1/6,
                     teacher_susp = df$teacher_susp[i], child_trans = df$child_trans[i], isolate = df$isolate[i], child_prob = .047*df$div[i], adult_prob = .016*df$div[i]) %>%
      mutate(id = "A/B (2)") %>% mutate(sim = i)
  }
    

  save(temp, file = paste0("results", i, ".RData"))
  
  rm(temp); gc()
}

library(foreach) 
library(doMC) 
doMC::registerDoMC(cores = detectCores())

detectCores()
foreach::getDoParWorkers()

(time_foreach <- system.time(
  foreach(i=1:nrow(df)) %dopar% sims(df, i)
)[3])
