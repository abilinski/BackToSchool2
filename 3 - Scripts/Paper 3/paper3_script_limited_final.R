####************************** SIMULATIONS **************************#### 


#***************************** RUN BASE MODELS *****************************####
# Note that I'm continuously updating this file
# But it should give an idea of how to run things

#### SETUP ####
# install packages
library(tidyverse)
library(tictoc)
library(igraph)
library(foreach)
library(BackToSchool)
library(doMC)
library(lhs)

source("abm6.R")

#This file is designed to be run on a cluster across 20 jobs -- the job.number variable is used to name output files from each node
job.number <- commandArgs(trailingOnly = TRUE)[1]

####************************** FUNCTIONS TO VARY PARAM SETS **************************####

#### RUN SINGLE ITERATION ####
sims = function(df, i, synthpop, class = NA, job.no = "0"){
  #set.seed(i)
  out = mult_runs(version = 2, N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i],
                  run_specials_now = df$run_specials_now[i], start_mult = df$start_mult[i], high_school = df$high_school[i],
                  attack = df$attack.df[i], child_susp = df$child_susp[i], time = df$time[i], synthpop = synthpop,
                  rel_trans = df$rel_trans[i], n_other_adults = df$n_other_adults[i], n_class = df$n_class[i],
                  notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], num_adults = 1,
                  start_type = df$start_type[i], child_trans = df$child_trans[i], type = df$type[i],
                  days_inf = df$days_inf[i], disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i],
                  total_days = df$total_days[i],  teacher_susp = df$teacher_susp[i], mult_asymp = df$mult_asymp[i],
                  isolate = df$isolate[i], test_sens = df$test_sens[i], test_frac = df$test_frac[i],
                  p_asymp_adult = df$p_asymp_adult[i], p_asymp_child = df$p_asymp_child[i],
                  p_subclin_adult = df$p_subclin_adult[i], p_subclin_child = df$p_subclin_child[i],
                  child_prob = df$child_prob[i], adult_prob = df$adult_prob[i], class = class,
                  test_days = df$test_days[i], test_type = df$test_type[i], rel_trans_HH = 2*df$variant.attack[i]/df$attack.df[i], n_HH = df$n_HH[i],
                  quarantine.length = df$quarantine.length[i], turnaround.time = df$turnaround.time[i],
                  test_start_day = df$test_start_day[i], family_susp = df$family_susp[i], test_quarantine = df$test_quarantine[i], vax_eff = df$vax_eff[i]) %>%
    mutate(id = paste(i, job.no, sep = ".")) %>% bind_cols(df[i,])
  
  #save(out, file = paste0("results", i, ".RData"))
  #rm(out)
  return(out)
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop, class = NA, job = "0"){  
  
  doMC::registerDoMC(cores = detectCores())
  
  foreach(i=1:nrow(df), .combine=rbind) %dopar% {
    each_filename <- paste("RESULT_", job, "_", i, ".RData", sep = "")
    out <- sims(df, i, synthpop, class = class, job.no = job)
    save(out, file = each_filename)
  }
  
}

# cluster
wd <- "/n/home00/jgiardina/BackToSchool2/3 - Output/Final_Runs_Output"
setwd(wd)

# simulation setup
nsamp <- 27500
random.draws <- randomLHS(n = nsamp, k = 8)
df_ELEM = data.frame(mitigation = qunif(random.draws[,1], min = 0, max = 1),
                     prob = qunif(random.draws[,4],min = 0, max = 60)*3/100000) %>%
  mutate(start_type = "cont", n_HH = 2, test_days = "week",
         test_type = "all", test_sens = .9,
         total_days = 5, run_specials_now = T,
         child_trans = 0.5, high_school = F,
         p_asymp_adult = .2, p_asymp_child = 0,
         p_subclin_adult = .2, p_subclin_child = .8,
         mult_asymp = .5, quarantine.length = c(7),
         turnaround.time = 1, n_other_adults = 30, n_class = 5,
         test_quarantine = F,  disperse_transmission = c(F), dedens = 1, n_start = 1, 
         days_inf = 5, time = 30, n_contacts = 20,
         n_staff_contact = 10, rel_trans = 1/8, start_mult = 0, disperse = T, 
         test_start_day = 1, n_tot = 1, isolate = T, sim = row_number()) %>%
  expand_grid(type = c("base", "Remote"),
              variant.attack = c(0.02, 0.035, 0.07),
              child.vax = c(0, 0.5),
              teacher_susp = c(0.5, 0.7),
              notify.scenario = c("yes.wo.test"),
              vax_eff = c(0.9)) %>%
  mutate(attack.df = variant.attack*(1-mitigation),
         child_susp = 0.5 + 0.5*child.vax,
         family_susp = teacher_susp,
         notify = ifelse(notify.scenario == "no", FALSE, TRUE),
         test = ifelse(notify.scenario == "yes.w.test60" | notify.scenario == "yes.w.test100", TRUE, FALSE),
         test_frac = ifelse(notify.scenario == "yes.w.test60", 0.6, ifelse(notify.scenario == "yes.w.test100", 1, 0)),
         adult_prob = prob/((1-family_susp)+family_susp*(1-vax_eff)), child_prob = prob)


# don't double run
print(dim(df_ELEM))

# make class
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
run_parallel(df_ELEM, synthpop, class = class, job = job.number)

#NOTE: Results are output as an RData file for each simulation -- results must be bound into a single dataframe before generating the figures.
