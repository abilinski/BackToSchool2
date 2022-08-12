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
library(dplyr)
library(data.table)
library(parallel)
library(filesstrings)

source("abm_correction.R")
options(warn = 2)

job.number <- commandArgs(trailingOnly = TRUE)[1]

####************************** FUNCTIONS TO VARY PARAM SETS **************************####

#### RUN SINGLE ITERATION ####
sims = function(df, i, synthpop, class = NA, job.no = "0"){
  #set.seed(i)
  id.number <- paste(i, job.no, sep = ".")
  out = mult_runs(version = df$version[i], N = df$n_tot[i], n_contacts = df$n_contacts[i], n_staff_contact = df$n_staff_contact[i],
                  run_specials_now = df$run_specials_now[i], start_mult = df$start_mult[i], high_school = df$high_school[i],
                  attack = df$attack.df[i], child_susp = df$child_susp[i], time = df$time[i], synthpop = synthpop,
                  rel_trans = df$rel_trans[i], n_other_adults = df$n_other_adults[i], n_class = df$n_class[i],
                  notify = df$notify[i], test = df$test[i], dedens = df$dedens[i], num_adults = df$num_adults[i], teacher_trans = df$teacher_trans[i],
                  start_type = df$start_type[i], child_trans = df$child_trans[i], type = df$type[i],
                  days_inf = df$days_inf[i], disperse_transmission = df$disperse_transmission[i], n_start = df$n_start[i],
                  total_days = df$total_days[i],  teacher_susp = df$teacher_susp[i], mult_asymp = df$mult_asymp[i], mult_asymp_child = df$mult_asymp_child[i],
                  isolate = df$isolate[i], test_sens = df$test_sens[i], test_frac = df$test_frac[i],
                  p_asymp_adult = df$p_asymp_adult[i], p_asymp_child = df$p_asymp_child[i],
                  p_subclin_adult = df$p_subclin_adult[i], p_subclin_child = df$p_subclin_child[i],
                  child_prob = df$child_prob[i], adult_prob = df$adult_prob[i], class = class,
                  test_days = df$test_days[i], test_type = df$test_type[i], no_test_vacc = df$no_test_vacc[i], rel_trans_HH = df$rel_trans_HH[i], rel_trans_HH_symp_child = df$rel_trans_child_symp_HH[i],
                  rel_trans_CC = df$rel_trans_CC[i], n_HH = df$n_HH[i],
                  quarantine.length = df$quarantine.length[i], quarantine.grace = df$quarantine.grace[i], turnaround.time = df$turnaround.time[i],
                  test_start_day = df$test_start_day[i], family_susp = df$family_susp[i], test_quarantine = df$test_quarantine[i], vax_eff = df$vax_eff[i], child_vax = df$child.vax[i], time_seed_inf = df$time_seed_inf[i], seed_asymp = df$seed_asymp[i], surveillance = df$surveillance[i], overdisp_off = df$overdisp_off[i], include_weekends = df$include_weekends[i], rel_trans_adult = df$rel_trans_adult[i], rapid_test_sens = df$rapid_test_sens[i],
                  adult_unvax_hosp_rate = df$adult_unvax_hosp_rate[i], child_unvax_hosp_rate = df$child_unvax_hosp_rate[i], adult_vax_hosp_rate = df$adult_vax_hosp_rate[i], child_vax_hosp_rate = df$child_vax_hosp_rate[i])
  out[,c("id")] <- id.number
  out <- bind_cols(out, df[i,!(colnames(df) %in% c("surveillance"))])
  
  #save(out, file = paste0("results", i, ".RData"))
  #rm(out)
  return(out)
}

#### RUN IN PARALLEL ####
run_parallel = function(df, synthpop, class = NA, job = "0"){  
  
  doMC::registerDoMC(cores = detectCores())
  
  for(j in 0:60){
    
    wd <- "/scratch"
    setwd(wd)
    
    start <- j*nrow(df)/61 + 1
    end <- (j+1)*nrow(df)/61
    
    foreach(i=start:end, .combine=rbind) %dopar% {
      each_filename <- paste("RESULT_", job, "_", j, "_", i, ".RData", sep = "")
      out <- tryCatch(
        withCallingHandlers(sims(df, i, synthpop, class = class, job.no = job),
                            error = function(e){stack <- sys.calls()
                            save(stack, file = paste("/n/home00/jgiardina/BackToSchool2/3 - Output/Error Output/JNO Full Paper", paste("error", job, j, i, sep = "_"), ".RData", sep = ""))
                            return(NULL)}),
        error = function(c){
          message(paste("Error occurred in row", i))
          message("Original error message:")
          message(c)
          return(NULL)
        })
      save(out, file = each_filename)
      return(NULL)
    }
    
    filelist <- list.files(pattern = paste("RESULT_", job, "_", j, "_.*", ".RData", sep = ""), full.names = TRUE)
    output <- tryCatch(
      rbindlist(mclapply(1:length(filelist), function(a){load(filelist[a]); out = out; return(out)}, mc.cores = detectCores())),
      error = function(c){message("Original error message:")
        message(c)
        move_files(filelist, "/n/home00/jgiardina/BackToSchool2/3 - Output/Error Output/JNO Full Paper")
        return(NULL)})
    setwd("/n/home00/jgiardina/BackToSchool2/3 - Output/Final Output/JNO Full Paper/Correction")
    save(output, file = paste(paste("elem_results", job, j, Sys.Date(), sep = "_"), ".RData", sep = ""))
    remove(output)
    remove(filelist)
    gc()
    
  }
  
}

# simulation setup

#Set hospitalization parameters
adult.ifr <- 500/1e6
child.ifr <- 20/1e6 

adult.p.die_hosp <- 2.1e-2
child.p.die_hosp <- 0.7e-2

adult.p_hosp <- round(adult.ifr/adult.p.die_hosp,3)
child.p_hosp <- round((24/63.7)*child.ifr/child.p.die_hosp,3) #Conversion factor is for 5-11 vs <17

nsamp <- 2

df_ELEM.temp <- expand_grid(mitigation = c(seq(from = 0, to = 0.99, by = 0.01), 0.9999),
                            prob = c(0.001, seq(from = 1, to = 60, by = 1))*3/100000) %>%
  mutate(start_type = "cont", n_HH = 2, test_days = "week",
         test_type = "all", test_sens = .9,
         total_days = 5, run_specials_now = T,
         child_trans = 0.5, teacher_trans = 1, high_school = F,
         p_asymp_adult = .2, p_asymp_child = 0.4,
         p_subclin_adult = .2, p_subclin_child = 0.4,
         mult_asymp = .5, mult_asymp_child = 1, quarantine.length = c(7), quarantine.grace = 3,
         turnaround.time = 1, n_other_adults = 30, n_class = 5,
         test_quarantine = F,  disperse_transmission = c(F), dedens = 1, n_start = 1, 
         days_inf = 5, time = 30, n_contacts = 20,
         n_staff_contact = 10, rel_trans_child_symp_HH = 1/child_trans, rel_trans = 1/8, start_mult = 0, disperse = T, 
         test_start_day = 1, n_tot = 1, isolate = T, no_test_vacc = F, time_seed_inf = NA, seed_asymp = F, surveillance = F, overdisp_off = F, num_adults = 2, version = 2, include_weekends = T, rel_trans_adult = 2,  rapid_test_sens = 0.8,
         adult_unvax_hosp_rate = adult.p_hosp, child_unvax_hosp_rate = child.p_hosp, adult_vax_hosp_rate = 0, child_vax_hosp_rate = 0) 

df_ELEM.delta =  df_ELEM.temp %>%
  expand_grid(type = c("base"),
              variant.attack = c(0.07),
              vax.rates = c("t50.c0", "t50.c25", "t50.c50", "t70.c0", "t70.c25", "t70.c50", "t70.c70"),
              notify.scenario = c("yes.wo.test", "yes.w.test90"),
              vax_eff = c(0.7)) %>%
  mutate(attack.df = variant.attack*(1-mitigation),
         child.vax = ifelse(vax.rates == "t50.c0" | vax.rates == "t70.c0", 0,
                            ifelse(vax.rates == "t50.c25" | vax.rates == "t70.c25", 0.25,
                                   ifelse(vax.rates == "t50.c50" | vax.rates == "t70.c50", 0.5,
                                          ifelse(vax.rates == "t70.c70", 0.7, -1)))),
         teacher_susp = ifelse(vax.rates == "t50.c0" | vax.rates == "t50.c25" | vax.rates == "t50.c50", 0.5, 0.7),
         child_susp = 0.5,
         family_susp = teacher_susp,
         notify = ifelse(notify.scenario == "no", FALSE, TRUE),
         test = ifelse(notify.scenario == "yes.w.test90" | notify.scenario == "yes.w.test100", TRUE, FALSE),
         test_frac = ifelse(notify.scenario == "yes.w.test90", 0.9, ifelse(notify.scenario == "yes.w.test100", 1, 0)),
         adult_prob = prob/((1-family_susp)+family_susp*(1-vax_eff)), child_prob = prob/((1 - child.vax)+child.vax*(1-vax_eff)), rel_trans_HH = 2*variant.attack/attack.df, rel_trans_CC = variant.attack/attack.df)

df_ELEM.delta.vax.eff =  df_ELEM.temp %>%
  expand_grid(type = c("base"),
              variant.attack = c(0.07),
              vax.rates = c("t70.c0", "t70.c25", "t70.c50", "t70.c70"),
              notify.scenario = c("yes.wo.test"),
              vax_eff = c(0.25, 0.5, 0.9)) %>%
  mutate(attack.df = variant.attack*(1-mitigation),
         child.vax = ifelse(vax.rates == "t50.c0" | vax.rates == "t70.c0", 0,
                            ifelse(vax.rates == "t50.c25" | vax.rates == "t70.c25", 0.25,
                                   ifelse(vax.rates == "t50.c50" | vax.rates == "t70.c50", 0.5,
                                          ifelse(vax.rates == "t70.c70", 0.7, -1)))),
         teacher_susp = ifelse(vax.rates == "t50.c0" | vax.rates == "t50.c25" | vax.rates == "t50.c50", 0.5, 0.7),
         child_susp = 0.5,
         family_susp = teacher_susp,
         notify = ifelse(notify.scenario == "no", FALSE, TRUE),
         test = ifelse(notify.scenario == "yes.w.test90" | notify.scenario == "yes.w.test100", TRUE, FALSE),
         test_frac = ifelse(notify.scenario == "yes.w.test90", 0.9, ifelse(notify.scenario == "yes.w.test100", 1, 0)),
         adult_prob = prob/((1-family_susp)+family_susp*(1-vax_eff)), child_prob = prob/((1 - child.vax)+child.vax*(1-vax_eff)), rel_trans_HH = 2*variant.attack/attack.df, rel_trans_CC = variant.attack/attack.df)

df_ELEM.high_vax_coverage =  df_ELEM.temp %>%
  expand_grid(type = c("base"),
              variant.attack = c(0.07),
              vax.rates = c("t90.c90"),
              notify.scenario = c("yes.wo.test"),
              vax_eff = c(0.7)) %>%
  mutate(attack.df = variant.attack*(1-mitigation),
         child.vax = ifelse(vax.rates == "t50.c0" | vax.rates == "t70.c0", 0,
                            ifelse(vax.rates == "t50.c25" | vax.rates == "t70.c25", 0.25,
                                   ifelse(vax.rates == "t50.c50" | vax.rates == "t70.c50", 0.5,
                                          ifelse(vax.rates == "t70.c70", 0.7,
                                                 ifelse(vax.rates == "t90.c90", 0.9, -1))))),
         teacher_susp = ifelse(vax.rates == "t50.c0" | vax.rates == "t50.c25" | vax.rates == "t50.c50", 0.5,
                               ifelse(vax.rates == "t90.c90", 0.9, 0.7)),
         child_susp = 0.5,
         family_susp = teacher_susp,
         notify = ifelse(notify.scenario == "no", FALSE, TRUE),
         test = ifelse(notify.scenario == "yes.w.test90" | notify.scenario == "yes.w.test100", TRUE, FALSE),
         test_frac = ifelse(notify.scenario == "yes.w.test90", 0.9, ifelse(notify.scenario == "yes.w.test100", 1, 0)),
         adult_prob = prob/((1-family_susp)+family_susp*(1-vax_eff)), child_prob = prob/((1 - child.vax)+child.vax*(1-vax_eff)), rel_trans_HH = 2*variant.attack/attack.df, rel_trans_CC = variant.attack/attack.df)

df_ELEM.prev.variants = df_ELEM.temp %>%
  expand_grid(type = c("base"),
              variant.attack = c(0.02, 0.035),
              vax.rates = c("t70.c0", "t70.c25", "t70.c50", "t70.c70"),
              notify.scenario = c("yes.wo.test"),
              vax_eff = c(0.7)) %>%
  mutate(attack.df = variant.attack*(1-mitigation),
         child.vax = ifelse(vax.rates == "t50.c0" | vax.rates == "t70.c0", 0,
                            ifelse(vax.rates == "t50.c25" | vax.rates == "t70.c25", 0.25,
                                   ifelse(vax.rates == "t50.c50" | vax.rates == "t70.c50", 0.5,
                                          ifelse(vax.rates == "t70.c70", 0.7, -1)))),
         teacher_susp = ifelse(vax.rates == "t50.c0" | vax.rates == "t50.c25" | vax.rates == "t50.c50", 0.5, 0.7),
         child_susp = 0.5,
         family_susp = teacher_susp,
         notify = ifelse(notify.scenario == "no", FALSE, TRUE),
         test = ifelse(notify.scenario == "yes.w.test60" | notify.scenario == "yes.w.test100", TRUE, FALSE),
         test_frac = ifelse(notify.scenario == "yes.w.test60", 0.6, ifelse(notify.scenario == "yes.w.test100", 1, 0)),
         adult_prob = prob/((1-family_susp)+family_susp*(1-vax_eff)), child_prob = prob/((1 - child.vax)+child.vax*(1-vax_eff)), rel_trans_HH = 2*variant.attack/attack.df, rel_trans_CC = variant.attack/attack.df)

df_ELEM <- rbind(df_ELEM.delta, df_ELEM.delta.vax.eff, df_ELEM.high_vax_coverage, df_ELEM.prev.variants)

df_ELEM <- df_ELEM[rep(row.names(df_ELEM), nsamp),] %>% mutate(i = row_number())


# don't double run
print(dim(df_ELEM))

# make class
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
run_parallel(df_ELEM, synthpop, class = class, job = job.number)

#NOTE: Results are output as an RData file for multiple sets of simulations simulation -- results must be bound into a single dataframe before generating the figures.