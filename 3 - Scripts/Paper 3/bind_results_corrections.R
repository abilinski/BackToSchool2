library(dplyr)
library(data.table)

setwd("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Correction/Raw Data")

test.sample.fraction <- 0.1

#Bind initial job files and summarize across groups
for(j in 1:50){
  
  filelist <- list.files(pattern = paste("elem_results_", j, "_.*RData", sep = ""))
  output <- rbindlist(lapply(1:length(filelist), function(a){load(filelist[a]); output = output; return(output)}))
  
  output <- data.table(output)
  
  print(paste("Initial Size for job", j, ":", nrow(output)))
  
  #Prepare data
  data_1 <- output[, `:=`(attack = as.numeric(attack),
                       test_quarantine = as.numeric(test_quarantine),
                       prob = prob*100000/3,
                       all.inschool = as.numeric(class) + as.numeric(related_arts) + as.numeric(random) + as.numeric(random_staff),
                       all.adults = as.numeric(adult) + as.numeric(family),
                       all.children = as.numeric(children),
                       all.overall = as.numeric(adult) + as.numeric(family) + as.numeric(children),
                       hosp = as.numeric(hosp_rate))]
  
  remove(output)
  
  gc()
  
  print("Data step 1 complete")
  
  data_2 <- data_1[, .(inschool.outbreak_inc.1 = mean(all.inschool >= 1),
                       all.inschool = mean(all.inschool),
                       all.adults = mean(all.adults),
                       all.children = mean(all.children),
                       all.overall = mean(all.overall),
                       hosp = mean(hosp),
                       
                       vax_inf_adult = mean(vax_inf_adult),
                       unvax_inf_adult = mean(unvax_inf_adult),
                       vax_inf_child = mean(vax_inf_child),
                       unvax_inf_child = mean(unvax_inf_child),
                       
                       length.infectious_obs.mean = mean(length.infectious_obs, na.rm = TRUE),
                       length.latent_obs.mean = mean(length.incubation_obs, na.rm = TRUE),
                       length.incubation_obs.mean = mean(length.symp.gap_obs, na.rm = TRUE),
                       
                       p_asymp_adult_obs.mean = mean(p_asymp_adult_obs, na.rm = TRUE),
                       p_asymp_child_obs.mean = mean(p_asymp_child_obs, na.rm = TRUE),
                       p_subclin_adult_obs.mean = mean(p_subclin_adult_obs, na.rm = TRUE),
                       p_subclin_child_obs.mean = mean(p_subclin_child_obs, na.rm = TRUE),
                       
                       child.vax.rate_obs.mean = mean(child.vax.rate_obs, na.rm = TRUE),
                       teacher.vax.rate_obs.mean = mean(teacher.vax.rate_obs, na.rm = TRUE),
                       family.vax.rate_obs.mean = mean(family.vax.rate_obs, na.rm = TRUE),
                       
                       vax.eff_obs.mean = mean(vax.eff_obs, na.rm = TRUE),
                       
                       child.prob_obs.mean = mean(child.prob_obs, na.rm = TRUE),
                       adult.prob_obs.mean = mean(adult.prob_obs, na.rm = TRUE),
                       
                       pcr_test_sens.obs = sum(pcr_tp_count)/(sum(pcr_tp_count)+sum(pcr_fn_count)),
                       test_regular_frac.obs = sum(test_regular)/sum(test_regular_eligible),
                       
                       n_students_obs.mean = mean(n_students_obs, na.rm = TRUE),
                       n_teachers_obs.mean = mean(n_teachers_obs, na.rm = TRUE),
                       n_staff_obs.mean  = mean(n_staff_obs, na.rm = TRUE),
                       
                       inf_ct_sympK_A_home.sum = sum(inf_ct_sympK_A_home, na.rm = TRUE),
                       inf_ct_asympK_A_home.sum = sum(inf_ct_asympK_A_home, na.rm = TRUE),
                       inf_ct_sympA_A_home.sum = sum(inf_ct_sympA_A_home, na.rm = TRUE),
                       inf_ct_asympA_A_home.sum = sum(inf_ct_asympA_A_home, na.rm = TRUE),
                       inf_ct_sympK_K_home.sum = sum(inf_ct_sympK_K_home, na.rm = TRUE),
                       inf_ct_asympK_K_home.sum = sum(inf_ct_asympK_K_home, na.rm = TRUE),
                       inf_ct_sympA_K_home.sum = sum(inf_ct_sympA_K_home, na.rm = TRUE),
                       inf_ct_asympA_K_home.sum = sum(inf_ct_asympA_K_home, na.rm = TRUE),
                       
                       inf_ct_sympK_A_class.sum = sum(inf_ct_sympK_A_class, na.rm = TRUE),
                       inf_ct_asympK_A_class.sum = sum(inf_ct_asympK_A_class, na.rm = TRUE),
                       inf_ct_sympA_A_class.sum = sum(inf_ct_sympA_A_class, na.rm = TRUE),
                       inf_ct_asympA_A_class.sum = sum(inf_ct_asympA_A_class, na.rm = TRUE),
                       inf_ct_sympK_K_class.sum = sum(inf_ct_sympK_K_class, na.rm = TRUE),
                       inf_ct_asympK_K_class.sum = sum(inf_ct_asympK_K_class, na.rm = TRUE),
                       inf_ct_sympA_K_class.sum = sum(inf_ct_sympA_K_class, na.rm = TRUE),
                       inf_ct_asympA_K_class.sum = sum(inf_ct_asympA_K_class, na.rm = TRUE),
                       
                       inf_ct_sympK_A_specials.sum = sum(inf_ct_sympK_A_specials, na.rm = TRUE),
                       inf_ct_asympK_A_specials.sum = sum(inf_ct_asympK_A_specials, na.rm = TRUE),
                       inf_ct_sympA_A_specials.sum = sum(inf_ct_sympA_A_specials, na.rm = TRUE),
                       inf_ct_asympA_A_specials.sum = sum(inf_ct_asympA_A_specials, na.rm = TRUE),
                       inf_ct_sympK_K_specials.sum = sum(inf_ct_sympK_K_specials, na.rm = TRUE),
                       inf_ct_asympK_K_specials.sum = sum(inf_ct_asympK_K_specials, na.rm = TRUE),
                       inf_ct_sympA_K_specials.sum = sum(inf_ct_sympA_K_specials, na.rm = TRUE),
                       inf_ct_asympA_K_specials.sum = sum(inf_ct_asympA_K_specials, na.rm = TRUE),
                       
                       inf_ct_sympK_A_rand.sum = sum(inf_ct_sympK_A_rand, na.rm = TRUE),
                       inf_ct_asympK_A_rand.sum = sum(inf_ct_asympK_A_rand, na.rm = TRUE),
                       inf_ct_sympA_A_rand.sum = sum(inf_ct_sympA_A_rand, na.rm = TRUE),
                       inf_ct_asympA_A_rand.sum = sum(inf_ct_asympA_A_rand, na.rm = TRUE),
                       inf_ct_sympK_K_rand.sum = sum(inf_ct_sympK_K_rand, na.rm = TRUE),
                       inf_ct_asympK_K_rand.sum = sum(inf_ct_asympK_K_rand, na.rm = TRUE),
                       inf_ct_sympA_K_rand.sum = sum(inf_ct_sympA_K_rand, na.rm = TRUE),
                       inf_ct_asympA_K_rand.sum = sum(inf_ct_asympA_K_rand, na.rm = TRUE),
                       
                       inf_ct_sympK_A_care.sum = sum(inf_ct_sympK_A_care, na.rm = TRUE),
                       inf_ct_asympK_A_care.sum = sum(inf_ct_asympK_A_care, na.rm = TRUE),
                       inf_ct_sympA_A_care.sum = sum(inf_ct_sympA_A_care, na.rm = TRUE),
                       inf_ct_asympA_A_care.sum = sum(inf_ct_asympA_A_care, na.rm = TRUE),
                       inf_ct_sympK_K_care.sum = sum(inf_ct_sympK_K_care, na.rm = TRUE),
                       inf_ct_asympK_K_care.sum = sum(inf_ct_asympK_K_care, na.rm = TRUE),
                       inf_ct_sympA_K_care.sum = sum(inf_ct_sympA_K_care, na.rm = TRUE),
                       inf_ct_asympA_K_care.sum = sum(inf_ct_asympA_K_care, na.rm = TRUE),
                       
                       inf_ct_sympA_A_staff.sum = sum(inf_ct_sympA_A_staff, na.rm = TRUE),
                       inf_ct_asympA_A_staff.sum = sum(inf_ct_asympA_A_staff, na.rm = TRUE),
                       
                       risk_ct_sympK_A_home.sum = sum(risk_ct_sympK_A_home, na.rm = TRUE),
                       risk_ct_asympK_A_home.sum = sum(risk_ct_asympK_A_home, na.rm = TRUE),
                       risk_ct_sympA_A_home.sum = sum(risk_ct_sympA_A_home, na.rm = TRUE),
                       risk_ct_asympA_A_home.sum = sum(risk_ct_asympA_A_home, na.rm = TRUE),
                       risk_ct_sympK_K_home.sum = sum(risk_ct_sympK_K_home, na.rm = TRUE),
                       risk_ct_asympK_K_home.sum = sum(risk_ct_asympK_K_home, na.rm = TRUE),
                       risk_ct_sympA_K_home.sum = sum(risk_ct_sympA_K_home, na.rm = TRUE),
                       risk_ct_asympA_K_home.sum = sum(risk_ct_asympA_K_home, na.rm = TRUE),
                       
                       risk_ct_sympK_A_class.sum = sum(risk_ct_sympK_A_class, na.rm = TRUE),
                       risk_ct_asympK_A_class.sum = sum(risk_ct_asympK_A_class, na.rm = TRUE),
                       risk_ct_sympA_A_class.sum = sum(risk_ct_sympA_A_class, na.rm = TRUE),
                       risk_ct_asympA_A_class.sum = sum(risk_ct_asympA_A_class, na.rm = TRUE),
                       risk_ct_sympK_K_class.sum = sum(risk_ct_sympK_K_class, na.rm = TRUE),
                       risk_ct_asympK_K_class.sum = sum(risk_ct_asympK_K_class, na.rm = TRUE),
                       risk_ct_sympA_K_class.sum = sum(risk_ct_sympA_K_class, na.rm = TRUE),
                       risk_ct_asympA_K_class.sum = sum(risk_ct_asympA_K_class, na.rm = TRUE),
                       
                       risk_ct_sympK_A_specials.sum = sum(risk_ct_sympK_A_specials, na.rm = TRUE),
                       risk_ct_asympK_A_specials.sum = sum(risk_ct_asympK_A_specials, na.rm = TRUE),
                       risk_ct_sympA_A_specials.sum = sum(risk_ct_sympA_A_specials, na.rm = TRUE),
                       risk_ct_asympA_A_specials.sum = sum(risk_ct_asympA_A_specials, na.rm = TRUE),
                       risk_ct_sympK_K_specials.sum = sum(risk_ct_sympK_K_specials, na.rm = TRUE),
                       risk_ct_asympK_K_specials.sum = sum(risk_ct_asympK_K_specials, na.rm = TRUE),
                       risk_ct_sympA_K_specials.sum = sum(risk_ct_sympA_K_specials, na.rm = TRUE),
                       risk_ct_asympA_K_specials.sum = sum(risk_ct_asympA_K_specials, na.rm = TRUE),
                       
                       risk_ct_sympK_A_rand.sum = sum(risk_ct_sympK_A_rand, na.rm = TRUE),
                       risk_ct_asympK_A_rand.sum = sum(risk_ct_asympK_A_rand, na.rm = TRUE),
                       risk_ct_sympA_A_rand.sum = sum(risk_ct_sympA_A_rand, na.rm = TRUE),
                       risk_ct_asympA_A_rand.sum = sum(risk_ct_asympA_A_rand, na.rm = TRUE),
                       risk_ct_sympK_K_rand.sum = sum(risk_ct_sympK_K_rand, na.rm = TRUE),
                       risk_ct_asympK_K_rand.sum = sum(risk_ct_asympK_K_rand, na.rm = TRUE),
                       risk_ct_sympA_K_rand.sum = sum(risk_ct_sympA_K_rand, na.rm = TRUE),
                       risk_ct_asympA_K_rand.sum = sum(risk_ct_asympA_K_rand, na.rm = TRUE),
                       
                       risk_ct_sympK_A_care.sum = sum(risk_ct_sympK_A_care, na.rm = TRUE),
                       risk_ct_asympK_A_care.sum = sum(risk_ct_asympK_A_care, na.rm = TRUE),
                       risk_ct_sympA_A_care.sum = sum(risk_ct_sympA_A_care, na.rm = TRUE),
                       risk_ct_asympA_A_care.sum = sum(risk_ct_asympA_A_care, na.rm = TRUE),
                       risk_ct_sympK_K_care.sum = sum(risk_ct_sympK_K_care, na.rm = TRUE),
                       risk_ct_asympK_K_care.sum = sum(risk_ct_asympK_K_care, na.rm = TRUE),
                       risk_ct_sympA_K_care.sum = sum(risk_ct_sympA_K_care, na.rm = TRUE),
                       risk_ct_asympA_K_care.sum = sum(risk_ct_asympA_K_care, na.rm = TRUE),
                       
                       risk_ct_sympA_A_staff.sum = sum(risk_ct_sympA_A_staff, na.rm = TRUE),
                       risk_ct_asympA_A_staff.sum = sum(risk_ct_asympA_A_staff, na.rm = TRUE),
                       
                       analysis.groups = paste(variant.attack, child.vax, teacher_susp, vax_eff, notify.scenario)),
                   by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario)]
  
  remove(data_1)
  gc()
  
  print(paste("Final size for job", j, ":", nrow(data_2)))
  
  print("Data processing complete")
  
  saveRDS(data_2, file = paste("output", j, ".rds", sep = ""))
  remove(data_2)
  print(paste("Subset saved:", j))
  gc()
}

filelist <- sapply(1:50, function(a){paste("output", a, ".rds", sep = "")})

set.seed(2834)

test.sample <- sample(1:length(filelist), round(test.sample.fraction*length(filelist)))
training.sample <- (1:length(filelist))[-test.sample]

output <- rbindlist(lapply(training.sample, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                     all.inschool = mean(all.inschool),
                     all.adults = mean(all.adults),
                     all.children = mean(all.children),
                     all.overall = mean(all.overall),
                     hosp = mean(hosp),
                     
                     vax_inf_adult = mean(vax_inf_adult),
                     unvax_inf_adult = mean(unvax_inf_adult),
                     vax_inf_child = mean(vax_inf_child),
                     unvax_inf_child = mean(unvax_inf_child),
                     
                     length.infectious_obs.mean = mean(length.infectious_obs.mean, na.rm = TRUE),
                     length.latent_obs.mean = mean(length.latent_obs.mean, na.rm = TRUE),
                     length.incubation_obs.mean = mean(length.incubation_obs.mean, na.rm = TRUE),
                     
                     p_asymp_adult_obs.mean = mean(p_asymp_adult_obs.mean, na.rm = TRUE),
                     p_asymp_child_obs.mean = mean(p_asymp_child_obs.mean, na.rm = TRUE),
                     p_subclin_adult_obs.mean = mean(p_subclin_adult_obs.mean, na.rm = TRUE),
                     p_subclin_child_obs.mean = mean(p_subclin_child_obs.mean, na.rm = TRUE),
                     
                     child.vax.rate_obs.mean = mean(child.vax.rate_obs.mean, na.rm = TRUE),
                     teacher.vax.rate_obs.mean = mean(teacher.vax.rate_obs.mean, na.rm = TRUE),
                     family.vax.rate_obs.mean = mean(family.vax.rate_obs.mean, na.rm = TRUE),
                     
                     vax.eff_obs.mean = mean(vax.eff_obs.mean, na.rm = TRUE),
                     
                     child.prob_obs.mean = mean(child.prob_obs.mean, na.rm = TRUE),
                     adult.prob_obs.mean = mean(adult.prob_obs.mean, na.rm = TRUE),
                     
                     pcr_test_sens.obs = mean(pcr_test_sens.obs),
                     test_regular_frac.obs = mean(test_regular_frac.obs),
                     
                     n_students_obs.mean = mean(n_students_obs.mean, na.rm = TRUE),
                     n_teachers_obs.mean = mean(n_teachers_obs.mean, na.rm = TRUE),
                     n_staff_obs.mean  = mean(n_staff_obs.mean, na.rm = TRUE),
                     
                     inf_ct_sympK_A_home.sum = sum(inf_ct_sympK_A_home.sum, na.rm = TRUE),
                     inf_ct_asympK_A_home.sum = sum(inf_ct_asympK_A_home.sum, na.rm = TRUE),
                     inf_ct_sympA_A_home.sum = sum(inf_ct_sympA_A_home.sum, na.rm = TRUE),
                     inf_ct_asympA_A_home.sum = sum(inf_ct_asympA_A_home.sum, na.rm = TRUE),
                     inf_ct_sympK_K_home.sum = sum(inf_ct_sympK_K_home.sum, na.rm = TRUE),
                     inf_ct_asympK_K_home.sum = sum(inf_ct_asympK_K_home.sum, na.rm = TRUE),
                     inf_ct_sympA_K_home.sum = sum(inf_ct_sympA_K_home.sum, na.rm = TRUE),
                     inf_ct_asympA_K_home.sum = sum(inf_ct_asympA_K_home.sum, na.rm = TRUE),
                     
                     inf_ct_sympK_A_class.sum = sum(inf_ct_sympK_A_class.sum, na.rm = TRUE),
                     inf_ct_asympK_A_class.sum = sum(inf_ct_asympK_A_class.sum, na.rm = TRUE),
                     inf_ct_sympA_A_class.sum = sum(inf_ct_sympA_A_class.sum, na.rm = TRUE),
                     inf_ct_asympA_A_class.sum = sum(inf_ct_asympA_A_class.sum, na.rm = TRUE),
                     inf_ct_sympK_K_class.sum = sum(inf_ct_sympK_K_class.sum, na.rm = TRUE),
                     inf_ct_asympK_K_class.sum = sum(inf_ct_asympK_K_class.sum, na.rm = TRUE),
                     inf_ct_sympA_K_class.sum = sum(inf_ct_sympA_K_class.sum, na.rm = TRUE),
                     inf_ct_asympA_K_class.sum = sum(inf_ct_asympA_K_class.sum, na.rm = TRUE),
                     
                     inf_ct_sympK_A_specials.sum = sum(inf_ct_sympK_A_specials.sum, na.rm = TRUE),
                     inf_ct_asympK_A_specials.sum = sum(inf_ct_asympK_A_specials.sum, na.rm = TRUE),
                     inf_ct_sympA_A_specials.sum = sum(inf_ct_sympA_A_specials.sum, na.rm = TRUE),
                     inf_ct_asympA_A_specials.sum = sum(inf_ct_asympA_A_specials.sum, na.rm = TRUE),
                     inf_ct_sympK_K_specials.sum = sum(inf_ct_sympK_K_specials.sum, na.rm = TRUE),
                     inf_ct_asympK_K_specials.sum = sum(inf_ct_asympK_K_specials.sum, na.rm = TRUE),
                     inf_ct_sympA_K_specials.sum = sum(inf_ct_sympA_K_specials.sum, na.rm = TRUE),
                     inf_ct_asympA_K_specials.sum = sum(inf_ct_asympA_K_specials.sum, na.rm = TRUE),
                     
                     inf_ct_sympK_A_rand.sum = sum(inf_ct_sympK_A_rand.sum, na.rm = TRUE),
                     inf_ct_asympK_A_rand.sum = sum(inf_ct_asympK_A_rand.sum, na.rm = TRUE),
                     inf_ct_sympA_A_rand.sum = sum(inf_ct_sympA_A_rand.sum, na.rm = TRUE),
                     inf_ct_asympA_A_rand.sum = sum(inf_ct_asympA_A_rand.sum, na.rm = TRUE),
                     inf_ct_sympK_K_rand.sum = sum(inf_ct_sympK_K_rand.sum, na.rm = TRUE),
                     inf_ct_asympK_K_rand.sum = sum(inf_ct_asympK_K_rand.sum, na.rm = TRUE),
                     inf_ct_sympA_K_rand.sum = sum(inf_ct_sympA_K_rand.sum, na.rm = TRUE),
                     inf_ct_asympA_K_rand.sum = sum(inf_ct_asympA_K_rand.sum, na.rm = TRUE),
                     
                     inf_ct_sympK_A_care.sum = sum(inf_ct_sympK_A_care.sum, na.rm = TRUE),
                     inf_ct_asympK_A_care.sum = sum(inf_ct_asympK_A_care.sum, na.rm = TRUE),
                     inf_ct_sympA_A_care.sum = sum(inf_ct_sympA_A_care.sum, na.rm = TRUE),
                     inf_ct_asympA_A_care.sum = sum(inf_ct_asympA_A_care.sum, na.rm = TRUE),
                     inf_ct_sympK_K_care.sum = sum(inf_ct_sympK_K_care.sum, na.rm = TRUE),
                     inf_ct_asympK_K_care.sum = sum(inf_ct_asympK_K_care.sum, na.rm = TRUE),
                     inf_ct_sympA_K_care.sum = sum(inf_ct_sympA_K_care.sum, na.rm = TRUE),
                     inf_ct_asympA_K_care.sum = sum(inf_ct_asympA_K_care.sum, na.rm = TRUE),
                     
                     inf_ct_sympA_A_staff.sum = sum(inf_ct_sympA_A_staff.sum, na.rm = TRUE),
                     inf_ct_asympA_A_staff.sum = sum(inf_ct_asympA_A_staff.sum, na.rm = TRUE),
                     
                     risk_ct_sympK_A_home.sum = sum(risk_ct_sympK_A_home.sum, na.rm = TRUE),
                     risk_ct_asympK_A_home.sum = sum(risk_ct_asympK_A_home.sum, na.rm = TRUE),
                     risk_ct_sympA_A_home.sum = sum(risk_ct_sympA_A_home.sum, na.rm = TRUE),
                     risk_ct_asympA_A_home.sum = sum(risk_ct_asympA_A_home.sum, na.rm = TRUE),
                     risk_ct_sympK_K_home.sum = sum(risk_ct_sympK_K_home.sum, na.rm = TRUE),
                     risk_ct_asympK_K_home.sum = sum(risk_ct_asympK_K_home.sum, na.rm = TRUE),
                     risk_ct_sympA_K_home.sum = sum(risk_ct_sympA_K_home.sum, na.rm = TRUE),
                     risk_ct_asympA_K_home.sum = sum(risk_ct_asympA_K_home.sum, na.rm = TRUE),
                     
                     risk_ct_sympK_A_class.sum = sum(risk_ct_sympK_A_class.sum, na.rm = TRUE),
                     risk_ct_asympK_A_class.sum = sum(risk_ct_asympK_A_class.sum, na.rm = TRUE),
                     risk_ct_sympA_A_class.sum = sum(risk_ct_sympA_A_class.sum, na.rm = TRUE),
                     risk_ct_asympA_A_class.sum = sum(risk_ct_asympA_A_class.sum, na.rm = TRUE),
                     risk_ct_sympK_K_class.sum = sum(risk_ct_sympK_K_class.sum, na.rm = TRUE),
                     risk_ct_asympK_K_class.sum = sum(risk_ct_asympK_K_class.sum, na.rm = TRUE),
                     risk_ct_sympA_K_class.sum = sum(risk_ct_sympA_K_class.sum, na.rm = TRUE),
                     risk_ct_asympA_K_class.sum = sum(risk_ct_asympA_K_class.sum, na.rm = TRUE),
                     
                     risk_ct_sympK_A_specials.sum = sum(risk_ct_sympK_A_specials.sum, na.rm = TRUE),
                     risk_ct_asympK_A_specials.sum = sum(risk_ct_asympK_A_specials.sum, na.rm = TRUE),
                     risk_ct_sympA_A_specials.sum = sum(risk_ct_sympA_A_specials.sum, na.rm = TRUE),
                     risk_ct_asympA_A_specials.sum = sum(risk_ct_asympA_A_specials.sum, na.rm = TRUE),
                     risk_ct_sympK_K_specials.sum = sum(risk_ct_sympK_K_specials.sum, na.rm = TRUE),
                     risk_ct_asympK_K_specials.sum = sum(risk_ct_asympK_K_specials.sum, na.rm = TRUE),
                     risk_ct_sympA_K_specials.sum = sum(risk_ct_sympA_K_specials.sum, na.rm = TRUE),
                     risk_ct_asympA_K_specials.sum = sum(risk_ct_asympA_K_specials.sum, na.rm = TRUE),
                     
                     risk_ct_sympK_A_rand.sum = sum(risk_ct_sympK_A_rand.sum, na.rm = TRUE),
                     risk_ct_asympK_A_rand.sum = sum(risk_ct_asympK_A_rand.sum, na.rm = TRUE),
                     risk_ct_sympA_A_rand.sum = sum(risk_ct_sympA_A_rand.sum, na.rm = TRUE),
                     risk_ct_asympA_A_rand.sum = sum(risk_ct_asympA_A_rand.sum, na.rm = TRUE),
                     risk_ct_sympK_K_rand.sum = sum(risk_ct_sympK_K_rand.sum, na.rm = TRUE),
                     risk_ct_asympK_K_rand.sum = sum(risk_ct_asympK_K_rand.sum, na.rm = TRUE),
                     risk_ct_sympA_K_rand.sum = sum(risk_ct_sympA_K_rand.sum, na.rm = TRUE),
                     risk_ct_asympA_K_rand.sum = sum(risk_ct_asympA_K_rand.sum, na.rm = TRUE),
                     
                     risk_ct_sympK_A_care.sum = sum(risk_ct_sympK_A_care.sum, na.rm = TRUE),
                     risk_ct_asympK_A_care.sum = sum(risk_ct_asympK_A_care.sum, na.rm = TRUE),
                     risk_ct_sympA_A_care.sum = sum(risk_ct_sympA_A_care.sum, na.rm = TRUE),
                     risk_ct_asympA_A_care.sum = sum(risk_ct_asympA_A_care.sum, na.rm = TRUE),
                     risk_ct_sympK_K_care.sum = sum(risk_ct_sympK_K_care.sum, na.rm = TRUE),
                     risk_ct_asympK_K_care.sum = sum(risk_ct_asympK_K_care.sum, na.rm = TRUE),
                     risk_ct_sympA_K_care.sum = sum(risk_ct_sympA_K_care.sum, na.rm = TRUE),
                     risk_ct_asympA_K_care.sum = sum(risk_ct_asympA_K_care.sum, na.rm = TRUE),
                     
                     risk_ct_sympA_A_staff.sum = sum(risk_ct_sympA_A_staff.sum, na.rm = TRUE),
                     risk_ct_asympA_A_staff.sum = sum(risk_ct_asympA_A_staff.sum, na.rm = TRUE)),
                 by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_training.rds")

remove(output.final)
gc()


output <- rbindlist(lapply(test.sample, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                           all.inschool = mean(all.inschool),
                           all.adults = mean(all.adults),
                           all.children = mean(all.children),
                           all.overall = mean(all.overall),
                           hosp = mean(hosp),
                           
                           vax_inf_adult = mean(vax_inf_adult),
                           unvax_inf_adult = mean(unvax_inf_adult),
                           vax_inf_child = mean(vax_inf_child),
                           unvax_inf_child = mean(unvax_inf_child),
                           
                           length.infectious_obs.mean = mean(length.infectious_obs.mean, na.rm = TRUE),
                           length.latent_obs.mean = mean(length.latent_obs.mean, na.rm = TRUE),
                           length.incubation_obs.mean = mean(length.incubation_obs.mean, na.rm = TRUE),
                           
                           p_asymp_adult_obs.mean = mean(p_asymp_adult_obs.mean, na.rm = TRUE),
                           p_asymp_child_obs.mean = mean(p_asymp_child_obs.mean, na.rm = TRUE),
                           p_subclin_adult_obs.mean = mean(p_subclin_adult_obs.mean, na.rm = TRUE),
                           p_subclin_child_obs.mean = mean(p_subclin_child_obs.mean, na.rm = TRUE),
                           
                           child.vax.rate_obs.mean = mean(child.vax.rate_obs.mean, na.rm = TRUE),
                           teacher.vax.rate_obs.mean = mean(teacher.vax.rate_obs.mean, na.rm = TRUE),
                           family.vax.rate_obs.mean = mean(family.vax.rate_obs.mean, na.rm = TRUE),
                           
                           vax.eff_obs.mean = mean(vax.eff_obs.mean, na.rm = TRUE),
                           
                           child.prob_obs.mean = mean(child.prob_obs.mean, na.rm = TRUE),
                           adult.prob_obs.mean = mean(adult.prob_obs.mean, na.rm = TRUE),
                           
                           pcr_test_sens.obs = mean(pcr_test_sens.obs),
                           test_regular_frac.obs = mean(test_regular_frac.obs),
                           
                           n_students_obs.mean = mean(n_students_obs.mean, na.rm = TRUE),
                           n_teachers_obs.mean = mean(n_teachers_obs.mean, na.rm = TRUE),
                           n_staff_obs.mean  = mean(n_staff_obs.mean, na.rm = TRUE),
                           
                           inf_ct_sympK_A_home.sum = sum(inf_ct_sympK_A_home.sum, na.rm = TRUE),
                           inf_ct_asympK_A_home.sum = sum(inf_ct_asympK_A_home.sum, na.rm = TRUE),
                           inf_ct_sympA_A_home.sum = sum(inf_ct_sympA_A_home.sum, na.rm = TRUE),
                           inf_ct_asympA_A_home.sum = sum(inf_ct_asympA_A_home.sum, na.rm = TRUE),
                           inf_ct_sympK_K_home.sum = sum(inf_ct_sympK_K_home.sum, na.rm = TRUE),
                           inf_ct_asympK_K_home.sum = sum(inf_ct_asympK_K_home.sum, na.rm = TRUE),
                           inf_ct_sympA_K_home.sum = sum(inf_ct_sympA_K_home.sum, na.rm = TRUE),
                           inf_ct_asympA_K_home.sum = sum(inf_ct_asympA_K_home.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_class.sum = sum(inf_ct_sympK_A_class.sum, na.rm = TRUE),
                           inf_ct_asympK_A_class.sum = sum(inf_ct_asympK_A_class.sum, na.rm = TRUE),
                           inf_ct_sympA_A_class.sum = sum(inf_ct_sympA_A_class.sum, na.rm = TRUE),
                           inf_ct_asympA_A_class.sum = sum(inf_ct_asympA_A_class.sum, na.rm = TRUE),
                           inf_ct_sympK_K_class.sum = sum(inf_ct_sympK_K_class.sum, na.rm = TRUE),
                           inf_ct_asympK_K_class.sum = sum(inf_ct_asympK_K_class.sum, na.rm = TRUE),
                           inf_ct_sympA_K_class.sum = sum(inf_ct_sympA_K_class.sum, na.rm = TRUE),
                           inf_ct_asympA_K_class.sum = sum(inf_ct_asympA_K_class.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_specials.sum = sum(inf_ct_sympK_A_specials.sum, na.rm = TRUE),
                           inf_ct_asympK_A_specials.sum = sum(inf_ct_asympK_A_specials.sum, na.rm = TRUE),
                           inf_ct_sympA_A_specials.sum = sum(inf_ct_sympA_A_specials.sum, na.rm = TRUE),
                           inf_ct_asympA_A_specials.sum = sum(inf_ct_asympA_A_specials.sum, na.rm = TRUE),
                           inf_ct_sympK_K_specials.sum = sum(inf_ct_sympK_K_specials.sum, na.rm = TRUE),
                           inf_ct_asympK_K_specials.sum = sum(inf_ct_asympK_K_specials.sum, na.rm = TRUE),
                           inf_ct_sympA_K_specials.sum = sum(inf_ct_sympA_K_specials.sum, na.rm = TRUE),
                           inf_ct_asympA_K_specials.sum = sum(inf_ct_asympA_K_specials.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_rand.sum = sum(inf_ct_sympK_A_rand.sum, na.rm = TRUE),
                           inf_ct_asympK_A_rand.sum = sum(inf_ct_asympK_A_rand.sum, na.rm = TRUE),
                           inf_ct_sympA_A_rand.sum = sum(inf_ct_sympA_A_rand.sum, na.rm = TRUE),
                           inf_ct_asympA_A_rand.sum = sum(inf_ct_asympA_A_rand.sum, na.rm = TRUE),
                           inf_ct_sympK_K_rand.sum = sum(inf_ct_sympK_K_rand.sum, na.rm = TRUE),
                           inf_ct_asympK_K_rand.sum = sum(inf_ct_asympK_K_rand.sum, na.rm = TRUE),
                           inf_ct_sympA_K_rand.sum = sum(inf_ct_sympA_K_rand.sum, na.rm = TRUE),
                           inf_ct_asympA_K_rand.sum = sum(inf_ct_asympA_K_rand.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_care.sum = sum(inf_ct_sympK_A_care.sum, na.rm = TRUE),
                           inf_ct_asympK_A_care.sum = sum(inf_ct_asympK_A_care.sum, na.rm = TRUE),
                           inf_ct_sympA_A_care.sum = sum(inf_ct_sympA_A_care.sum, na.rm = TRUE),
                           inf_ct_asympA_A_care.sum = sum(inf_ct_asympA_A_care.sum, na.rm = TRUE),
                           inf_ct_sympK_K_care.sum = sum(inf_ct_sympK_K_care.sum, na.rm = TRUE),
                           inf_ct_asympK_K_care.sum = sum(inf_ct_asympK_K_care.sum, na.rm = TRUE),
                           inf_ct_sympA_K_care.sum = sum(inf_ct_sympA_K_care.sum, na.rm = TRUE),
                           inf_ct_asympA_K_care.sum = sum(inf_ct_asympA_K_care.sum, na.rm = TRUE),
                           
                           inf_ct_sympA_A_staff.sum = sum(inf_ct_sympA_A_staff.sum, na.rm = TRUE),
                           inf_ct_asympA_A_staff.sum = sum(inf_ct_asympA_A_staff.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_home.sum = sum(risk_ct_sympK_A_home.sum, na.rm = TRUE),
                           risk_ct_asympK_A_home.sum = sum(risk_ct_asympK_A_home.sum, na.rm = TRUE),
                           risk_ct_sympA_A_home.sum = sum(risk_ct_sympA_A_home.sum, na.rm = TRUE),
                           risk_ct_asympA_A_home.sum = sum(risk_ct_asympA_A_home.sum, na.rm = TRUE),
                           risk_ct_sympK_K_home.sum = sum(risk_ct_sympK_K_home.sum, na.rm = TRUE),
                           risk_ct_asympK_K_home.sum = sum(risk_ct_asympK_K_home.sum, na.rm = TRUE),
                           risk_ct_sympA_K_home.sum = sum(risk_ct_sympA_K_home.sum, na.rm = TRUE),
                           risk_ct_asympA_K_home.sum = sum(risk_ct_asympA_K_home.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_class.sum = sum(risk_ct_sympK_A_class.sum, na.rm = TRUE),
                           risk_ct_asympK_A_class.sum = sum(risk_ct_asympK_A_class.sum, na.rm = TRUE),
                           risk_ct_sympA_A_class.sum = sum(risk_ct_sympA_A_class.sum, na.rm = TRUE),
                           risk_ct_asympA_A_class.sum = sum(risk_ct_asympA_A_class.sum, na.rm = TRUE),
                           risk_ct_sympK_K_class.sum = sum(risk_ct_sympK_K_class.sum, na.rm = TRUE),
                           risk_ct_asympK_K_class.sum = sum(risk_ct_asympK_K_class.sum, na.rm = TRUE),
                           risk_ct_sympA_K_class.sum = sum(risk_ct_sympA_K_class.sum, na.rm = TRUE),
                           risk_ct_asympA_K_class.sum = sum(risk_ct_asympA_K_class.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_specials.sum = sum(risk_ct_sympK_A_specials.sum, na.rm = TRUE),
                           risk_ct_asympK_A_specials.sum = sum(risk_ct_asympK_A_specials.sum, na.rm = TRUE),
                           risk_ct_sympA_A_specials.sum = sum(risk_ct_sympA_A_specials.sum, na.rm = TRUE),
                           risk_ct_asympA_A_specials.sum = sum(risk_ct_asympA_A_specials.sum, na.rm = TRUE),
                           risk_ct_sympK_K_specials.sum = sum(risk_ct_sympK_K_specials.sum, na.rm = TRUE),
                           risk_ct_asympK_K_specials.sum = sum(risk_ct_asympK_K_specials.sum, na.rm = TRUE),
                           risk_ct_sympA_K_specials.sum = sum(risk_ct_sympA_K_specials.sum, na.rm = TRUE),
                           risk_ct_asympA_K_specials.sum = sum(risk_ct_asympA_K_specials.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_rand.sum = sum(risk_ct_sympK_A_rand.sum, na.rm = TRUE),
                           risk_ct_asympK_A_rand.sum = sum(risk_ct_asympK_A_rand.sum, na.rm = TRUE),
                           risk_ct_sympA_A_rand.sum = sum(risk_ct_sympA_A_rand.sum, na.rm = TRUE),
                           risk_ct_asympA_A_rand.sum = sum(risk_ct_asympA_A_rand.sum, na.rm = TRUE),
                           risk_ct_sympK_K_rand.sum = sum(risk_ct_sympK_K_rand.sum, na.rm = TRUE),
                           risk_ct_asympK_K_rand.sum = sum(risk_ct_asympK_K_rand.sum, na.rm = TRUE),
                           risk_ct_sympA_K_rand.sum = sum(risk_ct_sympA_K_rand.sum, na.rm = TRUE),
                           risk_ct_asympA_K_rand.sum = sum(risk_ct_asympA_K_rand.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_care.sum = sum(risk_ct_sympK_A_care.sum, na.rm = TRUE),
                           risk_ct_asympK_A_care.sum = sum(risk_ct_asympK_A_care.sum, na.rm = TRUE),
                           risk_ct_sympA_A_care.sum = sum(risk_ct_sympA_A_care.sum, na.rm = TRUE),
                           risk_ct_asympA_A_care.sum = sum(risk_ct_asympA_A_care.sum, na.rm = TRUE),
                           risk_ct_sympK_K_care.sum = sum(risk_ct_sympK_K_care.sum, na.rm = TRUE),
                           risk_ct_asympK_K_care.sum = sum(risk_ct_asympK_K_care.sum, na.rm = TRUE),
                           risk_ct_sympA_K_care.sum = sum(risk_ct_sympA_K_care.sum, na.rm = TRUE),
                           risk_ct_asympA_K_care.sum = sum(risk_ct_asympA_K_care.sum, na.rm = TRUE),
                           
                           risk_ct_sympA_A_staff.sum = sum(risk_ct_sympA_A_staff.sum, na.rm = TRUE),
                           risk_ct_asympA_A_staff.sum = sum(risk_ct_asympA_A_staff.sum, na.rm = TRUE)),
                       by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_test.rds")

remove(output.final)
gc()


output <- rbindlist(lapply(1:50, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                           all.inschool = mean(all.inschool),
                           all.adults = mean(all.adults),
                           all.children = mean(all.children),
                           all.overall = mean(all.overall),
                           hosp = mean(hosp),
                           
                           vax_inf_adult = mean(vax_inf_adult),
                           unvax_inf_adult = mean(unvax_inf_adult),
                           vax_inf_child = mean(vax_inf_child),
                           unvax_inf_child = mean(unvax_inf_child),
                           
                           length.infectious_obs.mean = mean(length.infectious_obs.mean, na.rm = TRUE),
                           length.latent_obs.mean = mean(length.latent_obs.mean, na.rm = TRUE),
                           length.incubation_obs.mean = mean(length.incubation_obs.mean, na.rm = TRUE),
                           
                           p_asymp_adult_obs.mean = mean(p_asymp_adult_obs.mean, na.rm = TRUE),
                           p_asymp_child_obs.mean = mean(p_asymp_child_obs.mean, na.rm = TRUE),
                           p_subclin_adult_obs.mean = mean(p_subclin_adult_obs.mean, na.rm = TRUE),
                           p_subclin_child_obs.mean = mean(p_subclin_child_obs.mean, na.rm = TRUE),
                           
                           child.vax.rate_obs.mean = mean(child.vax.rate_obs.mean, na.rm = TRUE),
                           teacher.vax.rate_obs.mean = mean(teacher.vax.rate_obs.mean, na.rm = TRUE),
                           family.vax.rate_obs.mean = mean(family.vax.rate_obs.mean, na.rm = TRUE),
                           
                           vax.eff_obs.mean = mean(vax.eff_obs.mean, na.rm = TRUE),
                           
                           child.prob_obs.mean = mean(child.prob_obs.mean, na.rm = TRUE),
                           adult.prob_obs.mean = mean(adult.prob_obs.mean, na.rm = TRUE),
                           
                           pcr_test_sens.obs = mean(pcr_test_sens.obs),
                           test_regular_frac.obs = mean(test_regular_frac.obs),
                           
                           n_students_obs.mean = mean(n_students_obs.mean, na.rm = TRUE),
                           n_teachers_obs.mean = mean(n_teachers_obs.mean, na.rm = TRUE),
                           n_staff_obs.mean  = mean(n_staff_obs.mean, na.rm = TRUE),
                           
                           inf_ct_sympK_A_home.sum = sum(inf_ct_sympK_A_home.sum, na.rm = TRUE),
                           inf_ct_asympK_A_home.sum = sum(inf_ct_asympK_A_home.sum, na.rm = TRUE),
                           inf_ct_sympA_A_home.sum = sum(inf_ct_sympA_A_home.sum, na.rm = TRUE),
                           inf_ct_asympA_A_home.sum = sum(inf_ct_asympA_A_home.sum, na.rm = TRUE),
                           inf_ct_sympK_K_home.sum = sum(inf_ct_sympK_K_home.sum, na.rm = TRUE),
                           inf_ct_asympK_K_home.sum = sum(inf_ct_asympK_K_home.sum, na.rm = TRUE),
                           inf_ct_sympA_K_home.sum = sum(inf_ct_sympA_K_home.sum, na.rm = TRUE),
                           inf_ct_asympA_K_home.sum = sum(inf_ct_asympA_K_home.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_class.sum = sum(inf_ct_sympK_A_class.sum, na.rm = TRUE),
                           inf_ct_asympK_A_class.sum = sum(inf_ct_asympK_A_class.sum, na.rm = TRUE),
                           inf_ct_sympA_A_class.sum = sum(inf_ct_sympA_A_class.sum, na.rm = TRUE),
                           inf_ct_asympA_A_class.sum = sum(inf_ct_asympA_A_class.sum, na.rm = TRUE),
                           inf_ct_sympK_K_class.sum = sum(inf_ct_sympK_K_class.sum, na.rm = TRUE),
                           inf_ct_asympK_K_class.sum = sum(inf_ct_asympK_K_class.sum, na.rm = TRUE),
                           inf_ct_sympA_K_class.sum = sum(inf_ct_sympA_K_class.sum, na.rm = TRUE),
                           inf_ct_asympA_K_class.sum = sum(inf_ct_asympA_K_class.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_specials.sum = sum(inf_ct_sympK_A_specials.sum, na.rm = TRUE),
                           inf_ct_asympK_A_specials.sum = sum(inf_ct_asympK_A_specials.sum, na.rm = TRUE),
                           inf_ct_sympA_A_specials.sum = sum(inf_ct_sympA_A_specials.sum, na.rm = TRUE),
                           inf_ct_asympA_A_specials.sum = sum(inf_ct_asympA_A_specials.sum, na.rm = TRUE),
                           inf_ct_sympK_K_specials.sum = sum(inf_ct_sympK_K_specials.sum, na.rm = TRUE),
                           inf_ct_asympK_K_specials.sum = sum(inf_ct_asympK_K_specials.sum, na.rm = TRUE),
                           inf_ct_sympA_K_specials.sum = sum(inf_ct_sympA_K_specials.sum, na.rm = TRUE),
                           inf_ct_asympA_K_specials.sum = sum(inf_ct_asympA_K_specials.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_rand.sum = sum(inf_ct_sympK_A_rand.sum, na.rm = TRUE),
                           inf_ct_asympK_A_rand.sum = sum(inf_ct_asympK_A_rand.sum, na.rm = TRUE),
                           inf_ct_sympA_A_rand.sum = sum(inf_ct_sympA_A_rand.sum, na.rm = TRUE),
                           inf_ct_asympA_A_rand.sum = sum(inf_ct_asympA_A_rand.sum, na.rm = TRUE),
                           inf_ct_sympK_K_rand.sum = sum(inf_ct_sympK_K_rand.sum, na.rm = TRUE),
                           inf_ct_asympK_K_rand.sum = sum(inf_ct_asympK_K_rand.sum, na.rm = TRUE),
                           inf_ct_sympA_K_rand.sum = sum(inf_ct_sympA_K_rand.sum, na.rm = TRUE),
                           inf_ct_asympA_K_rand.sum = sum(inf_ct_asympA_K_rand.sum, na.rm = TRUE),
                           
                           inf_ct_sympK_A_care.sum = sum(inf_ct_sympK_A_care.sum, na.rm = TRUE),
                           inf_ct_asympK_A_care.sum = sum(inf_ct_asympK_A_care.sum, na.rm = TRUE),
                           inf_ct_sympA_A_care.sum = sum(inf_ct_sympA_A_care.sum, na.rm = TRUE),
                           inf_ct_asympA_A_care.sum = sum(inf_ct_asympA_A_care.sum, na.rm = TRUE),
                           inf_ct_sympK_K_care.sum = sum(inf_ct_sympK_K_care.sum, na.rm = TRUE),
                           inf_ct_asympK_K_care.sum = sum(inf_ct_asympK_K_care.sum, na.rm = TRUE),
                           inf_ct_sympA_K_care.sum = sum(inf_ct_sympA_K_care.sum, na.rm = TRUE),
                           inf_ct_asympA_K_care.sum = sum(inf_ct_asympA_K_care.sum, na.rm = TRUE),
                           
                           inf_ct_sympA_A_staff.sum = sum(inf_ct_sympA_A_staff.sum, na.rm = TRUE),
                           inf_ct_asympA_A_staff.sum = sum(inf_ct_asympA_A_staff.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_home.sum = sum(risk_ct_sympK_A_home.sum, na.rm = TRUE),
                           risk_ct_asympK_A_home.sum = sum(risk_ct_asympK_A_home.sum, na.rm = TRUE),
                           risk_ct_sympA_A_home.sum = sum(risk_ct_sympA_A_home.sum, na.rm = TRUE),
                           risk_ct_asympA_A_home.sum = sum(risk_ct_asympA_A_home.sum, na.rm = TRUE),
                           risk_ct_sympK_K_home.sum = sum(risk_ct_sympK_K_home.sum, na.rm = TRUE),
                           risk_ct_asympK_K_home.sum = sum(risk_ct_asympK_K_home.sum, na.rm = TRUE),
                           risk_ct_sympA_K_home.sum = sum(risk_ct_sympA_K_home.sum, na.rm = TRUE),
                           risk_ct_asympA_K_home.sum = sum(risk_ct_asympA_K_home.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_class.sum = sum(risk_ct_sympK_A_class.sum, na.rm = TRUE),
                           risk_ct_asympK_A_class.sum = sum(risk_ct_asympK_A_class.sum, na.rm = TRUE),
                           risk_ct_sympA_A_class.sum = sum(risk_ct_sympA_A_class.sum, na.rm = TRUE),
                           risk_ct_asympA_A_class.sum = sum(risk_ct_asympA_A_class.sum, na.rm = TRUE),
                           risk_ct_sympK_K_class.sum = sum(risk_ct_sympK_K_class.sum, na.rm = TRUE),
                           risk_ct_asympK_K_class.sum = sum(risk_ct_asympK_K_class.sum, na.rm = TRUE),
                           risk_ct_sympA_K_class.sum = sum(risk_ct_sympA_K_class.sum, na.rm = TRUE),
                           risk_ct_asympA_K_class.sum = sum(risk_ct_asympA_K_class.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_specials.sum = sum(risk_ct_sympK_A_specials.sum, na.rm = TRUE),
                           risk_ct_asympK_A_specials.sum = sum(risk_ct_asympK_A_specials.sum, na.rm = TRUE),
                           risk_ct_sympA_A_specials.sum = sum(risk_ct_sympA_A_specials.sum, na.rm = TRUE),
                           risk_ct_asympA_A_specials.sum = sum(risk_ct_asympA_A_specials.sum, na.rm = TRUE),
                           risk_ct_sympK_K_specials.sum = sum(risk_ct_sympK_K_specials.sum, na.rm = TRUE),
                           risk_ct_asympK_K_specials.sum = sum(risk_ct_asympK_K_specials.sum, na.rm = TRUE),
                           risk_ct_sympA_K_specials.sum = sum(risk_ct_sympA_K_specials.sum, na.rm = TRUE),
                           risk_ct_asympA_K_specials.sum = sum(risk_ct_asympA_K_specials.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_rand.sum = sum(risk_ct_sympK_A_rand.sum, na.rm = TRUE),
                           risk_ct_asympK_A_rand.sum = sum(risk_ct_asympK_A_rand.sum, na.rm = TRUE),
                           risk_ct_sympA_A_rand.sum = sum(risk_ct_sympA_A_rand.sum, na.rm = TRUE),
                           risk_ct_asympA_A_rand.sum = sum(risk_ct_asympA_A_rand.sum, na.rm = TRUE),
                           risk_ct_sympK_K_rand.sum = sum(risk_ct_sympK_K_rand.sum, na.rm = TRUE),
                           risk_ct_asympK_K_rand.sum = sum(risk_ct_asympK_K_rand.sum, na.rm = TRUE),
                           risk_ct_sympA_K_rand.sum = sum(risk_ct_sympA_K_rand.sum, na.rm = TRUE),
                           risk_ct_asympA_K_rand.sum = sum(risk_ct_asympA_K_rand.sum, na.rm = TRUE),
                           
                           risk_ct_sympK_A_care.sum = sum(risk_ct_sympK_A_care.sum, na.rm = TRUE),
                           risk_ct_asympK_A_care.sum = sum(risk_ct_asympK_A_care.sum, na.rm = TRUE),
                           risk_ct_sympA_A_care.sum = sum(risk_ct_sympA_A_care.sum, na.rm = TRUE),
                           risk_ct_asympA_A_care.sum = sum(risk_ct_asympA_A_care.sum, na.rm = TRUE),
                           risk_ct_sympK_K_care.sum = sum(risk_ct_sympK_K_care.sum, na.rm = TRUE),
                           risk_ct_asympK_K_care.sum = sum(risk_ct_asympK_K_care.sum, na.rm = TRUE),
                           risk_ct_sympA_K_care.sum = sum(risk_ct_sympA_K_care.sum, na.rm = TRUE),
                           risk_ct_asympA_K_care.sum = sum(risk_ct_asympA_K_care.sum, na.rm = TRUE),
                           
                           risk_ct_sympA_A_staff.sum = sum(risk_ct_sympA_A_staff.sum, na.rm = TRUE),
                           risk_ct_asympA_A_staff.sum = sum(risk_ct_asympA_A_staff.sum, na.rm = TRUE)),
                       by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_full.rds")

remove(output.final)
gc()
