library(data.table)
library(dplyr)
library(openxlsx)

#Set working directory where model output is stored
##Run the code in test_runs.R to generate the model output -- this code collects additional results used to unit test the model output

#Bind model output into single data table
filelist <- list.files(path = "Model Output", pattern = paste("elem_results_.*RData", sep = ""), full.names = TRUE)
output <- rbindlist(lapply(1:length(filelist), function(a){load(filelist[a]); output = output; return(output)}))

output <- data.table(output)

#Create summary measures of model output, grouped by model input parameters
output <- output[,.(inf_ct_sympK_A_home.sum = sum(inf_ct_sympK_A_home, na.rm = TRUE), inf_ct_asympK_A_home.sum = sum(inf_ct_asympK_A_home, na.rm = TRUE), inf_ct_sympA_A_home.sum = sum(inf_ct_sympA_A_home, na.rm = TRUE), inf_ct_asympA_A_home.sum = sum(inf_ct_asympA_A_home, na.rm = TRUE), inf_ct_sympK_K_home.sum = sum(inf_ct_sympK_K_home, na.rm = TRUE), inf_ct_asympK_K_home.sum = sum(inf_ct_asympK_K_home, na.rm = TRUE), inf_ct_sympA_K_home.sum = sum(inf_ct_sympA_K_home, na.rm = TRUE), inf_ct_asympA_K_home.sum = sum(inf_ct_asympA_K_home, na.rm = TRUE),
                    inf_ct_sympK_A_class.sum = sum(inf_ct_sympK_A_class, na.rm = TRUE), inf_ct_asympK_A_class.sum = sum(inf_ct_asympK_A_class, na.rm = TRUE), inf_ct_sympA_A_class.sum = sum(inf_ct_sympA_A_class, na.rm = TRUE), inf_ct_asympA_A_class.sum = sum(inf_ct_asympA_A_class, na.rm = TRUE), inf_ct_sympK_K_class.sum = sum(inf_ct_sympK_K_class, na.rm = TRUE), inf_ct_asympK_K_class.sum = sum(inf_ct_asympK_K_class, na.rm = TRUE), inf_ct_sympA_K_class.sum = sum(inf_ct_sympA_K_class, na.rm = TRUE), inf_ct_asympA_K_class.sum = sum(inf_ct_asympA_K_class, na.rm = TRUE),
                    inf_ct_sympK_A_specials.sum = sum(inf_ct_sympK_A_specials, na.rm = TRUE), inf_ct_asympK_A_specials.sum = sum(inf_ct_asympK_A_specials, na.rm = TRUE), inf_ct_sympA_A_specials.sum = sum(inf_ct_sympA_A_specials, na.rm = TRUE), inf_ct_asympA_A_specials.sum = sum(inf_ct_asympA_A_specials, na.rm = TRUE), inf_ct_sympK_K_specials.sum = sum(inf_ct_sympK_K_specials, na.rm = TRUE), inf_ct_asympK_K_specials.sum = sum(inf_ct_asympK_K_specials, na.rm = TRUE), inf_ct_sympA_K_specials.sum = sum(inf_ct_sympA_K_specials, na.rm = TRUE), inf_ct_asympA_K_specials.sum = sum(inf_ct_asympA_K_specials, na.rm = TRUE),
                    inf_ct_sympK_A_rand.sum = sum(inf_ct_sympK_A_rand, na.rm = TRUE), inf_ct_asympK_A_rand.sum = sum(inf_ct_asympK_A_rand, na.rm = TRUE), inf_ct_sympA_A_rand.sum = sum(inf_ct_sympA_A_rand, na.rm = TRUE), inf_ct_asympA_A_rand.sum = sum(inf_ct_asympA_A_rand, na.rm = TRUE), inf_ct_sympK_K_rand.sum = sum(inf_ct_sympK_K_rand, na.rm = TRUE), inf_ct_asympK_K_rand.sum = sum(inf_ct_asympK_K_rand, na.rm = TRUE), inf_ct_sympA_K_rand.sum = sum(inf_ct_sympA_K_rand, na.rm = TRUE), inf_ct_asympA_K_rand.sum = sum(inf_ct_asympA_K_rand, na.rm = TRUE),
                    inf_ct_sympK_A_care.sum = sum(inf_ct_sympK_A_care, na.rm = TRUE), inf_ct_asympK_A_care.sum = sum(inf_ct_asympK_A_care, na.rm = TRUE), inf_ct_sympA_A_care.sum = sum(inf_ct_sympA_A_care, na.rm = TRUE), inf_ct_asympA_A_care.sum = sum(inf_ct_asympA_A_care, na.rm = TRUE), inf_ct_sympK_K_care.sum = sum(inf_ct_sympK_K_care, na.rm = TRUE), inf_ct_asympK_K_care.sum = sum(inf_ct_asympK_K_care, na.rm = TRUE), inf_ct_sympA_K_care.sum = sum(inf_ct_sympA_K_care, na.rm = TRUE), inf_ct_asympA_K_care.sum = sum(inf_ct_asympA_K_care, na.rm = TRUE),
                    inf_ct_sympA_A_staff.sum = sum(inf_ct_sympA_A_staff, na.rm = TRUE), inf_ct_asympA_A_staff.sum = sum(inf_ct_asympA_A_staff, na.rm = TRUE),
                    
                    risk_ct_sympK_A_home.sum = sum(risk_ct_sympK_A_home, na.rm = TRUE), risk_ct_asympK_A_home.sum = sum(risk_ct_asympK_A_home, na.rm = TRUE), risk_ct_sympA_A_home.sum = sum(risk_ct_sympA_A_home, na.rm = TRUE), risk_ct_asympA_A_home.sum = sum(risk_ct_asympA_A_home, na.rm = TRUE), risk_ct_sympK_K_home.sum = sum(risk_ct_sympK_K_home, na.rm = TRUE), risk_ct_asympK_K_home.sum = sum(risk_ct_asympK_K_home, na.rm = TRUE), risk_ct_sympA_K_home.sum = sum(risk_ct_sympA_K_home, na.rm = TRUE), risk_ct_asympA_K_home.sum = sum(risk_ct_asympA_K_home, na.rm = TRUE),
                    risk_ct_sympK_A_class.sum = sum(risk_ct_sympK_A_class, na.rm = TRUE), risk_ct_asympK_A_class.sum = sum(risk_ct_asympK_A_class, na.rm = TRUE), risk_ct_sympA_A_class.sum = sum(risk_ct_sympA_A_class, na.rm = TRUE), risk_ct_asympA_A_class.sum = sum(risk_ct_asympA_A_class, na.rm = TRUE), risk_ct_sympK_K_class.sum = sum(risk_ct_sympK_K_class, na.rm = TRUE), risk_ct_asympK_K_class.sum = sum(risk_ct_asympK_K_class, na.rm = TRUE), risk_ct_sympA_K_class.sum = sum(risk_ct_sympA_K_class, na.rm = TRUE), risk_ct_asympA_K_class.sum = sum(risk_ct_asympA_K_class, na.rm = TRUE),
                    risk_ct_sympK_A_specials.sum = sum(risk_ct_sympK_A_specials, na.rm = TRUE), risk_ct_asympK_A_specials.sum = sum(risk_ct_asympK_A_specials, na.rm = TRUE), risk_ct_sympA_A_specials.sum = sum(risk_ct_sympA_A_specials, na.rm = TRUE), risk_ct_asympA_A_specials.sum = sum(risk_ct_asympA_A_specials, na.rm = TRUE), risk_ct_sympK_K_specials.sum = sum(risk_ct_sympK_K_specials, na.rm = TRUE), risk_ct_asympK_K_specials.sum = sum(risk_ct_asympK_K_specials, na.rm = TRUE), risk_ct_sympA_K_specials.sum = sum(risk_ct_sympA_K_specials, na.rm = TRUE), risk_ct_asympA_K_specials.sum = sum(risk_ct_asympA_K_specials, na.rm = TRUE),
                    risk_ct_sympK_A_rand.sum = sum(risk_ct_sympK_A_rand, na.rm = TRUE), risk_ct_asympK_A_rand.sum = sum(risk_ct_asympK_A_rand, na.rm = TRUE), risk_ct_sympA_A_rand.sum = sum(risk_ct_sympA_A_rand, na.rm = TRUE), risk_ct_asympA_A_rand.sum = sum(risk_ct_asympA_A_rand, na.rm = TRUE), risk_ct_sympK_K_rand.sum = sum(risk_ct_sympK_K_rand, na.rm = TRUE), risk_ct_asympK_K_rand.sum = sum(risk_ct_asympK_K_rand, na.rm = TRUE), risk_ct_sympA_K_rand.sum = sum(risk_ct_sympA_K_rand, na.rm = TRUE), risk_ct_asympA_K_rand.sum = sum(risk_ct_asympA_K_rand, na.rm = TRUE),
                    risk_ct_sympK_A_care.sum = sum(risk_ct_sympK_A_care, na.rm = TRUE), risk_ct_asympK_A_care.sum = sum(risk_ct_asympK_A_care, na.rm = TRUE), risk_ct_sympA_A_care.sum = sum(risk_ct_sympA_A_care, na.rm = TRUE), risk_ct_asympA_A_care.sum = sum(risk_ct_asympA_A_care, na.rm = TRUE), risk_ct_sympK_K_care.sum = sum(risk_ct_sympK_K_care, na.rm = TRUE), risk_ct_asympK_K_care.sum = sum(risk_ct_asympK_K_care, na.rm = TRUE), risk_ct_sympA_K_care.sum = sum(risk_ct_sympA_K_care, na.rm = TRUE), risk_ct_asympA_K_care.sum = sum(risk_ct_asympA_K_care, na.rm = TRUE),
                    risk_ct_sympA_A_staff.sum = sum(risk_ct_sympA_A_staff, na.rm = TRUE), risk_ct_asympA_A_staff.sum = sum(risk_ct_asympA_A_staff, na.rm = TRUE),
                    
                    inf_home_days_sum = sum(inf_home_days),
                    num_inf_sum = sum(num_inf),
                    
                    n_students_obs.mean = mean(n_students_obs, na.rm = TRUE), n_teachers_obs.mean = mean(n_teachers_obs, na.rm = TRUE), n_staff_obs.mean  = mean(n_staff_obs, na.rm = TRUE),
                    
                    p_asymp_adult_obs.mean = mean(p_asymp_adult_obs, na.rm = TRUE), p_asymp_child_obs.mean = mean(p_asymp_child_obs, na.rm = TRUE), p_subclin_adult_obs.mean = mean(p_subclin_adult_obs, na.rm = TRUE), p_subclin_child_obs.mean = mean(p_subclin_child_obs, na.rm = TRUE),
                    
                    exposed_not.symp_days_sum = sum(exposed_not.symp_days),
                    num_symp_sum = sum(num_symp),
                    
                    exposed_not.inf_days_sum = sum(exposed_not.inf_days),
                    num_exposed_sum = sum(num_exposed),
                    
                    child.vax.rate_obs.mean = mean(child.vax.rate_obs, na.rm = TRUE), teacher.vax.rate_obs.mean = mean(teacher.vax.rate_obs, na.rm = TRUE), family.vax.rate_obs.mean = mean(family.vax.rate_obs, na.rm = TRUE), vax.eff_obs.mean = mean(vax.eff_obs, na.rm = TRUE),
                    
                    child.start.count_sum = sum(child.start.count), adult.start.count_sum = sum(adult.start.count),
                    child.community.risk.days_sum = sum(child.community.risk.days), adult.community.risk.days_sum = sum(adult.community.risk.days),
                    
                    rapid_test_sens.obs = sum(rapid_tp_count)/(sum(rapid_tp_count)+sum(rapid_fn_count)), pcr_test_sens.obs = sum(pcr_tp_count)/(sum(pcr_tp_count)+sum(pcr_fn_count)), test_regular_frac.obs = sum(test_regular)/sum(test_regular_eligible), test_quarantine_frac.obs = sum(test_qs)/sum(test_q_eligible),
                    
                    rapid_tp_count.sum = sum(rapid_tp_count), rapid_fn_count.sum = sum(rapid_fn_count),
                    pcr_tp_count.sum = sum(pcr_tp_count), pcr_fn_count.sum = sum(pcr_fn_count),
                    test_regular.sum = sum(test_regular), test_regular_eligible.sum = sum(test_regular_eligible),
                    
                    children.unvax = sum(children*(1-child.vax)/(1-child.vax + child.vax*(1-vax_eff))), adult.unvax = sum((adult + family)*(1-family_susp)/(1-family_susp + family_susp*(1-vax_eff))),
                    children.unvax_obs = sum(unvax_inf_child), adult.unvax_obs = sum(unvax_inf_adult),
                    
                    hosp_child.sum = sum(hosp_child), hosp_adult.sum = sum(hosp_adult),
                    
                    group = paste(mitigation, prob, p_asymp_adult, p_asymp_child, p_subclin_adult, p_subclin_child, mult_asymp_child, mult_asymp, days_inf, rel_trans_child_symp_HH, vax_eff, attack.df, variant.attack, child.vax, teacher_susp, child_susp, child_trans, family_susp, adult_prob, child_prob, test, test_frac, isolate, notify, rel_trans_HH, rel_trans_child_symp_HH, rel_trans, rel_trans_adult, rel_trans_CC, high_school, n_class, type, test_sens, rapid_test_sens, test_frac, test_quarantine, adult_vax_hosp_rate, child_vax_hosp_rate, adult_unvax_hosp_rate, child_unvax_hosp_rate)),
                    
                    by = .(mitigation, prob, p_asymp_adult, p_asymp_child, p_subclin_adult, p_subclin_child, mult_asymp_child, mult_asymp, days_inf, rel_trans_child_symp_HH, vax_eff, attack.df, variant.attack, child.vax, teacher_susp, child_susp, child_trans, family_susp, adult_prob, child_prob, test, test_frac, isolate, notify, rel_trans_HH, rel_trans_child_symp_HH, rel_trans, rel_trans_adult, rel_trans_CC, high_school, n_class, type, test_sens, rapid_test_sens, test_frac, test_quarantine, adult_vax_hosp_rate, child_vax_hosp_rate, adult_unvax_hosp_rate, child_unvax_hosp_rate)]

attack.mult <- output %>%
  mutate(
    all_adjusted_contacts = (1-mitigation)*(
      risk_ct_sympA_A_home.sum * rel_trans_HH +
      risk_ct_sympA_K_home.sum * rel_trans_HH * child_susp +
      risk_ct_asympA_A_home.sum * rel_trans_HH * mult_asymp +
      risk_ct_asympA_K_home.sum * rel_trans_HH * mult_asymp * child_susp +
      risk_ct_sympK_A_home.sum * rel_trans_HH * child_trans * rel_trans_child_symp_HH +
      risk_ct_sympK_K_home.sum * rel_trans_HH * child_trans * rel_trans_child_symp_HH * child_susp +
      risk_ct_asympK_A_home.sum * rel_trans_HH * child_trans +
      risk_ct_asympK_K_home.sum * rel_trans_HH * child_trans * child_susp +
      
      risk_ct_sympA_A_class.sum +
      risk_ct_sympA_K_class.sum * child_susp +
      risk_ct_asympA_A_class.sum * mult_asymp +
      risk_ct_asympA_K_class.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_class.sum * child_trans +
      risk_ct_sympK_K_class.sum * child_trans * child_susp +
      risk_ct_asympK_A_class.sum * child_trans +
      risk_ct_asympK_K_class.sum * child_trans * child_susp +
      
      risk_ct_sympA_A_specials.sum * rel_trans +
      risk_ct_sympA_K_specials.sum * rel_trans * child_susp +
      risk_ct_asympA_A_specials.sum * rel_trans * mult_asymp +
      risk_ct_asympA_K_specials.sum * rel_trans * mult_asymp * child_susp +
      risk_ct_sympK_A_specials.sum * rel_trans * child_trans +
      risk_ct_sympK_K_specials.sum * rel_trans * child_trans * child_susp +
      risk_ct_asympK_A_specials.sum * rel_trans * child_trans +
      risk_ct_asympK_K_specials.sum * rel_trans * child_trans * child_susp +
        
      risk_ct_sympA_A_rand.sum * rel_trans +
      risk_ct_sympA_K_rand.sum * rel_trans * child_susp +
      risk_ct_asympA_A_rand.sum * rel_trans * mult_asymp +
      risk_ct_asympA_K_rand.sum * rel_trans * mult_asymp * child_susp +
      risk_ct_sympK_A_rand.sum * rel_trans * child_trans +
      risk_ct_sympK_K_rand.sum * rel_trans * child_trans * child_susp +
      risk_ct_asympK_A_rand.sum * rel_trans * child_trans +
      risk_ct_asympK_K_rand.sum * rel_trans * child_trans * child_susp +
      
      risk_ct_sympA_A_care.sum * rel_trans_CC +
      risk_ct_sympA_K_care.sum * rel_trans_CC * child_susp +
      risk_ct_asympA_A_care.sum * rel_trans_CC * mult_asymp +
      risk_ct_asympA_K_care.sum * rel_trans_CC * mult_asymp * child_susp +
      risk_ct_sympK_A_care.sum * rel_trans_CC * child_trans +
      risk_ct_sympK_K_care.sum * rel_trans_CC * child_trans * child_susp +
      risk_ct_asympK_A_care.sum * rel_trans_CC * child_trans +
      risk_ct_asympK_K_care.sum * rel_trans_CC * child_trans * child_susp +
        
      risk_ct_sympA_A_staff.sum * rel_trans * rel_trans_adult +
      risk_ct_asympA_A_staff.sum * rel_trans * rel_trans_adult * mult_asymp
    ),
    
    all_infs =
      inf_ct_sympA_A_home.sum +
      inf_ct_sympA_K_home.sum +
      inf_ct_asympA_A_home.sum +
      inf_ct_asympA_K_home.sum +
      inf_ct_sympK_A_home.sum +
      inf_ct_sympK_K_home.sum +
      inf_ct_asympK_A_home.sum +
      inf_ct_asympK_K_home.sum +
      
      inf_ct_sympA_A_class.sum +
      inf_ct_sympA_K_class.sum +
      inf_ct_asympA_A_class.sum +
      inf_ct_asympA_K_class.sum +
      inf_ct_sympK_A_class.sum +
      inf_ct_sympK_K_class.sum +
      inf_ct_asympK_A_class.sum +
      inf_ct_asympK_K_class.sum +
      
      inf_ct_sympA_A_specials.sum +
      inf_ct_sympA_K_specials.sum +
      inf_ct_asympA_A_specials.sum +
      inf_ct_asympA_K_specials.sum +
      inf_ct_sympK_A_specials.sum +
      inf_ct_sympK_K_specials.sum +
      inf_ct_asympK_A_specials.sum +
      inf_ct_asympK_K_specials.sum +
      
      inf_ct_sympA_A_rand.sum +
      inf_ct_sympA_K_rand.sum +
      inf_ct_asympA_A_rand.sum +
      inf_ct_asympA_K_rand.sum +
      inf_ct_sympK_A_rand.sum +
      inf_ct_sympK_K_rand.sum +
      inf_ct_asympK_A_rand.sum +
      inf_ct_asympK_K_rand.sum +
      
      inf_ct_sympA_A_care.sum +
      inf_ct_sympA_K_care.sum +
      inf_ct_asympA_A_care.sum +
      inf_ct_asympA_K_care.sum +
      inf_ct_sympK_A_care.sum +
      inf_ct_sympK_K_care.sum +
      inf_ct_asympK_A_care.sum +
      inf_ct_asympK_K_care.sum +
      
      inf_ct_sympA_A_staff.sum +
      inf_ct_asympA_A_staff.sum
  ) %>%
  group_by(variant.attack) %>%
  summarise(
    variant.attack_obs = sum(all_infs)/sum(all_adjusted_contacts)
  ) %>%
  mutate(rel.diff = variant.attack_obs/variant.attack)

mitigation.mult <- output %>%
  mutate(
    all_adjusted_contacts = variant.attack*(
      risk_ct_sympA_A_home.sum * rel_trans_HH +
      risk_ct_sympA_K_home.sum * rel_trans_HH * child_susp +
      risk_ct_asympA_A_home.sum * rel_trans_HH * mult_asymp +
      risk_ct_asympA_K_home.sum * rel_trans_HH * mult_asymp * child_susp +
      risk_ct_sympK_A_home.sum * rel_trans_HH * child_trans * rel_trans_child_symp_HH +
      risk_ct_sympK_K_home.sum * rel_trans_HH * child_trans * rel_trans_child_symp_HH * child_susp +
      risk_ct_asympK_A_home.sum * rel_trans_HH * child_trans +
      risk_ct_asympK_K_home.sum * rel_trans_HH * child_trans * child_susp +
      
      risk_ct_sympA_A_class.sum +
      risk_ct_sympA_K_class.sum * child_susp +
      risk_ct_asympA_A_class.sum * mult_asymp +
      risk_ct_asympA_K_class.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_class.sum * child_trans +
      risk_ct_sympK_K_class.sum * child_trans * child_susp +
      risk_ct_asympK_A_class.sum * child_trans +
      risk_ct_asympK_K_class.sum * child_trans * child_susp +
      
      risk_ct_sympA_A_specials.sum * rel_trans +
      risk_ct_sympA_K_specials.sum * rel_trans * child_susp +
      risk_ct_asympA_A_specials.sum * rel_trans * mult_asymp +
      risk_ct_asympA_K_specials.sum * rel_trans * mult_asymp * child_susp +
      risk_ct_sympK_A_specials.sum * rel_trans * child_trans +
      risk_ct_sympK_K_specials.sum * rel_trans * child_trans * child_susp +
      risk_ct_asympK_A_specials.sum * rel_trans * child_trans +
      risk_ct_asympK_K_specials.sum * rel_trans * child_trans * child_susp +
      
      risk_ct_sympA_A_rand.sum * rel_trans +
      risk_ct_sympA_K_rand.sum * rel_trans * child_susp +
      risk_ct_asympA_A_rand.sum * rel_trans * mult_asymp +
      risk_ct_asympA_K_rand.sum * rel_trans * mult_asymp * child_susp +
      risk_ct_sympK_A_rand.sum * rel_trans * child_trans +
      risk_ct_sympK_K_rand.sum * rel_trans * child_trans * child_susp +
      risk_ct_asympK_A_rand.sum * rel_trans * child_trans +
      risk_ct_asympK_K_rand.sum * rel_trans * child_trans * child_susp +
      
      risk_ct_sympA_A_care.sum * rel_trans_CC +
      risk_ct_sympA_K_care.sum * rel_trans_CC * child_susp +
      risk_ct_asympA_A_care.sum * rel_trans_CC * mult_asymp +
      risk_ct_asympA_K_care.sum * rel_trans_CC * mult_asymp * child_susp +
      risk_ct_sympK_A_care.sum * rel_trans_CC * child_trans +
      risk_ct_sympK_K_care.sum * rel_trans_CC * child_trans * child_susp +
      risk_ct_asympK_A_care.sum * rel_trans_CC * child_trans +
      risk_ct_asympK_K_care.sum * rel_trans_CC * child_trans * child_susp +
      
      risk_ct_sympA_A_staff.sum * rel_trans * rel_trans_adult +
      risk_ct_asympA_A_staff.sum * rel_trans * rel_trans_adult * mult_asymp
    ),
    
    all_infs =
      inf_ct_sympA_A_home.sum +
      inf_ct_sympA_K_home.sum +
      inf_ct_asympA_A_home.sum +
      inf_ct_asympA_K_home.sum +
      inf_ct_sympK_A_home.sum +
      inf_ct_sympK_K_home.sum +
      inf_ct_asympK_A_home.sum +
      inf_ct_asympK_K_home.sum +
      
      inf_ct_sympA_A_class.sum +
      inf_ct_sympA_K_class.sum +
      inf_ct_asympA_A_class.sum +
      inf_ct_asympA_K_class.sum +
      inf_ct_sympK_A_class.sum +
      inf_ct_sympK_K_class.sum +
      inf_ct_asympK_A_class.sum +
      inf_ct_asympK_K_class.sum +
      
      inf_ct_sympA_A_specials.sum +
      inf_ct_sympA_K_specials.sum +
      inf_ct_asympA_A_specials.sum +
      inf_ct_asympA_K_specials.sum +
      inf_ct_sympK_A_specials.sum +
      inf_ct_sympK_K_specials.sum +
      inf_ct_asympK_A_specials.sum +
      inf_ct_asympK_K_specials.sum +
      
      inf_ct_sympA_A_rand.sum +
      inf_ct_sympA_K_rand.sum +
      inf_ct_asympA_A_rand.sum +
      inf_ct_asympA_K_rand.sum +
      inf_ct_sympK_A_rand.sum +
      inf_ct_sympK_K_rand.sum +
      inf_ct_asympK_A_rand.sum +
      inf_ct_asympK_K_rand.sum +
      
      inf_ct_sympA_A_care.sum +
      inf_ct_sympA_K_care.sum +
      inf_ct_asympA_A_care.sum +
      inf_ct_asympA_K_care.sum +
      inf_ct_sympK_A_care.sum +
      inf_ct_sympK_K_care.sum +
      inf_ct_asympK_A_care.sum +
      inf_ct_asympK_K_care.sum +
      
      inf_ct_sympA_A_staff.sum +
      inf_ct_asympA_A_staff.sum,
    
    mitigation.mult = 1 - mitigation
  ) %>%
  group_by(mitigation.mult) %>%
  summarise(
    mitigation.mult_obs = sum(all_infs)/sum(all_adjusted_contacts)
  ) %>%
  mutate(rel.diff = mitigation.mult_obs/mitigation.mult)

home.mult <- output %>%
  mutate(
    home_adjusted_contacts = variant.attack*(
      risk_ct_sympA_A_home.sum +
      risk_ct_sympA_K_home.sum * child_susp +
      risk_ct_asympA_A_home.sum * mult_asymp +
      risk_ct_asympA_K_home.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_home.sum * child_trans * rel_trans_child_symp_HH +
      risk_ct_sympK_K_home.sum * child_trans * rel_trans_child_symp_HH * child_susp +
      risk_ct_asympK_A_home.sum * child_trans +
      risk_ct_asympK_K_home.sum * child_trans * child_susp),
    
    home_infs =
      inf_ct_sympA_A_home.sum +
      inf_ct_sympA_K_home.sum +
      inf_ct_asympA_A_home.sum +
      inf_ct_asympA_K_home.sum +
      inf_ct_sympK_A_home.sum +
      inf_ct_sympK_K_home.sum +
      inf_ct_asympK_A_home.sum +
      inf_ct_asympK_K_home.sum,
    
    home_mult = (1-mitigation)*rel_trans_HH
  ) %>%
  group_by(home_mult) %>%
  summarise(
    home_mult_obs = sum(home_infs)/sum(home_adjusted_contacts)
  ) %>%
  mutate(rel.diff = home_mult_obs/home_mult)
  
brief.mult <- output %>%
  mutate(
    brief_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_sympA_A_specials.sum +
      risk_ct_sympA_K_specials.sum * child_susp +
      risk_ct_asympA_A_specials.sum * mult_asymp +
      risk_ct_asympA_K_specials.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_specials.sum * child_trans +
      risk_ct_sympK_K_specials.sum * child_trans * child_susp +
      risk_ct_asympK_A_specials.sum * child_trans +
      risk_ct_asympK_K_specials.sum * child_trans * child_susp +
      
      risk_ct_sympA_A_rand.sum +
      risk_ct_sympA_K_rand.sum * child_susp +
      risk_ct_asympA_A_rand.sum * mult_asymp +
      risk_ct_asympA_K_rand.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_rand.sum * child_trans +
      risk_ct_sympK_K_rand.sum * child_trans * child_susp +
      risk_ct_asympK_A_rand.sum * child_trans +
      risk_ct_asympK_K_rand.sum * child_trans * child_susp +
      
      risk_ct_sympA_A_staff.sum * rel_trans_adult +
      risk_ct_asympA_A_staff.sum * rel_trans_adult * mult_asymp),
    
    brief_infs =
      inf_ct_sympA_A_specials.sum +
      inf_ct_sympA_K_specials.sum +
      inf_ct_asympA_A_specials.sum +
      inf_ct_asympA_K_specials.sum +
      inf_ct_sympK_A_specials.sum +
      inf_ct_sympK_K_specials.sum +
      inf_ct_asympK_A_specials.sum +
      inf_ct_asympK_K_specials.sum +
      
      inf_ct_sympA_A_rand.sum +
      inf_ct_sympA_K_rand.sum +
      inf_ct_asympA_A_rand.sum +
      inf_ct_asympA_K_rand.sum +
      inf_ct_sympK_A_rand.sum +
      inf_ct_sympK_K_rand.sum +
      inf_ct_asympK_A_rand.sum +
      inf_ct_asympK_K_rand.sum +
      
      inf_ct_sympA_A_staff.sum +
      inf_ct_asympA_A_staff.sum
  ) %>%
  group_by(rel_trans) %>%
  summarise(
    brief_mult_obs = sum(brief_infs)/sum(brief_adjusted_contacts)
  ) %>%
  mutate(rel.diff = brief_mult_obs/rel_trans)

brief_adult.mult <- output %>%
  mutate(
    brief.adult_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_sympA_A_staff.sum * rel_trans +
      risk_ct_asympA_A_staff.sum * rel_trans * mult_asymp),
    
    brief.adult_infs =
      inf_ct_sympA_A_staff.sum +
      inf_ct_asympA_A_staff.sum
  ) %>%
  group_by(rel_trans_adult) %>%
  summarise(
    brief.adult_mult_obs = sum(brief.adult_infs)/sum(brief.adult_adjusted_contacts)
  ) %>%
  mutate(rel.diff = brief.adult_mult_obs/rel_trans_adult)

care.mult <- output %>%
  mutate(
    care_adjusted_contacts = variant.attack*(
      risk_ct_sympA_A_care.sum +
      risk_ct_sympA_K_care.sum * child_susp +
      risk_ct_asympA_A_care.sum * mult_asymp +
      risk_ct_asympA_K_care.sum * mult_asymp * child_susp +
      risk_ct_sympK_A_care.sum * child_trans +
      risk_ct_sympK_K_care.sum * child_trans * child_susp +
      risk_ct_asympK_A_care.sum * child_trans +
      risk_ct_asympK_K_care.sum * child_trans * child_susp),
    
    care_infs =
      inf_ct_sympA_A_care.sum +
      inf_ct_sympA_K_care.sum +
      inf_ct_asympA_A_care.sum +
      inf_ct_asympA_K_care.sum +
      inf_ct_sympK_A_care.sum +
      inf_ct_sympK_K_care.sum +
      inf_ct_asympK_A_care.sum +
      inf_ct_asympK_K_care.sum,
    
    care_mult = (1-mitigation)*rel_trans_CC
  ) %>%
  group_by(care_mult) %>%
  summarise(
    care_mult_obs = sum(care_infs)/sum(care_adjusted_contacts)
  ) %>%
  mutate(rel.diff = care_mult_obs/care_mult)

child_trans.mult <- output %>%
  mutate(
    child.trans_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_sympK_A_home.sum * rel_trans_HH * rel_trans_child_symp_HH +
      risk_ct_sympK_K_home.sum * rel_trans_HH * rel_trans_child_symp_HH * child_susp +
      risk_ct_asympK_A_home.sum * rel_trans_HH +
      risk_ct_asympK_K_home.sum * rel_trans_HH * child_susp +
      
      risk_ct_sympK_A_class.sum +
      risk_ct_sympK_K_class.sum * child_susp +
      risk_ct_asympK_A_class.sum +
      risk_ct_asympK_K_class.sum * child_susp +
      
      risk_ct_sympK_A_specials.sum * rel_trans +
      risk_ct_sympK_K_specials.sum * rel_trans * child_susp +
      risk_ct_asympK_A_specials.sum * rel_trans +
      risk_ct_asympK_K_specials.sum * rel_trans * child_susp +
      
      risk_ct_sympK_A_rand.sum * rel_trans +
      risk_ct_sympK_K_rand.sum * rel_trans * child_susp +
      risk_ct_asympK_A_rand.sum * rel_trans +
      risk_ct_asympK_K_rand.sum * rel_trans * child_susp +
      
      risk_ct_sympK_A_care.sum * rel_trans_CC +
      risk_ct_sympK_K_care.sum * rel_trans_CC * child_susp +
      risk_ct_asympK_A_care.sum * rel_trans_CC +
      risk_ct_asympK_K_care.sum * rel_trans_CC * child_susp
      ),
    
    child.trans_infs =
      inf_ct_sympK_A_home.sum +
      inf_ct_sympK_K_home.sum +
      inf_ct_asympK_A_home.sum +
      inf_ct_asympK_K_home.sum +
      
      inf_ct_sympK_A_class.sum +
      inf_ct_sympK_K_class.sum +
      inf_ct_asympK_A_class.sum +
      inf_ct_asympK_K_class.sum +
      
      inf_ct_sympK_A_specials.sum +
      inf_ct_sympK_K_specials.sum +
      inf_ct_asympK_A_specials.sum +
      inf_ct_asympK_K_specials.sum +
      
      inf_ct_sympK_A_rand.sum +
      inf_ct_sympK_K_rand.sum +
      inf_ct_asympK_A_rand.sum +
      inf_ct_asympK_K_rand.sum +
      
      inf_ct_sympK_A_care.sum +
      inf_ct_sympK_K_care.sum +
      inf_ct_asympK_A_care.sum +
      inf_ct_asympK_K_care.sum
  ) %>%
  group_by(child_trans) %>%
  summarise(
    child.trans_mult_obs = sum(child.trans_infs)/sum(child.trans_adjusted_contacts)
  ) %>%
  mutate(rel.diff = child.trans_mult_obs/child_trans)

asymp_trans.mult <- output %>%
  mutate(
    asymp.trans_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_asympA_A_home.sum * rel_trans_HH +
      risk_ct_asympA_K_home.sum * rel_trans_HH * child_susp +
        
      risk_ct_asympA_A_class.sum +
      risk_ct_asympA_K_class.sum * child_susp +
        
      risk_ct_asympA_A_specials.sum * rel_trans +
      risk_ct_asympA_K_specials.sum * rel_trans * child_susp +
        
      risk_ct_asympA_A_rand.sum * rel_trans +
      risk_ct_asympA_K_rand.sum * rel_trans * child_susp +
        
      risk_ct_asympA_A_care.sum * rel_trans_CC +
      risk_ct_asympA_K_care.sum * rel_trans_CC * child_susp +
        
      risk_ct_asympA_A_staff.sum * rel_trans * rel_trans_adult
    ),
    
    asymp.trans_infs =
      inf_ct_asympA_A_home.sum +
      inf_ct_asympA_K_home.sum +
      
      inf_ct_asympA_A_class.sum +
      inf_ct_asympA_K_class.sum +
      
      inf_ct_asympA_A_specials.sum +
      inf_ct_asympA_K_specials.sum +
      
      inf_ct_asympA_A_rand.sum +
      inf_ct_asympA_K_rand.sum +
      
      inf_ct_asympA_A_care.sum +
      inf_ct_asympA_K_care.sum +
      
      inf_ct_asympA_A_staff.sum
  ) %>%
  group_by(mult_asymp) %>%
  summarise(
    asymp.trans_mult_obs = sum(asymp.trans_infs)/sum(asymp.trans_adjusted_contacts)
  ) %>%
  mutate(rel.diff = asymp.trans_mult_obs/mult_asymp)

child_symp_HH_trans.mult <- output %>%
  mutate(
    child.symp.HH.trans_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_sympK_A_home.sum * rel_trans_HH * child_trans +
      risk_ct_sympK_K_home.sum * rel_trans_HH * child_trans * child_susp
    ),
    
    child.symp.HH.trans_infs =
      inf_ct_sympK_A_home.sum +
      inf_ct_sympK_K_home.sum
  ) %>%
  group_by(rel_trans_child_symp_HH) %>%
  summarise(
    child.symp.HH.trans_mult_obs = sum(child.symp.HH.trans_infs)/sum(child.symp.HH.trans_adjusted_contacts)
  ) %>%
  mutate(rel.diff = child.symp.HH.trans_mult_obs/rel_trans_child_symp_HH)

child_susp.mult <- output %>%
  mutate(
    child.susp_adjusted_contacts = variant.attack*(1-mitigation)*(
      risk_ct_sympA_K_home.sum * rel_trans_HH +
      risk_ct_asympA_K_home.sum * rel_trans_HH * mult_asymp +
      risk_ct_sympK_K_home.sum * rel_trans_HH * child_trans * rel_trans_child_symp_HH +
      risk_ct_asympK_K_home.sum * rel_trans_HH * child_trans +
        
      risk_ct_sympA_K_class.sum +
      risk_ct_asympA_K_class.sum * mult_asymp +
      risk_ct_sympK_K_class.sum * child_trans +
      risk_ct_asympK_K_class.sum * child_trans +
        
      risk_ct_sympA_K_specials.sum * rel_trans +
      risk_ct_asympA_K_specials.sum * rel_trans * mult_asymp +
      risk_ct_sympK_K_specials.sum * rel_trans * child_trans +
      risk_ct_asympK_K_specials.sum * rel_trans * child_trans +
        
      risk_ct_sympA_K_rand.sum * rel_trans +
      risk_ct_asympA_K_rand.sum * rel_trans * mult_asymp +
      risk_ct_sympK_K_rand.sum * rel_trans * child_trans +
      risk_ct_asympK_K_rand.sum * rel_trans * child_trans +
        
      risk_ct_sympA_K_care.sum * rel_trans_CC +
      risk_ct_asympA_K_care.sum * rel_trans_CC * mult_asymp +
      risk_ct_sympK_K_care.sum * rel_trans_CC * child_trans +
      risk_ct_asympK_K_care.sum * rel_trans_CC * child_trans
    ),
    
    child.susp_infs =
      inf_ct_sympA_K_home.sum +
      inf_ct_asympA_K_home.sum +
      inf_ct_sympK_K_home.sum +
      inf_ct_asympK_K_home.sum +
      
      inf_ct_sympA_K_class.sum +
      inf_ct_asympA_K_class.sum +
      inf_ct_sympK_K_class.sum +
      inf_ct_asympK_K_class.sum +
      
      inf_ct_sympA_K_specials.sum +
      inf_ct_asympA_K_specials.sum +
      inf_ct_sympK_K_specials.sum +
      inf_ct_asympK_K_specials.sum +
      
      inf_ct_sympA_K_rand.sum +
      inf_ct_asympA_K_rand.sum +
      inf_ct_sympK_K_rand.sum +
      inf_ct_asympK_K_rand.sum +
      
      inf_ct_sympA_K_care.sum +
      inf_ct_asympA_K_care.sum +
      inf_ct_sympK_K_care.sum +
      inf_ct_asympK_K_care.sum
  ) %>%
  group_by(child_susp) %>%
  summarise(
    child.susp_mult_obs = sum(child.susp_infs)/sum(child.susp_adjusted_contacts)
  ) %>%
  mutate(rel.diff = child.susp_mult_obs/child_susp)

latent.period <- output %>%
  summarise(latent_obs = sum(exposed_not.inf_days_sum)/sum(num_exposed_sum)) %>%
  mutate(latent_expected = mean(floor(pmax(c(rgamma(1e7, shape = 5.8, scale=.95) - rnorm(1e7, mean = 2, sd = 0.4)), c(1 - 1e-10))))) %>%
  mutate(rel.diff = latent_obs/latent_expected)

incubation.period <- output %>%
  summarise(incubation_obs = sum(exposed_not.symp_days_sum)/sum(num_symp_sum)) %>%
  mutate(incubation_expected = mean(floor(rgamma(1e7, shape = 5.8, scale=.95)))) %>%
  mutate(rel.diff = incubation_obs/incubation_expected)

generate_expected_inf.period <- function(days_inf){
  latent.draws <- rgamma(1e7, shape = 5.8, scale = 0.95) - rnorm(1e7, mean = 2, sd = 0.4)
  starting.time <- pmax(latent.draws, c(1))
  
  ending.time <- rlnorm(1e7, meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2))) + starting.time
  
  inf.period_discrete <- ceiling(ending.time) - ceiling(starting.time)
  
  return(mean(inf.period_discrete))
}

infectious.period <- output %>%
  group_by(days_inf) %>%
  summarise(infectious_obs = sum(inf_home_days_sum)/sum(num_inf_sum)) %>%
  mutate(infectious_expected = generate_expected_inf.period(days_inf)) %>%
  mutate(rel.diff = infectious_obs/infectious_expected)

prob.asymp_adult <- output %>%
  group_by(p_asymp_adult) %>%
  summarise(p.asymp.adult_obs = mean(p_asymp_adult_obs.mean)) %>%
  mutate(rel.diff = p.asymp.adult_obs/p_asymp_adult)

prob.asymp_child <- output %>%
  group_by(p_asymp_child) %>%
  summarise(p.asymp.child_obs = mean(p_asymp_child_obs.mean)) %>%
  mutate(rel.diff = p.asymp.child_obs/p_asymp_child)

prob.subclin_adult <- output %>%
  mutate(p_subclin_adult_total = p_subclin_adult + p_asymp_adult) %>%
  group_by(p_subclin_adult_total) %>%
  summarise(p.subclin.adult_obs = mean(p_subclin_adult_obs.mean)) %>%
  mutate(rel.diff = p.subclin.adult_obs/p_subclin_adult_total)

prob.subclin_child <- output %>%
  mutate(p_subclin_child_total = p_subclin_child + p_asymp_child) %>%
  group_by(p_subclin_child_total) %>%
  summarise(p.subclin.child_obs = mean(p_subclin_child_obs.mean)) %>%
  mutate(rel.diff = p.subclin.child_obs/p_subclin_child_total)

screening_params <- output %>%
  group_by(test_sens, test_frac) %>%
  summarise(test.sens_obs = (sum(pcr_tp_count.sum)/(sum(pcr_tp_count.sum) + sum(pcr_fn_count.sum)))/(sum(test_regular.sum)/sum(test_regular_eligible.sum)),
            test.frac_obs = sum(test_regular.sum)/sum(test_regular_eligible.sum)) %>%
  mutate(rel.diff_test.sens = test.sens_obs/test_sens,
         rel.diff_test.frac = test.frac_obs/test_frac) %>% filter(test_frac != 0)

local.incidence.rate <- output %>%
  group_by(prob) %>%
  summarise(prob_obs = (sum(child.start.count_sum) + sum(adult.start.count_sum))/(sum(child.community.risk.days_sum) + sum(adult.community.risk.days_sum))) %>%
  mutate(rel.diff = prob_obs/prob)

vax.uptake_family <- output %>%
  group_by(family_susp) %>%
  summarise(vax.uptake_obs = mean(family.vax.rate_obs.mean)) %>%
  mutate(rel.diff = vax.uptake_obs/family_susp)

vax.uptake_teacher <- output %>%
  group_by(teacher_susp) %>%
  summarise(vax.uptake_obs = mean(teacher.vax.rate_obs.mean)) %>%
  mutate(rel.diff = vax.uptake_obs/teacher_susp)

vax.uptake_child <- output %>%
  group_by(child.vax) %>%
  summarise(vax.uptake_obs = mean(child.vax.rate_obs.mean)) %>%
  mutate(rel.diff = vax.uptake_obs/child.vax)

vax.effectiveness <- output %>%
  group_by(vax_eff) %>%
  summarise(vax.eff_obs = mean(vax.eff_obs.mean)) %>%
  mutate(rel.diff = vax.eff_obs/vax_eff)

hosp.rate_adult.unvax <- output %>%
  group_by(adult_unvax_hosp_rate) %>%
  summarise(hosp.rate_obs = sum(hosp_adult.sum)/sum(adult.unvax)) %>%
  mutate(rel.diff = hosp.rate_obs/adult_unvax_hosp_rate)

hosp.rate_child.unvax <- output %>%
  group_by(child_unvax_hosp_rate) %>%
  summarise(hosp.rate_obs = sum(hosp_child.sum)/sum(children.unvax)) %>%
  mutate(rel.diff = hosp.rate_obs/child_unvax_hosp_rate)

table1_check <- data.frame(parameter = character(),
                           target.value = numeric(), observed.value = numeric(), relative.difference = character())

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Baseline Attack Rate",
                        target.value = round(attack.mult$variant.attack, 6),
                        observed.value = round(attack.mult$variant.attack_obs, 6),
                        relative.difference = case_when(attack.mult$rel.diff - 1 == 0 ~ "0%", abs(attack.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(attack.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "At-school mitigation multiplier",
                        target.value = round(mitigation.mult$mitigation.mult, 6),
                        observed.value = round(mitigation.mult$mitigation.mult_obs, 6),
                        relative.difference = case_when(mitigation.mult$rel.diff - 1 == 0 ~ "0%", abs(mitigation.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(mitigation.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "At-home attack rate multiplier",
                        target.value = round(home.mult$home_mult, 6),
                        observed.value = round(home.mult$home_mult_obs, 6),
                        relative.difference = case_when(home.mult$rel.diff - 1 == 0 ~ "0%", abs(home.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(home.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Brief contact multiplier",
                        target.value = round(brief.mult$rel_trans, 6),
                        observed.value = round(brief.mult$brief_mult_obs, 6),
                        relative.difference = case_when(brief.mult$rel.diff - 1 == 0 ~ "0%", abs(brief.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(brief.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Staff-staff contact multiplier",
                        target.value = round(brief_adult.mult$rel_trans_adult, 6),
                        observed.value = round(brief_adult.mult$brief.adult_mult_obs, 6),
                        relative.difference = case_when(brief_adult.mult$rel.diff - 1 == 0 ~ "0%", abs(brief_adult.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(brief_adult.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Child care contact multiplier",
                        target.value = round(care.mult$care_mult, 6),
                        observed.value = round(care.mult$care_mult_obs, 6),
                        relative.difference = case_when(care.mult$rel.diff - 1 == 0 ~ "0%", abs(care.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(care.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Child infectiousness multiplier",
                        target.value = round(child_trans.mult$child_trans, 6),
                        observed.value = round(child_trans.mult$child.trans_mult_obs, 6),
                        relative.difference = case_when(child_trans.mult$rel.diff - 1 == 0 ~ "0%", abs(child_trans.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(child_trans.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Asymptomatic adult infectiousness multiplier",
                        target.value = round(asymp_trans.mult$mult_asymp, 6),
                        observed.value = round(asymp_trans.mult$asymp.trans_mult_obs, 6),
                        relative.difference = case_when(asymp_trans.mult$rel.diff - 1 == 0 ~ "0%", abs(asymp_trans.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(asymp_trans.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Symptomatic child at-home infectiousness multiplier",
                        target.value = round(child_symp_HH_trans.mult$rel_trans_child_symp_HH, 6),
                        observed.value = round(child_symp_HH_trans.mult$child.symp.HH.trans_mult_obs, 6),
                        relative.difference = case_when(child_symp_HH_trans.mult$rel.diff - 1 == 0 ~ "0%", abs(child_symp_HH_trans.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(child_symp_HH_trans.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Child susceptibility multiplier",
                        target.value = round(child_susp.mult$child_susp, 6),
                        observed.value = round(child_susp.mult$child.susp_mult_obs, 6),
                        relative.difference = case_when(child_susp.mult$rel.diff - 1 == 0 ~ "0%", abs(child_susp.mult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(child_susp.mult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Latent Period (days)",
                        target.value = round(latent.period$latent_expected, 6),
                        observed.value = round(latent.period$latent_obs, 6),
                        relative.difference = case_when(latent.period$rel.diff - 1 == 0 ~ "0%", abs(latent.period$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(latent.period$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Incubation Period (days)",
                        target.value = round(incubation.period$incubation_expected, 6),
                        observed.value = round(incubation.period$incubation_obs, 6),
                        relative.difference = case_when(incubation.period$rel.diff - 1 == 0 ~ "0%", abs(incubation.period$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(incubation.period$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Infectious Period (days)",
                        target.value = round(infectious.period$infectious_expected, 6),
                        observed.value = round(infectious.period$infectious_obs, 6),
                        relative.difference = case_when(infectious.period$rel.diff - 1 == 0 ~ "0%", abs(infectious.period$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(infectious.period$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Probability of asymptomatic infection (child)",
                        target.value = round(prob.asymp_child$p_asymp_child, 6),
                        observed.value = round(prob.asymp_child$p.asymp.child_obs, 6),
                        relative.difference = case_when(prob.asymp_child$rel.diff - 1 == 0 ~ "0%", abs(prob.asymp_child$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(prob.asymp_child$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Probability of asymptomatic infection (adult)",
                        target.value = round(prob.asymp_adult$p_asymp_adult, 6),
                        observed.value = round(prob.asymp_adult$p.asymp.adult_obs, 6),
                        relative.difference = case_when(prob.asymp_adult$rel.diff - 1 == 0 ~ "0%", abs(prob.asymp_adult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(prob.asymp_adult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Probability of subclinical infection (child)",
                        target.value = round(prob.subclin_child$p_subclin_child_total, 6),
                        observed.value = round(prob.subclin_child$p.subclin.child_obs, 6),
                        relative.difference = case_when(prob.subclin_child$rel.diff - 1 == 0 ~ "0%", abs(prob.subclin_child$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(prob.subclin_child$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Probability of subclinical infection (adult)",
                        target.value = round(prob.subclin_adult$p_subclin_adult_total, 6),
                        observed.value = round(prob.subclin_adult$p.subclin.adult_obs, 6),
                        relative.difference = case_when(prob.subclin_adult$rel.diff - 1 == 0 ~ "0%", abs(prob.subclin_adult$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(prob.subclin_adult$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Screening Test Sensitivity",
                        target.value = round(screening_params$test_sens, 6),
                        observed.value = round(screening_params$test.sens_obs, 6),
                        relative.difference = case_when(screening_params$rel.diff_test.sens - 1 == 0 ~ "0%", abs(screening_params$rel.diff_test.sens - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(screening_params$rel.diff_test.sens - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Screening Test Uptake",
                        target.value = round(screening_params$test_frac, 6),
                        observed.value = round(screening_params$test.frac_obs, 6),
                        relative.difference = case_when(screening_params$rel.diff_test.frac - 1 == 0 ~ "0%", abs(screening_params$rel.diff_test.frac - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(screening_params$rel.diff_test.frac - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Hospitalization Rate (unvaccinated child)",
                        target.value = round(hosp.rate_child.unvax$child_unvax_hosp_rate, 6),
                        observed.value = round(hosp.rate_child.unvax$hosp.rate_obs, 6),
                        relative.difference = case_when(hosp.rate_child.unvax$rel.diff - 1 == 0 ~ "0%", abs(hosp.rate_child.unvax$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(hosp.rate_child.unvax$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Hospitalization Rate (unvaccinated adult)",
                        target.value = round(hosp.rate_adult.unvax$adult_unvax_hosp_rate, 6),
                        observed.value = round(hosp.rate_adult.unvax$hosp.rate_obs, 6),
                        relative.difference = case_when(hosp.rate_adult.unvax$rel.diff - 1 == 0 ~ "0%", abs(hosp.rate_adult.unvax$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(hosp.rate_adult.unvax$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Vaccine uptake (student)",
                        target.value = round(vax.uptake_child$child.vax, 6),
                        observed.value = round(vax.uptake_child$vax.uptake_obs, 6),
                        relative.difference = case_when(vax.uptake_child$rel.diff - 1 == 0 ~ "0%", abs(vax.uptake_child$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(vax.uptake_child$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Vaccine uptake (teacher)",
                        target.value = round(vax.uptake_teacher$teacher_susp, 6),
                        observed.value = round(vax.uptake_teacher$vax.uptake_obs, 6),
                        relative.difference = case_when(vax.uptake_teacher$rel.diff - 1 == 0 ~ "0%", abs(vax.uptake_teacher$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(vax.uptake_teacher$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Vaccine uptake (family)",
                        target.value = round(vax.uptake_family$family_susp, 6),
                        observed.value = round(vax.uptake_family$vax.uptake_obs, 6),
                        relative.difference = case_when(vax.uptake_family$rel.diff - 1 == 0 ~ "0%", abs(vax.uptake_family$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(vax.uptake_family$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Vaccine effectiveness",
                        target.value = round(vax.effectiveness$vax_eff, 6),
                        observed.value = round(vax.effectiveness$vax.eff_obs, 6),
                        relative.difference = case_when(vax.effectiveness$rel.diff - 1 == 0 ~ "0%", abs(vax.effectiveness$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(vax.effectiveness$rel.diff - 1)*100, "%", sep = ""))))

table1_check <- rbind(table1_check,
                      data.frame(
                        parameter = "Local Incidence Rate (cases per residents per day)",
                        target.value = round(local.incidence.rate$prob, 6),
                        observed.value = round(local.incidence.rate$prob_obs, 6),
                        relative.difference = case_when(local.incidence.rate$rel.diff - 1 == 0 ~ "0%", abs(local.incidence.rate$rel.diff - 1) < 0.01 ~ "<1%", TRUE ~ paste(abs(local.incidence.rate$rel.diff - 1)*100, "%", sep = ""))))

write.xlsx(table1_check, "table1_check.xlsx")