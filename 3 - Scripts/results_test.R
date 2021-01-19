#*************************** Code to replicate results and figures ************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

####*********************************** SETUP ******************************************####
# set working directory
wd = "~/Dropbox/Schools/Public code/"
setwd(wd)

# source global options
source("global_options.R")

# update working directory
setwd("./4 - Output")


####*********************************** FUNCTIONS ******************************************####

#### CLEAN UP ####
prep_sims = function(sims, name_val = ""){
  out = sims %>% 
    mutate(school = ifelse(grepl("Elem", name_val), "Elementary school", "High school"),
           strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
           strategy = ifelse(test, paste("Screening", " (", ifelse(test_days==1, "1x/week", "2x/week"), ")", sep = ""), strategy),
           strategy = ifelse(teacher_susp < 1, "Teacher vaccination", strategy),
           seed_type = ifelse(start_type =="child", "Child", "Teacher"),
           seed_type = ifelse(start_type =="mix", "Mix", seed_type),
           attack_level = ifelse(attack.y==.01, "High", "Medium"),
           attack_level = ifelse(attack.y==.03, "Low", attack_level),
           attack_level = factor(attack_level, c("Low", "Medium", "High")),
           tot = as.numeric(tot),
           R0 = as.numeric(R0),
           school.infs = as.numeric(adult_tot) + as.numeric(children_tot),
           school.infs.symp = .6*as.numeric(adult_tot) +
             .2*as.numeric(children_tot),
           school.infs.symp2 = .8*as.numeric(adult_tot) +
             .4*as.numeric(children_tot),
           school.infs.perc = ifelse(tot==0, NA, school.infs.symp/tot),
           source.asymp = as.numeric(source_asymp),
           source.asymp.family = as.numeric(source_asymp_family_kids) + as.numeric(source_asymp_family_staff),
           quarantine_days = as.numeric(quarantined),
           detected = as.numeric(detected),
           id2 = ifelse(id==1, "5-day", id),
           id2 = ifelse(id==2, "Cohorting", id2),
           id2 = ifelse(id==3, "1/2 class size", id2),
           id2 = ifelse(id==4, "A/B", id2),
           id2 = ifelse(type=="A/B" & total_days==1, "A/B/C/D", id2),
           id2 = ifelse(type=="On/off" & total_days==1, "On/off (1)", id2),
           id2 = ifelse(type=="On/off" & total_days==2, "On/off (2)", id2),
           id2 = factor(id2, levels = rev(c("A/B/C/D", "On/off (1)", "On/off (2)", "A/B", "1/2 class size", "Cohorting", "5-day"))))
  return(out)
}

# Testing graphs
graph_sims3 = function(sims, n_teacher = 1, n_student = 1, ymax = NA, test_type_set = "1", attack_level_set="Medium",
                       var = "Elementary School"){
  
  dyn_elem = prep_sims(sims, "Elem") %>%
    mutate(days_missed = quarantine_days + ifelse(type=="A/B", (n_teacher+n_student)*30*5/7/2,0)) %>%
    filter(test=="FALSE" | test_type==test_type_set) %>%
    filter(attack_level==attack_level_set) %>%
    filter(!strategy=="Teacher vaccination") %>%
    filter(!(type=="base" & strategy=="Symptomatic isolation")) %>%
    mutate(grp = case_when(type=="A/B"~"A/B", type=="Remote"~"Remote", (type=="base" & test=="FALSE")~"5-Day, No Screening",
           (type=="base" & test=="TRUE")~paste("5-Day,", strategy)))
  
  out = dyn_elem %>% group_by(sim.y, id, id2, n_HH, disperse_transmission, dedens, 
                              test, isolate, teacher_susp, notify, child_susp, 
                              child_trans, start_type, school, strategy,
                              attack_level, adult_prob, scenario, type, 
                              test_days, test_type, test_frac, grp) %>%
    summarize(all.mean = mean(as.numeric(all)),
              detected = mean(detected),
              quarantine_days = mean(quarantine_days),
              days_missed = mean(days_missed),
              all.median = median(as.numeric(all)),
              teachers.mean = mean(as.numeric(adult)),
              students.mean = mean(as.numeric(children)),
              school.mean = teachers.mean + students.mean,
              family.mean = mean(as.numeric(family)),
              tot = mean(as.numeric(tot))) %>% ungroup() %>%
    mutate(School_inc = school.mean/(n_teacher + n_student), 
           scenario = scenario,
           adult_prob = as.numeric(adult_prob)/3*100000,
           id_cat = ifelse(scenario==1, "5-day", "A/B"),
           id_cat = ifelse(scenario==3, "Remote", id_cat),
           n_HH = ifelse(n_HH=="0", 0, as.numeric(n_HH)-1)) %>%
    filter(attack_level!="Low") 
  
  out2 = out %>% gather(var, value, School_inc, detected, quarantine_days, days_missed) %>%
    mutate(
      var_cat = case_when(var=="School_inc"~"School incidence", var=="detected"~"Cases detected",
                          var=="quarantine_days"~"Quarantine days", var=="days_missed"~"Days of school missed"),
      var_cat = factor(var_cat, levels = c("School incidence", "Cases detected", "Quarantine days", "Days of school missed"))) 
  
  plot_out = ggplot(out2 %>% filter(test_frac==.9), 
         aes(x = adult_prob, y = value, group = grp, col = grp)) + geom_line() + 
    facet_wrap(.~var_cat, scales = "free_y", ncol = 4) + theme_minimal() + theme_opts +
    labs(x = "", y = "") + 
    scale_color_brewer(name = "", palette = "Set1") + 
    #scale_color_manual(name = "Strategy", values = c(pal[c(1,4)], "black")) +
    scale_linetype(name = "Prevention measures") + 
    labs(title = var) + ylim(0, NA)
  
  return(list(plot_out, out))
  
}

####*********************************** PROCESS DATA ******************************************####
# DYNAMIC ELEMENTARY
load("fig_output_Elem Test.RData")
a = graph_sims3(sims, n_student = 638, n_teacher = 60, ymax = .25)
a[[1]]
