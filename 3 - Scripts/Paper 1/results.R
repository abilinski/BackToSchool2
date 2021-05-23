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
setwd("./4 - Output/Paper 1")

####*********************************** FUNCTIONS ******************************************####

#### CLEAN UP ####
prep_sims = function(sims, name_val = ""){
  out = sims %>% 
    mutate(school = ifelse(grepl("Elem", name_val), "Elementary school", "High school"),
           strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
           strategy = ifelse(test, "Weekly screening", strategy),
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
           id2 = ifelse(id==1, "5-day", id),
           id2 = ifelse(id==2, "Cohorting", id2),
           id2 = ifelse(id==3, "1/2 class size", id2),
           id2 = ifelse(id==4, "A/B", id2),
           id2 = ifelse(type=="A/B" & total_days==1, "A/B/C/D", id2),
           id2 = ifelse(type=="On/off" & total_days==1, "On/off (1)", id2),
           id2 = ifelse(type=="On/off" & total_days==2, "On/off (2)", id2),
           id2 = factor(id2, levels = rev(c("A/B/C/D", "On/off (1)", "On/off (2)", "A/B", "1/2 class size", "Cohorting", "5-day"))),
           strategy = factor(strategy, levels = c("Symptomatic isolation", "Classroom quarantine", "Teacher vaccination", "Weekly screening")))
  return(out)
}

#### MEAN GRAPHS ####
graph_sims1 = function(sims, title = "", palette = pal, ymax = NA){
  
  pal = palette
  
  out = sims %>% filter(!is.na(id2)) %>%
    group_by(id2, n_HH, disperse_transmission, dedens, 
             test, isolate, teacher_susp, notify, child_susp, 
             child_trans, start_type, school, strategy, attack_level) %>%
    summarize(mean.new = mean(as.numeric(tot)),
              mean.R0 = mean(as.numeric(R0)),
              perc.zero = mean(as.numeric(tot==0)),
              mt_5 = mean(as.numeric(tot>5)),
              mt_5_avg = mean(as.numeric(tot)[as.numeric(tot)>5]),
              fam_no_symp = mean(tot>0 & symp_kids==0)/mean(tot>0),
              mean.R0 = mean(R0),
              mean.school.infs = mean(as.numeric(school.infs)),
              mean.school.infs.symp = mean(as.numeric(school.infs.symp)),
              mean.school.infs.symp2 = mean(as.numeric(school.infs.symp2)),
              mean.school.infs.perc = mean(as.numeric(school.infs.perc), na.rm = T),
              mean.source.asymp = mean(source.asymp),
              mean.source.asymp.family = mean(source.asymp.family),
              
              ### change when possib
              mean.kids = mean(as.numeric(children_tot)),
              mean.staff = mean(as.numeric(school_adult_tot)),
              mean.family = mean(as.numeric(family_tot))) %>% ungroup()
  
  a = ggplot(out, 
             aes(x = attack_level, y = mean.new, group = id2, col = id2)) + 
    scale_color_manual(name = "", values = pal) + 
    geom_line() + 
    geom_point(col = "white", size = 5) + 
    facet_grid(school~strategy) + theme_minimal(base_size = size) +
    geom_text(size = font-1, aes(label = case_when(mean.new>10~round(mean.new), mean.new <10 & mean.new > 1~round(mean.new, 1), mean.new<1~round(mean.new, 1)))) + theme_opts + labs(x = "", y = "", title = title) + ylim(0, ymax)
  
  b = ggplot(out, 
             aes(x = attack_level, y = mean.school.infs.symp, group = id2, col = id2)) + 
    scale_color_manual(name = "", values = pal) + 
    geom_line() + 
    geom_point(col = "white", size = 5) + 
    facet_grid(school~strategy) + theme_minimal(base_size = size) +
    geom_text(size = font-1, aes(label = case_when(mean.school.infs.symp>10~round(mean.school.infs.symp), 
                                                   mean.school.infs.symp <10 & mean.school.infs.symp > 1~round(mean.school.infs.symp, 1), 
                                                   mean.school.infs.symp<1~round(mean.school.infs.symp, 2)))) + theme_opts + labs(x = "", y = "", title = title) + ylim(0, ymax)
  d = ggplot(out %>% mutate(mean.R0 = mean.R0), 
             aes(x = attack_level, y = mean.R0, group = id2, col = id2)) + 
    scale_color_manual(name = "", values = pal) + 
    geom_line() + 
    geom_point(col = "white", size = 5) + 
    facet_grid(school~strategy) + theme_minimal(base_size = size) +
    geom_text(size = font-1, aes(label = case_when(mean.R0>10~round(mean.R0), 
                                                   mean.R0<10 & mean.R0> 1~round(mean.R0, 1), 
                                                   mean.R0<1~round(mean.R0, 2)))) + theme_opts + labs(x = "", y = "", title = title) + ylim(0, ymax)
  
  # make plots
  out2 = out %>% gather(var, value, mean.kids, mean.staff, mean.family) %>%
    mutate(var_cat = ifelse(var=="mean.kids", "Students","Staff"),
           var_cat = ifelse(var=="mean.family", "Family", var_cat),
           var_cat = factor(var_cat, levels = c("Students", "Staff", "Family")))
  
  c = ggplot(out2,
             aes(x = attack_level, y = value, group = id2, col = id2)) + 
    scale_color_manual(name = "", values = pal) + 
    geom_line() + 
    geom_point(col = "white", size = 5) + 
    facet_grid(var_cat~strategy, scales = "free") + theme_minimal(base_size = size) +
    geom_text(size = font-1, aes(label = case_when(value>10~round(value), 
                                                   value <10 & value>1~round(value, 1), 
                                                   value<1~round(value, 2)))) + theme_opts + labs(x = "", y = "", title = title) + ylim(0, ymax)
  
  return(list(a,b,c,out,out2,d))
}

#### STOCHASTIC GRAPHS ####
graph_sims2 = function(sims, title = ""){
  
  out = sims %>% filter(id>0) %>% dplyr::group_by(sim.x, strategy, id) %>% 
    mutate(quant = quantile(tot, .995),
           IQR = IQR(tot),
           median = mean(tot),
           #tot = tot + rnorm(tot, mean = .05, sd = .1),
           #tot = ifelse(tot<0, 0, tot)
    ) %>% ungroup() %>% filter(tot < quant) %>%
    filter(strategy%in%c("Symptomatic isolation", "Weekly screening") & 
             id2 %in% c("5-day", "A/B")) %>%
    mutate(grp = paste(as.numeric(id),as.numeric(attack_level), strategy),
           id.strategy = factor(paste(id2, "\n", strategy, sep = "")),
           id.strategy = factor(id.strategy, levels = levels(id.strategy)[c(3,4,1,2)]),
           outlier = tot > median + IQR*1.5)
  
  out_fig = ggplot(out,
                   aes(x = attack_level, y = tot, group = grp, col = id.strategy)) +
    geom_boxplot(outlier.alpha = .04) + 
    facet_wrap(school~attack_level, scales = "free") + facet_grid(school~.) +
    theme_minimal(base_size = size) +
    theme_opts + labs(x = "", y = "", title = title) + 
    scale_color_manual(name = "", values = pal, breaks = c("5-day\nSymptomatic isolation",
                                                           "5-day\nWeekly screening",
                                                           "A/B\nSymptomatic isolation",
                                                           "A/B\nWeekly screening"))
  return(out_fig)
}

graph_sims3 = function(sims, n_teacher = 1, n_student = 1, ymax = NA){
  dyn_elem1 = prep_sims(sims, "Elem") %>% filter(type!="Remote")
  dyn_elem2 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation")
  dyn_elem3 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Classroom quarantine", notify = "TRUE")
  dyn_elem4 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Weekly screening", notify = "TRUE", test = "TRUE")
  dyn_elem5 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Teacher vaccination", teacher_susp = "0.33")
  dyn_elem = bind_rows(dyn_elem1, dyn_elem2, dyn_elem3, dyn_elem4, dyn_elem5)
  
  out = dyn_elem %>% group_by(sim.y, id, id2, n_HH, disperse_transmission, dedens, 
                              test, isolate, teacher_susp, notify, child_susp, 
                              child_trans, start_type, school, strategy,
                              attack_level, adult_prob, scenario, type) %>%
    summarize(all.mean = mean(as.numeric(all)),
              all.median = median(as.numeric(all)),
              teachers.mean = mean(as.numeric(adult)),
              students.mean = mean(as.numeric(children)),
              family.mean = mean(as.numeric(family)),
              tot = mean(as.numeric(tot))) %>% ungroup() %>%
    mutate(grp = factor(paste(n_HH, disperse_transmission, dedens, 
                              teacher_susp, child_susp, child_trans, start_type,
                              attack_level, adult_prob))) %>% 
    group_by(grp) %>% 
    mutate(All = (all.mean)/(n_teacher*2 + n_student*3),
           Staff = (teachers.mean)/n_teacher,
           Students = (students.mean)/n_student,
           Family = (family.mean)/(n_teacher + n_student*2),
           All_inc = All-All[scenario==3],
           Staff_inc = Staff-Staff[scenario==3],
           Students_inc = Students-Students[scenario==3],
           Family_inc = Family-Family[scenario==3],
           scenario = scenario,
           adult_prob = as.numeric(adult_prob)/3*100000,
           id_cat = ifelse(scenario==1, "5-day", "A/B"),
           id_cat = ifelse(scenario==3, "Remote", id_cat),
           n_HH = ifelse(n_HH=="0", 0, as.numeric(n_HH)-1),
           strategy = factor(strategy, levels = c("Symptomatic isolation", "Classroom quarantine", 
                                                  "Teacher vaccination", "Weekly screening"))) %>%
    filter(attack_level!="Low") 
  
  out2 = out %>% gather(var, value, All, Family, Staff, Students) %>%
    mutate(var = factor(var, levels = c("Students", "Staff", "Family", "All")),
           inc_pch = case_when(var=="All"~All_inc>.01, var =="Students"~Students_inc>.01,
                               var=="Staff"~Staff_inc>.01, var=="Family"~Family_inc>.01),
           inc_pch = ifelse(inc_pch==F, NA, inc_pch))
  
  plot_out = ggplot(out2, 
         aes(x = adult_prob, y = value, group = paste(var, attack_level, id_cat, strategy),
             col = id_cat, lty = attack_level)) + geom_line() + 
    geom_point(aes(pch = inc_pch)) + 
    scale_shape_manual(values = c(20, NA), guide = F) + 
    facet_grid(var~strategy) + theme_minimal() + theme_opts +
    labs(x = "", y = "") + 
    scale_color_manual(name = "Strategy", values = c(pal[c(1,4)], "black")) +
    scale_linetype(name = "Prevention measures") + 
    labs(title = "Cumulative incidence over 8 weeks") + ylim(0, ymax)
  
   plot_out2 = ggplot(out2 %>% filter(id_cat%in%c("5-day", "A/B") & attack_level=="Medium" & adult_prob %in% c(10, 50, 100)), 
                    aes(x = n_HH, y = value, group = paste(adult_prob, var, attack_level, id_cat, strategy),
                        col = as.factor(adult_prob), lty = id_cat)) + geom_line() + 
    facet_grid(var~strategy) + theme_minimal() + theme_opts + geom_point() + 
    labs(x = "Number of households mixed when school is out of session", y = "") + 
    scale_color_manual(name = "Community incidence", values = c(pal[c(1,2,4)], "black")) +
    scale_linetype(name = "Strategy") + 
    labs(title = "Cumulative incidence over 8 weeks") + ylim(0, ymax)
   
   out3 = out %>% gather(var, value, All_inc, Family_inc, Staff_inc, Students_inc) %>%
     separate(var, into = c("var_cat", "junk"), sep = "_") %>%
     mutate(var_cat = factor(var_cat, levels = c("Students", "Staff", "Family", "All")))
   
   plot_out3 = ggplot(out3, 
                     aes(x = adult_prob, y = value, group = paste(var_cat, attack_level, id_cat, strategy),
                         col = id_cat, lty = attack_level)) + geom_line() + geom_point() + 
     facet_grid(var_cat~strategy) + theme_minimal() + theme_opts +
     labs(x = "", y = "") + 
     scale_color_manual(name = "Strategy", values = c(pal[c(1,4)], "black")) +
     scale_linetype(name = "Prevention measures") + 
     labs(title = "Cumulative incidence over 8 weeks") + ylim(0, ymax)
   
  return(list(plot_out, out, plot_out2, plot_out3))
  
}

graph_sims4 = function(sims, n_teacher = 1, n_student = 1, ymax = NA){
  dyn_elem1 = prep_sims(sims, "Elem") %>% filter(type!="Remote")
  dyn_elem2 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation")
  dyn_elem3 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Classroom quarantine", notify = "TRUE")
  dyn_elem4 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Weekly screening", notify = "TRUE", test = "TRUE")
  dyn_elem5 = prep_sims(sims, "Elem") %>% filter(type=="Remote" & strategy=="Symptomatic isolation") %>% mutate(strategy= "Teacher vaccination", teacher_susp = "0.33")
  dyn_elem = bind_rows(dyn_elem1, dyn_elem2, dyn_elem3, dyn_elem4, dyn_elem5)
  
  out = dyn_elem %>% group_by(sim.y, id, id2, n_HH, disperse_transmission, dedens, 
                              test, isolate, teacher_susp, notify, child_susp, 
                              child_trans, start_type, school, strategy,
                              attack_level, adult_prob, scenario, type) %>%
    summarize(all.mean = mean(as.numeric(all)),
              all.median = median(as.numeric(all)),
              teachers.mean = mean(as.numeric(adult)),
              students.mean = mean(as.numeric(children)),
              family.mean = mean(as.numeric(family)),
              tot = mean(as.numeric(tot))) %>% ungroup() %>%
    mutate(grp = factor(paste(n_HH, disperse_transmission, dedens, 
                              teacher_susp, child_susp, child_trans, start_type,
                              attack_level, adult_prob))) %>% 
    group_by(grp) %>% 
    mutate(All = (all.mean)/(n_teacher*2 + n_student*3),
           Staff = (teachers.mean)/n_teacher,
           Students = (students.mean)/n_student,
           Family = (family.mean)/(n_teacher + n_student*2),
           scenario = scenario,
           adult_prob = as.numeric(adult_prob)/3*100000,
           id_cat = ifelse(scenario==1, "5-day", "A/B"),
           id_cat = ifelse(scenario==3, "Remote", id_cat),
           n_HH = ifelse(n_HH=="0", 0, as.numeric(n_HH)-1),
           strategy = factor(strategy, levels = c("Symptomatic isolation", "Classroom quarantine", 
                                                  "Teacher vaccination", "Weekly screening"))) %>%
    filter(attack_level!="Low") 
  
  out2 = out %>% gather(var, value, All, Family, Staff, Students) %>%
    mutate(var = factor(var, levels = c("Students", "Staff", "Family", "All")))
  
  plot_out2 = ggplot(out2 %>% filter(id_cat%in%c("5-day", "A/B") & attack_level=="Medium" & adult_prob %in% c(10, 50, 100)), 
                     aes(x = n_HH, y = value, group = paste(adult_prob, var, attack_level, id_cat, strategy),
                         col = as.factor(adult_prob), lty = id_cat)) + geom_line() + 
    facet_grid(var~strategy) + theme_minimal() + theme_opts + geom_point() + 
    labs(x = "Number of households mixed when school is out of session", y = "") + 
    scale_color_manual(name = "Community incidence", values = c(pal[c(1,2,4)], "black")) +
    scale_linetype(name = "Strategy") + 
    labs(title = "Cumulative incidence over 8 weeks") + ylim(0, ymax)
  
  return(list(plot_out2))
  
}

####*********************************** PROCESS DATA ******************************************####

# Base Elementary School
name_val = "fig_output_Base Elem.RData"; load(name_val)
base_elem = prep_sims(sims %>% filter(start_type==1), name_val = "Elem")
a = graph_sims1(base_elem, title = "A")
a1 = graph_sims2(base_elem, title = "A")

# Base High School
name_val = "fig_output_Base HS.RData"; load(name_val)
base_hs = prep_sims(sims)
b = graph_sims1(base_hs, title = "B")
b1 = graph_sims2(base_hs, title = "B")

# Base Elementary School - mod
name_val = "fig_output_Base_Elem_MOD.RData"; load(name_val)
base_elem = prep_sims(sims %>% filter(start_type==1), name_val = "Elem")
a.mod = graph_sims1(base_elem, title = "A")
a.mod.symp = graph_sims1(base_elem %>% filter(start_symp=="1"), title = "A")

# Base High School - mod
name_val = "fig_output_Base_HS_MOD.RData"; load(name_val)
base_hs = prep_sims(sims)
b.mod = graph_sims1(base_hs, title = "B")
b.mod.symp = graph_sims1(base_hs %>% filter(start_symp=="1"), title = "B")

# ELEMENTARY SCHOOL SUPPLEMENTS
# equal inf
name_val = "fig_output_Elem_supp1.RData"
load(name_val)
elem_s1 = prep_sims(sims, name_val = "Elem")

# more schedules
name_val = "fig_output_Elem_supp2.RData"
load(name_val)
elem_s2 = prep_sims(sims, name_val = "Elem") %>% filter(total_days!=5)

# overdispersed transmission
name_val = "fig_output_Elem_supp3.RData"
load(name_val)
elem_s3 = prep_sims(sims, name_val = "Elem")

# teacher as index case
name_val = "fig_output_Elem_supp5.RData"
load(name_val)
elem_s5 = prep_sims(sims, name_val = "Elem")

# make graphs
esa = graph_sims1(elem_s1, title = "Children have equal infectiousness")
esb = graph_sims1(elem_s2, title = "With additional schedules", palette = pal2)
esc = graph_sims1(elem_s3, title = "Children have overdispersed transmission")
esd = graph_sims1(elem_s5, title = "Teacher as index case")

# HIGH SCHOOL SUPPLEMENTS
# less suscepetible
name_val = "fig_output_HS_supp1.RData"; load(name_val)
high_s1 = prep_sims(sims)

# different schedules
name_val = "fig_output_HS_supp2.RData"; load(name_val)
high_s2 = prep_sims(sims)

# overdispersed transmission
name_val = "fig_output_HS_supp3.RData"; load(name_val)
high_s3 = prep_sims(sims)

hsa = graph_sims1(high_s1, title = "Adolescents less susceptible", ymax = 80)
hsb = graph_sims1(high_s2, title = "With different schedules", palette = pal2, ymax = 80)
hsc = graph_sims1(high_s3, title = "Adolescents have overdispersed transmission", ymax = 80)

# DYNAMIC ELEMENTARY
load("fig_output_Dynamic Elem.RData")
a3 = graph_sims3(sims, n_student = 638, n_teacher = 60, ymax = .3)
sims1 = sims
load("fig_output_Dynamic Elem Sens.RData")
a4 = graph_sims3(bind_rows(sims, sims1), n_student = 638, n_teacher = 60, ymax = .3)

# DYNAMIC HIGH SCHOOL
load("fig_output_Dynamic High.RData")
b3 = graph_sims3(sims, n_student = 1451, n_teacher = 124, ymax = .75)
sims1 = sims
load("fig_output_Dynamic High Sens.RData")
b4 = graph_sims4(bind_rows(sims, sims1), n_student = 1451, n_teacher = 124, ymax = 1)

####*********************************** RESULTS ******************************************####

#### Impact of in-school mitigation ####

# Paragraph 1
a[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(mean.new)
a[[4]] %>% filter(id2=="A/B" & strategy=="Classroom quarantine") %>% dplyr::select(mean.new)
max(a[[4]] %>% filter(attack_level=="High") %>% dplyr::select(mean.new))

# Paragraph 2
b[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(mean.new)

#### Quarantine, teacher vaccination, and screening ####

# Paragraph 1
k = b[[4]] %>% filter(id2=="5-day" & attack_level=="Low") %>% dplyr::select(mean.new)
k$mean.new[3]/k$mean.new[2]

k = b[[4]] %>% filter(id2=="5-day" & attack_level=="High") %>% dplyr::select(mean.new)
k$mean.new[3]/k$mean.new[2]

a[[4]] %>% filter(strategy %in% c("Classroom quarantine", "Teacher vaccination")) %>%
  group_by(id2, n_HH, disperse_transmission, dedens, child_susp, child_trans, start_type, school, attack_level) %>%
  summarize(overall = mean.new[1]/mean.new[2],
            teachers = mean.staff[1]/mean.staff[2]) %>%
  ungroup() %>% summarize(mean(overall), mean(teachers))

b[[4]] %>% filter(strategy %in% c("Classroom quarantine", "Teacher vaccination")) %>%
  group_by(id2, n_HH, disperse_transmission, dedens, child_susp, child_trans, start_type, school, attack_level) %>%
  summarize(overall = mean.new[1]/mean.new[2],
            teachers = mean.staff[1]/mean.staff[2]) %>%
  ungroup() %>% summarize(mean(overall), mean(teachers))

esd[[4]] %>% filter(strategy %in% c("Classroom quarantine", "Teacher vaccination")) %>%
  group_by(id2, n_HH, disperse_transmission, dedens, child_susp, child_trans, start_type, school, attack_level) %>%
  summarize(overall = mean.new[1]/mean.new[2],
            teachers = mean.staff[1]/mean.staff[2]) %>%
  ungroup() %>% summarize(mean(overall), mean(teachers))

# Paragraph 2
a[[4]] %>% filter(id2=="5-day" & strategy=="Weekly screening") %>% dplyr::select(mean.new)
b[[4]] %>% filter(id2=="5-day" & strategy=="Weekly screening") %>% dplyr::select(mean.new)

a[[4]] %>% ungroup() %>% summarize(kids = mean(mean.kids)/mean(mean.new), 
                                   staff = mean(mean.staff)/mean(mean.new),
                                   family = mean(mean.family)/mean(mean.new),
                                   tot = mean(mean.new))
b[[4]] %>% ungroup() %>% summarize(kids = mean(mean.kids)/mean(mean.new), 
                                   staff = mean(mean.staff)/mean(mean.new),
                                   family = mean(mean.family)/mean(mean.new),
                                   tot = mean(mean.new))


#### Observability ####
a[[4]] %>% ungroup() %>% summarize(out = mean(mean.school.infs.symp/mean.new))
b[[4]] %>% ungroup() %>% summarize(out = mean(mean.school.infs.symp/mean.new))

a[[4]] %>% ungroup() %>% summarize(out = mean(mean.school.infs.symp2/mean.new))
b[[4]] %>% ungroup() %>% summarize(out = mean(mean.school.infs.symp2/mean.new))

#### Stochastic variation in secondary transmission ####
a[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)
b[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)

#a[[4]] %>% filter(strategy=="Classroom quarantine") %>% dplyr::select(id2, attack_level, perc.zero, mt_5, mt_5_avg)
#b[[4]] %>% filter(strategy=="Classroom quarantine") %>% dplyr::select(id2, attack_level, perc.zero, mt_5, mt_5_avg)

#### Transmissions over the course of the semester ####
f = a3[[2]] %>% filter(scenario!=3 & id_cat=="A/B") %>% group_by(adult_prob, attack_level) %>%
  summarize(mean(All), mean(All_inc<.01), mean(Staff_inc), mean(Staff_inc<.01))

f = a3[[2]] %>% filter(scenario!=3 & id_cat=="5-day" & attack_level == "Medium") %>% 
  group_by(adult_prob, strategy) %>%
  summarize(mean(All), mean(All_inc), mean(All_inc<.01), 
            mean(Staff), mean(Staff_inc), mean(Staff_inc<.01))

f = a3[[2]] %>% filter(scenario!=3 & id_cat=="5-day" & attack_level == "High") %>% 
  group_by(adult_prob, strategy) %>%
  summarize(mean(All_inc<.01), mean(Staff_inc<.01), mean(Staff_inc))

g = b3[[2]] %>% filter(scenario!=3 & id_cat=="5-day" & attack_level == "Medium") %>% 
  group_by(adult_prob, strategy) %>%
  summarize(mean(All_inc<.01), mean(Staff_inc<.01))

g = b3[[2]] %>% filter(scenario!=3 & id_cat=="5-day" & attack_level == "High") %>% 
  group_by(adult_prob, strategy) %>%
  summarize(mean(All), mean(All_inc), mean(All_inc<.01), mean(Staff_inc<.01), mean(Staff_inc))

#### Sensitivity analyses ####
bind_rows(a[[4]] %>% mutate(val = "base"), esa[[4]] %>% mutate(val = "sens")) %>%
  group_by(id2, n_HH, disperse_transmission, dedens, start_type, school, attack_level, strategy) %>%
  arrange(id2, n_HH, disperse_transmission, dedens, start_type, school, attack_level, strategy) %>%
  summarize(out = mean.new[2]/mean.new[1]) %>% ungroup() %>% summarize(mean(out))

bind_rows(b[[4]] %>% mutate(val = "base"), hsa[[4]] %>% mutate(val = "sens")) %>%
  group_by(id2, n_HH, disperse_transmission, dedens, start_type, school, attack_level, strategy) %>%
  summarize(out = mean.new[2]/mean.new[1]) %>% ungroup() %>% summarize(mean(out))

a[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)
esc[[4]] %>% filter(id2=="5-day" & strategy=="Symptomatic isolation") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)

hsc[[4]] %>% filter(id2=="5-day" & strategy=="Symptomatic isolation") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)
b[[4]] %>% filter(id2=="5-day" & strategy=="Classroom quarantine") %>% dplyr::select(perc.zero, mt_5, mt_5_avg)

setwd("./Saved figures")

jpeg("Fig2.jpg", width = 8, height = 6.5, units = "in", res = 500) 
  ggarrange(a[[1]],b[[1]], common.legend = T, ncol = 1, legend = "right")
dev.off()

jpeg("Fig3.jpg", width = 7, height = 6, units = "in", res = 500) 
figure = ggarrange(a1,b1, ncol = 1, common.legend = TRUE, legend="right")
annotate_figure(figure, left = "Number of secondary transmissions", bottom = "School attack rate")
dev.off()

jpeg("Fig4.jpg", width = 8, height = 6.5, units = "in", res = 500) 
print(a3[[1]])
dev.off()

jpeg("Fig5.jpg", width = 8, height = 6.5, units = "in", res = 500) 
print(b3[[1]])
dev.off()

