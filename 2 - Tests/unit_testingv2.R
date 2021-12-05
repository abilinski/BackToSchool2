
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
  #save(out, file = paste0("results", i, "_", Sys.time(), ".RData"))
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

#*************************** TEST MAKE_SCHOOL *********************************#
#*

# general checks
chk_general = function(n = 30, synthpop_val = synthpop, 
                      includeFamily_val = F, n_class_val = 4){
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  return(sum(!g$adult)==nrow(synthpop_val))
}

chk_general(synthpop_val = synthpop)
chk_general(synthpop_val = synthpop_MS)
chk_general(synthpop_val = synthpop_HS)

chk_general(synthpop_val = synthpop, includeFamily_val = T)
chk_general(synthpop_val = synthpop_MS, includeFamily_val = T)
chk_general(synthpop_val = synthpop_HS, includeFamily_val = T)

# check number of classes
chk_classes = function(n = 30, synthpop_val = synthpop, 
                       includeFamily_val = F, n_class_val = 4){
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  return((length(unique(g$class))-1)==n_class_val*length(unique(synthpop_val$age)))
}

sapply(c(4,10,16), function(a) chk_classes(n_class_val = a))
sapply(c(4,10,16), function(a) chk_classes(n_class_val = a, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_classes(n_class_val = a, synthpop_val = synthpop_HS))

# check number of other adults
chk_adults = function(n, synthpop_val = synthpop, 
                      includeFamily_val = F, n_class_val = 4){
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  return(sum(g$adult & !g$family & g$class==99)==n)
}

sapply(seq(1,100, by = 10), function(a) chk_adults(a))
sapply(seq(1,100, by = 10), function(a) chk_adults(a, includeFamily_val = T))
sapply(seq(1,100, by = 10), function(a) chk_adults(a, n_class = 10))
sapply(seq(1,100, by = 10), function(a) chk_adults(a, n_class = 20)) 
# FAIL BUT JUST BECAUSE THERE ARE MORE THAN 99 CLASSES

# check teachers
chk_teachers = function(n = 30, synthpop_val = synthpop, 
                      includeFamily_val = F, n_class_val = 4){
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  return(sum(g$adult & g$class!=99)==n_class_val*length(unique(synthpop_val$age)))
}

sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a))
sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a, synthpop_val = synthpop_HS))

sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a, includeFamily = T))
sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a, includeFamily = T, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_teachers(n_class_val = a, includeFamily = T, synthpop_val = synthpop_HS))

# check families
chk_family = function(n = 30, synthpop_val = synthpop, 
                        includeFamily_val = F, n_class_val = 4){
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  return(sum(g$family)==includeFamily_val*(length(unique(synthpop_val$HH_id))*2 + n_class_val*length(unique(synthpop_val$age))+n) &
           sum(g$family_staff)==includeFamily_val*(n_class_val*length(unique(synthpop_val$age))+n))
}

sapply(c(4,10,16), function(a) chk_family(n_class_val = a))
sapply(c(4,10,16), function(a) chk_family(n_class_val = a, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_family(n_class_val = a, synthpop_val = synthpop_HS))

sapply(c(4,10,16), function(a) chk_family(n_class_val = a, includeFamily = T))
sapply(c(4,10,16), function(a) chk_family(n_class_val = a, includeFamily = T, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_family(n_class_val = a, includeFamily = T, synthpop_val = synthpop_HS))

# check group numbers
chk_group = function(n = 30, synthpop_val = synthpop, 
                      includeFamily_val = F, n_class_val = 4){
  
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  h = g %>% filter(!adult) %>% group_by(HH_id) %>% summarize(out = length(unique(group)))
  return(!sum(h$out!=1))
}

sapply(c(4,10,16), function(a) chk_group(n_class_val = a))
sapply(c(4,10,16), function(a) chk_group(n_class_val = a, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_group(n_class_val = a, synthpop_val = synthpop_HS))

sapply(c(4,10,16), function(a) chk_group(n_class_val = a, includeFamily = T))
sapply(c(4,10,16), function(a) chk_group(n_class_val = a, includeFamily = T, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_group(n_class_val = a, includeFamily = T, synthpop_val = synthpop_HS))


# check group numbers
chk_class = function(n = 30, synthpop_val = synthpop, 
                     includeFamily_val = F, n_class_val = 4){
  
  g = make_school(n_other_adults = n, synthpop = synthpop_val,
                  includeFamily = includeFamily_val, n_class = n_class_val)
  h = g %>% filter(class!=99) %>% group_by(class) %>% summarize(out = length(class))
  return(c(min(h$out), max(h$out), mean(h$out), median(h$out)))
}

sapply(c(4,10,16), function(a) chk_class(n_class_val = a))
sapply(c(4,10,16), function(a) chk_class(n_class_val = a, synthpop_val = synthpop_MS))
sapply(c(4,10,16), function(a) chk_class(n_class_val = a, synthpop_val = synthpop_HS))

#*************************** TEST INITIALIZE_SCHOOL *********************************#

g = make_school(n_other_adults = 30, synthpop = synthpop,
                includeFamily = T, n_class = 4)
  
# check parameters
chk_params = function(a, n_contacts = 10, n_contacts_brief = 0, rel_trans_HH = 1,
                      rel_trans = 1/8, rel_trans_brief = 1/50, p_asymp_adult = .35,
                      p_asymp_child = .7, p_subclin_adult = .15, p_subclin_child  = .25, 
                      isolate = T, dedens = T, notify = T, run_specials = F, start = g, rel_trans_HH_symp_child = 2){
  
  f = initialize_school(start = start, n_contact = n_contacts, n_contacts_brief = n_contacts_brief,
                        rel_trans_HH = rel_trans_HH, rel_trans = rel_trans, rel_trans_brief = rel_trans_brief,
                        p_asymp_adult = p_asymp_adult, p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, 
                        p_subclin_child = p_subclin_child, isolate = isolate, dedens = dedens, 
                        run_special = run_specials, notify = notify, rel_trans_HH_symp_child = rel_trans_HH_symp_child) 
  pop = nrow(f)
  adult = sum(f$adult)
  kid = sum(!f$adult)

  return(c(sum(f$n_contact==n_contacts) == pop, sum(f$n_contact_brief==n_contacts_brief) == pop,
           sum(f$relative_trans_HH==rel_trans_HH) == pop, sum(f$relative_trans==rel_trans) == pop, 
           sum(f$relative_trans_brief==rel_trans_brief) == pop, 
           sum(f$p_asymp==p_asymp_adult) == adult, sum(f$p_subclin==p_subclin_adult) == adult,
           sum(f$p_asymp==p_asymp_child)==kid, sum(f$p_subclin==p_subclin_child)==kid,
           sum(f$run_specials==run_specials)==pop,
           sum(f$notify==notify)==pop, sum(f$dedens==dedens)==pop, sum(f$isolate==isolate)==pop,
           sum(f$relative_trans_HH_symp_child==rel_trans_HH_symp_child)==kid)) 
  
}

sapply(c(4,10,16), function(a) chk_params(n_contacts = a))
sapply(c(4,10,16, 20), function(a) chk_params(n_contacts_brief = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(rel_trans_HH = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(rel_trans = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(rel_trans_brief = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(p_asymp_child = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(p_asymp_adult = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(p_subclin_child = a))
sapply(c(seq(0,1,by=.1)), function(a) chk_params(p_subclin_adult = a))
sapply(c(0,1), function(a) chk_params(isolate = a))
sapply(c(0,1), function(a) chk_params(notify = a)) # check
sapply(seq(0,1,by=.1), function(a) chk_params(dedens = a)) # check
sapply(seq(0,1,by=.1), function(a) chk_params(run_specials = a)) # check
sapply(seq(1,5,by=1), function(a) chk_params(rel_trans_HH_symp_child = a)) # one false when equal

# check attack rates
chk_attack = function(a, attack = .02, teacher_trans = .8, child_trans = .5, dedens = .6, start = g){
  f = initialize_school(attack = attack, teacher_trans = teacher_trans, child_trans = child_trans, 
                        dedens = dedens, start = start)
  
  adult = sum(f$adult & !f$family)
  family = sum(f$family)
  kid = sum(!f$adult)
  
  return(c(sum(f$class_trans_prob==attack*child_trans*dedens)==kid, 
           sum(f$class_trans_prob==attack*dedens)==family,
           sum(f$class_trans_prob==attack*dedens*teacher_trans)==adult))
}

sapply(seq(0.01,1,by=.1), function(a) chk_attack(attack = a))
sapply(seq(0.01,1,by=.1), function(a) chk_attack(teacher_trans = a))
sapply(seq(0.01,1,by=.1), function(a) chk_attack(child_trans = a))
sapply(seq(0.01,1,by=.1), function(a) chk_attack(dedens = a))

# check vaccination status
chk_vacc = function(child_susp = .5, child_vax = .25, family_susp = .9,
                    teacher_susp = .8, vax_eff = .8, trials = 200, start){
  
  g = data.frame()
  for(i in 1:trials){
  f = initialize_school(child_susp = child_susp, child_vax = child_vax, 
                        teacher_susp = teacher_susp, family_susp = family_susp, vax_eff = vax_eff, start = start)
  
  g = bind_rows(g, f %>% group_by(adult, family) %>% summarize(vacc = mean(vacc), susp = mean(susp)))
  
  }
  
  h = g %>% group_by(adult, family) %>% summarize(vacc = mean(vacc), susp = mean(susp)) %>% 
    ungroup() %>%
    mutate(vacc_real = case_when(family~family_susp, (adult & !family)~teacher_susp, !adult~child_vax),
           susp_real = 1-vacc*vax_eff, susp_real = ifelse(!adult, susp_real*child_susp, susp_real))

  return(c(sum(abs(h$vacc-h$vacc_real)) < .05, sum(abs(h$susp-h$susp_real))<.05))
}

a = sapply(c(.01, 1, by = .3), function(a) chk_vacc(child_susp = a, start = g))
b = sapply(c(.01, 1, by = .3), function(a) chk_vacc(child_vax = a, start = g))
c = sapply(c(.01, 1, by = .3), function(a) chk_vacc(family_susp = a, start = g))
d = sapply(c(.01, 1, by = .3), function(a) chk_vacc(teacher_susp = a, start = g))
e = sapply(c(.01, 1, by = .3), function(a) chk_vacc(vax_eff = a, start = g))
a;b;c;d;e

# check specials
chk_spec = function(run_specials, start = g){
  f = initialize_school(start = start, run_specials = run_specials)
  print(f %>% group_by(adult, family, class == 99) %>% summarize(sum(specials)))
}

sapply(c(0,1), function(a) chk_spec(run_special = a))

# check miscellaneous
chk_no_test_vax = function(start = g, no_test_vacc){
  f = initialize_school(start = start, no_test_vacc = no_test_vacc)
  
  return(c(mean(f$inc_test[f$vacc==1]!=no_test_vacc), mean(f$inc_test[f$vacc==0])==1))
}
sapply(c(0,1), function(a) chk_no_test_vax(no_test_vacc = a))


#*************************** TEST MAKE_SCHEDULE *********************************#

h = initialize_school(start = g)

chk_value = function(time = 45, type = "base", total_days = 5, df = h){
  test = make_schedule(time = time, type = type, total_days = total_days, df = df) %>% 
    mutate(level = case_when(!adult~"Children", (adult&!family)~"Staff", family~"Family"))
  print(ggplot(test, aes(x = t, y = paste(group, id), fill = present)) + geom_tile() + 
    facet_wrap(.~level, scales = "free"))
}

chk_value()
chk_value(type = "Remote")
chk_value(type = "A/B", total_days = 2)


#*************************** TEST RUN_HOUSEHOLD *********************************#

#### TEST FUNCTION ####
test_HH = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 1, trials = 100, synthpop_val = synthpop, n_class = 4,
n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, overdisp_off = T,
mult_asymp_child = 1, rel_trans_HH_symp_child = 1, time = 30){

  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
 

  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          p_asymp_adult = p_asymp_adult,
                          rel_trans_HH_symp_child = rel_trans_HH_symp_child)
    s = make_schedule(time = 45, type = "Remote", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, overdisp_off = overdisp_off, time = time, mult_asymp_child = mult_asymp_child)
    
    #print(out$class_trans_prob)
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$HH_id[out$id%in%starts]  
    
    chk = floor(out$t_end_inf_home[out$id%in%starts]) - ceiling(out$t_inf[out$id%in%starts]) + 1
    fam = out[out$HH_id%in%id_keep & !out$id%in%out$id[out$start],]
    non_fam = out[!out$HH_id%in%id_keep & !out$start,]
    
    v1[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1])          # non-seed family-members infected
    v2[i] = mean(non_fam$t_inf!=-1 & non_fam$t_inf>out$start.time[1])   # people infected outside family
    v3[i] = mean(chk)                                                   # duration of infection
    v4[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1] & fam$source %in% starts)    # non-seed family-members infected BY SEED
    v5[i] = mean(out$symp[out$id%in%starts])                            # percentage of seeds symptomatic
  }
  
  return(c(mean(v1), mean(v2), mean(v3), mean(v4), mean(v5)))
}

#### READ ME ####
# Make sure that !df$start is commented out of df$inf_home prior to running
# Transmission probabilty from index should follow geometric CDF -- e.g., 1-(1-a)^5.5

#### TESTS ####

# everyone infected
sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100))

# infected at attack = .1
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200))

# infected at attack = .1, adults have mult_asymp = .5, but all are symp
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp = .5, seed_asymp = F))

# infected at attack = .1, kids have mult_asymp = .5, but all are symp
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp_child = .5, seed_asymp = F))

# infected at attack = .1, kids have transmission doubled in HH
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, rel_trans_HH_symp_child = .5, seed_asymp = F))

# infected at attack = .1, kids have mult_asymp = .5, 50% are symp
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp_child = .5, p_asymp_child = .5, seed_asymp = F))

# infected at attack = .1, seed_asymp = T
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp = .5, seed_asymp = T))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp_child = .5, seed_asymp = T))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp_child = .5, rel_trans_HH_symp_child = 2, seed_asymp = T))

# infected at attack = .5, time = 1, seed_asymp = T
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .5, trials = 200, mult_asymp = .5, seed_asymp = T, time = 1))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .5, trials = 200, mult_asymp_child = .5, seed_asymp = T, time = 1))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .5, trials = 200, mult_asymp_child = .5, rel_trans_HH_symp_child = 2, seed_asymp = T, time = 1))

# run with continuous introductions
sapply(c("cont"), function(a) test_HH(start_type = a, attack = .1, trials = 100))

# varying synthpop
sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100, synthpop_val=synthpop_MS))
sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100, synthpop_val=synthpop_HS))

# varying synthpop, attack = .1
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, synthpop_val=synthpop_MS))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, synthpop_val=synthpop_HS))


#*************************** TEST RUN_CLASS *********************************#

# starting functions
test_class = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                   rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                   n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
                   seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                   n_start = 1, high_school = F, time = 30){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate)
    
    #print(h %>% group_by(adult, family, class==99) %>% summarize(length(id)))
    
    s = make_schedule(time = 45, type = "base", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, time = time)
    
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$class[out$id%in%starts]  

    chk = floor(out$t_end_inf_home[out$id%in%starts]) - ceiling(out$t_inf[out$id%in%starts]) + 1
    fam = out[out$class%in%id_keep & !out$id%in%out$id[out$start],]
    non_fam = out[!out$class%in%id_keep & !out$start,]
    
    v1[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1])
    v2[i] = mean(non_fam$t_inf!=-1 & non_fam$t_inf>out$start.time[1])
    v3[i] = mean(chk)
    v4[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1] & fam$source %in% starts)
    v5[i] = sum(out$t_exposed!=-1 & !out$start)
    
  }
  
  return(c(mean(v1), mean(v2), mean(v3), mean(v4), mean(v5)))
}

sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, trials = 20))
sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, trials = 20, n_start = 2))
sapply(c("adult"), function(a) test_class(start_type = a, trials = 20)) # 24/54 are teachers

sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, attack = .1, trials = 20, n_start = 2))
sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, attack = .1, trials = 20, n_start = 2, child_susp = .5))
sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, attack = .1, trials = 20, n_start = 2, child_susp = .5, child_trans = .5))

# 5/7*5 = 3.57
# 1-.9^3.57 = 31%
# 1-.95^3.57 = 17%
# 1-.975^3.57 = 9%

sapply(c("family", "child", "teacher"), function(a) test_class(start_type = a, trials = 20, synthpop_val = synthpop_HS, high_school = T, n_class = 16))
sapply(c("child"), function(a) test_class(start_type = a, trials = 1, synthpop_val = synthpop_HS, high_school = T, rel_trans = 1, n_class = 16, time = 1))
# toggle line 417 to test
sapply(c("child"), function(a) test_class(start_type = a, trials = 10, synthpop_val = synthpop_HS, high_school = T, attack = .1, rel_trans = 1/8, n_class = 16, time = 1))
# 5/7*.1*1/8*180
sapply(c("child"), function(a) test_class(start_type = a, trials = 10, synthpop_val = synthpop_HS, high_school = T, attack = .5, rel_trans = 1/8, n_class = 16, time = 1))
# 5/7*.5*1/8*140

#*************************** TEST RUN_RAND *************************************#
#* Check the timing as well
test_rand = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                      rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                      n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
                      seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                      n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v5 = rep(0, trials)
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact)
    
    #print(h %>% group_by(adult, family, class==99) %>% summarize(length(id)))
    
    s = make_schedule(time = 45, type = "base", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf)
    
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$class[out$id%in%starts]  
    
    #print(table(out$t_exposed))
    print(sum(out$t_exposed==time_seed_inf))
    #print(table(out$location))
    v5[i] = sum(out$t_exposed!=-1 & !out$start)
    
  }
  
  return(c(mean(v5)))
}

# Comment out class & specials for this test
sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 20, time = 1))
sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 20, time = 1, n_start = 2))
sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1/5, trials = 100, time = 1))
sapply(c("child"), function(a) test_rand(start_type = a, rel_trans = 1/5, trials = 100, time = 1, child_susp = .5))
sapply(c("child"), function(a) test_rand(start_type = a, rel_trans = 1/5, trials = 100, time = 1, child_trans = .5))

sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 2, time = 5))
sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 2, time = 10))

sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 20, time = 1, n_contact = 5))
sapply(c("family", "child", "teacher", "adult"), function(a) test_rand(start_type = a, rel_trans = 1, trials = 20, time = 1, n_start = 2, n_contact = 5))

#*************************** TEST RUN_STAFF_RAND *************************************#
test_staff_rand = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                     rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                     n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
                     seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                     n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v5 = rep(0, trials)
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact)
    
    #print(h %>% group_by(adult, family, class==99) %>% summarize(length(id)))
    
    s = make_schedule(time = 45, type = "base", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact)
    
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$class[out$id%in%starts]  
    
    #print(table(out$t_exposed))
    #print(sum(out$t_exposed==time_seed_inf))
    #print(table(out$location))
    v5[i] = sum(out$t_exposed!=-1 & !out$start)
    
  }
  
  return(c(mean(v5)))
}

# Comment out class, random & specials for this test
sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 2, time = 1, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 20, time = 1, n_start = 2, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1/5, trials = 100, time = 1, n_staff_contact = 10))
sapply(c("adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1/5, trials = 100, time = 1, child_susp = .5, n_staff_contact = 10))
sapply(c("adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1/5, trials = 100, time = 1, child_trans = .5, n_staff_contact = 10))
sapply(c("adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1/5, trials = 100, time = 1, teacher_susp = .7, n_staff_contact = 10))

sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 2, time = 5, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 2, time = 10, n_staff_contact = 10))

sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 20, time = 1, n_staff_contact = 5))
sapply(c("family", "child", "teacher", "adult"), function(a) test_staff_rand(start_type = a, rel_trans = 1, rel_trans_adult = 1, trials = 20, time = 1, n_start = 2, n_staff_contact = 5))

sapply(c("cont"), function(a) test_staff_rand(start_type = a, rel_trans_adult = 1/5, trials = 100, time = 10, child_trans = .5))


#*************************** TEST RUN_SPECIALS *************************************#

test_specials = function(start_type = "special", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                     rel_trans_adult = 0, rel_trans = 1, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                     n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                    seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                     n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                     type = "A/B", total_days = 2, num_adults = 2, run_specials_now = F, chk_staff = T){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  v6 = rep(0, trials)
  v7 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax, run_specials = run_specials_now)
    
    v1[i] = sum(h$specials)
    
    # check the specials contact matrix
    #specials = data.frame(teacher = rep(h$id[h$specials], each = 4), class = sample(1:max(h$class[!is.na(h$class) & h$class<99]), 4*sum(h$specials), replace = T))
    #table(specials$class)
    #table(specials$teacher)
    
    s = make_schedule(time = 45, type = "base", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact, n_HH = n_HH, overdisp_off = T,
                    num_adults = num_adults)
    
    # starting individuals
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    class_infs = unique(out$class[out$t_exposed!=-1 & !out$start])
    
    v2[i] = sum(out$t_exposed!=-1 & !out$start)
    
    if(chk_staff){
      v3[i] = length(class_infs)
      v4[i] = mean(out$t_exposed[!out$adult & out$class%in%class_infs]!=-1)
      v5[i] = mean(out$t_exposed[out$adult & out$class%in%class_infs]!=-1)    
      
    }
    
    if(!chk_staff){
      v3[i] = sum(out$t_exposed!=-1 & !out$start & out$specials)
      v4[i] = out$specials_count[out$start]
      v5[i] = sum(v3[i]==v4[i])
      v6[i] = ifelse(length(out$location[out$t_exposed!=-1 & !out$start])>0, mean(out$location[out$t_exposed!=-1 & !out$start]=="Related arts"), 1)
    }
  }
  
  if(chk_staff){ 
    return(data.frame(num_specials_teachers = mean(v1), num_infected = mean(v2), 
                                   num_classes_infected = mean(v3), 
                                   num_classes_infected_is_4 = mean(v3==4), 
                                   perc_kids_inf_in_class = mean(v4), 
                                   perc_adults_inf_in_class = mean(v5)))
  }else{
    return(data.frame(num_specials_teachers = mean(v1), num_infected = mean(v2), 
                      num_infected_specials_teachers = mean(v3),
                      min = min(v3), max = max(v3),
                      chk = mean(v4),
                      chk2 = mean(v5), loc = mean(v6)))
    }
  
  
}

#### READ ME ####
# Comment out class and random transmission before this test.
# Also uncomment specials2 and df$specials_count
# Around lines 1047-8.

# FOR TEACHER TRANSMITTING TO STUDENTS
# number of times 4 are uniquely chosen
# prob: (24*23*22*21)/(24^4) = 76%; run 1000 sims to check that this is right
# expected number of unique draws: 4*(24*23*22*21)/(24^4) + 3*(choose(4,2)*24*23*22)/(24^4) + 2*(choose(4,1)*24*23 + choose(4,2)/2*24*23)/(24^4) + 1*(24)/(24^4)
sapply(c("special"), function(a) test_specials(start_type = a, trials = 100, time = 1))
sapply(c("special"), function(a) test_specials(start_type = a, trials = 100, time = 1, run_specials_now = T))
sapply(c("special"), function(a) test_specials(start_type = a, trials = 10, time = 2))

sapply(c("special"), function(a) test_specials(start_type = a, trials = 10, time = 1, attack = .5))
sapply(c("special"), function(a) test_specials(start_type = a, trials = 10, time = 1, child_susp = .5))
sapply(c("special"), function(a) test_specials(start_type = a, trials = 10, time = 1, child_vax = 1))
sapply(c("special"), function(a) test_specials(start_type = a, trials = 10, time = 1, teacher_susp = 1))

# FOR STUDENTS TRANSMITTING TO TEACHERS
# would expect 20/24=0.83 for num_infected
# this might be a little low? -- John?
sapply(c("child", "teacher"), function(a) test_specials(start_type = a, trials = 1000, time = 1, run_specials_now = F, chk_staff = F))
sapply(c("child"), function(a) test_specials(start_type = a, trials = 100, time = 1, run_specials_now = F, chk_staff = F))

# would expect 60/24=2.5 for num_infected
sapply(c("child"), function(a) test_specials(start_type = a, trials = 1000, time = 1, run_specials_now = T, chk_staff = F))

# run over longer horizon
sapply(c("child"), function(a) test_specials(start_type = a, trials = 100, time = 2, chk_staff = F))
sapply(c("child"), function(a) test_specials(start_type = a, trials = 10, time = 30, chk_staff = F))

# half transmission
sapply(c("child"), function(a) test_specials(start_type = a, trials = 100, time = 1, run_specials_now = F, chk_staff = F, child_trans = 0.5))

# teacher vaxxed
sapply(c("child"), function(a) test_specials(start_type = a, trials = 100, time = 1, run_specials_now = F, chk_staff = F, teacher_susp = 1))

# reduce attack rate
sapply(c("child"), function(a) test_specials(start_type = a, trials = 100, time = 1, run_specials_now = F, chk_staff = F, attack = .5))

#*************************** TEST RUN_CARE *************************************#
test_care = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                           rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                           n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                           seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                           n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                     type = "A/B", total_days = 2, num_adults = 2){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  v6 = rep(0, trials)
  v7 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax)
    
    #print(h %>% group_by(adult, family, class==99) %>% summarize(length(id)))
    
    s = make_schedule(time = 45, type = "Remote", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact, n_HH = n_HH, overdisp_off = T,
                    num_adults = num_adults)
    
    # starting individuals
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$HH_id[out$id%in%starts]  
    
    # infected households
    HHs_inf = unique(out$HH_id[out$t_exposed!=-1 & !out$start])
    HHs_inf_df = out[out$HH_id %in% HHs_inf,]
    HHs_inf_kids = HHs_inf_df %>% filter(!adult); HHs_inf_adults = HHs_inf_df %>% filter(adult)
    # outcomes
    v1[i] = sum(out$t_exposed!=-1 & !out$start & out$HH_id==id_keep)  # infected from household
    v2[i] = sum(out$t_exposed!=-1 & !out$start)                       # infected total
    v3[i] = length(HHs_inf)                                           # number of households with infections
    v4[i] = sum(out$t_exposed!=-1)                                    # total number infected
    v5[i] = sum(!out$adult[out$HH_id%in%id_keep])==0  #sum(out$start & out$family_staff)                       # seed infections from staff of family
    v6[i] = ifelse(nrow(HHs_inf_kids)>0, mean(HHs_inf_kids$t_exposed!=-1 & HHs_inf_kids$HH_id%in%HHs_inf), NA) # percentage of children infected in contact households
    v7[i] = ifelse(nrow(HHs_inf_adults)>0, mean(HHs_inf_adults$t_exposed!=-1 & HHs_inf_adults$HH_id%in%HHs_inf), NA) # percentage of adults infected in contact households
  }
  
  return(data.frame(inf_from_seed = mean(v1), inf_total_non_seed = mean(v2), inf_total = mean(v4),
                    num_HH_non_seed = mean(v3), seed_staff = mean(v5), pedi_perc = mean(v6, na.rm = T),
         adult_perc = mean(v7, na.rm = T)))
}

# no one should be transmitting here
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 10, time = 1, n_HH = 0))

# or here -- transmissions kept to 0
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 10, time = 10, n_HH = 1, type = "base", total_days = 5))

# transmission to 1 household; all kids; 2 adults mixing
# from family -- p(inf) = P(not staff family)*P(parent selected)*(1-P(second parent selected)) 
# Approx .94*2/3*(1-1/3) = .42
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, type = "Remote", total_days = 5))

# check for 2 households; all kids; 2 adults mixing
# from family -- E(HH) = P(not staff family)*P(parent selected)*(1-P(second parent selected)) 
# Approx 2*(.94*6/15*(1-1/5)) = .42
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 3, type = "Remote", total_days = 5))

# check for infecting all adults
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

# check for infecting child_susp
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, child_susp = .5, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

# check for infecting child_trans
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, child_trans = .5, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

# check for infecting family_susp
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, family_susp = 1, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

# check for infecting child_vax
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, child_vax = 1, rel_trans_CC = 1, trials = 200, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

# check for rel_trans_CC
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 0, trials = 20, time = 1, n_HH = 2, num_adults = 50, type = "Remote", total_days = 5))

#*************************** TEST MAKE QUARANTINE *************************************#

# test inner functions
# class_quarantine isn't generally returned directly
test_quarantine1 = function(high_school = F, quarantine.length = 14, quarantine.grace = 3,
                           start_type = "child", attack = 1, rel_trans_CC = 0,
                           rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                           n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                           seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                           n_start = 1, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                           type = "base", total_days = 2, num_adults = 2, version = 2, nper = 8,
                           id.inf1 = 1, t.notify1 = 10, id.inf2 = c(2,39), t.notify2 = 11, id.inf3 = 3, t.notify3 = 28){
  
    # set up base functions
    g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
    df = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax) 

    s = make_schedule(time = 45, type = type, total_days = 5, df = df)

    # make high school schedule if applicable
    # set up scheduling if high school
    hs.classes = NA
    if(high_school){
      hs.classes = make_hs_classes(df = df, nper = nper)    
      classes.ind = sapply(df$id, function(a) hs.classes$class[hs.classes$id == a])
    }
    
    # set up the quarantine object
    if(version == 1 | type=="base") df$group[df$group!=99] = 0  # make sure quarantine doesn't go by group
    if(!high_school){class_quarantine = expand_grid(class = unique(df$class[df$class!=99]), group = unique(df$group[df$group
                                                                                                                    !=99])) %>%
      mutate(class_group = paste(class, group), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length, num = 0)
    }else{class_quarantine = data.frame(class = unique(hs.classes$class), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length)}
    df = df %>% mutate(class_group = paste(class, group))
    
    # make a kid infected
    df.u = df %>% filter(id %in% id.inf1) %>% mutate(t_notify = t.notify1); print(df.u$class_group)
    class_quarantine1 = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
    print(table(class_quarantine1$t_notify))
    if(high_school){chk1 = mean(hs.classes$class[hs.classes$id%in%id.inf1]%in%class_quarantine1$class[class_quarantine1$t_notify!=-1])
    }else{chk1 = mean(df$class_group[df$id%in%id.inf1]%in%class_quarantine1$class_group[class_quarantine1$t_notify!=-1])}
    
    # make more kids infected
    df.u = df %>% filter(id %in% id.inf2) %>% mutate(t_notify = t.notify2); print(df.u$class_group)
    class_quarantine2 = make_quarantine(class_quarantine1, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
    print(table(class_quarantine2$t_notify))
    if(high_school){chk2 = mean(hs.classes$class[hs.classes$id%in%c(id.inf1, id.inf2)]%in%class_quarantine2$class[class_quarantine2$t_notify!=-1])
    }else{chk2 = mean(df$class_group[df$id%in%c(id.inf1, id.inf2)]%in%class_quarantine2$class_group[class_quarantine2$t_notify!=-1])}
    
    # make even more kids infected
    df.u = df %>% filter(id %in% id.inf3) %>% mutate(t_notify = t.notify3); print(df.u$class_group)
    class_quarantine3 = make_quarantine(class_quarantine2, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
    print(table(class_quarantine3$t_notify))
    if(high_school){
      chk3 = mean(hs.classes$class[hs.classes$id%in%c(id.inf3, id.inf2)]%in%class_quarantine3$class[class_quarantine3$t_notify!=-1]) 
    }else{
      chk3 = mean(df$class_group[df$id%in%c(id.inf3, id.inf2)]%in%class_quarantine3$class_group[class_quarantine3$t_notify!=-1])}
    
    print(c(chk1, chk2, chk3))
    return(list(class_quarantine1, class_quarantine2, class_quarantine3))
  }

#### READ ME ####
# Note that class_quarantine is initialized inside the function.
# If initialization changes, this needs to be updated

# Elementary school # 
# converts on 3 but not on 2
# converts more than 1 class on 2
a = test_quarantine1(t.notify3 = 28)

# doesn't convert early
b = test_quarantine1(t.notify3 = 27)

# teacher
c = test_quarantine1(id.inf1 = 639)

# A/B
a = test_quarantine1(type = "A/B")
b = test_quarantine1(type = "A/B", t.notify3 = 27)
c = test_quarantine1(type = "A/B", id.inf1 = 639)

# High school # 
c1 = test_quarantine1(high_school = T, id.inf2 = c(1,39))
c2 = test_quarantine1(high_school = T, id.inf2 = c(1,39), id.inf3 = 1)
c3 = test_quarantine1(high_school = T)


# check through the main analysis
test_quarantine2 = function(quarantine.length = 14, quarantine.grace = 3, nper = 8,
                           start_type = "child", attack = 1, rel_trans_CC = 0,
                           rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                           n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                           seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                           n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                           type = "A/B", total_days = 2, num_adults = 2, notify = F){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  v6 = rep(0, trials)
  v7 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_asymp_adult = p_asymp_adult,
                          p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax)
    
    #print(h %>% group_by(adult, family, class==99) %>% summarize(length(id)))
    
    s = make_schedule(time = 45, type = type, total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact, n_HH = n_HH, overdisp_off = T,
                    num_adults = num_adults, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, nper = nper,
                    notify = notify, type = type)
    
    # starting individuals
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$class[out$id%in%starts]  
    out_ids = out$class[out$id%in%starts | (out$HH_id%in%out$HH_id[out$id==starts] & out$class!=99)]
    
    v1[i] = sum(out$quarantined)
    v2[i] = length(unique(out$class[out$quarantined>0]))
    v3[i] = ifelse(v2[i]>0, mean(unique(out$class[out$quarantined>0] %in% id_keep)), NA)
    v4[i] = ifelse(v2[i]>0, mean(unique(out$class[out$quarantined>0] %in% out_ids)), NA)
    v5[i] = mean(out$symp[out$id%in%starts])
    v6[i] = mean(out$t_notify[out$id%in%starts]==-17)
    v7[i] = length(unique(out$id[out$quarantined>0]))
    
  }
  
  return(data.frame(num_q_days = mean(v1), num_class_q = mean(v2), mean(v3, na.rm = T), 
                    mean(v4, na.rm = T), mean(v5), mean(v6), mean(v7)))
}

#### READ ME ####
# Run with class_trans set equal to 0.
# Note that 44% of adults are teachers in the school.

# check base schedules
sapply(c("family", "child", "teacher", "adult"), function(a) test_quarantine2(start_type = a, time = 1, isolate = F, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult"), function(a) test_quarantine2(start_type = a, time = 1, isolate = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, trial = 100, time = 5, isolate = T, notify = T, n_staff_contact = 10))

# schedules
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, type = "base", trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, type = "A/B", trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, type = "Remote", trial = 100, time = 5, isolate = T, notify = T, n_staff_contact = 10))

# kid symptoms
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, p_asymp_child = .999, p_subclin_child = .001, trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, p_subclin_child = .999, p_asymp_child = .001, trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))

# adult symptoms
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, trial =1, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, p_asymp_adult = .999, trial =1, time = 5, isolate = T, notify = T, n_staff_contact = 10))
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, p_subclin_adult = 1, trial =1, time = 5, isolate = T, notify = T, n_staff_contact = 10))

# run with and without class trans set at 0
sapply(c("family", "child", "teacher", "adult", "special"), function(a) test_quarantine2(start_type = a, p_asymp_child = .999, p_subclin_child = .001, p_asymp_adult = .999, trial = 10, time = 5, isolate = T, notify = T, n_staff_contact = 10))

#*************************** TEST MAKE_INFECTED *************************************#
test_make_infected1 = function(quarantine.length = 14, quarantine.grace = 3, nper = 8,
                               start_type = "child", attack = .1, rel_trans_CC = 0,
                               rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 1, trials = 100, synthpop_val = synthpop, n_class = 4,
                               n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                               seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                               n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                               type = "A/B", total_days = 2, num_adults = 2, notify = F, init = 1, days_inf = 5, set = NA, turnaround.time = 1,
                               overdisp_off = T, rel_trans_HH_symp_child = 2, mult_asymp_child = 1){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  v6 = rep(0, trials)
  v7 = rep(0, trials)
  v8 = rep(0, trials)
  v9 = rep(0, trials)
  v10 = rep(0, trials)
  v11 = rep(0, trials)
  v12 = rep(0, trials)
  v13 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_asymp_adult = p_asymp_adult,
                          p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax,
                          rel_trans_HH_symp_child = rel_trans_HH_symp_child, notify = notify)
    
    if(is.na(set)) h$t_exposed = 3
    
    # infect some folks
    out = make_infected(df.u = h[h$id %in% init,], 
                        days_inf = days_inf, set = set, 
                        mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child, 
                        seed_asymp = seed_asymp, turnaround.time = turnaround.time, 
                        overdisp_off = overdisp_off)
    
    
    # starting individuals
    v1[i] = sum(out$symp)
    v2[i] = sum(out$p_asymp)
    v3[i] = sum(out$sub_clin)
    v4[i] = mean(floor(out$t_end_inf[out$t_inf!=-1])-ceiling(out$t_inf[out$t_inf!=-1]) + 1) 
    v5[i] = mean(floor(out$t_end_inf_home[out$t_inf!=-1])-ceiling(out$t_inf[out$t_inf!=-1]) + 1) 
    v6[i] = mean(ceiling(out$t_notify[out$t_inf!=-1])-ceiling(out$t_inf[out$t_inf!=-1]) + 1)
    v7[i] = mean(out$class_trans_prob)
    v8[i] = mean(out$relative_trans_HH)
    v9[i] = mean(out$t_inf!=-1)
    v10[i] = mean(out$t_symp[out$t_inf!=-1]-out$t_inf[out$t_inf!=-1])
    v11[i] = mean(out$t_symp[out$t_inf!=-1]-out$t_exposed[out$t_inf!=-1])
    v12[i] = mean(out$t_inf[out$t_inf!=-1]-out$t_exposed[out$t_inf!=-1])
    v13[i] = mean(ceiling(out$t_notify[out$t_inf!=-1])-floor(out$t_end_inf[out$t_inf!=-1])) #floor(out$t_end_inf[out$t_inf!=-1])
  }
  
  return(data.frame(symp = mean(v1),
                    asymp = mean(v2),
                    sub_clin = mean(v3), 
                    dur = mean(v4), 
                    dur_home = mean(v5), 
                    notify_time = mean(v6), 
                    class_trans = mean(v7), 
                    rel_trans = mean(v8),
                    mean(v9), mean(v10), 
                    mean(v11), mean(v12),
                    mean(v13)))
}

# children
run = expand_grid(notify = c(T,F), isolate = c(T,F), 
                  p_asymp_child = c(0,1), p_subclin_child = c(0,1), mult_asymp_child = c(.5, 1),
                  rel_trans_HH_symp_child = c(2,5))

g = t(sapply(1:nrow(run), function(a) test_make_infected1(isolate = run$isolate[a], 
                                                    notify = run$notify[a], 
                                                    p_asymp_child = run$p_asymp_child[a],
                                                    p_subclin_child = run$p_subclin_child[a],
                                                    mult_asymp_child = run$mult_asymp_child[a],
                                                    rel_trans_HH_symp_child = run$rel_trans_HH_symp_child[a],
                                                    trials = 200)))

g1 = t(sapply(1:nrow(run), function(a) test_make_infected1(isolate = run$isolate[a], 
                                                          notify = run$notify[a], 
                                                          p_asymp_child = run$p_asymp_child[a],
                                                          p_subclin_child = run$p_subclin_child[a],
                                                          mult_asymp_child = run$mult_asymp_child[a],
                                                          rel_trans_HH_symp_child = run$rel_trans_HH_symp_child[a],
                                                          trials = 200, set = 10)))

# adults
run = expand_grid(notify = c(T,F), isolate = c(T,F), 
                  p_asymp_adult = c(0,1), p_subclin_adult = c(0,1), mult_asymp = c(.5, 1),
                  rel_trans_HH_symp_child = c(1,2))
g2 = t(sapply(1:nrow(run), function(a) test_make_infected1(isolate = run$isolate[a], 
                                                    notify = run$notify[a], 
                                                    p_asymp_adult = run$p_asymp_adult[a],
                                                    p_subclin_adult = run$p_subclin_adult[a],
                                                    mult_asymp = run$mult_asymp[a],
                                                    mult_asymp_child = 7,
                                                    rel_trans_HH_symp_child = run$rel_trans_HH_symp_child[a],
                                                    trials = 200, init = 700, overdisp_off = T)))

g3 = t(sapply(1:nrow(run), function(a) test_make_infected1(isolate = run$isolate[a], 
                                                           notify = run$notify[a], 
                                                           p_asymp_adult = run$p_asymp_adult[a],
                                                           p_subclin_adult = run$p_subclin_adult[a],
                                                           mult_asymp = run$mult_asymp[a],
                                                           mult_asymp_child = 7,
                                                           rel_trans_HH_symp_child = run$rel_trans_HH_symp_child[a],
                                                           trials = 200, set = 5, init = 701, overdisp_off = T)))

# test through main function
# Readme: comment out inf_home start seed
test_make_infected2 = function(quarantine.length = 14, quarantine.grace = 3, nper = 8,
                               start_type = "child", attack = 1, rel_trans_CC = 0,
                               rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 1, trials = 100, synthpop_val = synthpop, n_class = 4,
                               n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1, child_vax = 0,
                               seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                               n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                               type = "A/B", total_days = 2, num_adults = 2, notify = F, init = 1, days_inf = 5, set = NA, turnaround.time = 1,
                               overdisp_off = T, rel_trans_HH_symp_child = 2, mult_asymp_child = 1){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  v5 = rep(0, trials)
  v6 = rep(0, trials)
  v7 = rep(0, trials)
  v8 = rep(0, trials)
  v9 = rep(0, trials)
  v10 = rep(0, trials)
  v11 = rep(0, trials)
  v12 = rep(0, trials)
  v13 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_asymp_adult = p_asymp_adult,
                          p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate, n_contacts = n_contact, child_vax = child_vax,
                          rel_trans_HH_symp_child = rel_trans_HH_symp_child, notify = notify)
    
    if(is.na(set)) h$t_exposed = 3
    s = make_schedule(time = 45, type = type, total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact, n_HH = n_HH, overdisp_off = T,
                    num_adults = num_adults, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, nper = nper,
                    notify = notify, type = type)
    
    # starting individuals
    v1[i] = mean(out$inf_days[out$t_inf!=-1])
    v2[i] = mean(out$inf_home_days[out$t_inf!=-1])
    v3[i] = mean(out$symp_days[out$t_inf!=-1])
    v4[i] = mean(out$symp_and_inf_days[out$t_inf!=-1])
    v5[i] = mean(out$last[out$t_inf!=-1 & out$symp & !out$sub_clin]-out$t_notify[out$t_inf!=-1 & out$symp & !out$sub_clin])
    #floor(out$t_end_inf[out$t_exposed>=0])
  }
  
  return(data.frame(mean(v1), mean(v2),
                    mean(v3), mean(v4), mean(v5)))
}

# children
run = expand_grid(notify = c(T,F), isolate = c(T,F), 
                  p_asymp_child = c(0,1), p_subclin_child = c(0,1), mult_asymp_child = c(.5, 1),
                  rel_trans_HH_symp_child = c(2,5))

g = t(sapply(1:nrow(run), function(a) test_make_infected2(isolate = T, attack = 0, 
                                                          notify = run$notify[a], 
                                                          p_asymp_child = run$p_asymp_child[a],
                                                          p_subclin_child = run$p_subclin_child[a],
                                                          p_asymp_adult = 0, p_subclin_adult = 0,
                                                          mult_asymp_child = run$mult_asymp_child[a],
                                                          rel_trans_HH_symp_child = run$rel_trans_HH_symp_child[a],
                                                          trials = 1)))


#*************************** TEST MAKE_HS_CLASSES *************************************#

#### HIGH SCHOOL SCHEDULING ####
# starting functions
test_make_hs = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                      rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                      n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
                      seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                      n_start = 1, high_school = F){
  
  g = make_school(n_other_adults = n_other_adults, synthpop = synthpop_val,
                  includeFamily = T, n_class = n_class)
  
  
  v1 = rep(0, trials)
  v2 = rep(0, trials)
  v3 = rep(0, trials)
  v4 = rep(0, trials)
  
  for(i in 1:trials){
    h = initialize_school(start = g, attack = attack, child_susp = child_susp, child_trans = child_trans,
                          rel_trans = rel_trans, rel_trans_brief = 0, rel_trans_HH = rel_trans_HH, 
                          family_susp = family_susp, teacher_susp = teacher_susp, disperse_transmission = F,
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                          isolate = isolate)
    
    out = make_hs_classes(df = h, nper = 8)
  }
  
  return(out)
}

chk = test_make_hs(start_type = a, trials = 1, synthpop_val = synthpop_HS, high_school = T, n_class = 16)

# check distribution by age
table(chk$class, chk$age)
table(chk$class)
table(chk$class[chk$age==0])
chk %>% group_by(class) %>% summarize(mean(age!=0))
chk %>% group_by(class) %>% summarize(sum(age==0))

# check that everyone has classes
p2 = chk %>% group_by(id) %>% summarize(p = length(class))
table(p2$p)

# number that hsould be included
dim(synthpop_HS)
1451+64

# approx number of contacts
print(length(chk$id[chk$class%in%chk$class[chk$id==29]]))
print(length(unique(chk$id[chk$class%in%chk$class[chk$id==29]])))

#*************************** TEST MULT_RUNS *************************************#
#* Is every argument passed to every subfunction?
# make_school = 4
# initialize_school = 19
# make_schedule = 4
# run_model = 63
df_SENS1 = make_df(attack = c(.02, .04), n_tot = 1, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = c(.7, .9),
                   scenario = c("Base case"), teacher_susp = c(.9),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2), child_vax = 0,
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = T, notify = T) 

run_parallel(df_SENS1[1,], synthpop, class = class)
# starts at time 15
# saves 30 days of output

#*************************** ADDITIONAL TESTS *********************************#

mean(out$start_kids)
child_prob*45


mean(out$start_adults) 
adult_prob*45

