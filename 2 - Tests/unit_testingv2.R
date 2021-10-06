library(BackToSchool)

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
                      isolate = T, dedens = T, notify = T, run_specials = F, start = g){
  
  f = initialize_school(start = start, n_contact = n_contacts, n_contacts_brief = n_contacts_brief,
                        rel_trans_HH = rel_trans_HH, rel_trans = rel_trans, rel_trans_brief = rel_trans_brief,
                        p_asymp_adult = p_asymp_adult, p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, 
                        p_subclin_child = p_subclin_child, isolate = isolate, dedens = dedens, 
                        run_special = run_specials, notify = notify) 
  pop = nrow(f)
  adult = sum(f$adult)
  kid = sum(!f$adult)

  return(c(sum(f$n_contact==n_contacts) == pop, sum(f$n_contact_brief==n_contacts_brief) == pop,
           sum(f$relative_trans_HH==rel_trans_HH) == pop, sum(f$relative_trans==rel_trans) == pop, 
           sum(f$relative_trans_brief==rel_trans_brief) == pop, 
           sum(f$p_asymp==p_asymp_adult) == adult, sum(f$p_subclin==p_subclin_adult) == adult,
           sum(f$p_asymp==p_asymp_child)==kid, sum(f$p_subclin==p_subclin_child)==kid,
           sum(f$run_specials==run_specials)==pop,
           sum(f$notify==notify)==pop, sum(f$dedens==dedens)==pop, sum(f$isolate==isolate)==pop)) 
  
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

# starting functions
test_HH = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 1, trials = 100, synthpop_val = synthpop, n_class = 4,
n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, overdisp_off = T){

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
                          p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child)
    s = make_schedule(time = 45, type = "Remote", total_days = 5, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, overdisp_off = overdisp_off)
    
    #print(out$class_trans_prob)
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$HH_id[out$id%in%starts]  
    
    chk = floor(out$t_end_inf_home[out$id%in%starts]) - ceiling(out$t_inf[out$id%in%starts]) + 1
    fam = out[out$HH_id%in%id_keep & !out$id%in%out$id[out$start],]
    non_fam = out[!out$HH_id%in%id_keep & !out$start,]
    
    v1[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1])
    v2[i] = mean(non_fam$t_inf!=-1 & non_fam$t_inf>out$start.time[1])
    v3[i] = mean(chk)
    v4[i] = mean(fam$t_inf!=-1 & fam$t_inf>=out$start.time[1] & fam$source %in% starts)
    
  }
  
  return(c(mean(v1), mean(v2), mean(v3), mean(v4)))
}

#sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 2, overdisp_off = F))
sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp = .5, seed_asymp = F))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .1, trials = 200, mult_asymp = .5, seed_asymp = T))
sapply(c("family", "child", "adult"), function(a) test_HH(start_type = a, attack = .02, trials = 200, mult_asymp = .5, seed_asymp = T))
sapply(c("cont"), function(a) test_HH(start_type = a, attack = .1, trials = 100))

sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100, synthpop_val=synthpop_MS))
sapply(c("family", "child", "adult", "cont"), function(a) test_HH(start_type = a, trials = 100, synthpop_val=synthpop_HS))

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

#*************************** TEST RUN_CARE *************************************#
test_care = function(start_type = "child", df = h, sched = s, attack = 1, rel_trans_CC = 0,
                           rel_trans_adult = 0, rel_trans = 0, rel_trans_HH = 0, trials = 100, synthpop_val = synthpop, n_class = 4,
                           n_other_adults = 30, child_susp = 1, child_trans = 1, teacher_susp = 0, family_susp = 0, mult_asymp = 1,
                           seed_asymp = F, p_asymp_child = 0, p_subclin_child = 0, p_asymp_adult = 0, p_subclin_adult = 0, isolate = F,
                           n_start = 1, high_school = F, time = 30, time_seed_inf = 1, n_contact = 10, n_staff_contact = 10, n_HH = 1,
                     type = "A/B", total_days = 2){
  
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
    
    s = make_schedule(time = 45, type = type, total_days = total_days, df = h)
    
    out = run_model(start_type = start_type, df = h, sched = s, adult_prob = 75/100000, child_prob = 33/100000,
                    rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, mult_asymp = mult_asymp,
                    days_inf = 5, seed_asymp = seed_asymp, n_start = n_start, high_school = high_school, 
                    time = time, time_seed_inf = time_seed_inf, n_staff_contact = n_staff_contact, n_HH = n_HH, overdisp_off = T)
    
    starts = out$id[out$start & out$t_inf > 0 & out$t_end_inf_home >= out$start.time[1] & out$t_inf<45]
    id_keep = out$class[out$id%in%starts]  
    
    #print(table(out$t_exposed))
    #print(sum(out$t_exposed==time_seed_inf))
    #print(table(out$location))
    v5[i] = sum(out$t_exposed!=-1 & !out$start)
    
  }
  
  return(c(mean(v5)))
}

sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 1, time = 1, n_HH = 0))
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 1, time = 10, n_HH = 1, type = "base", total_days = 5))
sapply(c("family", "child", "teacher", "adult"), function(a) test_care(start_type = a, rel_trans_CC = 1, trials = 2, time = 10, n_HH = 1))


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

