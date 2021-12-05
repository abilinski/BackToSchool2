#************************************** School ABM ****************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

#' @import tidyverse
#' @import igraph 
#' @import tictoc
library(tidyverse)
library(igraph)
library(tictoc)

#' Synthetic Maryland elementary school population
#'
#' A data frame containing a synthetic population of children
#' ages 5-10, representative of the state of Maryland.
#' This is used by make_school() to sort children into classes.
#'
#' @docType data
#' @format A data frame with
#' \describe{
#'  \item{HH_id}{household ID}
#'  \item{age}{age}
#'  \item{flag_mult}{true if more than one child in the household, not used}
#'  \item{id}{individual id #}
#' }
#'
#' @usage data(synthMaryland)
#'
#' @keywords datasets
#'
#' @source Wheaton, W.D., U.S. Synthetic Population 2010 Version 1.0 Quick Start Guide, RTI International, May 2014.
#' (\href{https://fred.publichealth.pitt.edu/syn_pops}{website}).  Created with script demographic_data2.R.
#' 
"synthpop"


#' Synthetic Maryland high school population
#'
#' A data frame containing a synthetic population of children
#' ages 14-17, representative of the state of Maryland.
#' This is used by make_school() to sort children into classes.
#'
#' @docType data
#' @format A data frame with
#' \describe{
#'  \item{HH_id}{household ID}
#'  \item{age}{age}
#'  \item{flag_mult}{true if more than one child in the household, not used}
#'  \item{id}{individual id #}
#' }
#'
#' @usage data(synthMaryland_HS)
#'
#' @keywords datasets
#'
#' @source Wheaton, W.D., U.S. Synthetic Population 2010 Version 1.0 Quick Start Guide, RTI International, May 2014.
#' (\href{https://fred.publichealth.pitt.edu/syn_pops}{website}).  Created with script demographic_data2.R.
#' 
"synthpop_HS"

#' Make school
#'
#' This function allows you to sort a synthetic population into classes.
#' It also assigns children to groups for alternating schedules and
#' ensures that children are in the same group as siblings.
#' It adds non-primary teacher staff, and if families are included, includes
#' two adult family members per child and one per adult staff member.
#'
#' @param synthpop synthetic population; defaults to synthMaryland stored in file
#' @param n_other_adults Number of adults in the school other than primary teachers; defaults to 30
#' @param includeFamily whether to include family and adult family members of teachers, default = FALSE
#' @param n_class number of classes per grade
#'
#' @return out data frame of child and teacher attributes
#'
#' @export
make_school = function(
  # starting synthetic population
  synthpop, n_other_adults = 30, includeFamily = F, n_class = 4){
  
  # select rows
  kids = synthpop %>%
    
    # make classes as close to equal size as possible
    mutate(class.placeholder = rep(1:n_class, length.out = nrow(synthpop)),
           class = as.numeric(as.factor(paste(age, class.placeholder))),
           adult = F, family = F, family_staff = F) %>%
    
    # get rid of class placeholder variable
    dplyr::select(-class.placeholder) %>%
    
    # randomly reorder kids in classes
    group_by(age) %>% mutate(class = sample(class)) %>% group_by(class) %>%
    
    # randomly sort to groups
    mutate(group = sample(rep(sample(0:1), length.out = length(class))),  # when split in 2
           group_quarter = sample(rep(sample(0:3), length.out = length(class)))) %>%  # when split in  4
    
    # reassign siblings to all be in the same group
    gather(var, value, group, group_quarter) %>% group_by(HH_id, var) %>%
    mutate(old = value, value = value[1], swap = old!=value, swap2 = F) %>% ungroup() %>% arrange(HH_id)
  
  # now reassign children w/o siblings to replace siblings who were swapped
  # if none exists, it doesn't make a swap
  # this is not elegant/sophisticated
  # but gets pretty decent balance
  for(i in 1:nrow(kids)){
    
    # check if a child was swapped
    if(kids$swap[i]==T){
      
      # look for kids in the classroom into which they were swapped
      # that don't have siblings
      vec = as.vector(kids$id[kids$id!=kids$id[i] & kids$class==kids$class[i] & kids$var==kids$var[i] &
                                kids$value==kids$value[i] & !kids$flag_mult & !kids$swap2])
      
      # if these exist
      if(length(vec)>0){
        
        # take one if there's only 1
        if(length(vec)==1){
          id.swap = vec
        } else(id.swap = sample(vec, 1))  # otw select one randomly
        
        # then reassign that kid to the old classroom
        # and note that the kid was swapped
        kids$value[kids$id==id.swap & kids$var==kids$var[i]] = kids$old[i]
        kids$swap2[kids$id==id.swap & kids$var==kids$var[i]] = T
      }
    }
  }
  
  # spread kids data frame so that group variables are columns again
  kids = kids %>% dplyr::select(-old, -swap, -swap2) %>% spread(var, value)
  
  # make teachers data frame
  teachers = data.frame(HH_id = max(kids$HH_id+1):(max(kids$HH_id) + max(kids$class)),
                        age = 0, class = unique(kids$class), adult = T, group = 99,
                        group_quarter = 99, family = F, family_staff = F)
  
  # make other adults data frame
  other_adults = data.frame(HH_id = max(teachers$HH_id + 1):max(teachers$HH_id + n_other_adults), 
                            age = 0, class = 99, adult = T, class_grp2 = 5, class_grp4 = 5, 
                            group = 99, group_quarter = 99, family =  F, family_staff = F)
  
  # bind kid and teacher data frames
  out = kids %>% bind_rows(teachers) %>% mutate(class_grp2 = class%%2, class_grp4 = class%%4) %>%
    arrange(adult, class) %>% bind_rows(other_adults)
  
  if(includeFamily){
    
    # make family data frame
    family = data.frame(HH_id = c(rep(1:max(synthpop$HH_id), each = 2), unique(teachers$HH_id), unique(other_adults$HH_id)),
                        age = 0, flag_mult = NA, family = T, adult = T,
                        group = 99, group_quarter = 99, class = 99) %>%
      mutate(family_staff = ifelse(HH_id > max(synthpop$HH_id), T, F))
    
    out = out %>% bind_rows(family)
    
  }
  
  return(out)
  
}

#' Initialize school
#'
#' This function takes in a data frame exported by make_school().
#' It adds epidemiological attributes of the full school community.

#' @param n_contacts Number of sustained contacts outside of the classroom; defaults to 10
#' @param n_contacts_brief Number of brief contacts outside of the classroom; defaults to 0
#' @param rel_trans_HH Relative attack rate of household contact (vs. classrom); defaults to 1
#' @param rel_trans_HH_symp_child Additional relative attack rate of a symptomatic infected child in the household; defaults to 1
#' @param rel_trans Relative attack rate of sustained contact (vs. classroom); defaults to 1/8
#' @param rel_trans_brief Relative attack rate of brief contact (vs. classroom); defaults to 1/50
#' @param p_asymp_adult Fraction of adults with asymptomatic disease; defaults to 0.4
#' @param p_asymp_child Fraction of children with asymptomatic disease; defaults to 0.8
#' @param p_subclin_adult Fraction of adults with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_child Fraction of children with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param attack Average daily attack rate in adults; defaults to 0.01
#' @param child_trans Relative transmissibility of children (vs. adults); defaults to 1
#' @param child_susp Relative transmissibility of children (vs. adults); defaults to .5
#' @param child_vax Vaccination rate of children; defaults to 0
#' @param teacher_trans Factor by which teacher transmissibility is reduced due to intervention; defaults to 1
#' @param teacher_susp Factor by which teacher transmissibility is reduced due to intervention; defaults to 1
#' @param family_susp Factor by which adult transmissibility is reduced due to intervention; defaults to 1
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param notify Whether classrooms are notified following a positive test; defaults to T
#' @param dedens Whether dedensification measures reduce attack rate; defaults to F
#' @param run_specials Whether special subjects are run; defaults to F
#' @param vax_eff Vaccine efficacy, defaults to 0.9
#' @param no_test_vacc Indicates whether vaccinated individuals are excluded from TTS & screening; defaults to F
#' @param start Data frame from make_school()
#'
#' @return out data frame of child and teacher attributes.
#'
#' @export
initialize_school = function(n_contacts = 10, n_contacts_brief = 0, rel_trans_HH = 1, rel_trans_HH_symp_child = 1,
                             rel_trans = 1/8, rel_trans_brief = 1/50, p_asymp_adult = .35,
                             p_asymp_child = .7, p_subclin_adult = 0, p_subclin_child  = 0,
                             attack = .01, child_trans = 1, child_susp = .5, family_susp = 1,
                             teacher_trans = 1, teacher_susp = 1, disperse_transmission = T, child_vax = 0,
                             isolate = T, dedens = T, run_specials = F, start, vax_eff = .9, notify = T, 
                             no_test_vacc = F){
  
  # make non-teacher adults
  n = nrow(start)
  m = nrow(start[!start$family,])
  c = max(start$class)
  
  # initialize data frame
  df = start %>%
    mutate(id = row_number(),
           start = F,
           t_exposed = -99,
           t_inf = -1,
           symp = NA,
           sub_clin = NA,
           t_symp = -1,
           t_end_inf = -1,
           t_end_inf_home = -1,
           t_notify = -17,
           c_trace = -1,
           c_trace_start = -1,
           tot_inf = 0,
           detected = 0,
           detected_q = 0,
           detected_q_start = 0,
           quarantined = 0,
           quarantined2 = 0,
           test_ct = 0,
           test_ct_q = 0,
           n_contact = n_contacts,
           n_contact_brief = n_contacts_brief,
           relative_trans = rel_trans,
           relative_trans_HH = rel_trans_HH,
           relative_trans_HH_symp_child = ifelse(adult, 1, rel_trans_HH_symp_child),
           relative_trans_brief = rel_trans_brief,
           attack_rate = attack,
           dedens = dedens,
           source = 0,
           source_symp = NA,
           tot_inf = 0,
           run_specials = run_specials,
           super_spread = disperse_transmission,
           out = 0,
           location = "",
    ) %>%
    mutate(p_asymp = ifelse(adult, p_asymp_adult, p_asymp_child),
           p_subclin = ifelse(adult, p_subclin_adult, p_subclin_child),
           
           # isolation
           isolate = rbinom(n, size = 1, prob = isolate),
           notify = notify,
           
           # transmission probability
           class_trans_prob = attack,
           class_trans_prob = ifelse(adult, class_trans_prob, child_trans*class_trans_prob),
           class_trans_prob = dedens*class_trans_prob,
           class_trans_prob = ifelse(adult & !family, class_trans_prob*teacher_trans, class_trans_prob),
           
           # susceptibility
           child_susp_val = child_susp,
           child_vax_val = child_vax,
           teacher_susp_val = teacher_susp,
           family_susp_val = family_susp,
           vax_eff_val = vax_eff,
           
           vacc = ifelse(adult, 1, rbinom(n, size = 1, prob = child_vax_val)),
           vacc = ifelse(adult & !family, rbinom(n, size = 1, prob = teacher_susp_val), vacc),
           vacc = ifelse(family, rbinom(n, size = 1, prob = family_susp_val), vacc),
           inc_test = ifelse(no_test_vacc & vacc, 0, 1),
           
           susp = ifelse(vacc==0, 1, rbinom(n, size = 1, prob = 1-vax_eff_val)),
           susp = ifelse(!adult, child_susp_val*susp, susp),
           
           specials = ifelse(run_specials, id%in%(m:(m-14)), id%in%(m:(m-4)))) %>% ungroup()
  
  return(df)
}

#' Make schedule
#'
#' Make a schedule of when individuals in the school community are
#' present/absent
#'
#' @param time number of days; defaults to 30
#' @param type "base", "On/off", "A/B", "Remote"; defaults to "base"
#' @param total_days number of days in school; defaults to 5
#' @param df data frame from make_school()
#'
#' @return d Returns a n x time data frame that indicates whether an individual is
#' in the school building at a particular time
#'
#' @export
make_schedule = function(time = 30, type = "base", total_days = 5, df){
  
  # basic time vector
  vec = data.frame(
    
    # time since start in days
    t = 1:time,
    
    # day of the week
    day = rep(c("M", "T", "W", "Th", "F", "Sa", "Su"), length.out = time),
    
    # group code
    group_two = rep(c(0,1,0,1,9,9,9), length.out = time),
    group_twov2 = rep(c(0,0,1,1,9,9,9), length.out = time),
    group_one = rep(c(0,1,2,3,9,9,9), length.out = time),
    group_week = rep(c(1,1,1,1,1,9,9,0,0,0,0,0,9,9), length.out = time),
    sched_type = type
    
  ) %>%
    
    mutate(
      
      # get rid of weekends
      present = !day %in% c("Sa", "Su"), # Sunday
      
      # ON / OFF
      present = ifelse(sched_type == "Remote", F, present),
      present = ifelse(sched_type == "On/off" & total_days < 5 & day %in% c("Th", "F"), F, present),
      present = ifelse(sched_type == "On/off" & total_days < 3 & day == "W", F, present),
      present = ifelse(sched_type == "On/off" & total_days < 2 & day == "T", F, present),
      present = ifelse(sched_type == "On/off" & total_days==5 & group_week!=1, F, present),
      
    )
  
  # replicate for each person
  vec_exp = vec %>% slice(rep(1:n(), times = max(df$id))) %>% mutate(id = rep(1:max(df$id), each = time))
  
  # time matrix
  d = df %>% select(id, adult, class, group, group_quarter, family) %>% left_join(vec_exp, "id") %>%
    # A/B
    mutate(present = ifelse(family, F, present),
           present = ifelse(sched_type == "A/B" & total_days == 1 & !adult & group_quarter != group_one, F, present),
           present = ifelse(sched_type == "A/B" & total_days == 2 & !adult & group != group_two, F, present),
           present = ifelse(sched_type == "A/B" & total_days == 2.2 & !adult & group != group_twov2, F, present),
           # drop Fridays for adults
           present = ifelse(sched_type == "A/B" & adult & day == "F", F, present),
           present = ifelse(sched_type == "A/B" & total_days == 4 & !adult & group != group_week, F, present))
  
  return(d)
}

#' Set household transmission
#'
#' Determine who is infected at a timestep
#' in the same household as an infected individual
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#'
#' @return infs id of infected individuals
#'
#' @export
run_household = function(a, df){
  
  # if there is more than one person in the household
  if(df$HH_id[df$id==a]>0 & sum(df$HH_id==df$HH_id[df$id==a])>1) {
    # identify HH members
    HH_vec = df[df$HH_id==df$HH_id[df$id==a] & df$id!=a,]
    
    # determine whether a HH member becomes infected
    prob_HH = rbinom(nrow(HH_vec), size = 1, prob = df$class_trans_prob[df$id==a]*df$relative_trans_HH[df$id==a]*HH_vec$susp*HH_vec$not_inf)
    HH = HH_vec$id
    
    # list infected individuals
    infs = HH*prob_HH
    
  }
  
  # otherwise
  else{infs = 0}
  
  #print(df$class_trans_prob[df$id==a]*df$relative_trans_HH[df$id==a]*HH_vec$susp*HH_vec$not_inf)
  return(infs)
}

#' Set class transmission
#'
#' Determine who is infected at a timestep
#' in the same classroom as an infected individual
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#'
#' @return infs id of infected individuals
#'
#' @export
run_class = function(a, df, high_school = F, hs.classes = NA){
  
  if(df$class[df$id==a]!=99 & !high_school) {
    # identify class members
    class_vec = df[df$class==df$class[df$id==a] & df$id!=a,]
    
    # determine whether a class member becomes infected
    prob_class = rbinom(nrow(class_vec), size = 1, prob = df$class_trans_prob[df$id==a]*class_vec$susp*class_vec$present_susp)
    class = class_vec$id
    
    # list infected individuals
    infs = class*prob_class
    #print(df$class_trans_prob[df$id==a]*class_vec$susp*class_vec$present_susp)
    
    
  } else if(df$class[df$id==a]!=99 & high_school){
    
    # pull class members
    hs.class.members = hs.classes$id[hs.classes$class%in%hs.classes$class[hs.classes$id==a]]
    
    # identify class members
    class_vec = df[df$id%in%hs.class.members & df$id!=a,]
    class_vec$count = sapply(class_vec$id, function(a) sum(hs.class.members==a))
    #class_vec$count = sapply(class_vec$id, function(a) 1)
    
    # determine whether a class member becomes infected
    prob_class = rbinom(nrow(class_vec), size = 1, prob = df$class_trans_prob[df$id==a]*df$relative_trans[df$id==a]*class_vec$susp*class_vec$present_susp*class_vec$count)
    class = class_vec$id
    
    # list infected individuals
    infs = class*prob_class
    
  }else{
    infs = 0
  }
  
  return(infs)
}

#' Set random transmission
#'
#' Determine who is infected at a timestep
#' from random contact with an infected individual
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#' @param random_contacts graph of random contacts at time t
#'
#' @return infs id of infected individuals
#'
#' @export
run_rand = function(a, df, random_contacts){
  
  # pull contacts from random graph
  id = which(df$id[df$present]==a)
  contact_id = df$id[df$present][random_contacts[[id]][[1]]]
  #print(length(contact_id))
  contacts = df[df$id %in% contact_id,]
  
  # determine whether a contact becomes infected
  prob_rand = rbinom(nrow(contacts), size = 1,
                     prob = df$class_trans_prob[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp)
  
  # infected individuals
  infs = contacts$id*prob_rand
  #print(a); print(infs)
  return(infs)
}

#' Set random staff transmission
#'
#' Determine who is infected at a timestep
#' from random contact between in-school adults
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#' @param random_contacts graph of random contacts at time t
#'
#' @return infs id of infected individuals
#'
#' @export
run_staff_rand = function(a, df, n_contact, rel_trans_adult = 2){
  
  if(n_contact>0){
    # pull contacts from random graph
    tot = length(df$id[df$present & df$adult & !df$family])
    contact_take = ifelse(n_contact<=tot, n_contact, tot)
    contact_id = sample(df$id[df$present & df$adult & !df$family], contact_take)
    contacts = df[df$id %in% contact_id & df$id!=a,]
    #print(dim(contacts))
    
    # determine whether a contact becomes infected
    prob_rand = rbinom(nrow(contacts), size = 1,
                       prob = df$class_trans_prob[df$id==a]*df$relative_trans[df$id==a]*contacts$susp*contacts$present_susp*rel_trans_adult)
    # infected individuals
    infs = contacts$id*prob_rand
    
    return(infs)
  }else return(0)
}

#' Set care-based transmission
#'
#' Determine who is infected at a timestep
#' from contact with an infected individual out of school
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#' @param contacts graph of random contacts at time t
#' @param num_adults number of adults interacting with children, defaults to 2
#' 
#' @return infs id of infected individuals
#'
#' @export
run_care = function(a, df, care_contacts, rel_trans_CC = 2, num_adults = 2){
  
  # pull contacts from random graph
  #id = which(unique(df$HH_id[!df$present & !df$family])==a)
  HHs = care_contacts$HH_id[care_contacts$cat==care_contacts$cat[care_contacts$HH_id==df$HH_id[df$id==a]]]
  
  # only have 2 adults at a time
  contacts = df[df$HH_id %in% HHs & !df$present,]
  adults = contacts$id[contacts$adult]
  if(num_adults < length(adults)) {keep = sample(adults, num_adults)
  }else{keep = adults}
  contacts = contacts[(!(contacts$adult) | contacts$id %in% keep) & (contacts$HH_id != df$HH_id[df$id==a]),]
  
  if(!df$adult[df$id==a] | a %in% keep){
    
    # determine whether a contact becomes infected
    prob_rand = rbinom(nrow(contacts), size = 1,
                       prob = df$class_trans_prob[df$id==a]*contacts$susp*round(contacts$not_inf)*rel_trans_CC)
    
    # infected individuals
    infs = contacts$id*prob_rand
  }else{infs = 0}
  
  return(infs)
}

#' Update quarantine
#'
#' Mark classes for quarantine based on current symptomatic infections
#' 
#' @param class_quarantine data frame of quarantine times
#' @param df.u data frame of infections whose classes should be quarantined
#' 
#' @return class_quarantine updated
#' 
#' @export
make_quarantine = function(class_quarantine, df.u, quarantine.length = 10, quarantine.grace = 3, hs = F, hs.classes = NA){
  
  if(!hs){
    # note classes to potentially be quarantined
    for(k in 1:nrow(class_quarantine)){
      hold = class_quarantine$hold[k] 
      class_quarantine$hold[k] = ifelse(class_quarantine$class_group[k]%in%paste(df.u$class, df.u$group) | paste(class_quarantine$class[k], "99")%in%paste(df.u$class, df.u$group),  
                                        max(df.u$t_notify[df.u$class==class_quarantine$class[k]]), class_quarantine$hold[k])
    }
    
    # quarantine if class has been back for more than grace period of days after previous quarantine
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + quarantine.length + quarantine.grace,
                                       class_quarantine$hold, class_quarantine$t_notify)
    class_quarantine$num = ifelse(class_quarantine$hold > class_quarantine$t_notify, class_quarantine$num + 1, class_quarantine$num)
  }else{ 
    
    hs.classes2 = hs.classes %>% inner_join(df.u %>% select(id, t_notify), c("id" = "id"))
    for(k in 1:nrow(class_quarantine)){
      hold = class_quarantine$hold[k] 
      #print(hs.classes$t_notify[hs.classes$class==class_quarantine$class[k]])
      
      class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%hs.classes$class[hs.classes$id%in%df.u$id],
                                        max(hs.classes2$t_notify[hs.classes2$class==class_quarantine$class[k]]), class_quarantine$hold[k])
    }
    
    # quarantine if class has been back for more than grace period of days after previous quarantine
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + quarantine.length + quarantine.grace,
                                       class_quarantine$hold, class_quarantine$t_notify)
  }
  return(class_quarantine)
}

#' Set infection parameters
#'
#' Set infection parameters for individuals infected at a particular timestep
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#' @param set indication of seeding model vs. creating infections
#' @param mult_asymp multiplier on asymptomatic infection for adults; default is 1
#' @param mult_asymp_child multiplier on asymptomatic infection for children; default is 1
#' @param seed_asymp when making a seed, force to be asymptomatic; default is false
#' @param turnaround.time test turnaround time, default = 1 day
#' @param overdisp_off all overdispersion off; defaults to F
#'
#' @return df.u with updated parameters
#'
#' @export
# note to self -- add additional parameters to change around here
make_infected = function(df.u, days_inf, set = NA, mult_asymp = 1, mult_asymp_child = 1, seed_asymp = F, turnaround.time = 1, overdisp_off = F){
  
  if(is.na(set)){
    #  set infectivity  parameters
    df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
    df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    df.u$t_symp = df.u$t_exposed + rgamma(nrow(df.u), shape = 5.8, scale=.95)
    val = rnorm(nrow(df.u), mean = 2, sd = .4)
    df.u$t_inf = ifelse(df.u$t_symp - val > df.u$t_exposed + 1, df.u$t_symp - val, df.u$t_exposed + 1)
  } else{
    #  set infectivity  parameters
    if(seed_asymp) {
      df.u$symp = 0
      df.u$sub_clin = 0
    }else{
      df.u$symp = rbinom(nrow(df.u), size = 1, prob = 1-df.u$p_asymp)
      df.u$sub_clin = ifelse(df.u$symp, rbinom(nrow(df.u), size = 1, prob =  df.u$p_subclin/(1-df.u$p_asymp)), 1)
    }
    df.u$t_inf = set + runif(nrow(df.u), min = -0.5, max = 0.5)
    df.u$t_symp = df.u$t_inf + rnorm(nrow(df.u), mean = 2, sd = .4)
    df.u$t_exposed = df.u$t_symp - rgamma(nrow(df.u), shape = 5.8, scale=.95)
  }
  
  # add overdispersion
  attack_mult = rlnorm(nrow(df.u), meanlog = log(.84)-log((.84^2+.3)/.84^2)/2, sdlog = sqrt(log((.84^2+.3)/.84^2)))/.84
  chk = (df.u$super_spread | df.u$adult)*as.numeric(!overdisp_off)
  df.u$class_trans_prob = ifelse(chk, df.u$class_trans_prob*attack_mult, df.u$class_trans_prob)
  
  # adjust for asymptomatic infection if applicable
  df.u$class_trans_prob = ifelse(!df.u$symp, ifelse(df.u$adult, df.u$class_trans_prob*mult_asymp, df.u$class_trans_prob*mult_asymp_child), df.u$class_trans_prob)
  df.u$relative_trans_HH = ifelse(df.u$symp, df.u$relative_trans_HH*df.u$relative_trans_HH_symp_child, df.u$relative_trans_HH)
  
  # add end time
  df.u$t_end_inf_home = df.u$t_inf +
    rlnorm(nrow(df.u), meanlog = log(days_inf)-log((days_inf^2 + 2)/days_inf^2)/2, sdlog = sqrt(log((days_inf^2 + 2)/days_inf^2)))
  df.u$t_end_inf = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$t_symp<df.u$t_end_inf_home, df.u$t_symp, df.u$t_end_inf_home) 
  df.u$t_notify = ifelse(df.u$symp==1 & !df.u$sub_clin & df.u$isolate & df.u$notify, df.u$t_symp + turnaround.time, -17)
  
  return(df.u)
}

#' Set specials transmission
#'
#' Determine who is infected at a timestep
#' from specials
#'
#' @param a id of infected individual
#' @param df school data frame from make_school()
#' @param specials classroom and teacher ids of specials at time t
#'
#' @return infs id of infected individuals
#'
#' @export
run_specials = function(a, df, specials){
  
  if(!df$adult[df$id==a] & df$class[df$id==a]%in%specials$class){
    
    # pull teachers
    specials_vec = df[df$id%in%specials$teacher[specials$class==df$class[df$id==a]],]
    
    # determine whether teacher becomes infected
    prob_specials = rbinom(nrow(specials_vec), size = 1, prob = df$class_trans_prob[df$id==a]*df$relative_trans[df$id==a]*specials_vec$susp*specials_vec$present_susp)
    infs = specials_vec$id*prob_specials
    
  }else if(df$specials[df$id==a]){
    specials_class_vec = df[df$class%in%specials$class[specials$teacher==a],]
    
    # determine whether a class member becomes infected
    prob_specials = rbinom(nrow(specials_class_vec), size = 1,
                           prob = df$class_trans_prob[df$id==a]*df$relative_trans[df$id==a]*specials_class_vec$susp*specials_class_vec$present_susp)
    specials_vec = specials_class_vec$id
    infs = specials_vec*prob_specials
    
  } else{infs = 0}
  
  return(infs)
}

#' Run model
#'
#' Perform a single model run
#'
#' @param df school data frame used in run_model setup
#' @param nper number of school periods; defaults to 8
#' 
#' @export
make_hs_classes = function(df, nper){
  
  hs.classes = data.frame(period = numeric(), class = numeric(), id = numeric())
  
  for(p in 1:nper){
    m = max(df$class[df$class < 99])
    temp = data.frame(period = p, id = df$id[df$class!=99], 
                      age = df$age[df$class!=99],
                      class = df$class[df$class!=99]) %>%
      group_by(age) %>%
      mutate(class = ifelse(age>0, sample(class), class),
             class_old = class,
             class = (period-1)*m + class)
    hs.classes = hs.classes %>% bind_rows(temp)
  }
  
  return(hs.classes)
}


#' Run model
#'
#' Perform a single model run
#'
#' @param time length of time to run model; defaults to 30
#' @param notify whether classrooms are notified and quarantined; defaults to F
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of school tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "staff" and "students"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday
#' @param n_staff_contact number of contacts a teacher/staff member has with other teachers/staff members; defaults to 1
#' @param n_HH number of households a household interacts with when not attending school; defaults to 0
#' @param bubble whether out-of-school interactions occur with a 'bubble'; defaults to F
#' @param n_start number of infections to seed model; defaults to 1
#' @param mult_asymp multiplier on asymptomatic infection for adults; default is 1
#' @param mult_asymp_child multiplier on asymptomatic infection for children; default is 1
#' @param days_inf length of infectious period (assuming mild case or quarantined on symptoms)
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param high_school whether to use a high school schedule of random period mixing; defaults to F
#' @param nper number of school periods; defaults to 8
#' @param start_type type of seed; default is "mix" (also "adult", "child", "cont")
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 10
#' @param quarantine.grace length of grace period after which a quarantined class returns not to be "re-quarantined"
#' @param start_mult value to indicate relative frequency of adult/child infections; defaults to 1 (adults 2x as likely as kids)
#' @param num_adults number of adults interacting with children, defaults to 2
#' @param include_weekends if TRUE excludes weekends from additional out-of-school mixing, defaults to F
#' @param turnaround.time test turnaround time, default = 1 day
#' @param child_prob if start_type = "cont", set daily probability of infectious entry for children, defaults to .05
#' @param adult_prob if start_type = "cont", set daily probability of infectious entry for adults, defaults to .01
#' @param type "base", "On/off", "A/B", "Remote"; defaults to "base"
#' @param rel_trans_CC relative transmission in childcare vs. classroom; defaults to 2
#' @param rel_trans_adult relative transmission in staff-staff interactions vs. classroom; defaults to 2
#' @param test_quarantine whether quarantined individuals attend school but are tested daily; defaults to FALSE
#' @param surveillance whether surveillance is underway; defaults to F
#' @param rapid_test_sens sensitivity of rapid tests, defaults to 80%
#' @param overdisp_off all overdispersion off; defaults to F
#' @param version v1 quarantines full cohort in A/B; v2 only sub-cohort; defaults to 2
#' @param df school data frame from make_school()
#' @param sched schedule data frame from make_schedule()
#'
#' @return df updated df with transmission results
#' @return time_seed_inf when the first individual was dropped in
#' @return class_quarantine a matrix of class quarantine times
#' @return mat a check on if the people who you think are present are actually the ones present
#'
#' @export
#### NOTE: I found this to be slower when coded w/tidyverse.
#### Therefore for the most part, this is coded in base.
run_model = function(time = 30,
                     notify = F,
                     test = F,
                     test_days = "week",
                     test_sens =  .7,
                     test_frac = .9,
                     test_start_day = 1,
                     n_staff_contact = 0,
                     n_HH = 0,
                     n_start = 1,
                     days_inf = 6,
                     mult_asymp = 1,
                     mult_asymp_child = 1,
                     seed_asymp = F,
                     time_seed_inf = NA,
                     high_school = F,
                     nper = 8,
                     start_mult = 1,
                     start_type = "mix",
                     test_type = "all",
                     adult_prob = 0.013,
                     child_prob = 0.056,
                     quarantine.length = 10,
                     quarantine.grace = 3,
                     rel_trans_CC = 2, rel_trans_adult = 2,
                     num_adults = 2,
                     bubble = F,
                     include_weekends = T,
                     turnaround.time = 1,
                     type = "base",
                     version = 2,
                     test_quarantine = F,
                     surveillance = F,
                     rapid_test_sens = 0.8,
                     overdisp_off = F,
                     df, sched){
  
  #### SEED MODEL ####
  # seed with an infectious case
  if(is.na(time_seed_inf)) time_seed_inf = sample(1:14, 1)     # any day in the cycle
  
  # any individual not family member
  # note adults 2x as likely as kids to be infected
  if(start_type=="mix") id.samp = sample(df$id[!df$family], n_start, prob = (df$adult[!df$family]*start_mult+1)/(sum(df$adult[!df$family]*(start_mult+1)) + sum(!df$adult)))                
  
  # specific types
  if(start_type=="adult") id.samp = sample(df$id[!df$family & df$adult], n_start)       
  if(start_type=="teacher") id.samp = sample(df$id[!df$family & df$adult & df$class!=99], n_start)               
  if(start_type=="child") id.samp = sample(df$id[!df$family & !df$adult], n_start) 
  if(start_type=="family") id.samp = sample(df$id[df$family], n_start) 
  if(start_type=="special") id.samp = sample(df$id[df$specials], n_start) 
  
  # set up scheduling if high school
  hs.classes = NA
  if(high_school){
    hs.classes = make_hs_classes(df = df, nper = nper)    
    classes.ind = sapply(df$id, function(a) hs.classes$class[hs.classes$id == a])
  }
  
  # quarantine
  if(version == 1 | type=="base") df$group[df$group!=99] = 0  # make sure quarantine doesn't go by group
  if(!high_school){class_quarantine = expand_grid(class = unique(df$class[df$class!=99]), group = unique(df$group[df$group
                                                                                                                  !=99])) %>%
    mutate(class_group = paste(class, group), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length, num = 0)
  }else{class_quarantine = data.frame(class = unique(hs.classes$class), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length)}
  mat = matrix(NA, nrow = max(df$id), ncol = time)
  
  # vary over time
  if(start_type == "cont"){
    
    # pull out_IDs
    adult_IDs = df$id[df$adult] 
    child_IDs = df$id[!df$adult]
    
    # pick times
    vec = 1:(time+15)
    adult_pulls = rbinom(time+15, size = length(adult_IDs), prob = adult_prob)
    adult_times = rep(vec, adult_pulls)
    
    child_pulls = rbinom(time+15, size = length(child_IDs), prob = child_prob)
    child_times = rep(vec, child_pulls)
    
    # pick people
    adults = sample(adult_IDs, length(adult_times)) 
    kids = sample(child_IDs, length(child_times))
    
    # set up vectors
    time_seed_inf = c(adult_times, child_times)
    
    id.samp = c(adults, kids)
    df.temp = data.frame(id.samp, time_seed_inf) %>% arrange(id.samp) %>%
      left_join(df, c("id.samp" = "id")) %>% filter(susp!=0)
    time_seed_inf = 15 # start on Monday with testing
    
    
  }else{
    
    # compress if time_seed_inf is a vector
    df.temp = data.frame(time_seed_inf, id.samp) # backward compatibility
  }
  
  df$start.time = time_seed_inf
  
  # setup
  if(nrow(df.temp)>0){
    df[df$id%in%df.temp$id.samp,] = make_infected(df.u = df[df$id%in%df.temp$id.samp,], days_inf = days_inf,
                                                  set = df.temp$time_seed_inf, seed_asymp = seed_asymp,
                                                  mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child,
                                                  turnaround.time = turnaround.time, overdisp_off = overdisp_off)
    df$start = df$id %in% df.temp$id.samp
    df$start.init = df$id %in% df.temp$id.samp
    
  }
  
  # test days
  # if null, make this Monday
  if(test_days == "week") {testing_days = seq(test_start_day, (time+15), by = 7)}
  if(test_days == "day") {testing_days = 1:(time+15)}
  if(test_days == "2x_week"){
    if(turnaround.time>1){
      testing_days = c(seq(5, (time+15), by = 7), seq(1, (time+15), by = 7))
    } else{
      testing_days = c(seq(4, (time+15), by = 7), seq(1, (time+15), by = 7))
    }}
  df$switch = 0
  df$temp_switch = 0
  df$specials_count = 0
  
  #print(testing_days)
  
  # testing
  if(test_type=="all"){ df$test_type = !df$family & df$inc_test & (!df$vacc | test_frac>=0.7)
  } else if(test_type=="staff"){df$test_type = df$adult & !df$family
  } else if(test_type=="students"){df$test_type = !df$adult}
  class_test_ind = 0
  class_test_ind_q = 0
  test_frac_orig = test_frac
  df$uh.oh = 0
  #df$inf_days = 0
  #df$inf_home_days = 0
  #df$symp_days = 0
  #df$symp_and_inf_days = 0
  #df$last = 0
  
  #print(paste("start notification:", df$t_notify[df$start]))
  # run over time steps
  for(t in time_seed_inf:(time_seed_inf+time-1)){
    #print(paste("Time:", t, sched$day[sched$t==t][1], sched$group_two[sched$t==t][1]))
    
    # class quarantines
    classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + quarantine.length-1),]
    df$present = sched$present[sched$t==t]
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
    df$symp_now = !df$family & !is.na(df$symp) & df$symp==1 & !df$sub_clin & df$t_inf <= t & df$t_end_inf >= t
    #print(paste("Time:", t, sched$day[sched$t==t][1]))
    #print(classes_out)
    df$flag = (paste(df$class, df$group))%in%classes_out$class_group | (df$class%in%classes_out$class & df$group==99)
    
    # present
    if(test_quarantine==T) {
      df$test_type_q = !df$family & df$inc_test & (df$flag | df$symp_now)
      if(t == 1) df$test_q_keep = df$test_type_q
      #print("got to test_q"); print(dim(df)); print(df %>% group_by(vacc) %>% summarize(sum(test_type_q)))
      df$test_ct_q = df$test_ct_q + df$present*as.numeric(df$test_type_q)
      df$test_q = rbinom(nrow(df), size = 1, prob = rapid_test_sens*df$present*df$test_type_q)
      df$t_end_inf = ifelse(df$inf & df$test_q & df$present, t, df$t_end_inf)
      df$t_notify = ifelse(df$inf & df$test_q & df$present, t+1, df$t_notify)
      df$detected = ifelse(df$inf & df$test_q & df$present, 1, df$detected)
      df$detected_q = ifelse(df$inf & df$test_q & df$present, 1, df$detected_q)
      df$detected_q_start = ifelse(df$inf & df$test_q & df$present & df$start, 1, df$detected_q_start)
      class_test_ind_q = class_test_ind_q + length(unique(df$class[df$test_type_q & df$present & !df$quarantined]))
      df$q_out = df$detected & df$inf
    }else{df$q_out = !df$vacc & df$flag}
    
    # re-estimated who is present
    df$present = sched$present[sched$t==t] & !df$q_out & !df$HH_id%in%df$HH_id[df$q_out]
    df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t & !df$start
    df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
    
    # checks
    #df$inf_days = df$inf_days + df$inf
    #df$inf_home_days = df$inf_home_days + df$inf_home
    #df$symp_days = df$symp_days + ifelse(df$symp_now==1 & !is.na(df$symp_now), 1, 0)
    #df$symp_and_inf_days = df$symp_and_inf_days + df$symp_now*df$inf
    #df$last = ifelse(df$inf, t, df$last)
    
    if(high_school & nrow(classes_out)>0){
      df$nq = !unlist(lapply(classes.ind, function(a) sum(a %in% classes_out$class)>0))
      df$present =  sched$present[sched$t==t] & df$nq
    }
    df$not_inf = df$t_exposed==-99 | df$t_exposed>t # if exposed from community, can be exposed earlier
    if(t==1) df$not_inf_keep = df$not_inf
    df$present_susp = df$present & df$not_inf
    df$quarantined = df$quarantined + as.numeric(df$q_out & sched$present[sched$t==t])
    #print(sum(as.numeric(df$q_out)))
    #print(sum(as.numeric(df$q_out & sched$present[sched$t==t])))
    
    df$quarantined2 = df$quarantined2 + as.numeric(df$q_out)
    df$quarantined_now = df$q_out & sched$present[sched$t==t]
    
    # check who is present
    mat[,(t-time_seed_inf+1)] = df$present
    
    # infectious and at school
    df$trans_now = df$present & df$inf & !df$family
    df$trans_outside = !df$present & df$inf & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)#& !df$family
    df$mix_outside = !df$present & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)#& !df$family
    if(high_school){
      df$trans_outside = !df$present & df$inf & (!df$adult | df$family)
      if(nrow(classes_out)>0) df$trans_outside = df$trans_outside & df$nq
    }
    #if(nrow(classes_out)>0){
    #print("Quarantined classes:")
    #print(classes_out)
    #}
    
    #if(sum(df$trans_now>0)){
    #print("Currently infectious, in school:")
    #print(df %>% filter(trans_now) %>% select(id, HH_id, class, group, adult, family, symp))
    #print(paste("In attendance: ", sum(df$quarantined_now & df$trans_now)))
    #}
    
    #if(sum(df$trans_outside>0)){
    #print("Currently infectious, outside of school:")
    #print(df %>% filter(trans_outside) %>% select(id, HH_id, class, group, adult, family, symp))}
    
    # set infections to 0 for this timestep
    df$now = F
    
    ## group testing
    if(test & t%in%testing_days){
      #print(test); print(t); print(testing_days)
      #print(t)
      #print("got to testing"); print(dim(df)); print(df %>% group_by(vacc) %>% summarize(sum(test_type)))
      df$test_ct = df$test_ct + rbinom(nrow(df), size = 1, prob = df$present*test_frac*as.numeric(df$test_type))
      df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
      df$t_end_inf = ifelse(df$inf & df$test & df$present, t, df$t_end_inf)
      df$t_notify = ifelse(df$inf & df$test & df$present, t+1, df$t_notify)
      df$detected = ifelse(df$inf & df$test & df$present, 1, df$detected)
      class_test_ind = class_test_ind + length(unique(df$class[df$test_type & df$present & !df$quarantined]))
      
      #print(paste("Time:", t))
      #print(sum(df$test))
      #print(sum(df$inf & df$test & df$present))
      #print(df$id[df$inf & df$test & df$present])
      #print(df$class[df$inf & df$test & df$present])
      #print(df$family[df$inf & df$test & df$present])
      
      if(surveillance==T & sum(df$inf & df$test & df$present)>=1){
        #print("switch to reg")
        test_frac = 0.9
        testing_days = c(testing_days, t+1)
        surveillance = F
        df$switch = t
      }
      
      if(surveillance==F & sum(df$inf & df$test & df$present)==0 & (t-1)%in%testing_days){
        #print("switch to surv")
        test_frac = test_frac_orig
        surveillance = T
        df$switch = 0
        df$temp_switch = df$temp_switch + 1
      }
      
      #print(surveillance)
      #print(df$switch[1])
      #print(testing_days)
      #print(test_frac)
      # set up notification -- now below
      #df.u = df %>% filter(inf & test)
      #if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)}
      
    }
    
    #### SELECT NEXT GENERATION INFECTIONS ####
    # run model for infectious individuals at home
    if(sum(df$inf_home)>0) {
      
      home_infs = df$id[df$inf_home]
      if(sum(df$inf_home > 1)) home_infs = sample(home_infs)
      
      for(a in home_infs){
        
        # HOUSEHOLD CONTACTS
        inf_vec = run_household(a, df)
        #print(paste("Household:", inf_vec))
        df$location[df$id%in%inf_vec] = "Household"
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] #+ sum(inf_vec>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
      }
    }
    
    # run model for infectious individuals at school
    if(sum(df$trans_now)>0) {
      
      # RANDOM CONTACT STRUCTURE
      # sample from a random regular graph
      # this approach ensures reciprocity
      # you may want to split out to ensure reciprocity in contact type
      random_contacts = sample_k_regular(sum(df$present), df$n_contact[1] + df$n_contact_brief[1])
      #if(n_staff_contact>0) random_staff_contacts = sample_k_regular(sum(df$present & df$adult & !df$family), n_staff_contact)
      
      # SPECIAL SUBJECTS
      specials = data.frame(teacher = rep(df$id[df$specials], each = 4), class = sample(1:max(df$class[!is.na(df$class) & df$class<99]), 4*sum(df$specials), replace = T))
      #specials2 = specials %>% group_by(teacher) %>% summarize(class = unique(class)) %>% ungroup()
      #df$specials_count = df$specials_count + sapply(df$class, function(a) sum(specials2$class==a))
      
      # run transmission in schools
      school_infs = df$id[df$trans_now]
      if(sum(df$trans_now > 1)) school_infs = sample(school_infs)
      
      # choose contacts that become infected
      for(a in school_infs){
        
        # CLASS CONTACTS
        class_trans = run_class(a, df, high_school = high_school, hs.classes = hs.classes)
        #class_trans = 0
        df$location[df$id%in%class_trans] = "Class"
        
        # RANDOM CONTACTS
        rand_trans = tryCatch({run_rand(a, df, random_contacts)}, error = function(err) {0})
        #rand_trans = 0 
        df$location[df$id%in%rand_trans] = "Random contacts"
        
        # RANDOM ADULT CONTACTS
        # note this doesn't strictly keep reciprocity
        # but k-regular graphs can be finnicky at low ##s
        # and this captures the general pattern
        if(df$adult[df$id==a] & !df$family[df$id==a]){
          rand_staff_trans = run_staff_rand(a, df, n_staff_contact, rel_trans_adult)
        }else{rand_staff_trans = 0}
        df$location[df$id%in%rand_staff_trans] = "Staff contacts"
        
        # SPECIALS CONTACTS
        specials_trans = run_specials(a, df, specials)
        #specials_trans = 0
        df$location[df$id%in%specials_trans] = "Related arts"
        
        # return id if person is infected
        # and 0 otherwise
        inf_vec = c(class_trans, rand_trans, rand_staff_trans, specials_trans)
        #print(paste("School:", inf_vec))
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(unique(inf_vec)>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
        
      }
    }
    
    # run model for infectious individuals OUTSIDE school
    HHs = unique(df$HH_id[!df$adult & df$mix_outside])
    len = length(HHs)
    if(sum(df$trans_outside)>0 & n_HH>0 & len > 1 &
       (include_weekends | !sched$day[sched$t==t][1]%in%c("Sa", "Su"))) {
      
      #print(sched$day[sched$t==t][1])
      if(!bubble){
        
        # how many households are around
        tot = ceiling(len/n_HH)
        
        # care contacts
        care_contacts = data.frame(HH_id = HHs,
                                   cat = sample(rep(1:tot, each = n_HH)[1:len]))
      }
      
      # run transmission in care groups
      non_school_infs = df$id[df$trans_outside]
      if(sum(df$trans_outside > 1)) non_school_infs = sample(non_school_infs)
      
      # choose contacts that become infected
      for(a in non_school_infs){
        
        # CARE CONTACTS
        care_trans = run_care(a, df, care_contacts, rel_trans_CC, num_adults = num_adults)
        df$location[df$id%in%care_trans] = "Child care"
        
        # return id if person is infected
        # and 0 otherwise
        inf_vec = c(care_trans)
        #print(paste("Care:", inf_vec))
        
        # add to total # of infections from this person
        df$tot_inf[df$id==a] = df$tot_inf[df$id==a] #+ sum(inf_vec>0)
        
        # flag people infected at this time step
        df$now = ifelse(df$id%in%inf_vec, T, df$now)
        df$source = ifelse(df$id%in%inf_vec, a, df$source)
        df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
        df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
        df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
        
      }
    }
    
    #### SET UP NEXT GENERATION INFECTEDS ####
    # update values updated at this stage
    # need to put in probability distributions
    if(sum(df$now>0)){
      
      df$start[df$now] = 0      # remove seed if infected earlier
      df$t_exposed[df$now] = t
      df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child, turnaround.time = turnaround.time, overdisp_off = overdisp_off)
      #print(paste0("New exposures:"))
      #print(table(df$location[df$now]))
      #print(df %>% filter(now) %>% arrange(source) %>% select(id, HH_id, class, group, adult, family, source, location, symp))
    }
    
    # round values
    df$t_notify = ceiling(df$t_notify)
    
    if(notify & sum(df$t_notify==(t+1) & !df$family)>0){
      df.u = df %>% filter(t_notify==(t+1) & !family)
      #print("Quarantined: "); print(df.u %>% dplyr::select(adult, family, class, symp, sub_clin, t_notify, start))
      
      # set up notification
      class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
      df$uh.oh = df$uh.oh + sum(df$source[df$now]%in%(df$id[df$class%in%classes_out$class]) & df$location[df$now]!="Household")>0
      
    }
    #print(t); print(class_quarantine)
    #print(df %>% #filter(!adult) %>% 
    #        group_by(class) %>% summarize(mean(quarantined), sum(quarantined)))
    #if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)
    
  }
  # remember to add mat back in
  #print(df$id[df$t_exposed!=-99 & df$class==df$class[df$start]])
  #print(sum(df$t_exposed!=-99))
  df$class_test_ind = class_test_ind
  df$class_test_ind_q = class_test_ind_q
  df$surveillance = surveillance
  #print(sum(class_quarantine$t_notify>-1))#; print(tail(class_quarantine))
  return(df) #, time_seed_inf, class_quarantine, mat))
}

#' Run model multiple times and summarize results
#'
#' @param N number of runs
#' @param n_other_adults Number of adults in the school other than primary teachers; defaults to 30
#' @param n_contacts Number of sustained contacts outside of the classroom; defaults to 10
#' @param n_contacts_brief Number of brief contacts outside of the classroom; defaults to 20
#' @param rel_trans_HH Relative attack rate of household contact (vs. classrom); defaults to 1
#' @param rel_trans_HH_symp_child Additional relative attack rate of a symptomatic infected child in the household; defaults to 1
#' @param rel_trans Relative attack rate of sustained contact (vs. classroom); defaults to 1/8
#' @param rel_trans_brief Relative attack rate of brief contact (vs. classroom); defaults to 1/50
#' @param rel_trans_CC relative transmission in childcare vs. classroom; defaults to 2
#' @param rel_trans_adult relative transmission in staff-staff interactions vs. classroom; defaults to 2
#' @param p_asymp_adult Fraction of adults with asymptomatic (unsuspected) disease; defaults to 0.2
#' @param p_asymp_child Fraction of children with asymptomatic (unsuspected) disease; defaults to 0.8
#' @param p_subclin_adult Fraction of adults with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param p_subclin_child Fraction of children with subclinical but not techincally asymptomatic disease; defaults to 0
#' @param attack Average daily attack rate in adults; defaults to 0.01
#' @param child_trans Relative transmissibility of children (vs. adults); defaults to 1
#' @param child_susp Relative transmissibility of children (vs. adults); defaults to .5
#' @param child_vax Vaccination rate of children; defaults to 0
#' @param teacher_trans Factor by which teacher transmissibility is reduced due to intervention; defaults to 1
#' @param teacher_susp Teacher vaccination rate; defaults to 0.8
#' @param family_susp Household member vaccination rate; defaults to 0.7
#' @param disperse_transmission Whether transmission is overdispersed (vs. all have equal attack rate); default to T
#' @param n_staff_contact number of contacts a teacher/staff member has with other teachers/staff members; defaults to 1
#' @param n_HH number of households a household interacts with when not attending school; defaults to 0
#' @param bubble whether out-of-school interactions occur with a 'bubble'; defaults to F
#' @param n_start number of infections to seed model; defaults to 1
#' @param time_seed_inf time(s) at which to introduce new infectious individuals; defaults to NA and randomly selects one time
#' @param days_inf length of infectious period (assuming mild case or quarantined on symptoms)
#' @param mult_asymp multiplier on asymptomatic infection for adults; default is 1
#' @param mult_asymp_child multiplier on asymptomatic infection for children; default is 1
#' @param seed_asymp whether to seed with an asymptomatic case
#' @param isolate Whether symptomatic individuals isolate when symptoms emerge; defaults to T
#' @param dedens Whether dedensification measures reduce attack rate; defaults to F
#' @param run_specials_now Whether special subjects are run; defaults to F
#' @param time length of time to run model; defaults to 30
#' @param notify whether classrooms are notified and quarantined; defaults to F
#' @param test whether there is weekly testing; defaults to F
#' @param test_sens test sensitivity; defaults to 0.7
#' @param test_frac fraction of school tested; defaults to 0.9
#' @param test_days test frequency; "day", "week", "2x_week"; defaults to "week"
#' @param test_type group tested; defaults to "all", also allows "staff" and "students"
#' @param test_start_day day tests are implemented for weekly testing; defaults to 1 = Monday
#' @param n_class number of classes per grade
#' @param high_school whether to use a high school schedule of random period mixing; defaults to F
#' @param nper number of school periods; defaults to 8
#' @param start_mult value to indicate relative frequency of adult/child infections; defaults to 1 (adults 2x as likely as kids)
#' @param start_type type of seed; default is "mix" (also "adult", "child")
#' @param child_prob if start_type = "cont", set daily probability of infectious entry for children, defaults to .05
#' @param adult_prob if start_type = "cont", set daily probability of infectious entry for adults, defaults to .01
#' @param quarantine.length length of quarantine when someone is infectious; defaults to 10
#' @param quarantine.grace length of grace period after which a quarantined class returns not to be "re-quarantined"
#' @param turnaround.time test turnaround time, default = 3 days
#' @param type schedule; "base", "On/off", "A/B", "Remote"; defaults to "base"
#' @param version v1 quarantines full cohort in A/B; v2 only sub-cohort; defaults to 2
#' @param total_days number of days in school; defaults to 5
#' @param num_adults number of adults interacting with children, defaults to 2
#' @param includeFamily whether to include family, default = FALSE
#' @param include_weekends if TRUE excludes weekends from additional out-of-school mixing, defaults to F
#' @param vax_eff Vaccine efficacy, defaults to 0.9
#' @param test_quarantine whether quarantined individuals attend school but are tested daily; defaults to FALSE
#' @param surveillance whether surveillance is underway; defaults to F
#' @param rapid_test_sens sensitivity of rapid tests, defaults to 80%
#' @param overdisp_off all overdispersion off; defaults to F
#' @param no_test_vacc Indicates whether vaccinated individuals are excluded from TTS & screening; defaults to F
#' @param synthpop synthetic population; defaults to synthpop based on Maryland elementary school
#' @param class make_school object; defaults to NA and will call for each simulation
#' 
#' @export
mult_runs = function(N, n_other_adults, n_contacts, rel_trans_HH,
                     rel_trans, rel_trans_CC, rel_trans_adult, p_asymp_adult, child_prob, adult_prob,
                     p_asymp_child, attack, child_trans, child_susp, child_vax, p_subclin_adult, p_subclin_child,
                     teacher_susp, disperse_transmission, n_staff_contact, n_HH, num_adults, family_susp,
                     n_start, time_seed_inf, days_inf, mult_asymp, mult_asymp_child, seed_asymp, isolate, dedens, run_specials_now,
                     time, notify, test, test_sens, test_frac, test_days, test_type, quarantine.length, quarantine.grace,
                     type, total_days, includeFamily, synthpop, class, n_class, high_school, nper, start_mult, start_type,
                     include_weekends, turnaround.time, test_start_day, test_quarantine, vax_eff, surveillance, 
                     rapid_test_sens, overdisp_off, no_test_vacc, rel_trans_HH_symp_child, teacher_trans,
                     n_contacts_brief = 0, rel_trans_brief = 1/50, bubble = F, version = 2){
  
  keep = data.frame(all = numeric(N), tot = numeric(N), R0 = numeric(N), Rt = numeric(N), start = numeric(N), start_adult = numeric(N), asymp_kids = numeric(N),
                    source_asymp = numeric(N), source_asymp_family_kids = numeric(N), source_asymp_family_staff = numeric(N), start_family = numeric(N),
                    adult = numeric(N), teacher = numeric(N), family = numeric(N), staff_family = numeric(N), children = numeric(N), children_tot = numeric(N), family_tot = numeric(N),
                    adult_tot = numeric(N), attack = numeric(N), class = numeric(N), household = numeric(N), detected = numeric(N),
                    detected_staff = numeric(N), detected_students = numeric(N), detected_staff_subclin = numeric(N), detected_students_subclin = numeric(N),
                    symp = numeric(N), symp_kids = numeric(N), avg_infs = numeric(N), class_test_ind = numeric(N), test_qs = numeric(N),
                    quarantine_check = numeric(N), quarantined = numeric(N), quarantined_tot = numeric(N), quarantined_kids = numeric(N), from_kids = numeric(N), related_arts = numeric(N), 
                    child_care = numeric(N), random = numeric(N), random_staff = numeric(N), num_classroom = numeric(N), avg_class = numeric(N), clin_staff = numeric(N), clin_students = numeric(N), clin_family = numeric(N))
  
  #tic()
  # run over time
  for(i in 1:N){
    
    ## make class
    if(is.na(class)){
      
      class = make_school(synthpop = synthpop, n_other_adults = n_other_adults, includeFamily = includeFamily, n_class = n_class)
      
    }
    ## make school
    school = initialize_school(n_contacts = n_contacts, n_contacts_brief = n_contacts_brief, rel_trans_HH = rel_trans_HH,
                               rel_trans = rel_trans, rel_trans_brief = rel_trans_brief, p_asymp_adult = p_asymp_adult,
                               p_asymp_child = p_asymp_child, p_subclin_adult = p_subclin_adult, p_subclin_child = p_subclin_child,
                               attack = attack, child_trans = child_trans, child_susp = child_susp, child_vax = child_vax, family_susp = family_susp,
                               teacher_trans = teacher_trans, teacher_susp = teacher_susp, disperse_transmission = disperse_transmission,
                               isolate = isolate, dedens = dedens, run_specials = run_specials_now, start = class, vax_eff = vax_eff, notify = notify, 
                               no_test_vacc = no_test_vacc, rel_trans_HH_symp_child = rel_trans_HH_symp_child)
    
    ## make schedule
    sched = make_schedule(time = time + 15, df = school, type = type, total_days = total_days)
    
    ## run model
    df = run_model(time = time, notify = notify, test = test, df = school, sched = sched, 
                   test_sens = test_sens, test_frac = test_frac, test_days = test_days, days_inf = days_inf,
                   mult_asymp = mult_asymp, mult_asymp_child = mult_asymp_child, seed_asymp = seed_asymp, n_HH = n_HH, n_staff_contact = n_staff_contact,
                   n_start = n_start, time_seed_inf = time_seed_inf, high_school = high_school, nper = nper, 
                   start_mult = start_mult, start_type = start_type, child_prob = child_prob, adult_prob = adult_prob, test_type = test_type,
                   rel_trans_CC = rel_trans_CC, rel_trans_adult = rel_trans_adult, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, 
                   num_adults = num_adults, bubble = bubble, include_weekends = include_weekends, turnaround.time = turnaround.time,
                   test_start_day = test_start_day, type = type, test_quarantine = test_quarantine, version = version, surveillance = surveillance,
                   rapid_test_sens = rapid_test_sens, overdisp_off = overdisp_off)
    
    time_keep = df$start.time[1]
    #print(time_keep)
    #print(length(time_keep:(time_keep+time-1)))
    
    # store output
    keep$all[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1)
    keep$tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    
    keep$all_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & df$t_end_inf_home >= 15)
    keep$tot_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 15 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 15)
    keep$detected_15_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==15)
    
    keep$all_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & df$t_end_inf_home >= 22)
    keep$tot_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 22 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 22)
    keep$detected_22_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==22)
    
    keep$all_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & df$t_end_inf_home >= 29)
    keep$tot_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 29 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 29)
    keep$detected_29_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==29)
    
    keep$all_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & df$t_end_inf_home >= 36)
    keep$tot_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 36 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 36)
    keep$detected_36_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==36)
    
    keep$all_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    keep$tot_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$all_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & df$t_end_inf_home >= 43)
    keep$tot_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43_kids[i] = sum(!df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$all_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43  & df$t_end_inf_home >= 43)
    keep$tot_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= 43 & !df$HH_id%in%c(df$HH_id[df$start]) & df$t_end_inf_home >= 43)
    keep$detected_43_staff[i] = sum(df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$detected & df$t_end_inf==43)
    
    keep$from_kids[i] = 0 #sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]) & !df$adult[df$source])
    keep$R0[i] = sum(df$tot_inf[df$start])
    keep$Rt[i] =  mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep], na.rm = T)
    keep$avg_infs[i] = mean(df$tot_inf[df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$start])
    keep$start[i] = sum(df$start & df$t_end_inf_home>=time_keep & df$t_inf < time_keep+time - 1)
    keep$class_test_ind[i] = df$class_test_ind[1]
    keep$class_test_ind_q[i] = df$class_test_ind_q[1]
    keep$test_qs[i] = sum(df$test_ct_q)
    keep$detected[i] = sum(df$detected)
    keep$detected_q[i] = sum(df$detected_q)
    keep$detected_q_start[i] = sum(df$detected_q_start)
    keep$detected_staff[i] = sum(df$detected[df$adult])
    keep$detected_students[i] = sum(df$detected[!df$adult])
    keep$detected_staff_subclin[i] = sum(df$detected[df$adult & df$sub_clin], na.rm = T)
    keep$detected_students_subclin[i] = sum(df$detected[!df$adult & df$sub_clin], na.rm = T)
    keep$quarantine_check[i] = max(df$t_end_inf-df$t_end_inf_home, na.rm = T)#(df$uh.oh[1])
    keep$avg_class[i] = unlist(df %>% filter(t_exposed!=-99 & t_exposed <= time_keep + time - 1 & class!=99) %>% group_by(class) %>% 
                                 summarize(num = length(class)) %>% ungroup() %>% summarize(mean(num, na.rm = T)))
    keep$quarantined[i] = sum(df$quarantined)
    keep$quarantined2[i] = sum(df$quarantined2)
    keep$quarantined_kids[i] = sum(df$quarantined[!df$adult])#length(unique(df$id[df$quarantined>0])) #sum(df$quarantined[!df$adult])
    keep$start_adult[i] = sum(df$adult & df$start & df$t_end_inf_home>=time_keep)
    keep$start_family[i] = sum(df$family & df$start & df$t_end_inf_home>=time_keep)
    keep$start_kids[i] = sum(!df$adult & df$start & df$t_end_inf_home>=time_keep)
    keep$start_symp[i] = sum(df$symp[df$start], na.rm = T)
    keep$source_asymp[i] = sum(!df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$source_asymp_family_kids[i] = sum(df$family & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$source_asymp_family_staff[i] =sum(df$family_staff & !df$source_symp & df$t_inf <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$HH_id%in%c(df$HH_id[df$start]), na.rm = T)
    keep$adult[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1)
    keep$teacher[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1 & df$class < 99)
    keep$family[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$family & df$t_inf <= time_keep + time - 1 )
    keep$staff_family[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$family_staff & df$t_inf <= time_keep + time - 1)
    keep$children[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$adult & df$t_inf <= time_keep + time - 1)
    keep$children_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & !df$adult & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$school_adult_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & !df$family & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$family_tot[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$adult & df$family & df$t_inf <= time_keep + time - 1 & !df$HH_id%in%c(df$HH_id[df$start]))
    keep$symp[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$symp_kids[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==1 & df$t_inf <= time_keep + time - 1 & !df$adult, na.rm = T)
    keep$asymp_kids[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$symp==0 & df$t_inf <= time_keep + time - 1 & !df$adult, na.rm = T)
    keep$sick_at_end[i] = sum(df$t_inf<=time_keep + time - 1 & df$t_end_inf > time_keep + time - 1)
    keep$class[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Class")
    keep$household[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Household")
    keep$related_arts[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Related arts")
    keep$child_care[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Child care")
    keep$random[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Random contacts")
    keep$random_staff[i] = sum(df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$location == "Staff contacts")
    keep$num_classroom[i] = length(unique(df$class[df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$class < 99]))
    keep$clin_staff[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_students[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & !df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_family[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$clin_staff2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$adult & !df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_students2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & !df$adult & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$clin_family2[i] = sum(df$t_notify>=15 & df$symp & !df$sub_clin & df$family & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1 & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff[i] = sum(df$t_notify>=15 & df$adult & !df$family & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_students[i] = sum(df$t_notify>=15 & !df$adult & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_family[i] = sum(df$t_notify>=15 & df$family & df$t_notify <= time_keep + time - 1, na.rm = T)
    keep$notify_staff2[i] = sum(df$t_notify>=15 & df$adult & !df$family & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_students2[i] = sum(df$t_notify>=15 & !df$adult & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$notify_family2[i] = sum(df$t_notify>=15 & df$family & df$t_notify <= time_keep + time - 1 & df$t_inf!=0 & df$t_end_inf_home>=time_keep & df$t_inf <= time_keep + time - 1, na.rm = T)
    keep$surveillance[i] = df$surveillance[1]
    keep$switch[i] = df$switch[1]
    keep$temp_switch[i] = df$temp_switch[1]
    #keep$specials_count[i] = sum(df$specials_count[df$start])
    
    # Alyssa's new checks
    keep$start_kids[i] = sum(df$start.init[!df$adult])
    keep$start_adults[i] = sum(df$start.init[df$adult])
    keep$start_kids_time[i] = mean(df$t_inf[df$start.init & !df$adult])
    keep$start_adult_time[i] = mean(df$t_inf[df$start.init & df$adult])
    keep$not_inf_start[i] = sum(df$not_inf_keep)
    keep$test_type[i] = sum(df$test_type)
    keep$inc_test[i] = sum(df$inc_test & df$vacc)
    keep$vaxxed[i] = sum(df$vacc)
    keep$test_q_start[i] = sum(df$test_q_keep)
    
        #print(i) 
  }
  
  #toc()
  
  return(keep) #keep) #list(keep, mod, class, sched))
}

