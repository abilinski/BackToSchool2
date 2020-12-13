#Set working directory
setwd("/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool")

#Load packages
library(tidyverse)
library(Hmisc)
library(igraph)

#Load data
load("BackToSchool/data/synthMD.RData")
load("BackToSchool/data/synthMD_HS.RData")

#Load source functions
source("BackToSchool/R/abm6.R")

#############################
# TEST make_school FUNCTION #
#############################

test_make_school <- function(school, base_pop = synthMD, n_other_adults = 30, includeFamily = F, n_class = 4){
  expected_n_students <- nrow(base_pop)
  expected_n_staff <- length(unique(base_pop$age))*n_class + n_other_adults
  expected_n_family_students <- length(unique(base_pop$HH_id))*2*includeFamily
  expected_n_family_staff <- expected_n_staff*includeFamily
  expected_n <- expected_n_students + expected_n_staff + expected_n_family_students + expected_n_family_staff
  
  actual_n_students <- nrow(subset(school, adult == FALSE))
  actual_n_staff <- nrow(subset(school, adult == TRUE & family == FALSE & family_staff == FALSE))
  actual_n_family_students <- nrow(subset(school, adult == TRUE & family == TRUE & family_staff == FALSE))
  actual_n_family_staff <- nrow(subset(school, adult == TRUE & family == TRUE & family_staff == TRUE))
  actual_n <- nrow(school)
  
  test_students <- (expected_n_students == actual_n_students)
  test_staff <- (expected_n_staff == actual_n_staff)
  test_family_students <- (expected_n_family_students == actual_n_family_students)
  test_family_staff <- (expected_n_family_staff == actual_n_family_staff)
  test_n <- (expected_n == actual_n)
  
  #Compare Sizes
  print(paste("Expected number of students matches actual number of students:", test_students))
  print(paste(expected_n_students, "vs.", actual_n_students))
  print(paste("Expected number of staff matches actual number of staff:", test_staff))
  print(paste(expected_n_staff, "vs.", actual_n_staff))
  print(paste("Expected number of students' family matches actual number of students' family:", test_family_students))
  print(paste(expected_n_family_students, "vs.", actual_n_family_students))
  print(paste("Expected number of staff family matches actual number of staff family:", test_family_staff))
  print(paste(expected_n_family_staff, "vs.", actual_n_family_staff))
  print(paste("Expected number of people matches actual number of people:", test_n))
  print(paste(expected_n, "vs.", actual_n))
  
  #Compare students to ensure correct grouping
  students <- (subset(school, adult == FALSE))
  family_assignment_correct <- TRUE
  for(i in nrow(students)){
    student_1 <- students[i,]
    for(j in nrow(students)){
      student_2 <- students[j,]
      if(student_1$HH_id == student_2$HH_id){
        if(student_1$group != student_2$group | student_1$group_quarter != student_2$group_quarter){
          family_assignment_correct <- FALSE}
        if(student_1$age == student_2$age){
          if(student_1$class != student_2$class){
            family_assignment_correct <- FALSE}
        }
      }
    }
  }
  
  print(paste("Students grouped correctly by family:", family_assignment_correct))
  
  summary_classes <- count(group_by(students, age, class))
  
  max_class_size <- aggregate(summary_classes$n, by = list(summary_classes$age), max)
  colnames(max_class_size) <- c("age", "max_class")
  
  print("Max Class Sizes by Age:")
  print(max_class_size)
  
  min_class_size <- aggregate(summary_classes$n, by = list(summary_classes$age), min)
  colnames(min_class_size) <- c("age", "min_class")
  
  print("Min Class Sizes by Age:")
  print(min_class_size)
  
  avg_class_size <- aggregate(summary_classes$n, by = list(summary_classes$age), mean)
  colnames(avg_class_size) <- c("age", "avg_class")
  
  print("Average Class Sizes by Age:")
  print(avg_class_size)
}

###Basic School###
school_default <- make_school()
test_make_school(school_default)
###PASS###

###School with Family###
school_family <- make_school(includeFamily = TRUE)
test_make_school(school_family, includeFamily = TRUE)
###PASS###

###School with 0 classes per grade###
school_noclass <- make_school(n_class = 0)
test_make_school(school_noclass, n_class = 0)
###SHOULD BREAK BUT DOES NOT###

###School with 10 classes per grade###
school_10class <- make_school(n_class = 10)
test_make_school(school_10class, n_class = 10)
###PASS###

###School with 100 classes per grade###
school_100class <- make_school(n_class = 100)
test_make_school(school_100class, n_class = 100)
###make_school function breaks###

###################################
# TEST initialize_school FUNCTION #
###################################

###Try to change transmission probability###
init_school1 <- initialize_school(start = school_default)
describe(init_school1$class_trans_prob)

init_school2 <- initialize_school(start = school_default, dedens = T)
describe(init_school2$class_trans_prob)
###attack rate does not control class_trans_prob under default (which always sets class_trans_prob to 0) --> must default to dedens = TRUE###

###Try to make everyone have same transmission probability###
init_school3 <- initialize_school(start = school_default, dedens = 1, disperse_transmission = FALSE)
describe(init_school3$class_trans_prob)
###Adults have overdispersed transmission by default, even when disperse_transmission = FALSE###

###Make sure susceptibility parameters are changeable###
init_school4 <- initialize_school(start = school_family, dedens = 1, teacher_susp = 0.75)
describe(init_school4$susp)
###PASS###

###################################
# TEST make_schedule FUNCTION     #
###################################

init_school_default <- initialize_school(start = school_default)

###Test basic functionality###
schedule_default <- make_schedule(df = init_school_default)
###PASS####

###################################################################################
# TEST run_household FUNCTION                                                     #
#                                                                                 #
# NOTE: run with altered initialize_school to set class_trans_prob to attack_rate #
###################################################################################

#Altered initialize_school function
initialize_school = function(n_contacts = 10, n_contacts_brief = 0, rel_trans_HH = 1,
                             rel_trans = 1/8, rel_trans_brief = 1/50, p_asymp_adult = .35,
                             p_asymp_child = .7, attack = .01, child_trans = 1, child_susp = .5,
                             teacher_trans = 1, teacher_susp = 1, disperse_transmission = T,
                             isolate = 1, dedens = F, run_specials = F, start){
  
  # make non-teacher adults
  n = nrow(start)
  c = max(start$class)
  
  # initialize data frame
  df = start %>%
    mutate(id = row_number(),
           t_exposed = -1,
           t_inf = -1,
           symp = -1,
           t_symp = -1,
           t_end_inf = -1,
           t_end_inf_home = -1,
           t_notify = -17,
           c_trace = -1,
           c_trace_start = -1,
           tot_inf = 0,
           n_contact = n_contacts,
           n_contact_brief = n_contacts_brief,
           relative_trans = rel_trans,
           relative_trans_HH = rel_trans_HH,
           relative_trans_brief = rel_trans_brief,
           attack_rate = attack,
           dedens = dedens,
           source = 0,
           tot_inf = 0,
           run_specials = run_specials,
           super_spread = disperse_transmission,
           out = 0,
           location = "",
    ) %>%
    mutate(p_asymp = ifelse(adult, p_asymp_adult, p_asymp_child),
           
           # isolation
           isolate = rbinom(n, size = 1, prob = isolate),
           
           # transmission probability
           class_trans_prob = attack,
           #class_trans_prob = ifelse(super_spread | adult, attack*rlnorm(n, meanlog = log(.84)-log((.84^2+.3)/.84^2)/2, sdlog = sqrt(log((.84^2+.3)/.84^2)))/.84, attack),
           #class_trans_prob = ifelse(adult, class_trans_prob, child_trans*class_trans_prob),
           #class_trans_prob = dedens*class_trans_prob,
           #class_trans_prob = ifelse(adult & !family, class_trans_prob*teacher_trans, class_trans_prob),
           
           
           # susceptibility
           susp = ifelse(adult, 1, child_susp),
           susp = ifelse(adult & !family, teacher_susp, susp),
           
           # note to self -- adjust this in parameters
           specials = ifelse(run_specials, id%in%(n:(n-14)), id%in%(n:(n-4)))) %>% ungroup()
  
  return(df)
}

#########################################################################################
# Excerpt code from run_model to create school_prep function to add flags from schedule #
#########################################################################################

school_prep <- function(school, schedule, t){
  
  # present
  school$present = schedule$present[schedule$t==t]
  school$not_inf = school$t_exposed==-1
  school$present_susp = school$present & school$not_inf
  
  # infectious
  school$inf_home = school$t_inf > -1 & school$t_inf <= t & school$t_end_inf_home >= t
  school$inf = school$t_inf > -1 & school$t_inf <= t & school$t_end_inf >= t
  
  # infectious and at school
  school$trans_now = school$present & school$inf & !school$family
  school$trans_outside = !school$present & school$inf & !school$family
  
  return(school)
}

###Make sure run_household correctly infects family member when attack_rate = 1###
#Initialize school with attack rate = 1, siblings only (no family)
init_school5 <- initialize_school(start = school_default, attack = 1)
run_household(62, school_prep(init_school5, schedule_default, 1))
###PASS###

test_run_household <- function(initialized_school, percent_infected, attack_rate, replicates){
  
  number_infected <- c()
  
  schedule <- make_schedule(df = initialized_school)
  
  initialized_school <- school_prep(school = initialized_school, schedule = schedule, t = 1)
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    for(a in infected){
      new_infected <- c(new_infected, run_household(a, initialized_school))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  mult_family <- unique(count(group_by(initialized_school, HH_id))[which(count(group_by(initialized_school, HH_id))$n > 1), "HH_id"])$HH_id
  
  avg_family_size <- mean(count(group_by(initialized_school, HH_id))[which(count(group_by(initialized_school, HH_id))$n > 1),]$n)
  
  number_family <- nrow(initialized_school[which(initialized_school$HH_id %in% mult_family),])
  
  average_susp <- mean(initialized_school[which(initialized_school$HH_id %in% mult_family),]$susp)
  
  average_rel_trans <- mean(initialized_school[which(initialized_school$HH_id %in% mult_family),]$relative_trans_HH)
  
  print(paste("Compare number of siblings infected with expected new infections:"))
  print(paste(mean(number_infected), "vs.", sum(dbinom(1:(floor(avg_family_size)-1), floor(avg_family_size)-1, percent_infected*average_rel_trans)*(1-(1-attack_rate*average_susp)^(1:(floor(avg_family_size)-1))))*(1-percent_infected)*(avg_family_size)*length(mult_family)))
}

###Test transmission when family transmission is only between siblings, 100% Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 1)

test_run_household(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100)
###PASS###
            
###Test transmission when family transmission is only between siblings, Medium Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.5)

test_run_household(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100)
###PASS###

###Test transmission when family transmission is only between siblings, Low Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.01)

test_run_household(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100)
###PASS###

###Test transmission when family transmission is only between siblings, Zero Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0)

test_run_household(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100)
###PASS###

###NOTE: Above tests are robust to changes in percent_infected###

###Test transmission when all family members are present, 100% Attack Rate###
init_school_family <- initialize_school(start = school_family, attack = 1)

test_run_household(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 1, replicates = 100)
###PASS###

###Test transmission when all family members are present, Medium Attack Rate###
init_school_family <- initialize_school(start = school_family, attack = 0.5)

test_run_household(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0.5, replicates = 100)
###PASS###

###Test transmission when all family members are present, Low Attack Rate###
init_school_family <- initialize_school(start = school_family, attack = 0.1)

test_run_household(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0.1, replicates = 100)
###PASS###

###Test transmission when all family members are present, Zero Attack Rate###
init_school_family <- initialize_school(start = school_family, attack = 0)

test_run_household(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0, replicates = 100)
###PASS###

###NOTE: Above tests are robust to changes in percent_infected###

###################################################################################
# TEST run_class FUNCTION                                                         #
#                                                                                 #
# NOTE: run with altered initialize_school to set class_trans_prob to attack_rate #
###################################################################################

###Make sure run_class correctly infects entire class when attack_rate = 1###
#Initialize school with attack rate = 1, siblings only (no family)
init_school6 <- initialize_school(start = school_default, attack = 1)

class1 <- init_school6[init_school6$class == init_school6[which(init_school6$id == 1),]$class,]$id
print(paste(length(setdiff(run_class(1, school_prep(init_school6, schedule_default, 1)), c(0))), "vs.", 0.5*(length(class1))))
###Output should be list with about half of the class minus the original infected student, plus the teacher: PASS###

test_run_class <- function(initialized_school, percent_infected, attack_rate, replicates, high_school = FALSE, nper = 8){
  
  number_infected <- c()
  
  schedule <- make_schedule(df = initialized_school)
  
  initialized_school <- school_prep(school = initialized_school, schedule = schedule, t = 1)
  
  hs.classes <- NA
  
  if(high_school){
    hs.classes <- data.frame(period = numeric(), class = numeric(), id = numeric())
    for(p in 1:nper){
      temp <- data.frame(period = p, class = sample(initialized_school$class[initialized_school$class!=99]), id = initialized_school$id[initialized_school$class!=99])
      hs.classes = hs.classes %>% bind_rows(temp)
    }
  }
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    for(a in infected){
      new_infected <- c(new_infected, run_class(a = a, df = initialized_school, high_school = high_school, hs.classes = hs.classes))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  avg_class_size <- mean(count(group_by(initialized_school, class))[which(count(group_by(initialized_school, class))$class != 99),]$n)
  
  number_classes <- length(unique(initialized_school$class[initialized_school$class != 99]))
  
  pop_size <- length(unique(initialized_school$id[initialized_school$class != 99]))
  
  average_susp <- mean(initialized_school[which(initialized_school$class != 99),]$susp)
  
  average_rel_trans <- mean(initialized_school[which(initialized_school$class != 99),]$relative_trans)
  
  if(high_school){avg_contact.group_size_hs <- mean(sapply(initialized_school$id[initialized_school$class != 99], function(x){length(unique(hs.classes$id[hs.classes$class == hs.classes$class[hs.classes$id == x]]))}))}
  
  print(paste("Compare number of classmates and teachers infected with expected new infections: probability a given person in a class is newly infected*avg_class_size*number_classes"))
  print(paste(mean(number_infected), "vs.", ifelse(high_school, sum(dbinom(1:floor(avg_contact.group_size_hs), floor(avg_contact.group_size_hs), percent_infected)*(1-(1-attack_rate*average_susp*average_rel_trans)^(1:floor(avg_contact.group_size_hs))))*(1-percent_infected)*pop_size, sum(dbinom(1:floor(avg_class_size), floor(avg_class_size), percent_infected)*(1-(1-attack_rate*average_susp)^(1:floor(avg_class_size))))*(1-percent_infected)*avg_class_size*number_classes)))
}

###Test transmission, no high school, 100% Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 1)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100)
###PASS###

###Test transmission, no high school, Medium Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.5)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100)
###PASS###

###Test transmission, no high school, Low Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.01)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100)
###PASS###

###Test transmission, no high school, Zero Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100)
###PASS###

###NOTE: Above tests are robust to changes in percent_infected###

###Test transmission, high school, 100% Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 1)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Medium Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.5)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Low Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0.01)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Zero Attack Rate###
init_school_default <- initialize_school(start = school_default, attack = 0)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100, high_school = TRUE)
###PASS###

###NOTE: Above tests are robust to changes in percent_infected###

###################################################################################
# TEST run_rand FUNCTION                                                          #
#                                                                                 #
# NOTE: run with altered initialize_school to set class_trans_prob to attack_rate #
###################################################################################

school_default_t1_attack1 <- school_prep(initialize_school(start = school_default, attack = 1, child_susp = 1, rel_trans = 1), schedule_default, 1)

###Make sure run_run correctly infects entire school when attack_rate = 1 and n_contacts is set to size of entire school###
random_contacts_entire_school <- sample_k_regular(sum(school_default_t1_attack1$present), sum(school_default_t1_attack1$present) - 1)

print(paste("Number of Students Infected:", sum(unique(run_rand(1, school_default_t1_attack1, random_contacts_entire_school))>0)))
print(paste("Number of previously uninfected:", length(school_default_t1_attack1$present)-1))

###PASS###

###Question: Is run_model set to handle brief contacts yet?###

test_run_rand <- function(initialized_school, percent_infected, attack_rate, replicates){
  
  number_infected <- c()
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    random_contacts <- sample_k_regular(sum(initialized_school$present), initialized_school$n_contact[1] + initialized_school$n_contact_brief[1])
    for(a in infected){
      new_infected <- c(new_infected, run_rand(a, initialized_school, random_contacts))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  present <- sum(initialized_school$present)
  
  n_contacts <- initialized_school$n_contact[1] + initialized_school$n_contact_brief[1]
  
  average_susp <- mean(initialized_school[which(initialized_school$present),]$susp)
  
  average_rel_trans <- mean(initialized_school[which(initialized_school$present),]$relative_trans)
  
  print(paste("Compare number of individuals infected with expected new infections: probability a given person in a school is newly infected*present"))
  print(paste(mean(number_infected), "vs.", sum(dbinom(1:n_contacts, n_contacts, percent_infected)*(1-(1-attack_rate*average_susp*average_rel_trans)^(1:n_contacts))*(1-percent_infected)*present)))
}

###Test transmission with 100% Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 1), schedule = schedule_default, 1)

test_run_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100)
###PASS###

###Test transmission with Medium Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0.5), schedule = schedule_default, 1)

test_run_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100)
###PASS###

###Test transmission with Low Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0.01), schedule = schedule_default, 1)

test_run_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100)
###PASS###

###Test transmission with Zero Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0), schedule = schedule_default, 1)

test_run_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100)
###PASS###

###NOTE: Above tests passed relatively well, but mean number of newly infected is always slightly lower than the expected value. This might be due to the reciprocal nature of the contact graph.###

###NOTE: Tests above are robust to changes in percent_infected###

###################################################################################
# TEST run_staff_rand FUNCTION                                                    #
#                                                                                 #
# NOTE: run with altered initialize_school to set class_trans_prob to attack_rate #
###################################################################################

school_default_attack1_t1 <- school_prep(initialize_school(start = school_default, attack = 1, rel_trans = 1), schedule = schedule_default, 1)

###Make sure run_staff_rand correctly infects all staff when attack_rate = 1 and n_contacts is set to size of staff###

print(paste("Number of Staff Infected:", sum(unique(run_staff_rand(554, school_default_attack1_t1, sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))))>0)))
print(paste("Number of previously uninfected:", sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))-1))

###PASS###

###Make sure student cannot infect staff through run_staff_rand###

print(paste("Number of Staff Infected:", sum(unique(run_staff_rand(1, school_default_attack1_t1, sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))))>0)))

###FAIL, but run_model only passes ID if infected individual is a staff member###

###Modify function to only infect from staff###
run_staff_rand = function(a, df, n_contact){
  
  if(n_contact>0 & df$adult[df$id==a] & !df$family[df$id==a]){
    # pull contacts from random graph
    tot = length(df$id[df$present & df$adult & !df$family])
    contact_take = ifelse(n_contact<=tot, n_contact, tot)
    contact_id = sample(df$id[df$present & df$adult & !df$family], contact_take)
    contacts = df[df$id %in% contact_id & df$id!=a,]
    
    # determine whether a contact becomes infected
    prob_rand = rbinom(nrow(contacts), size = 1,
                       prob = df$class_trans_prob[df$id==a]*contacts$susp*contacts$not_inf*df$relative_trans[df$id==a])
    # infected individuals
    infs = contacts$id*prob_rand
    
    return(infs)
  }else return(0)
}

###Make sure student cannot infect staff through run_staff_rand###

print(paste("Number of Staff Infected:", sum(unique(run_staff_rand(1, school_default_attack1_t1, sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))))>0)))

###PASS###

test_run_staff_rand <- function(initialized_school, percent_infected, attack_rate, replicates, n_contacts){
  
  number_infected <- c()
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    
    for(a in infected){
      new_infected <- c(new_infected, run_staff_rand(a, initialized_school, n_contacts))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  staff_present <- sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))
  
  average_susp <- mean(initialized_school[which(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family)),]$susp)
  
  average_rel_trans <- mean(initialized_school[which(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family)),]$relative_trans)
  
  print(paste("Compare number of staff infected with expected new infections: probability a given staff member in a school is newly infected*present"))
  print(paste(mean(number_infected), "vs.", sum(dbinom(1:n_contacts, n_contacts, percent_infected)*(1-(1-attack_rate*average_susp*average_rel_trans)^(1:n_contacts))*(1-percent_infected)*staff_present)))
}

###Test transmission with 100% Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 1), schedule = schedule_default, 1)

test_run_staff_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100, n_contacts = 1)
###PASS###

###Test transmission with Medium Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0.5), schedule = schedule_default, 1)

test_run_staff_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100, n_contacts = 1)
###PASS###

###Test transmission with Low Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0.01), schedule = schedule_default, 1)

test_run_staff_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100, n_contacts = 1)
###PASS###

###Test transmission with Zero Attack Rate###
init_school_default <- school_prep(initialize_school(start = school_default, attack = 0), schedule = schedule_default, 1)

test_run_staff_rand(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100, n_contacts = 1)
###PASS###

###NOTE: Tests above are robust to changes in percent_infected and n_contacts###

###################################################################################
# TEST run_care FUNCTION                                                          #
#                                                                                 #
# NOTE: run with altered initialize_school to set class_trans_prob to attack_rate #
###################################################################################

###Make sure run_care correctly infects all households when attack_rate = 1 and all households not present in-school contact each other###

init_school_family <- initialize_school(start = school_family, child_susp = 1, attack = 1)
school_family_scheduled <- school_prep(init_school_family, make_schedule(df = init_school_family), 6)

len <- length(unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]))
n_HH <- length(unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]))

# contacts from childcare
care_contacts <- data.frame(HH_id = unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]), cat = sample(rep(1:ceiling(len/n_HH), each = n_HH)[1:len]))

print(paste("Number of Individuals Infected:", sum((unique(run_care(1, school_family_scheduled, care_contacts)))>0)))
print(paste("Number of previously uninfected:", sum(!school_family_scheduled$present & !school_family_scheduled$family_staff & !(school_family_scheduled$adult & !school_family_scheduled$family) & school_family_scheduled$HH_id!=school_family_scheduled$HH_id[school_family_scheduled$id == 1])))

###PASS###

test_run_care <- function(initialized_school, percent_infected, attack_rate, replicates, n_HH){
  
  number_infected <- c()
  len <- length(unique(initialized_school$HH_id[!initialized_school$adult &! initialized_school$present]))
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    care_contacts <- data.frame(HH_id = unique(initialized_school$HH_id[!initialized_school$adult & !initialized_school$present]), cat = sample(rep(1:ceiling(len/n_HH), each = n_HH)[1:len]))
    for(a in infected){
      new_infected <- c(new_infected, run_care(a, initialized_school, care_contacts))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  average_susp <- mean(initialized_school[which(!initialized_school$present & !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),]$susp)
  
  average_nonhh_group_size <- mean(count(group_by(merge(initialized_school, care_contacts, by = c("HH_id")), cat))$n) - mean(count(group_by(initialized_school[which(!initialized_school$present & !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),]), HH_id)$n)
  
  number_atrisk <- nrow(initialized_school[which(!initialized_school$present & !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),])
  
  print(paste("Compare number of individuals infected with expected new infections"))
  print(paste(mean(number_infected), "vs.", sum(dbinom(1:ceiling(average_nonhh_group_size), ceiling(average_nonhh_group_size), percent_infected)*(1-(1-attack_rate*average_susp)^(1:ceiling(average_nonhh_group_size)))*(1-percent_infected)*number_atrisk)))
}

###Test transmission with 100% Attack Rate###
init_school_temp <- initialize_school(start = school_family, attack = 1)
init_school_family <- school_prep(init_school_temp, schedule = make_schedule(df = init_school_temp), 6)

test_run_care(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 1, replicates = 100, n_HH = 2)
###PASS###

###Test transmission with Medium Attack Rate###
init_school_temp <- initialize_school(start = school_family, attack = 0.5)
init_school_family <- school_prep(init_school_temp, schedule = make_schedule(df = init_school_temp), 6)

test_run_care(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0.5, replicates = 100, n_HH = 2)
###PASS###

###Test transmission with Low Attack Rate###
init_school_temp <- initialize_school(start = school_family, attack = 0.01)
init_school_family <- school_prep(init_school_temp, schedule = make_schedule(df = init_school_temp), 6)

test_run_care(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0.01, replicates = 100, n_HH = 2)
###PASS###

###Test transmission with Zero Attack Rate###
init_school_temp <- initialize_school(start = school_family, attack = 0)
init_school_family <- school_prep(init_school_temp, schedule = make_schedule(df = init_school_temp), 6)

test_run_care(initialized_school = init_school_family, percent_infected = 0.1, attack_rate = 0, replicates = 100, n_HH = 2)
###PASS###

###NOTE: Tests above are robust to changes in percent_infected and n_HH. Excpected infected is slightly higher than actual, since ceiling function is used.###

######################################################################################################
# Bookkeeping for run_model                                                                          #
#                                                                                                    #
# Make sure output of each step is reasonable with defaults and does not produce errors in next step #
######################################################################################################

#Reload original functions
source("BackToSchool/R/abm6.R")

#Create School
school <- make_school(includeFamily = TRUE)
df <- initialize_school(start = school, dedens = TRUE, run_specials = TRUE, attack = 0.5)

###############################
# MUST DEFAULT dedens = TRUE  #
###############################

#Create Schedule
sched <- make_schedule(df = df)

#Initialize variable values
time = 30
notify = F
test = F
test_days = "week"
test_sens =  .7
test_frac = .9
n_staff_contact = 0
n_HH = 0
n_start = 1
days_inf = 6
mult_asymp = 1
seed_asymp = F
time_seed_inf = NA
high_school = TRUE
nper = 8
start_mult = 1
start_type = "mix"
test_type = "all"
adult_prob = 0.01
child_prob = 0.04

#Start run_model steps
time_seed_inf <- sample(1:14, 1)
print(time_seed_inf)
id.samp <- sample(df$id[!df$family], n_start, prob = (df$adult[!df$family]*start_mult+1)/sum(df$adult))
print(id.samp)

##Test other types of seeding##################################################
id.samp_adult = sample(df$id[!df$family & df$adult], n_start)
print(id.samp_adult)
id.samp_teacher = sample(df$id[!df$family & df$adult & df$class!=99], n_start)
print(id.samp_teacher)
id.samp_child = sample(df$id[!df$family & !df$adult], n_start)
print(id.samp_child)

vec = 1:time
adult_times = vec[as.logical(rbinom(time, size = 1, prob = adult_prob))]
child_times = vec[as.logical(rbinom(time, size = 1, prob = child_prob))]
  
adults = sample(df$id[df$adult & !df$family], length(adult_times)) 
kids = sample(df$id[!df$adult], length(child_times))

time_seed_inf_cont = c(adult_times, child_times)
print(time_seed_inf_cont) ##Returns integer(0) if no seed infection is drawn
id.samp_cont = c(adults, kids)
print(id.samp_cont) ##Returns integer(0) if no seed infection is drawn
###############################################################################

df[df$id%in%id.samp,] = make_infected(df[df$id%in%id.samp,], days_inf = days_inf,
                                      set  = time_seed_inf, seed_asymp = seed_asymp, mult_asymp = mult_asymp)
df$start = df$id %in% id.samp
df$id[df$start] #Matches id.samp

testing_days = seq(7, (time+15), by = 7)
print(testing_days)

class_quarantine = data.frame(class = unique(df$class[df$class!=99]), t_notify = -17, hold = -17)
head(class_quarantine)
mat = matrix(NA, nrow = max(df$id), ncol = time)
head(mat)

df$test_type = T

##Test first run through time for loop

t <- time_seed_inf

# class quarantines
classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + 10),]
print(classes_out)

# present
df$present = sched$present[sched$t==t] & !df$class%in%classes_out$class
print(df$id[df$present])
df$not_inf = df$t_exposed==-1
print(df$id[df$not_inf])
print(df$id[!df$not_inf])
df$present_susp = df$present & df$not_inf
print(df$id[df$present_susp])
df$quarantined = df$quarantined + as.numeric(df$class%in%classes_out$class)
print(df$id[df$quarantined])

# check who is present
mat[,(t-time_seed_inf+1)] = df$present
head(mat)

# infectious
df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t
print(df$id[df$inf_home])
df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
print(df$id[df$inf])

# infectious and at school
df$trans_now = df$present & df$inf & !df$family
print(df$id[df$trans_now])
df$trans_outside = !df$present & df$inf & !df$family
print(df$id[df$trans_outside])

# set infections to 0 for this timestep
df$now = F

# set up scheduling if high school
hs.classes = NA
if(high_school){
  hs.classes = data.frame(period = numeric(), class = numeric(), id = numeric())
  for(p in 1:nper){
    temp = data.frame(period = p, class = sample(df$class[df$class!=99]), id = df$id[df$class!=99])
    hs.classes = hs.classes %>% bind_rows(temp)
  }
}
head(hs.classes)

#########################################################
# Shouldn't period scheduling be the same across weeks? #
#########################################################
 
#### SELECT NEXT GENERATION INFECTIONS ####
# run model for infectious individuals at home

home_infs = df$id[df$inf_home]
print(home_infs)
if(sum(df$inf_home > 1)) home_infs = sample(home_infs)
  
for(a in home_infs){
    
  # HOUSEHOLD CONTACTS
  inf_vec = run_household(a, df)
  print(inf_vec)
  df$location[df$id%in%inf_vec] = "Household"
    
  # add to total # of infections from this person
  df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
  print(df$tot_inf[df$id==a])
    
  # flag people infected at this time step
  df$now = ifelse(df$id%in%inf_vec, T, df$now)
  print(df$id[df$now])
  df$source = ifelse(df$id%in%inf_vec, a, df$source)
  print(df$id[df$source])
  df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
  print(df$id[!df$not_inf])
  df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
}

# run model for infectious individuals at school
if(sum(df$trans_now)>0) {
  
  # RANDOM CONTACT STRUCTURE
  # sample from a random regular graph
  # this approach ensures reciprocity
  # you may want to split out to ensure reciprocity in contact type
  random_contacts = sample_k_regular(sum(df$present), df$n_contact[1] + df$n_contact_brief[1])
  print(random_contacts)
  #if(n_staff_contact>0) random_staff_contacts = sample_k_regular(sum(df$present & df$adult & !df$family), n_staff_contact)
  
  # SPECIAL SUBJECTS
  specials = data.frame(teacher = rep(df$id[df$specials], each = 4), class = sample(1:max(df$class[!is.na(df$class)]), 4*sum(df$specials)))
  head(specials)
  
  ##############################################################################################
  # Specials are always run? There seems to be a default to always have five specials teachers #
  # Also, why isn't every class assigned to a specials teacher?                                #
  # Finally, why are family members assigned as specials teachers?                             #
  ##############################################################################################
  
  # run transmission in schools
  school_infs = df$id[df$trans_now]
  print(school_infs)
  if(sum(df$trans_now > 1)) school_infs = sample(school_infs)
  
  # choose contacts that become infected
  for(a in school_infs){
    
    # CLASS CONTACTS
    class_trans = run_class(a, df, high_school = high_school, hs.classes = hs.classes)
    print(class_trans)
    df$location[df$id%in%class_trans] = "Class"
    
    # RANDOM CONTACTS
    rand_trans = tryCatch({run_rand(a, df, random_contacts)}, error = function(err) {0})
    print(rand_trans)
    df$location[df$id%in%rand_trans] = "Random contacts"
    
    # RANDOM ADULT CONTACTS
    # note this doesn't strictly keep reciprocity
    # but k-regular graphs can be finnicky at low ##s
    # and this captures the general pattern
    if(df$adult[df$id==a] & !df$family[df$id==a]){
      rand_staff_trans = run_staff_rand(a, df, n_staff_contact)
    }else{rand_staff_trans = 0}
    df$location[df$id%in%rand_staff_trans] = "Staff contacts"
    
    # SPECIALS CONTACTS
    specials_trans = run_specials(a, df, specials)
    print(specials_trans)
    df$location[df$id%in%specials_trans] = "Related arts"
    
    # return id if person is infected
    # and 0 otherwise
    inf_vec = c(class_trans, rand_trans, rand_staff_trans, specials_trans)
    print(inf_vec)
    
    # add to total # of infections from this person
    df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
    print(df$tot_inf[df$id==a])
    
    # flag people infected at this time step
    df$now = ifelse(df$id%in%inf_vec, T, df$now)
    print(df$id[df$now])
    df$source = ifelse(df$id%in%inf_vec, a, df$source)
    print(df$id[df$source == a])
    df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
    print(df$id[!df$not_inf])
    df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
    
  }
}

# run model for infectious individuals OUTSIDE school
if(sum(df$trans_outside)>0 & n_HH>0) {
  len = length(unique(df$HH_id[!df$adult & !df$present]))
  tot = ifelse(ceiling(len/n_HH)==0, 1, ceiling(len/n_HH))
  if(len==0){HHs = 0}else{HHs = unique(df$HH_id[!df$adult & !df$present])}
  #print("got here"); print(HHs)
  #print(len); print(tot); print(unique(df$HH_id[!df$adult & !df$present])); print(sample(rep(1:tot, each = n_HH)[1:len]))
  # contacts from childcare
  care_contacts = data.frame(HH_id = HHs,
                             cat = sample(rep(1:tot, each = n_HH)[1:len]))
  
  # run transmission in schools
  non_school_infs = df$id[df$trans_outside]
  if(sum(df$trans_outside > 1)) school_infs = sample(non_school_infs)
  
  # choose contacts that become infected
  for(a in non_school_infs){
    
    # CARE CONTACTS
    care_trans = run_care(a, df, care_contacts)
    df$location[df$id%in%care_trans] = "Child care"
    
    # return id if person is infected
    # and 0 otherwise
    inf_vec = c(care_trans)
    
    # add to total # of infections from this person
    df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
    
    # flag people infected at this time step
    df$now = ifelse(df$id%in%inf_vec, T, df$now)
    df$source = ifelse(df$id%in%inf_vec, a, df$source)
    df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
    df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
    
  }
}

#### SET UP NEXT GENERATION INFECTEDS ####
# update values updated at this stage
# need to put in probability distributions
if(sum(df$now>0)){
  
  df$t_exposed[df$now] = t
  df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp = mult_asymp)
  df.u = df[df$now,]
  print(df.u)
  
  # set up notification
  if(notify){
    for(k in 1:nrow(class_quarantine)){
      class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%df.u$class,
                                        max(df.u$t_notify[class_quarantine$class[k]%in%df.u$class]), class_quarantine$hold[k])
    }
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + 17,
                                       class_quarantine$hold, class_quarantine$t_notify)
    head(class_quarantine)
  }
  
}

## group testing
if(test & t%in%testing_days){
  df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
  df$t_end_inf = ifelse(df$inf & df$test, t, df$t_end_inf)
  df$t_notify = ifelse(df$inf & df$test, t, df$t_end_inf)
  df$detected = ifelse(df$inf & df$test, 1, df$detected)
  print(df$id[df$detected])
  
  df.u = df %>% filter(inf & test)
  # set up notification
  if(notify){
    for(k in 1:nrow(class_quarantine)){
      hold = class_quarantine$hold[k] 
      class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%df.u$class,
                                        max(df.u$t_notify[class_quarantine$class[k]%in%df.u$class]), class_quarantine$hold[k])
    }
    class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + 17,
                                       class_quarantine$hold, class_quarantine$t_notify)
    head(class_quarantine)
  }
  
}
#print(t); print(class_quarantine)
#if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)

for(t in (time_seed_inf+1):(time-1)){
  
  # class quarantines
  classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + 10),]
  
  # present
  df$present = sched$present[sched$t==t] & !df$class%in%classes_out$class
  df$not_inf = df$t_exposed==-1
  df$present_susp = df$present & df$not_inf
  df$quarantined = df$quarantined + as.numeric(df$class%in%classes_out$class)
  
  # check who is present
  mat[,(t-time_seed_inf+1)] = df$present
  
  # infectious
  df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t
  df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
  
  # infectious and at school
  df$trans_now = df$present & df$inf & !df$family
  df$trans_outside = !df$present & df$inf & !df$family
  
  # set infections to 0 for this timestep
  df$now = F
  
  # set up scheduling if high school
  hs.classes = NA
  if(high_school){
    hs.classes = data.frame(period = numeric(), class = numeric(), id = numeric())
    for(p in 1:nper){
      temp = data.frame(period = p, class = sample(df$class[df$class!=99]), id = df$id[df$class!=99])
      hs.classes = hs.classes %>% bind_rows(temp)
    }
  }
  
  #### SELECT NEXT GENERATION INFECTIONS ####
  # run model for infectious individuals at home
  if(sum(df$inf_home)>0) {
    
    home_infs = df$id[df$inf_home]
    if(sum(df$inf_home > 1)) home_infs = sample(home_infs)
    
    for(a in home_infs){
      
      # HOUSEHOLD CONTACTS
      inf_vec = run_household(a, df)
      df$location[df$id%in%inf_vec] = "Household"
      
      # add to total # of infections from this person
      df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
      
      # flag people infected at this time step
      df$now = ifelse(df$id%in%inf_vec, T, df$now)
      df$source = ifelse(df$id%in%inf_vec, a, df$source)
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
    specials = data.frame(teacher = rep(df$id[df$specials], each = 4), class = sample(1:max(df$class[!is.na(df$class)]), 4*sum(df$specials)))
    
    # run transmission in schools
    school_infs = df$id[df$trans_now]
    if(sum(df$trans_now > 1)) school_infs = sample(school_infs)
    
    # choose contacts that become infected
    for(a in school_infs){
      
      # CLASS CONTACTS
      class_trans = run_class(a, df, high_school = high_school, hs.classes = hs.classes)
      df$location[df$id%in%class_trans] = "Class"
      
      # RANDOM CONTACTS
      rand_trans = tryCatch({run_rand(a, df, random_contacts)}, error = function(err) {0})
      df$location[df$id%in%rand_trans] = "Random contacts"
      
      # RANDOM ADULT CONTACTS
      # note this doesn't strictly keep reciprocity
      # but k-regular graphs can be finnicky at low ##s
      # and this captures the general pattern
      if(df$adult[df$id==a] & !df$family[df$id==a]){
        rand_staff_trans = run_staff_rand(a, df, n_staff_contact)
      }else{rand_staff_trans = 0}
      df$location[df$id%in%rand_staff_trans] = "Staff contacts"
      
      # SPECIALS CONTACTS
      specials_trans = run_specials(a, df, specials)
      df$location[df$id%in%specials_trans] = "Related arts"
      
      # return id if person is infected
      # and 0 otherwise
      inf_vec = c(class_trans, rand_trans, rand_staff_trans, specials_trans)
      
      # add to total # of infections from this person
      df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
      
      # flag people infected at this time step
      df$now = ifelse(df$id%in%inf_vec, T, df$now)
      df$source = ifelse(df$id%in%inf_vec, a, df$source)
      df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
      df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
      
    }
  }
  
  # run model for infectious individuals OUTSIDE school
  if(sum(df$trans_outside)>0 & n_HH>0) {
    len = length(unique(df$HH_id[!df$adult & !df$present]))
    tot = ifelse(ceiling(len/n_HH)==0, 1, ceiling(len/n_HH))
    if(len==0){HHs = 0}else{HHs = unique(df$HH_id[!df$adult & !df$present])}
    #print("got here"); print(HHs)
    #print(len); print(tot); print(unique(df$HH_id[!df$adult & !df$present])); print(sample(rep(1:tot, each = n_HH)[1:len]))
    # contacts from childcare
    care_contacts = data.frame(HH_id = HHs,
                               cat = sample(rep(1:tot, each = n_HH)[1:len]))
    
    # run transmission in schools
    non_school_infs = df$id[df$trans_outside]
    if(sum(df$trans_outside > 1)) school_infs = sample(non_school_infs)
    
    # choose contacts that become infected
    for(a in non_school_infs){
      
      # CARE CONTACTS
      care_trans = run_care(a, df, care_contacts)
      df$location[df$id%in%care_trans] = "Child care"
      
      # return id if person is infected
      # and 0 otherwise
      inf_vec = c(care_trans)
      
      # add to total # of infections from this person
      df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
      
      # flag people infected at this time step
      df$now = ifelse(df$id%in%inf_vec, T, df$now)
      df$source = ifelse(df$id%in%inf_vec, a, df$source)
      df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
      df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
      
    }
  }
  
  #### SET UP NEXT GENERATION INFECTEDS ####
  # update values updated at this stage
  # need to put in probability distributions
  if(sum(df$now>0)){
    
    df$t_exposed[df$now] = t
    df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp = mult_asymp)
    df.u = df[df$now,]
    
    # set up notification
    if(notify){
      for(k in 1:nrow(class_quarantine)){
        class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%df.u$class,
                                          max(df.u$t_notify[class_quarantine$class[k]%in%df.u$class]), class_quarantine$hold[k])
      }
      class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + 17,
                                         class_quarantine$hold, class_quarantine$t_notify)
      #print(class_quarantine)
    }
    
  }
  
  ## group testing
  if(test & t%in%testing_days){
    df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
    df$t_end_inf = ifelse(df$inf & df$test, t, df$t_end_inf)
    df$t_notify = ifelse(df$inf & df$test, t, df$t_end_inf)
    df$detected = ifelse(df$inf & df$test, 1, df$detected)
    
    df.u = df %>% filter(inf & test)
    # set up notification
    if(notify){
      for(k in 1:nrow(class_quarantine)){
        hold = class_quarantine$hold[k] 
        class_quarantine$hold[k] = ifelse(class_quarantine$class[k]%in%df.u$class,
                                          max(df.u$t_notify[class_quarantine$class[k]%in%df.u$class]), class_quarantine$hold[k])
      }
      class_quarantine$t_notify = ifelse(class_quarantine$hold > class_quarantine$t_notify + 17,
                                         class_quarantine$hold, class_quarantine$t_notify)
    }
    
  }
  #print(t); print(class_quarantine)
  #if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)
  
}

######################################
# Error when t is extended past time #
######################################

###########################################
# Error in run_household for rbinom draw? #
###########################################