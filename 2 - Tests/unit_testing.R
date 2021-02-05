#Load packages
library(tidyverse)
library(Hmisc)
library(igraph)
library(devtools)

install_github("abilinski/BackToSchool2", subdir = "1 - R package/BackToSchool")
library(BackToSchool)

#Load data
data(synthMaryland, package = "BackToSchool")
synthMD <- synthpop
remove(synthpop)
data(synthMaryland_HS, package = "BackToSchool")
synthMD_HS <- synthpop_HS
remove(synthpop_HS)

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
school_default <- make_school(synthpop = synthMD)
test_make_school(school_default)
###PASS###

###Basic High School###
school_default_HS <- make_school(synthpop = synthMD_HS)
test_make_school(school_default_HS, base_pop = synthMD_HS)
###PASS###

###School with Family###
school_family <- make_school(synthpop = synthMD, includeFamily = TRUE)
test_make_school(school_family, includeFamily = TRUE)
###PASS###

###School with 0 classes per grade###
school_noclass <- make_school(synthpop = synthMD, n_class = 0)
test_make_school(school_noclass, n_class = 0)
###SHOULD BREAK BUT DOES NOT###

###School with 10 classes per grade###
school_10class <- make_school(synthpop = synthMD, n_class = 10)
test_make_school(school_10class, n_class = 10)
###PASS###

###School with 100 classes per grade###
school_100class <- make_school(synthpop = synthMD, n_class = 100)
test_make_school(school_100class, n_class = 100)
###make_school function breaks###

###################################
# TEST initialize_school FUNCTION #
###################################

###Try to change transmission probability###
init_school1 <- initialize_school(start = school_default)
describe(init_school1$class_trans_prob)

init_school2 <- initialize_school(start = school_default, dedens = F)
describe(init_school2$class_trans_prob)
###class_trans_prob set to 0 if dedens = F###

###Make sure susceptibility parameters are changeable###
init_school4 <- initialize_school(start = school_family, teacher_susp = 0.75)
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
###################################################################################

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
init_school5 <- initialize_school(start = school_default, attack = 1, child_susp = 1)

#Index Infection ID:
start_id <- init_school5$id[which(duplicated(init_school5$HH_id))[1]]
print(start_id)

#All Family IDs:
init_school5$id[which(init_school5$HH_id == init_school5$HH_id[which(duplicated(init_school5$HH_id))[1]])]

#Compare to run_household result
run_household(start_id, school_prep(init_school5, schedule_default, 1))
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
###NOTE: Actual infections slightly larger than expected since "floor" is used for family size###

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
###NOTE: Actual infections slightly larger than expected since "floor" is used for family size###

###################################################################################
# TEST run_class FUNCTION                                                         #
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
init_school_default <- initialize_school(start = school_default_HS, attack = 1)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 1, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Medium Attack Rate###
init_school_default <- initialize_school(start = school_default_HS, attack = 0.5)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.5, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Low Attack Rate###
init_school_default <- initialize_school(start = school_default_HS, attack = 0.01)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0.01, replicates = 100, high_school = TRUE)
###PASS###

###Test transmission, high school, Zero Attack Rate###
init_school_default <- initialize_school(start = school_default_HS, attack = 0)

test_run_class(initialized_school = init_school_default, percent_infected = 0.1, attack_rate = 0, replicates = 100, high_school = TRUE)
###PASS###

###NOTE: Above tests are robust to changes in percent_infected###

###################################################################################
# TEST run_rand FUNCTION                                                          #
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
###################################################################################

school_default_attack1_t1 <- school_prep(initialize_school(start = school_default, attack = 1, rel_trans = 1), schedule = schedule_default, 1)

###Make sure run_staff_rand correctly infects all staff when attack_rate = 1 and n_contacts is set to size of staff###

print(paste("Number of Staff Infected:", sum(unique(run_staff_rand(school_default_attack1_t1$id[which(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))[1]], school_default_attack1_t1, sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family)), rel_trans_adult = 1))>0)))
print(paste("Number of previously uninfected:", sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family))-1))

###PASS, but rel_trans_adult must be low enough so probability of infection per contact <= 1###

###Make sure student cannot infect staff through run_staff_rand###

print(paste("Number of Staff Infected:", sum(unique(run_staff_rand(1, school_default_attack1_t1, sum(school_default_attack1_t1$adult & school_default_attack1_t1$present & !(school_default_attack1_t1$family)), rel_trans_adult = 1))>0)))

###FAIL, but run_model only passes ID if infected individual is a staff member###

test_run_staff_rand <- function(initialized_school, percent_infected, attack_rate, replicates, n_contacts){
  
  number_infected <- c()
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id[which(initialized_school$adult & initialized_school$present & !(initialized_school$family))], percent_infected*length(initialized_school$id[which(initialized_school$adult & initialized_school$present & !(initialized_school$family))]))
    new_infected <- c()
    
    for(a in infected){
      new_infected <- c(new_infected, run_staff_rand(a, initialized_school, n_contacts, rel_trans_adult = 1))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  staff_present <- sum(initialized_school$adult & initialized_school$present & !(initialized_school$family))
  
  average_susp <- mean(initialized_school[which(initialized_school$adult & initialized_school$present & !(initialized_school$family)),]$susp)
  
  average_rel_trans <- mean(initialized_school[which(initialized_school$adult & initialized_school$present & !(initialized_school$family)),]$relative_trans)
  
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
###################################################################################

###Make sure run_care correctly infects all households when attack_rate = 1 and all households not present in-school contact each other###

init_school_family <- initialize_school(start = school_family, child_susp = 1, attack = 1)
school_family_scheduled <- school_prep(init_school_family, make_schedule(df = init_school_family), 6)

len <- length(unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]))
n_HH <- length(unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]))

# contacts from childcare
care_contacts <- data.frame(HH_id = unique(school_family_scheduled$HH_id[!school_family_scheduled$adult & !school_family_scheduled$present]), cat = sample(rep(1:ceiling(len/n_HH), each = n_HH)[1:len]))

print(paste("Number of Individuals Infected:", sum((unique(run_care(1, school_family_scheduled, care_contacts, rel_trans_CC = 1, num_adults = 1000)))>0)))
print(paste("Number of previously uninfected:", sum(!school_family_scheduled$present & !school_family_scheduled$family_staff & !(school_family_scheduled$adult & !school_family_scheduled$family) & school_family_scheduled$HH_id!=school_family_scheduled$HH_id[school_family_scheduled$id == 1])))

###PASS###

test_run_care <- function(initialized_school, percent_infected, attack_rate, replicates, n_HH){
  
  number_infected <- c()
  len <- length(unique(initialized_school$HH_id[!initialized_school$adult & !initialized_school$present]))
  
  for(i in 1:replicates){
    infected <- sample(initialized_school$id, percent_infected*length(initialized_school$id))
    new_infected <- c()
    care_contacts <- data.frame(HH_id = unique(initialized_school$HH_id[!initialized_school$adult & !initialized_school$present]), cat = sample(rep(1:ceiling(len/n_HH), each = n_HH)[1:len]))
    for(a in infected){
      new_infected <- c(new_infected, run_care(a, initialized_school, care_contacts, rel_trans_CC = 1))
    }
    number_infected <- c(number_infected, length(setdiff(setdiff(new_infected, infected),0)))
  }
  
  average_susp <- mean(initialized_school[which(!initialized_school$present & !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),]$susp)
  
  average_nonhh_group_size <- mean(count(group_by(merge(initialized_school[which(!initialized_school$present & !initialized_school$adult),], care_contacts, by = c("HH_id")), cat))$n) + min(mean(count(group_by(merge(initialized_school[which(!initialized_school$present & initialized_school$adult),], care_contacts, by = c("HH_id")), cat))$n),2) - mean(count(group_by(initialized_school[which(!initialized_school$present & !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),]), HH_id)$n)
  
  number_atrisk <- nrow(initialized_school[which(!initialized_school$present & !initialized_school$adult),]) + min(nrow(initialized_school[which(!initialized_school$present & initialized_school$adult& !initialized_school$family_staff & !(initialized_school$adult & !initialized_school$family)),]), 2*len/n_HH)
  
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

###NOTE: Tests above are robust to changes in percent_infected and n_HH. Expected infected is slightly higher than actual, since ceiling function is used.###

######################################################################################################
# Bookkeeping for run_model                                                                          #
#                                                                                                    #
# Make sure output of each step is reasonable with defaults and does not produce errors in next step #
######################################################################################################

#Create School
school <- make_school(synthpop = synthMD, includeFamily = TRUE)
df <- initialize_school(start = school, dedens = TRUE, run_specials = TRUE, attack = 0.5)

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
high_school = F
nper = 8
start_mult = 1
start_type = "mix"
test_type = "all"
adult_prob = 0.013
child_prob = 0.056
quarantine.length = 10
quarantine.grace = 3
rel_trans_CC = 2
rel_trans_adult = 2
num_adults = 2
bubble = F
include_weekends = T

#Start run_model steps
time_seed_inf <- sample(1:14, 1)
print(time_seed_inf)
id.samp <- sample(df$id[!df$family], n_start, prob = (df$adult[!df$family]*start_mult+1)/(sum(df$adult[!df$family]*(start_mult+1)) + sum(!df$adult)))
print(id.samp)

##Test other types of seeding##################################################
id.samp_adult = sample(df$id[!df$family & df$adult], n_start)
print(id.samp_adult)
id.samp_teacher = sample(df$id[!df$family & df$adult & df$class!=99], n_start)
print(id.samp_teacher)
id.samp_child = sample(df$id[!df$family & !df$adult], n_start)
print(id.samp_child)

##Set-up quarantine
class_quarantine = data.frame(class = unique(df$class[df$class!=99]), t_notify = -quarantine.grace-quarantine.length, hold = -quarantine.grace-quarantine.length)
head(class_quarantine)
mat = matrix(NA, nrow = max(df$id), ncol = time)
head(mat)

##set actual seeds
df[df$id%in%id.samp,] = make_infected(df.u = df[df$id%in%id.samp,], days_inf = days_inf,
                                      set  = time_seed_inf, seed_asymp = seed_asymp, mult_asymp = mult_asymp)
df$start = df$id %in% id.samp
df$id[df$start] #Matches id.samp

df.u = df[df$id%in%id.samp,]

##Test days
testing_days = seq(7, (time+15), by = 7)
print(testing_days)

##Testing
df$test_type = !df$family

df$uh.oh = 0

##Test first run through time for loop

t <- time_seed_inf

###Class Quarantines
classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + quarantine.length-1),]
print(classes_out)

###Present
df$present = sched$present[sched$t==t] & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class]
print(df$id[df$present])

df$not_inf = df$t_exposed==-1 | df$t_exposed > t # if exposed from community, can be exposed earlier
print(df$id[df$not_inf])
print(df$id[!df$not_inf])

df$present_susp = df$present & df$not_inf
print(df$id[df$present_susp])

df$quarantined = df$quarantined + as.numeric(df$class%in%classes_out$class & sched$present[sched$t==t])
print(df$id[df$quarantined])
df$quarantined2 = df$quarantined + as.numeric(df$class%in%classes_out$class)
print(df$id[df$quarantined2])
df$quarantined_now = df$class%in%classes_out$class & sched$present[sched$t==t]
print(df$id[df$quarantined_now])

# check who is present
mat[,(t-time_seed_inf+1)] = df$present
head(mat)

# infectious
df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t & !df$family
print(df$id[df$inf_home])
df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
print(df$id[df$inf])

# infectious and at school
df$trans_now = df$present & df$inf & !df$family
print(df$id[df$trans_now])
df$trans_outside = !df$present & df$inf & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)
print(df$id[df$trans_outside])

# set infections to 0 for this timestep
df$now = F

## group testing
if(test & t%in%testing_days){
  #print(test); print(t); print(testing_days)
  #print(t)
  df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
  df$t_end_inf = ifelse(df$inf & df$test, t, df$t_end_inf)
  df$t_notify = ifelse(df$inf & df$test, t, df$t_end_inf)
  df$detected = ifelse(df$inf & df$test, 1, df$detected)
  print(df$id[df$detected == 1])
  
  # set up notification
  df.u = df %>% filter(inf & test)
  if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)
  head(class_quarantine)
  }
  
}

#### SELECT NEXT GENERATION INFECTIONS ####
# run model for infectious individuals at home
if(sum(df$inf_home)>0) {
  
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
    
    # flag people infected at this time step
    df$now = ifelse(df$id%in%inf_vec, T, df$now)
    print(df$id[df$now])
    df$source = ifelse(df$id%in%inf_vec, a, df$source)
    print(df$id[df$source == a])
    df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
    print(df$id[df$source_symp])
    df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
    print(df$id[df$not_inf])
    df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
    print(df$id[df$present_susp])
  }
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
  print(head(specials))
  
  ##############################################################################################
  # Specials are always run? There seems to be a default to always have five specials teachers #
  # Also, why isn't every class assigned to a specials teacher?                                #
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
      rand_staff_trans = run_staff_rand(a, df, n_staff_contact, rel_trans_adult)
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
    df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
    print(df$id[df$source_symp])
    df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
    print(df$id[df$not_inf])
    df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
    print(df$id[df$present_susp])
    
  }
}

# run model for infectious individuals OUTSIDE school
if(sum(df$trans_outside)>0 & n_HH>0 & (include_weekends | !sched$day[sched$t==t][1]%in%c("Sa", "Su"))) {
  
  if(!bubble){
    len = length(unique(df$HH_id[!df$adult & !df$present]))
    tot = ifelse(ceiling(len/n_HH)==0, 1, ceiling(len/n_HH))
    if(len==0){HHs = 0}else{HHs = unique(df$HH_id[!df$adult & !df$present])}
    care_contacts = data.frame(HH_id = HHs,
                               cat = sample(rep(1:tot, each = n_HH)[1:len]))
    print(head(care_contacts))
  }
  
  # run transmission in care groups
  non_school_infs = df$id[df$trans_outside]
  print(non_school_infs)
  if(sum(df$trans_outside > 1)) school_infs = sample(non_school_infs)
  
  # choose contacts that become infected
  for(a in non_school_infs){
    
    # CARE CONTACTS
    care_trans = run_care(a, df, care_contacts, rel_trans_CC, num_adults = num_adults)
    df$location[df$id%in%care_trans] = "Child care"
    
    # return id if person is infected
    # and 0 otherwise
    inf_vec = c(care_trans)
    print(inf_vec)
    
    # add to total # of infections from this person
    df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
    print(df$tot_inf[df$id==a])
    
    # flag people infected at this time step
    df$now = ifelse(df$id%in%inf_vec, T, df$now)
    print(df$id[df$now])
    df$source = ifelse(df$id%in%inf_vec, a, df$source)
    print(df$id[df$source == a])
    df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
    print(df$id[df$source_symp])
    df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
    print(df$id[df$not_inf])
    df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
    print(df$id[df$present_susp])
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
  if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)}
  df$uh.oh = df$uh.oh + sum(df$source[df$now]%in%(df$id[df$class%in%classes_out$class]) & df$location[df$now]!="Household")>0
  #print("New exposures:")
  #print(df %>% filter(now) %>% arrange(source) %>% select(id, HH_id, class, group, adult, family, source, location, symp))
}

for(t in (time_seed_inf+1):(time_seed_inf+time-1)){
  #print(paste("Time:", t, sched$day[sched$t==t][1], sched$group_two[sched$t==t][1]))
  
  # class quarantines
  classes_out = class_quarantine[class_quarantine$t_notify > -1 & class_quarantine$t_notify <= t & t <= (class_quarantine$t_notify + quarantine.length-1),]
  
  # present
  df$present = sched$present[sched$t==t] & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class]
  if(high_school & nrow(classes_out)>0){
    df$nq = !unlist(lapply(classes.ind, function(a) sum(a %in% classes_out$class)>0))
    df$present =  sched$present[sched$t==t] & df$nq
  }
  df$not_inf = df$t_exposed==-1 | df$t_exposed > t # if exposed from community, can be exposed earlier
  df$present_susp = df$present & df$not_inf
  df$quarantined = df$quarantined + as.numeric(df$class%in%classes_out$class & sched$present[sched$t==t])
  df$quarantined2 = df$quarantined + as.numeric(df$class%in%classes_out$class)
  df$quarantined_now = df$class%in%classes_out$class & sched$present[sched$t==t]
  
  # check who is present
  mat[,(t-time_seed_inf+1)] = df$present
  
  # infectious
  df$inf_home = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf_home >= t & !df$family
  df$inf = df$t_inf > -1 & df$t_inf <= t & df$t_end_inf >= t
  
  # infectious and at school
  df$trans_now = df$present & df$inf & !df$family
  df$trans_outside = !df$present & df$inf & !df$class%in%classes_out$class & !df$HH_id%in%df$HH_id[df$class%in%classes_out$class] & (!df$adult | df$family)#& !df$family
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
    df$test = rbinom(nrow(df), size = 1, prob = test_sens*test_frac*as.numeric(df$test_type))
    df$t_end_inf = ifelse(df$inf & df$test, t, df$t_end_inf)
    df$t_notify = ifelse(df$inf & df$test, t, df$t_end_inf)
    df$detected = ifelse(df$inf & df$test, 1, df$detected)
    
    # set up notification
    df.u = df %>% filter(inf & test)
    if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)}
    
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
        rand_staff_trans = run_staff_rand(a, df, n_staff_contact, rel_trans_adult)
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
      df$source_symp = ifelse(df$id%in%inf_vec, df$symp[df$id==a], df$source_symp)
      df$not_inf = ifelse(df$id%in%inf_vec, F, df$not_inf)
      df$present_susp = ifelse(df$id%in%inf_vec, F, df$present_susp)
      
    }
  }
  
  # run model for infectious individuals OUTSIDE school
  if(sum(df$trans_outside)>0 & n_HH>0 & (include_weekends | !sched$day[sched$t==t][1]%in%c("Sa", "Su"))) {
    
    if(!bubble){
      len = length(unique(df$HH_id[!df$adult & !df$present]))
      tot = ifelse(ceiling(len/n_HH)==0, 1, ceiling(len/n_HH))
      if(len==0){HHs = 0}else{HHs = unique(df$HH_id[!df$adult & !df$present])}
      care_contacts = data.frame(HH_id = HHs,
                                 cat = sample(rep(1:tot, each = n_HH)[1:len]))
    }
    
    # run transmission in care groups
    non_school_infs = df$id[df$trans_outside]
    if(sum(df$trans_outside > 1)) school_infs = sample(non_school_infs)
    
    # choose contacts that become infected
    for(a in non_school_infs){
      
      # CARE CONTACTS
      care_trans = run_care(a, df, care_contacts, rel_trans_CC, num_adults = num_adults)
      df$location[df$id%in%care_trans] = "Child care"
      
      # return id if person is infected
      # and 0 otherwise
      inf_vec = c(care_trans)
      
      # add to total # of infections from this person
      df$tot_inf[df$id==a] = df$tot_inf[df$id==a] + sum(inf_vec>0)
      
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
    
    df$t_exposed[df$now] = t
    df[df$now,] = make_infected(df[df$now,], days_inf = days_inf, mult_asymp = mult_asymp)
    df.u = df[df$now,]
    
    # set up notification
    if(notify){class_quarantine = make_quarantine(class_quarantine, df.u, quarantine.length = quarantine.length, quarantine.grace = quarantine.grace, hs = high_school, hs.classes = hs.classes)}
    df$uh.oh = df$uh.oh + sum(df$source[df$now]%in%(df$id[df$class%in%classes_out$class]) & df$location[df$now]!="Household")>0
    #print("New exposures:")
    #print(df %>% filter(now) %>% arrange(source) %>% select(id, HH_id, class, group, adult, family, source, location, symp))
  }
  
  #print(t); print(class_quarantine)
  #if(sum(class_quarantine$t_notify!=-1)>0) print(class_quarantine)
  
}
# remember to add mat back in
#print(df$id[df$t_exposed!=-1 & df$class==df$class[df$start]])
#print(sum(df$t_exposed!=-1))
print(head(df)) #, time_seed_inf, class_quarantine, mat))

###########################################
# Error in run_household for rbinom draw? #
###########################################