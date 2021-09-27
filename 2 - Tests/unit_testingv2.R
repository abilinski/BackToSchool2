library(BackToSchool)

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

chk_vacc = function(a){
  
}

#*************************** TEST MAKE_SCHEDULE *********************************#

#*************************** TEST MULT_RUNS *************************************#
#* Is every argument passed to every subfunction?
# make_school = 4
# initialize_school = 19
# make_schedule = 4
# run_model = 63

#* How many days are you keeping?

#*************************** ADDITIONAL TESTS *********************************#

