#************************ Transmission function tests ************************#
#                                                                             #
#                                                                             #
#                                                                             #
# This code runs unit tests on transmission function in abm6.R.               #
#*****************************************************************************#

# General strategy:
# Set probability to 0 - check none
# Set probability to 1 - check all
# Run a bunch of times and check that the expectation seems right
# Vary for each type in the model:
    # student
    # teacher
    # staff
    # parent
    # family member of staff/teacher

# These could be cleaner, compressed, but for the sake of convenience,
# I have not made these changes.

#### SETUP ####
library(igraph)
library(tidyverse)
set.seed(2)

# make a class 
class = make_school(synthpop = synthMD, includeFamily = T)

# make a school
df = initialize_school(start = class) %>% mutate(not_inf = T, present = T)

# identify someone of each type
a = c(1, 554, 600, 581, 1417)
trials = 100

#### HOUSEHOLDS ####
for(i in 1:length(a)){

    # set transmission probability to 1
    z1 = run_household(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, susp = 1))
    z2 = df$id[df$HH_id==df$HH_id[df$id==a[i]] & df$id!=a[i]]
    print(mean(z1 == z2)) # should be equal (T)
    
    # set transmission probability to 0
    z3 = run_household(a[i], df = df %>% mutate(class_trans_prob = 0, disperse_transmission = F, susp = 1))
    print(mean(z3)) # should be zero
    
    # make people already infected
    z4 = run_household(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, susp = 1, not_inf = F))
    print(mean(z4)) # should be zero
    
    # change whether people are present
    z5 = run_household(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, susp = 1, present = F))
    print(mean(z5==z2)) # should be 1
    
    # run probabilistic options
    val = rep(0, trials)
    val2 = rep(0, trials)
    for(j in 1:trials){
        
        # set transmission probability to 0.5
        z6 = run_household(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = 1))
        val[j] = mean(z6>0)
        
        # set transmission probability to 0.5 + susceptibility to 0.5
        z7 = run_household(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = .5))
        val2[j] = mean(z7>0)
    }
    print(mean(val)) # should be about .5
    print(mean(val2)) # should be about .25
    
}

#### SCHOOLS ####
# these should only pan out if you have a class (i.e. i in 1,2)

## NOT HIGH SCHOOL
for(i in 1:length(a)){
    
    # set transmission probability to 1
    z1 = run_class(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, susp = 1, present_susp = T))
    if(df$class[df$id==a[i]]==99) {
        z2 = 0
        } else {z2 = df$id[df$class==df$class[df$id==a[i]] & df$id!=a[i]]}
    print(mean(z1 == z2)) # should be equal (T)
    
    # set transmission probability to 0
    z3 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0, disperse_transmission = F, susp = 1, present_susp = T))
    print(mean(z3)) # should be zero
    
    # make people already infected
    z4 = run_class(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, susp = 1, not_inf = F, present_susp = F))
    print(mean(z4)) # should be zero
    
    # run probabilistic options
    val = rep(0, trials)
    val2 = rep(0, trials)
    for(j in 1:trials){
        
        # set transmission probability to 0.5
        z6 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = 1, present_susp = T))
        val[j] = mean(z6>0)
        
        # set transmission probability to 0.5 + susceptibility to 0.5
        z7 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = .5, present_susp = T))
        val2[j] = mean(z7>0)
    }
    print(mean(val)) # should be about .5
    print(mean(val2)) # should be about .25
    
}

## HIGH SCHOOL

# set up scheduling if high school
hs.classes = NA
if(high_school){
    hs.classes = data.frame(period = numeric(), class = numeric(), id = numeric())
    for(p in 1:nper){
        temp = data.frame(period = p, class = sample(df$class[df$class!=99 & df$present]), id = df$id[df$class!=99 & df$present])
        hs.classes = hs.classes %>% bind_rows(temp)
    }
}

for(i in 1:length(a)){
    
    # set transmission probability to 1
    z1 = run_class(a[i], df = df %>% mutate(class_trans_prob = 1, relative_trans = 1, disperse_transmission = F, susp = 1, present_susp = T), high_school = T, hs.classes = hs.classes)
    if(df$class[df$id==a[i]]==99) {
        z2 = 0
    } else {z2 = hs.classes$id[hs.classes$class==hs.classes$class[hs.classes$id==a[i]] & hs.classes$id!=a[i]]}
    print(mean(sort(unique(z1)) == sort(unique(z2)))) # should be equal (T)
    
    # set transmission probability to 0
    z3 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0, disperse_transmission = F, relative_trans = 1, susp = 1, present_susp = T), high_school = T, hs.classes = hs.classes)
    print(mean(z3)) # should be zero
    
    # make people already infected
    z4 = run_class(a[i], df = df %>% mutate(class_trans_prob = 1, disperse_transmission = F, relative_trans = 1, susp = 1, not_inf = F, present_susp = F), high_school = T, hs.classes = hs.classes)
    print(mean(z4)) # should be zero
    
    # set transmission probability to 0
    z5 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0, disperse_transmission = F, relative_trans = 0, susp = 1, present_susp = T), high_school = T, hs.classes = hs.classes)
    print(mean(z5)) # should be zero
    
    # run probabilistic options
    val = rep(0, trials)
    val2 = rep(0, trials)
    for(j in 1:trials){
        
        # set transmission probability to 0.5
        z6 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = 1, present_susp = T, relative_trans = 1), high_school = T, hs.classes = hs.classes)
        val[j] = mean(z6>0)
        
        # set transmission probability to 0.5 + susceptibility to 0.5
        z7 = run_class(a[i], df = df %>% mutate(class_trans_prob = 0.5, disperse_transmission = F, susp = .5, present_susp = T, relative_trans = 1), high_school = T, hs.classes = hs.classes)
        val2[j] = mean(z7>0)
    }
    print(mean(val)) # should be about .5
    print(mean(val2)) # should be about .25
    
}



