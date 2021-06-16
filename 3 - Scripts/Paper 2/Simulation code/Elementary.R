####************************** SIMULATIONS **************************#### 

# source files
source(functions.R)

# local
wd = "/n/home00/abilinski/Schools/ES_16_Jun_1"

# cluster
setwd(wd)

# number of  simulations
n_tot = 1

# simulation setup
df_ELEM = make_df(n_tot = n_tot*2, start_type = "cont", n_HH = 2,
                  test_days = c("week", "2x_week"), 
                  test_type = c("all"),
                  test_frac = c(.7, .9),
                  test_sens = .9,
                  scenario = c("Base case"), teacher_susp = c(.2,1),
                  prob = c(1,5,10,25,50,100)*3/100000, time = 30,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5, quarantine.length = c(7,10),
                  turnaround.time = c(1),
                  n_other_adults = 30, n_class = 5, 
                  test_quarantine = test_q, notify = notify_val)

df_ELEM_SURV = make_df(n_tot = n_tot, start_type = "cont", n_HH = 2,
                  test_days = c("week"), 
                  test_type = c("all"),
                  test_frac = c(0.05, 0.1, 0.2),
                  test_sens = .8,
                  scenario = c("Base case"), teacher_susp = c(.2, .5),
                  prob = c(1,5,10,25)*3/100000, time = 30,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5, quarantine.length = c(7), attack = c(0.02, 0.04),
                  turnaround.time = c(1),
                  n_other_adults = 30, n_class = 5, 
                  test_quarantine = test_q, notify = notify_val) 

df_ELEM_ADD = make_df(attack = .02, n_tot = 4000, start_type = "cont", n_HH = 2,
                  test_days = c("week", "2x_week"),
                  test_type = c("all"),
                  test_frac = c(.7, .9),
                  test_sens = .9,
                  scenario = c("Base case"), teacher_susp = c(.2,1),
                  prob = c(1,5,10)*3/100000, time = 30,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5, quarantine.length = c(7,10),
                  turnaround.time = c(1),
                  n_other_adults = 30, n_class = 5, 
                  test_quarantine = test_q, notify = notify_val)

df_ELEM1 = make_df(scenario = c("A/B (2)"), attack = .02, 
                   n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_sens = .9, test = F, teacher_susp = c(.2,1),
                   prob = c(1,5,10,25,50,100)*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = 10,
                   turnaround.time = c(1),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val)

df_ELEM2 = make_df(attack = .02, notify = F, test = F,
                   n_tot =n_tot, start_type = "cont", n_HH = 2,
                   scenario = c("Remote"), teacher_susp = c(.2, 1),
                   prob = c(1,5,10,25,50,100)*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = 10, turnaround.time = c(1),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q) 

df_SENS1 = make_df(attack = .02, n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.2),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(2),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val) 

df_SENS2 = make_df(attack = c(.01, .03), n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case", "A/B (2)"), teacher_susp = c(.2),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(1),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val) 

df_SENS3 = make_df(attack = .01, days_inf = 10,
                   n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case",  "A/B (2)"), teacher_susp = c(.2),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(1),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val) 

df_SENS4 = make_df(attack = .02, n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("staff"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.2),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(1),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val) 

df_SENS5 = make_df(attack = .02, n_tot = n_tot, start_type = "cont", n_HH = 2,
                   test_days = c("week"), 
                   test_type = c("all"),
                   test_frac = .9,
                   test_sens = .9,
                   scenario = c("Base case"), teacher_susp = c(.2),
                   prob = 25*3/100000, time = 30,
                   child_trans = .5, child_susp = .5, high_school = F,
                   p_asymp_adult = .2, p_asymp_child = 0,
                   p_subclin_adult = .2, p_subclin_child = .8,
                   mult_asymp = .5, quarantine.length = c(7,10),
                   turnaround.time = c(1),
                   test_start_day = c(1:7),
                   n_other_adults = 30, n_class = 5, 
                   test_quarantine = test_q, notify = notify_val) 

df_ELEM = rbind(df_ELEM, df_ELEM_SURV, df_ELEM_ADD, df_ELEM1, df_ELEM2, df_SENS1, df_SENS2, df_SENS3, df_SENS4, df_SENS5) %>% mutate(i = row_number())

# don't double run
nums = data.frame(results =  0) 
if(length(list.files())>0) nums = data.frame(id = list.files()) %>% separate(id, into = c("results", "date"), sep = "_") %#>% mutate(results = sub("results", "", results), results = as.numeric(results))
df_ELEM = df_ELEM %>% filter(!i%in%nums$results)
print(dim(df_ELEM))

# make class
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])

# run code
run_parallel(df_ELEM, synthpop, class = class)
