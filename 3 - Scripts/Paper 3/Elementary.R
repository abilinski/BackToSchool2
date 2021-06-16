####************************** SIMULATIONS **************************#### 

# source files
source(functions.R)

# local
wd = "/n/home00/abilinski/Schools/P3_16_Jun"

# cluster
setwd(wd)

# simulation setup
nsamp = 1000
df_ELEM = data.frame(attack=runif(nsamp, .005, .035),
                      child_susp = runif(nsamp, .1, .9)*.9,
                      teacher_susp = runif(nsamp, .2, 1)*.9,
                      adult_susp = teacher_susp,
                      prob = runif(nsamp,1,50)*3/100000,
                      notify = sample(c(T,F), nsamp, replace = T),
                     test = sample(c(T,F), nsamp, replace = T),
                     test_frac = runif(nsamp, .5, .9)) %>%
  mutate(start_type = "cont", n_HH = 2, test_days = "week",
         test_type = "all", test_sens = .9,
         child_trans = 1, high_school = F,
         p_asymp_adult = .2, p_asymp_child = 0,
         p_subclin_adult = .2, p_subclin_child = .8,
         mult_asymp = .5, quarantine.length = c(7),
         turnaround.time = 1, n_other_adults = 30, n_class = 5,
         test_quarantine = F,  disperse_transmission = c(F), dedens = 1, n_start = 1, 
         days_inf = 5, time = 30, n_contacts = 20,
         n_staff_contact = 10, rel_trans = 1/8, start_mult = 0, prob = 0, disperse = T, 
         test_start_day = 1) %>%
  expand_grid(scenario = c("Base case", "Remote"))


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
