####************************** SIMULATIONS **************************#### 

#### DYNAMIC ELEMENTARY SENSITIVITY ANALYSIS ####
source("functions.R")

setwd(paste0(wd, "Dynamic_Elem_Sens2"))
file.remove(list.files())
df_ELEM = make_df(n_tot = 1000, start_type = "cont", n_HH = c(0, 5, 10),
                  scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                  prob = c(1,10,25,50,100)*3/100000, time = 60,
                  child_trans = .5, child_susp = .5, high_school = F,
                  p_asymp_adult = .2, p_asymp_child = 0,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  mult_asymp = .5,
                  n_other_adults = 30, n_class = 5) 
set.seed(3232)
class = make_school(synthpop = synthpop, n_other_adults = df_ELEM$n_other_adults[1], 
                    includeFamily = T, n_class = df_ELEM$n_class[1])
run_parallel(df_ELEM, synthpop, class = class)
