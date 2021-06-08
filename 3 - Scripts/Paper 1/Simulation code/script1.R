####************************** SIMULATIONS **************************#### 
#### BASE CASES ####
source("functions.R")
source("abm7.R")

#### ELEMENTARY SCHOOL BASE ####
setwd(paste0(wd, "Base_Elem2"))

# remove files
file.remove(list.files())
df_ELEM = make_df(n_tot = 2000, start_type = c("mix", "teacher"), teacher_susp = c(.33,1),
                  child_trans = .5, child_susp = .5, high_school = F, 
                  p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                  p_subclin_adult = .2, p_subclin_child = .8,
                  n_other_adults = 30, n_class = 5, n_HH = 2)
run_parallel(df_ELEM, synthpop)

#### HIGH SCHOOL BASE ####
# set working directory
setwd(paste0(wd, "Base_HS2"))

# remove files
file.remove(list.files())

# choose parameter set
df_HS = make_df(n_tot = 2000, n_class = 16, high_school = T, teacher_susp = c(.33,1),
                p_asymp_adult = .2, p_asymp_child = .4,
                p_subclin_adult = .2, p_subclin_child = .4,
                mult_asymp = .5,
                n_HH = 2, start_type = "mix") 

# run code
run_parallel(df_HS, synthpop_HS)



