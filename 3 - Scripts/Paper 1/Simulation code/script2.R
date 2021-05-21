####************************** SIMULATIONS **************************#### 

#### SUPPLEMENTS ####
source("functions.R")

n_supp = 2000

tic()
#### ELEMENTARY SCHOOL ####

#### EQUAL TRANS, 5 seconds
setwd(paste0(wd, "Elem_supp1_1"))
file.remove(list.files())
df_ELEM_supp1 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = 1, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = .4, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .4,
                        start_type = "mix",
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp1, synthpop)

