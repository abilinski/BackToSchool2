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

#### ALTERNATIVE SCHEDULES, 5 seconds
setwd(paste0(wd, "Elem_supp2_1"))
file.remove(list.files())
df_ELEM_supp2 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "mix",
                        scenario = c("A/B (2)", "A/B (1)", "On/off (2)", "On/off (1)"),
                        n_other_adults = 30, n_class = 5) %>% mutate(total_days = ifelse(grepl("A/B", scenario) & total_days==2, 2.2, total_days))
run_parallel(df_ELEM_supp2, synthpop)

#### OVERDISPERSION, 5 seconds
setwd(paste0(wd, "Elem_supp3_1"))
file.remove(list.files())
df_ELEM_supp3 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "mix",
                        disperse_transmission = T,
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp3, synthpop)


#### START TYPE TEACHER
setwd(paste0(wd, "Elem_supp5_1"))
file.remove(list.files())
df_ELEM_supp5 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                        child_trans = .5, child_susp = .5, high_school = F, 
                        p_asymp_adult = .2, p_asymp_child = 0, mult_asymp = .5,
                        p_subclin_adult = .2, p_subclin_child = .8,
                        start_type = "teacher",
                        n_other_adults = 30, n_class = 5)
run_parallel(df_ELEM_supp5, synthpop)

# HIGH SCHOOL SUPPLEMENTAL ANALYSES

# SUSCEPTIBLE - 8 seconds
setwd(paste0(wd, "HS_supp1_1"))
file.remove(list.files())
df_HS_supp1 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 0.5,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp1, synthpop_HS)

# SCENARIOS - 7 seconds
setwd(paste0(wd, "HS_supp2_1"))
file.remove(list.files())
df_HS_supp2 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 1,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      scenario = c("A/B (2)", "A/B (1)", "On/off (2)", "On/off (1)"),
                      n_class = 16, high_school = T) %>% mutate(total_days = ifelse(grepl("A/B", scenario) & total_days==2, 2.2, total_days))

run_parallel(df_HS_supp2, synthpop_HS)

# DISPERSION - 10 seconds
setwd(paste0(wd, "HS_supp3_1"))
file.remove(list.files())
df_HS_supp3 = make_df(n_tot = n_supp, teacher_susp = c(.33,1),
                      start_type = "mix", child_susp = 1,
                      p_asymp_adult = .2, p_asymp_child = .4,
                      p_subclin_adult = .2, p_subclin_child = .4,
                      mult_asymp = .5,
                      disperse_transmission = T,
                      n_class = 16, high_school = T)
run_parallel(df_HS_supp3, synthpop_HS)
toc()