####************************** SIMULATIONS **************************#### 

#### DYNAMIC HIGH ####
wd = ""
setwd(wd)
source("functions.R")

setwd(paste0(wd, "Dynamic_High2"))
#file.remove(list.files())
df_HS = make_df(n_tot = 1000, n_class = 16, high_school = T, n_HH = 2,
                start_type = "cont",
                scenario = c("Base case", "A/B (2)", "Remote"), teacher_susp = c(.33,1),
                prob = c(1,10,25,50,100)*3/100000, time = 60)

#nums = data.frame(results =  0)
#if(length(list.files())>0) nums = data.frame(id = list.files()) %>% separate(id, into = c("res#ults", "date"), sep = "_") %>% mutate(results = sub("results", "", results), results = as.nume#ric(results))
#df_HS = df_HS %>% filter(!i%in%nums$results)
print(dim(df_HS))

set.seed(3232) 
class = make_school(synthpop = synthpop_HS, n_other_adults = df_HS$n_other_adults[1], 
                    includeFamily = T, n_class = df_HS$n_class[1])
run_parallel(df_HS, synthpop_HS, class = class)



