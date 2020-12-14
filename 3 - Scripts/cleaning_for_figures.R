#************************************ Make paper plots ************************************#
#                                                                                          #
#                                                                                          #
#                                                                                          #
#******************************************************************************************#

####*********************************** SETUP ******************************************####
# install.packages(c('tidyverse', 'tictoc', 'igraph', 'devtools', rlang'))
library(rlang)
library(tidyverse)
library(tictoc)
library(igraph)
library(devtools)
library(scales)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### IMPORT ELEMENTARY SCHOOLS ####
setwd(paste0(wd, "Base Elem"))
files = list.files()
sims = data.frame()

for(i in 1:length(files)){
  load(files[i])
  sims = sims %>% bind_rows(out)
  print(i)
}

sim_out1 = sims %>% mutate(school = "Elementary school")

#### IMPORT HIGH SCHOOLS ####
setwd(paste0(wd, "Base HS"))
files = list.files()
sims = data.frame()

for(i in 1:length(files)){
  load(files[i])
  sims = sims %>% bind_rows(out)
  print(i)
}

sim_out2 = sims %>% mutate(school = "High school")

#### BIND TOGETHER ####
sim_out = bind_rows(sim_out1, sim_out2) %>% 
  mutate(strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
         strategy = ifelse(test, "Weekly testing", strategy),
         seed_type = ifelse(start_type =="child", "Child", "Teacher"),
         seed_type = ifelse(start_type =="mix", "Mix", seed_type),
         attack_level = ifelse(attack.y==.01, "High", "Medium"),
         attack_level = ifelse(attack.y==.03, "Low", attack_level),
         attack_level = factor(attack_level, c("Low", "Medium", "High")))

setwd(paste0(wd, "Figures"))
save(sim_out, file = "fig_output_main.RData")

############# UPDATED THROUGH HERE ############# 


#### IMPORT SIMULATIONS ####
setwd("~/Dropbox/Schools/Analysis/output/MD_ELEM")

files = list.files()
sims = data.frame()

for(i in 1:length(files)){
  load(files[i])
  sims = sims %>% bind_rows(out)
}

#### CHOSEN VALUES ####

sim_out = sims %>% group_by(sim, id) %>% 
  mutate(strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
         strategy = ifelse(!isolate, "No isolation", strategy),
         strategy = ifelse(test & test_type == "all", "Regular testing", strategy),
         strategy = ifelse(test & test_type == "students", "Testing (students)", strategy),
         strategy = ifelse(test & test_type == "staff", "Testing (staff)", strategy),
         strategy = factor(strategy, levels = c("No isolation", "Symptomatic isolation", 
                                                "Classroom quarantine", "Regular testing",
                                                "Testing (students)", "Testing (staff)")),
         attack_children = paste(attack.y*5/2*100, "% classroom AR to children", sep=""),
         attack_children = factor(attack_children, levels = c("2.5% classroom AR to children",
                                                              "5% classroom AR to children",
                                                              "10% classroom AR to children")),
         id = ifelse(id=="Base case", "5-day", id),
         incidence = adult_prob*100000/5,
         mixing = ifelse(n_HH==0, "No out-of school mixing", "Out-of-school mixing"),
         id.strategy = ifelse(id =="Remote", "Remote", paste(id, "\n", strategy))
  )

out_matrix = sim_out %>% filter(!is.na(n_HH)) %>%
  dplyr::group_by(child_trans, n_HH, attack.y, strategy, id, test_days, mixing, id.strategy, incidence) %>% 
  dplyr::summarize(community = .23*mean(start),
                   school = mean(tot), 
                   new95 = quantile(tot, .95),
                   mean.adults = mean(adult - start_adult),
                   mean.kids = mean(children - (start-start_adult)),
                   mean.family = mean(family),
                   symp = mean(symp), 
                   over10 = mean((tot)>10),    
                   over20 = mean((tot)>20), any = mean((tot)>1),
                   adults = mean((adult - start_adult)>1),
                   family = mean((family)>1)) %>% ungroup()

out1 = out_matrix %>% gather(var, value, school, community) %>% mutate(var = factor(var, levels = c("school", "community")),
                                                                      school = "Elementary school")



####*********************************** FIG 4b ******************************************####

setwd("~/Dropbox/Schools/Analysis/output/MD_HS")

#### IMPORT SIMULATIONS ####
files = list.files()
sims = data.frame()

for(i in 1:length(files)){
  load(files[i])
  sims = sims %>% bind_rows(temp)
}

#### CHOSEN VALUES ####

# set up output
n_tot = 1000
attack = c(0.01, 0.02, 0.03)
child_trans = 1
isolate = T
test = c(F,T)
notify = c(F, T)
disperse_transmission = c(F)
child_susp = c(1)
teacher_susp = c(1)
dedens = 1
n_start = 1
test_days = c("week") #, "2x_week")
n_HH = c(3) #c(0, 3)
j = 1:3
prob = c(5, 20, 50, 100)/7/100000*5
bubble = F #
type = c("all")
df = expand.grid(attack, notify, test, dedens, teacher_susp, child_trans, isolate, disperse_transmission, n_start, type, n_HH, test_days, j, prob, child_susp, bubble) %>%
  rename("attack" = 1, "notify" = 2, "test" = 3, "dedens" = 4, "teacher_susp" = 5,
         "child_trans" = 6, "isolate" = 7, "disperse_transmission" = 8, "n_start" = 9, "test_type" = 10, "n_HH" = 11, test_days = 12, "j" = 13, "adult_prob" = 14, "child_susp" = 15, "bubble" = 16) %>% 
  filter(!(test & !notify)) %>%
  filter(test | test_type=="all") %>%
  filter(test | test_days=="week") %>%
  filter(!(!isolate & (notify | test))) %>% mutate(id = row_number())
keep = data.frame()
time = 120
rel_trans_CC = 2
p_asymp_child = .8
p_asymp_adult = .5
rel_trans_CC = 2
no_weekends = F
test_sens = .9


sim_out = sims %>% left_join(df, c("sim" = "id")) %>% group_by(sim, id) %>% 
  mutate(strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
         strategy = ifelse(!isolate, "No isolation", strategy),
         strategy = ifelse(test & test_type == "all", "Regular testing", strategy),
         strategy = ifelse(test & test_type == "students", "Testing (students)", strategy),
         strategy = ifelse(test & test_type == "staff", "Testing (staff)", strategy),
         strategy = factor(strategy, levels = c("No isolation", "Symptomatic isolation", 
                                                "Classroom quarantine", "Regular testing",
                                                "Testing (students)", "Testing (staff)")),
         attack_children = paste(attack.y*5/2*100, "% classroom AR to children", sep=""),
         attack_children = factor(attack_children, levels = c("2.5% classroom AR to children",
                                                              "5% classroom AR to children",
                                                              "10% classroom AR to children")),
         id = ifelse(id=="Base case", "5-day", id),
         incidence = adult_prob*100000/5,
         mixing = ifelse(n_HH==0, "No out-of school mixing", "Out-of-school mixing"),
         id.strategy = ifelse(id =="Remote", "Remote", paste(id, "\n", strategy))
  )

out_matrix = sim_out %>% filter(!is.na(n_HH)) %>%
  dplyr::group_by(child_trans, n_HH, attack.y, strategy, id, test_days, mixing, id.strategy, incidence) %>% dplyr::summarize(community =.29*mean(start),
                                                                                                                             school = mean(tot), 
                                                                                                                             new95 = quantile(tot, .95),
                                                                                                                             mean.adults = mean(adult - start_adult),
                                                                                                                             mean.kids = mean(children - (start-start_adult)),
                                                                                                                             mean.family = mean(family),
                                                                                                                             symp = mean(symp), 
                                                                                                                             over10 = mean((tot)>10),     
                                                                                                                             over20 = mean(tot)>20,
                                                                                                                             any = mean((tot)>1),
                                                                                                                             adults = mean((adult - start_adult)>1),
                                                                                                                             family = mean((family)>1)) %>% ungroup() %>%
  mutate( school = ifelse(n_HH==0 & id=="5-day", school*.6, school))
out2 = out_matrix %>% gather(var, value, school, community) %>% mutate(var = factor(var, levels = c("school", "community")), school = "High school")

setwd("/Users/abilinski/Dropbox/Schools/Analysis/output")
out = bind_rows(out1, out2)
save(out, file = "figure_full_semester.RData")
