#********************************** Demographic Data **************************************#
#                                                                                          #
#                                                                                          #
# This file pulls demographic data for Maryland from FRED                                  #
# and uses it to create sibling relationships.                                             #
#******************************************************************************************#

#### SETUP ####

# libraries
library(tidyverse)
library(RColorBrewer)

# set working directory
wd = "~/Dropbox/Schools/Public code/0 - Synthetic Populations/"
state = "Maryland"
setwd(paste0(wd, "0 - FRED data/", state))

# read in data
folders = list.files()
p = data.frame()
for(i in 1:length(folders)){
  
  # set file name
  file = paste0(folders[i], "/people.txt")
  
  # read file
  temp = read.table(file, header = T)
  
  # concatenate
  p = bind_rows(p, temp)
}

#### BASIC ANALYSIS ####

# estimate the number in school
q = p %>% group_by(sp_hh_id) %>% mutate(num_school = sum(school_id!="X"),
                                        age_cat = ifelse(age < 18, "<18", "65+"),
                                        age_cat = ifelse(age>=18 & age < 65, "18-64", age_cat),
                                        has_5_to_11 = sum(age>=11 & age<14),  # recoded to middle school
                                        has_over_65 = sum(age_cat=="65+"),)


# take a look at parents
q2 = q %>% ungroup() %>% filter(has_5_to_11>0) %>% group_by(sp_hh_id) %>% summarize(
  tot_adults = sum(age >= 18),
  over_40 = sum(age > 40), 
  old = sum(age_cat == "65+")) 
  
q3 = q %>% filter(has_5_to_11 > 0)

# num HH by school
# these are lower than more recent MD estimates
# but I think the sibling estimates will be fine
s = p %>% filter(age >=11 & age < 14) %>% group_by(school_id) %>% summarize(num_HHs = length(unique(sp_hh_id)),
                                                                           num_children = length(unique(sp_id))) %>%
  gather(var, value, num_HHs, num_children) %>% group_by(var) %>% 
    summarize(mean = mean(value, na.rm = T), median = median(value, na.rm = T),
              Q25 = quantile(value, .25, na.rm = T), Q75 = quantile(value, .75, na.rm = T))

#### GENERATE HOUSEHOLDS ####

# within school matrix
# calibrated to give 501 students
HH_num = 410

# pull over relevant ages
r = q %>% filter(age >=11 & age < 14) %>% group_by(sp_hh_id) %>% summarize(age_dist = paste(sort(age), collapse = ",")) %>%
  group_by(age_dist) %>% summarize(num = length(age_dist)) %>% 
  ungroup() %>% mutate(tot = sum(num), perc_age_dist = num/tot) %>% mutate(HHs = round(perc_age_dist*HH_num))
  
# reformat for model matrix
synthpop_MS = r %>% 
  slice(rep(1:n(), times = HHs)) %>% 
  mutate(HH_id = row_number(),
         flag_mult = grepl(",", age_dist)) %>%
  separate(age_dist, into = paste("kid", 1:3, sep = ""), sep = ",") %>% 
  gather(var, age, kid1, kid2, kid3) %>% filter(!is.na(age)) %>% 
  select(HH_id, age, flag_mult) %>% ungroup() %>% arrange(age)  %>%
  mutate(age = as.numeric(age), adult = F, id = row_number())

table(synthpop_MS$age)

# save
setwd(paste0(wd, "/2 - Output"))
save(synthpopMS, file = paste0("synthMS", state, ".RData"))

#### MAKE FIGURE ####

# make sibling heatmap
map = data.frame(expand.grid(5:10, 5:10)) %>% rename("age1" = 1, "age2" = 2) %>% mutate(value = c())
for(i in 1:nrow(map)){
    map$value[i]=sum(r$HHs[grepl(map$age1[i], r$age_dist) & grepl(map$age2[i], r$age_dist)])/sum(r$HHs[grepl(map$age1[i], r$age_dist)])
}

png(paste0("sibling_heatmap_", state, ".png"),width=5, height=5, units = "in", res = 300)
  ggplot(map, aes(x = age1, y = age2)) + 
    geom_tile(col = "black", fill = "white") + theme_minimal() + 
    #scale_fill_gradient(high = "#132B43", low = "#56B1F7", name = "") + 
    geom_text(aes(label = round(value, 2))) + theme(panel.grid = element_blank()) + 
    scale_x_continuous(breaks = 5:10) + scale_y_continuous(breaks = 5:10) + 
    labs(x = "Age 1", y = "Age 2", title = "Fraction of households with a child of age 1\n containing a child of age 2")
dev.off()

