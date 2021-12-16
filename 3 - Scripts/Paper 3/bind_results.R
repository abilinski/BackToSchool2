library(dplyr)
library(data.table)

setwd("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Full Paper Output/Raw Data")

test.sample.fraction <- 0.1

#Set hospitalization parameters
adult.ifr <- 500/1e6
child.ifr <- 20/1e6 

adult.p.die_hosp <- 2.1e-2
child.p.die_hosp <- 0.7e-2

adult.p_hosp <- round(adult.ifr/adult.p.die_hosp,3)
child.p_hosp <- round((24/63.7)*child.ifr/child.p.die_hosp,3) #Conversion factor is for 5-11 vs <17

#Bind initial job files and summarize across groups
for(j in 1:50){
  
  filelist <- list.files(pattern = paste("elem_results_", j, "_.*RData", sep = ""))
  output <- rbindlist(lapply(1:length(filelist), function(a){load(filelist[a]); output = output; return(output)}))
  
  output <- data.table(output)
  
  print(paste("Initial Size for job", j, ":", nrow(output)))
  
  #Prepare data
  data_1 <- output[, `:=`(attack = as.numeric(attack),
                       test_quarantine = as.numeric(test_quarantine),
                       prob = prob*100000/3,
                       all.inschool = as.numeric(class) + as.numeric(related_arts) + as.numeric(random) + as.numeric(random_staff),
                       all.adults = as.numeric(adult) + as.numeric(family),
                       all.children = as.numeric(children),
                       all.overall = as.numeric(adult) + as.numeric(family) + as.numeric(children))]
  
  remove(output)
  
  gc()
  
  print("Data step 1 complete")
  
  data_2 <- data_1[, `:=`(hosp = ((all.adults*(adult.p_hosp)*((1-family_susp)/(1-vax_eff*family_susp)))/(492*2+n_class*6*2+n_other_adults*2) + (all.children*(child.p_hosp)*((1-child.vax)/(1-vax_eff*child.vax)))/638)*1e5,
                          adult.hosp = ((all.adults*(adult.p_hosp)*((1-family_susp)/(1-vax_eff*family_susp)))/(492*2+n_class*6*2+n_other_adults*2))*1e5)]
  
  remove(data_1)
  
  gc()
  
  data_3 <- data_2[, .(inschool.outbreak_inc.1 = mean(all.inschool >= 1),
                       all.inschool = mean(all.inschool),
                       all.adults = mean(all.adults),
                       all.children = mean(all.children),
                       all.overall = mean(all.overall),
                       hosp = mean(hosp),
                       adult.hosp = mean(adult.hosp),
                       analysis.groups = paste(variant.attack, child.vax, teacher_susp, vax_eff, notify.scenario)),
                   by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario)]
  
  remove(data_2)
  gc()
  
  print(paste("Final size for job", j, ":", nrow(data_3)))
  
  print("Data processing complete")
  
  saveRDS(data_3, file = paste("output", j, ".rds", sep = ""))
  remove(data_3)
  print(paste("Subset saved:", j))
  gc()
}

filelist <- sapply(1:50, function(a){paste("output", a, ".rds", sep = "")})

set.seed(2834)

test.sample <- sample(1:length(filelist), round(test.sample.fraction*length(filelist)))
training.sample <- (1:length(filelist))[-test.sample]

output <- rbindlist(lapply(training.sample, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                     all.inschool = mean(all.inschool),
                     all.adults = mean(all.adults),
                     all.children = mean(all.children),
                     all.overall = mean(all.overall),
                     hosp = mean(hosp),
                     adult.hosp = mean(adult.hosp)),
                 by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_training.rds")

remove(output.final)
gc()


output <- rbindlist(lapply(test.sample, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                           all.inschool = mean(all.inschool),
                           all.adults = mean(all.adults),
                           all.children = mean(all.children),
                           all.overall = mean(all.overall),
                           hosp = mean(hosp),
                           adult.hosp = mean(adult.hosp)),
                       by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_test.rds")

remove(output.final)
gc()


output <- rbindlist(lapply(1:50, function(a){output <- readRDS(filelist[a]); return(output)}))

output.final <- output[, .(inschool.outbreak_inc.1 = mean(inschool.outbreak_inc.1),
                           all.inschool = mean(all.inschool),
                           all.adults = mean(all.adults),
                           all.children = mean(all.children),
                           all.overall = mean(all.overall),
                           hosp = mean(hosp),
                           adult.hosp = mean(adult.hosp)),
                       by = .(prob, mitigation, variant.attack, vax.rates, type, child.vax, teacher_susp, family_susp, vax_eff, notify.scenario, analysis.groups)]

remove(output)
gc()

output.final <- output.final[,`:=`(variant.title = ifelse(variant.attack == 0.02, "Wild-Type", ifelse(variant.attack == 0.035, "Alpha Variant", ifelse(variant.attack == 0.07, "Delta Variant", "ERROR"))))]

saveRDS(output.final, file = "revision_output_final_full.rds")

remove(output.final)
gc()
