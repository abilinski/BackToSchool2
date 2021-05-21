# #*************************** Code to replicate results and figures ************************#

# plotcols <- c("black", "#96bb7c", "#23689b")
library(RColorBrewer)
plotcols <- brewer.pal(3, 'Dark2')

####*********************************** FUNCTIONS ******************************************####

#### CLEAN UP ####
prep_sims = function(sims, name_val = ""){
  out = sims %>% 
    mutate(school = ifelse(grepl("Elem", name_val), "Elementary school", "High school"),
           strategy = ifelse(notify, "Classroom quarantine", "Symptomatic isolation"),
           strategy = ifelse(test=="TRUE", "Testing", strategy),
           strategy = ifelse(teacher_susp < 1, "Teacher vaccination", strategy),
           test_freq = ifelse(test_days==1 & test=="TRUE", "Testing 1x/week", "Testing 2x/week"),
           test_freq = ifelse(test=="FALSE", "No testing", test_freq),
           seed_type = ifelse(start_type =="child", "Child", "Teacher"),
           seed_type = ifelse(start_type =="mix", "Mix", seed_type),
           attack_level = ifelse(attack.y==.01, "High", "Medium"),
           attack_level = ifelse(attack.y==.03, "Low", attack_level),
           attack_level = factor(attack_level, c("Low", "Medium", "High")),
           tot = as.numeric(tot),
           R0 = as.numeric(R0),
           school.infs = as.numeric(adult_tot) + as.numeric(children_tot),
           school.infs.symp = .6*as.numeric(adult_tot) +
             .2*as.numeric(children_tot),
           school.infs.symp2 = .8*as.numeric(adult_tot) +
             .4*as.numeric(children_tot),
           school.infs.perc = ifelse(tot==0, NA, school.infs.symp/tot),
           source.asymp = as.numeric(source_asymp),
           source.asymp.family = as.numeric(source_asymp_family_kids) + as.numeric(source_asymp_family_staff),
           quarantine_days = as.numeric(quarantined),
           detected = as.numeric(detected),
           detected_subclin = as.numeric(detected_staff_subclin) + as.numeric(detected_students_subclin),
           clin = as.numeric(clin_staff) + as.numeric(clin_students),
           # detected2 = detected + clin,
           detected2 = detected_subclin + clin,
           id2 = ifelse(id==1, "5-day", id),
           id2 = ifelse(id==2, "Cohorting", id2),
           id2 = ifelse(id==3, "1/2 class size", id2),
           id2 = ifelse(id==4, "A/B", id2),
           id2 = ifelse(type=="A/B" & total_days==1, "A/B/C/D", id2),
           id2 = ifelse(type=="On/off" & total_days==1, "On/off (1)", id2),
           id2 = ifelse(type=="On/off" & total_days==2, "On/off (2)", id2),
           id2 = factor(id2, levels = rev(c("A/B/C/D", "On/off (1)", "On/off (2)", "A/B", "1/2 class size", "Cohorting", "5-day"))))
  return(out)
}

# Testing graphs
graph_sims3 = function(sims, n_teacher = 1, n_student = 1, n_class = 1, ymax = NA, test_type_set = "1", attack_level_set="Medium",
                       var = "", ql = "10", tt = "1", legend=T,
                       pooled_pcr = 100,
                       followup_test = 100,
                       collection = 8,
                       binax = 12,
                       npools = 3,  # pools per classroom per testing day
                       planned_day = 35.5,
                      unplanned_day = 85.9
              
){
  classsize <- (n_student + n_teacher)/(n_class+1)
  
  tot_days = (n_teacher+n_student)*30*5/7
  dyn_elem = prep_sims(sims, "Elem") %>%
    mutate(days_missed = (quarantine_days + ifelse(type=="A/B", tot_days*3/5,0) + 
             ifelse(type=="Remote", tot_days,0))/tot_days) %>%
    filter(test=="FALSE" | test_type==test_type_set) %>%
    filter(attack_level==attack_level_set) %>%
    # filter(!strategy=="Teacher vaccination") %>%
    filter(!(type=="base" & strategy=="Symptomatic isolation")) %>%
    mutate(grp = case_when(type=="A/B"~"A/B", type=="Remote"~"Remote", (type=="base")~"5-Day")) 
  
  out = dyn_elem %>% 
    filter(scenario==3 | 
             (scenario %in% c(1,2) & test==F & notify==T & quarantine.length ==10) | 
             (scenario==1 & test==T & quarantine.length ==ql & turnaround.time==tt)) %>% 
    group_by(id, id2, n_HH, disperse_transmission, dedens, 
                              test, isolate, teacher_susp, notify, child_susp, 
                              child_trans, start_type, school, 
                              attack_level, adult_prob, scenario, type, days_inf, test_start_day,
                              test_days, test_type, test_frac, test_freq, grp, quarantine.length,
                              turnaround.time, strategy) %>%
    summarize(all.mean = mean(as.numeric(all)),
              detected = mean(detected),
              detected2 = mean(detected2),
              class_tests = mean(as.numeric(class_test_ind)),
              quarantine_days = mean(quarantine_days)/tot_days,
              days_missed = mean(days_missed),
              all.median = median(as.numeric(all)),
              teachers.mean = mean(as.numeric(adult)),
              students.mean = mean(as.numeric(children)),
              school.mean = teachers.mean + students.mean,
              family.mean = mean(as.numeric(family)),
              tot = mean(as.numeric(tot))) %>% ungroup() %>%
    mutate(School_inc = school.mean/(n_teacher + n_student), 
           Teacher_inc = teachers.mean/(n_teacher),
           Student_inc = students.mean/(n_student),
           scenario = scenario,
           adult_prob = as.numeric(adult_prob)/3*100000,
           id_cat = ifelse(scenario==1, "5-day", "A/B"),
           id_cat = ifelse(scenario==3, "Remote", id_cat),
           n_HH = ifelse(n_HH=="0", 0, as.numeric(n_HH)-1),
           costs_pooled_tests = class_tests * npools / n_student * pooled_pcr + class_tests * classsize/n_student * collection, 
           costs_followup_tests = detected / n_student * classsize/npools * followup_test,
           costs_planned_care = planned_day *30*5/7 * pmax(days_missed- quarantine_days,0) * n_student/(n_student+n_teacher), # only count for students
           costs_unplanned_care = unplanned_day*30*5/7 * quarantine_days * n_student/(n_student+n_teacher),
           total_costs_testing = costs_pooled_tests + costs_followup_tests,
           total_costs_care = costs_planned_care + costs_unplanned_care,
           total_costs = total_costs_care + total_costs_testing,
           costs_rapidtesting = class_tests * classsize/n_student * (binax + collection) + 
             detected / n_student * (followup_test + collection)  + # cost of confirmatory PCRs
             class_tests * classsize/n_student * (0.005) * (followup_test + collection),  # cost of false positive rapid tests
           total_costs_rapid = total_costs_care + costs_rapidtesting
           
    )
           
  out2 = out %>% 
    gather(var, value, School_inc, detected2, quarantine_days, days_missed) %>%
    mutate(
      var_cat = case_when(var=="School_inc"~"Monthly incidence in staff and students", 
                          var=="detected2"~"Monthly cases detected per school",
                          var=="quarantine_days"~"Proportion of days in isolation/quarantine", 
                          var=="days_missed"~"Proportion of days out of school"),
      var_cat = factor(var_cat, levels = c("Monthly incidence in staff and students", 
                                           "Monthly cases detected per school", "Proportion of days in isolation/quarantine", "Proportion of days out of school"))) 
  

  plotout_1 <- ggplot(out2 %>% filter(var=="School_inc"), 
                    aes(x = adult_prob, y = value, group = paste(grp, test_freq),
                        col = grp, lty = test_freq)) + 
    geom_line() + 
    theme_minimal() + #theme_opts +
    theme(  title =element_text(size=8)) + 
    labs(x = "", y = "", title = ifelse(legend, "Monthly infections\n(proportion of school, number)", " ")) + 
    scale_color_manual(name = "", values = plotcols) + 
    scale_linetype(name = "") + 
    scale_y_continuous(limits=c(0,0.19), sec.axis = sec_axis(trans = ~  . * (n_teacher + n_student), name = "")) +
    annotate(geom="text", x=1, y=0.19, label=ifelse(legend,"A","E"))

  plotout_2 <- ggplot(out2 %>% filter(var=="detected2"), 
                      aes(x = adult_prob, y = value/(n_teacher + n_student), 
                          group = paste(grp, test_freq), col = grp, lty = test_freq)) + 
    geom_line() + 
    theme_minimal() + #theme_opts +
    theme(  title =element_text(size=8)) + 
    labs(x = "", y = "", title = ifelse(legend,"Monthly cases diagnosed\n(proportion of school, number)"," ")) +
    scale_color_manual(name = "", values = plotcols) + 
    scale_linetype(name = "") + 
    scale_y_continuous(limits = c(0,0.19), sec.axis = sec_axis(trans = ~  . * (n_teacher + n_student), name = "")) +
    annotate(geom="text", x=1, y=0.19, label=ifelse(legend,"B","F"))
  
  plotout_3 <- ggplot(out2 %>% filter(var=="quarantine_days"), 
                      aes(x = adult_prob, y = value, 
                          group = paste(grp, test_freq), col = grp, lty = test_freq)) + 
    geom_line() + 
    theme_minimal() + #theme_opts +
    theme(  title =element_text(size=8)) + 
    labs(x = "", y = "", title = ifelse(legend,"Proportion of school days \nspent in quarantine/isolation","")) +
    scale_color_manual(name = "", values = plotcols) + 
    scale_linetype(name = "") + 
    ylim(0,0.25) +
    annotate(geom="text", x=1, y=0.25, label=ifelse(legend,"C","G"))
  
  plotout_4 <- ggplot(out2 %>% filter(var=="days_missed"), 
                      aes(x = adult_prob, y = 1-value, 
                          group = paste(grp, test_freq), col = grp, lty = test_freq)) + 
    geom_line() + 
    theme_minimal() + #theme_opts +
    theme(  title =element_text(size=8)) + 
    labs(x = "", y = "", title = ifelse(legend,"Proportion of school days\nattended in person","")) +
    scale_color_manual(name = "", values = plotcols) + 
    scale_linetype(name = "") +
    annotate(geom="text", x=1, y=0.99, label=ifelse(legend,"D","H"))
  
  
  if(legend)
    plot_out <- ggarrange(plotout_1, plotout_2, plotout_3, plotout_4, nrow = 1, widths = c(1,1,0.85,0.85),legend='none')  else
      plot_out <- ggarrange(plotout_1, plotout_2, plotout_3, plotout_4, nrow = 1, widths = c(1,1,0.85,0.85), common.legend = T,legend = 'bottom')
  
  return(list(plot_out, out))
  
}

####*********************************** PROCESS DATA ******************************************####
library(tidyverse)
library(tictoc)
library(igraph)
library(foreach) 
library(doMC) 
library(ggpubr)
library(reshape2)
# library(patchwork)

# DYNAMIC ELEMENTARY
# load("fig_output_Elem Test.RData")
load("fig_output_OutputES_7_May.RData", verbose = T)
sims = apply(sims, 2, function(a) as.character(a))
sims = data.frame(sims)
elem <- prep_sims(sims)
load("fig_output_OutputMS_7_May.RData", verbose = T)
sims = apply(sims, 2, function(a) as.character(a))
sims = data.frame(sims)
middle <- prep_sims(sims)

# # check what we have for sensitivity analyses
# elem %>% filter(adult_prob==0.0003, test==T) %>% count(turnaround.time, attack_level, 
#                     test_sens, test_frac, 
#                     teacher_susp, child_susp, 
#                     test_type, 
#                     quarantine.length, 
#                     turnaround.time, 
#                     test_days, test_start_day,
#                     days_inf,  test, notify, scenario, type)
# 
# middle %>% filter(adult_prob==0.0003) %>% count(turnaround.time, attack_level, 
#                                                        test_sens, test_frac, 
#                                                        teacher_susp, child_susp, 
#                                                        test_type, 
#                                                        quarantine.length, 
#                                                        turnaround.time, 
#                                                        test_days, test_start_day,
#                                                        days_inf,  test, notify, scenario, type)

# # add additional runs to elementary?
# load("fig_output_Elem_PLUS.RData", verbose = T)
# sims <- prep_sims(sims)
# sims %>% filter(adult_prob<=0.0002) %>% count(turnaround.time, test_sens, test_frac,
#                                               teacher_susp,
#                                               test_type,
#                                               quarantine.length,
#                                               turnaround.time,
#                                               test_days, test_start_day,
#                                               days_inf,  strategy, scenario, type, adult_prob)
# elem <- rbind(elem, sims)

n_teacher_elem <- 60
n_student_elem <- 638
n_class_elem <- 30
n_teacher_middle <- 51
n_student_middle <- 460
n_class_middle <- 21

elemplot = graph_sims3(elem %>% filter(turnaround.time==1, attack_level=="Medium",
                                              test_sens==0.9, test_frac==0.9,
                                              teacher_susp==0.2, 
                                              test_type==1,
                                              test_start_day==1, 
                                              days_inf==5,
                                              quarantine.length==10
                                               ),
                       n_student = n_student_elem, n_teacher = n_teacher_elem, n_class = n_class_elem,
                       ymax = .25, tt = "1", 
                       ql=10, var = "Elementary school", legend=T)
# annotate_figure(elemplot[[1]], fig.lab="Elementary", fig.lab.size=12)
middleplot = graph_sims3(middle %>% filter(turnaround.time==1, attack_level=="Medium",
                                           test_sens==0.9, test_frac==0.9,
                                           teacher_susp==0.2, 
                                           test_type==1,
                                           test_start_day==1,
                                           days_inf==5,
                                           quarantine.length==10), 
                           n_student = n_student_middle, n_teacher = n_teacher_middle, n_class = n_class_middle, 
                         ymax = .25, tt = "1", 
                         ql=10, var = "Middle school", legend=F)

middleplot_highattack = graph_sims3(middle %>% filter(turnaround.time==1, attack_level=="High",
                                           test_sens==0.9, test_frac==0.9,
                                           teacher_susp==0.2, 
                                           test_type==1,
                                           test_start_day==1,
                                           days_inf==5,
                                           quarantine.length==10), 
                         n_student = n_student_middle, n_teacher = n_teacher_middle, n_class = n_class_middle, 
                         attack_level_set = "High",
                         ymax = .25, tt = "1", 
                         ql=10, var = "Middle school", legend=F)

# annotate_figure(middleplot[[1]], fig.lab="Middle", fig.lab.size=12)
figure1 <- ggarrange(
  annotate_figure(elemplot[[1]]+theme(plot.margin = margin(c(1.5, 1, 1, 1), unit="lines")), fig.lab="Elementary", fig.lab.size=12, fig.lab.face = 'bold'),
  annotate_figure(middleplot[[1]], 
                  bottom = text_grob("Community notifications per 100k per day", size = 9), 
                  fig.lab="Middle", fig.lab.size=12, fig.lab.face = 'bold'), 
  nrow = 2, common.legend = T, heights=c(10,10)) 

pdf(file = "Fig1_schools.pdf", width = 10, height=6) 
figure1 
dev.off()

# make table of these results


inc <- 25
plotout <- middleplot[[2]]
plotout %>% filter(adult_prob==inc) %>% group_by(grp, test_freq) %>% 
  summarize("Increase in incidence with school attendance" = 
              round((School_inc)/
                      (plotout %>% filter(adult_prob==inc, notify==F, test==F, scenario==3) %>% select(School_inc)),3),
            "Increase in incidence," = 
              round((School_inc)/
                      (plotout %>% filter(adult_prob==inc, notify==F, test==F, scenario==3) %>% select(School_inc)),3),
            "Increase teacher inc" = round((Teacher_inc)/
                                             (plotout %>% filter(adult_prob==inc, notify==F, test==F, scenario==3) %>% select(Teacher_inc)),3),
            "Increase student inc" = round((Student_inc)/
                                             (plotout %>% filter(adult_prob==inc, notify==F, test==F, scenario==3) %>% select(Student_inc)),3),
            "Difference in infections" = 
              round((plotout %>% filter(adult_prob==inc, notify==T, test==F, scenario==1) %>% select(school.mean) - school.mean),3),
  )


# Columns/outcomes: Incident cases, incremental incident cases (relative to 5 days in person w no testing), % of all school days spent in-person, laboratory costs, societal costs
# Rows could be the interventions.
inc <- 5
n_teacher <- n_teacher_middle
n_student <- n_student_middle
plotout <- middleplot[[2]]
# m10 <- 
  plotout %>% filter(adult_prob==inc) %>% group_by(grp, test_freq) %>% 
  summarize("Infected (proportion of school)"  = round(School_inc,3), 
            "Difference in proportion of school infected, vs full-time without screening" = 
              round(School_inc - (plotout %>% filter(adult_prob==inc, notify==T, test==F, scenario==1) %>% select(School_inc)),3),
            "Proportion of incremental infections prevented" = 
              round(((plotout %>% filter(adult_prob==inc, notify==T, test==F, scenario==1) %>% select(School_inc))-School_inc)/
                      ((plotout %>% filter(adult_prob==inc, notify==T, test==F, scenario==1) %>% select(School_inc)) - 
                         (plotout %>% filter(adult_prob==inc, notify==F, test==F, scenario==3) %>% select(School_inc))),2),
            "Proportion of cases detected" = round(detected2/(teachers.mean + students.mean),2),
            # "Diagnosed as COVID case (proportion of school)" = round(detected2/((n_teacher + n_student)),3),
            "In-person attendance (proportion of school days)" = round(1-days_missed,2),
            "Testing costs ($ per student)" = round(total_costs_testing,0),
            "Societal costs ($ per student)"= round(total_costs,0))

  plotout %>% filter(adult_prob==inc) %>% group_by(grp, test_freq) %>% 
    summarize("increase in time in quarantine over 5-day" = quarantine_days/
                (plotout %>% filter(adult_prob==inc, notify==T, test==F, scenario==1) %>% select(quarantine_days)),
              "quarantine days per month" = quarantine_days*30)
  
write.csv(rbind(as.matrix(e10), as.matrix(e50), as.matrix(m10), as.matrix(m50)), file="stable.csv")


elemplot2 = graph_sims3(elem %>% filter(turnaround.time==1, attack_level=="Medium",
                                        test_sens==0.9, test_frac==0.9,
                                        teacher_susp==0.2, 
                                        test_type==1,
                                        test_start_day==1, 
                                        days_inf==5,
                                        # quarantine.length==10
),
n_student = n_student_elem, n_teacher = n_teacher_elem, n_class = n_class_elem,
ymax = .25, tt = "1", 
ql=7, var = "Elementary school", legend=T)

middleplot2 = graph_sims3(middle %>% filter(turnaround.time==1, attack_level=="Medium",
                                           test_sens==0.9, test_frac==0.9,
                                           teacher_susp==0.2, 
                                           test_type==1,
                                           test_start_day==1,
                                           days_inf==5), 
                         n_student = n_student_middle, n_teacher = n_teacher_middle, n_class = n_class_middle,
                         ymax = .25, tt = "1", 
                         ql=7, var = "Middle school", legend=F)

s2fig <- ggarrange(
  annotate_figure(elemplot2[[1]]+theme(plot.margin = margin(c(1.5, 1, 1, 1), unit="lines")), fig.lab="Elementary, with 7-day quarantine if screening", fig.lab.size=12, fig.lab.face = 'bold'),
  annotate_figure(middleplot2[[1]], 
                  bottom = text_grob("Community notifications per 100k per day", size = 9), 
                  fig.lab="Middle, with 7-day quarantine if screening", fig.lab.size=12, fig.lab.face = 'bold'), 
  nrow = 2, common.legend = T, heights=c(10,10)) 
 
pdf(file = "S2Fig_schools.pdf", width = 10, height=6) 
s2fig
dev.off()

###********************************** ESTIMATE COSTS *********************************************####

# Costs of testing:

# # Assume all members of pool have PCR tests repeated when the pool is positive.
# # Assume no significant loss of sensivity from pooling, at least during the first week of infection.
# # (refs https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30362-5/fulltext, 
# #   https://www.nature.com/articles/s41586-020-2885-5 ).

# And assume that each detected cases is in a different pool (will slightly overestimate total positive pools.)

# switching to assume three pools per classroom based on Andrea's feedback.

# middle school: 460 kids, 45 staff, 15 classes

# # Costs of child care
# unitcosts$planned_day <- 35.5
# # Average cost of group child care for pre-K child in US estimated in 2018 as $760 per month = 760/(30*5/7) -> $35.50 per day. 
# # https://www.americanprogress.org/issues/early-childhood/reports/2018/11/15/460970/understanding-true-cost-child-care-infants-toddlers/ 
# # Costs of child care have increased during covid, https://www.americanprogress.org/issues/early-childhood/reports/2020/09/03/489900/true-cost-providing-safe-child-care-coronavirus-pandemic/ , 
# # but should be lower for school age versus preschool, so we'll use this estimate.
# unitcosts$unplanned_day <- 85.9
# # 7 hours of care x Mean hourly child care worker wage of $12.27
# # https://www.bls.gov/oes/current/oes399011.htm
# # Applies to both isolation and quarantine days. 
# # Shouldn't apply to teachers if whole class is quarantining on 5-day model (after testing), 
# # but may need to pay for substitutes in hybrid model (at Mean wage $15.56*8 = 124.50 per day per https://www.bls.gov/oes/current/oes399011.htm).
# # ** Lets ignore teacher isolation/quarantine costs for now

# for vaccination scenario analysis
mitigation <- "High"; level <- "Middle" ; ql <-10
outcomes <- middleplot_highattack[[2]] %>%  filter(
  attack_level==mitigation, 
  teacher_susp==0.2, 
  test_type==1,
  turnaround.time==1, 
  test_frac==0.9)
n_student <- n_student_middle
n_teacher <- n_teacher_middle
n_class <- n_class_middle
classsize <- (n_student + n_teacher)/(n_class+1)


Fig3 <- Fig3_rapid <- list()
Fig2 <- SFig_care <- SFig_test <- Fig2_rapid <- list()
mitigation <- "Medium"

for (level in c("Elementary", "Middle"))
for (ql in c(7,10))
  
{
  if(level=="Elementary") 
  {  if(ql==10)
    outcomes <- elemplot[[2]] %>%  filter(
                  attack_level==mitigation, 
                  teacher_susp==0.2 , 
                  test_type==1,
                  turnaround.time==1, 
                  test_frac==0.9) else
      outcomes <- elemplot2[[2]] %>%  filter(
        attack_level==mitigation, 
        teacher_susp==0.2 , 
        test_type==1,
        turnaround.time==1, 
        test_frac==0.9) 
    n_student <- n_student_elem
    n_teacher <- n_teacher_elem
    n_class <- n_class_elem
  } else
    { 
      if(ql==10)
        outcomes <- middleplot[[2]] %>%  filter(
      attack_level==mitigation, 
      teacher_susp==0.2, 
      test_type==1,
      turnaround.time==1, 
      test_frac==0.9) else
        outcomes <- middleplot2[[2]] %>%  filter(
          attack_level==mitigation, 
          teacher_susp==0.2 , 
          test_type==1,
          turnaround.time==1, 
          test_frac==0.9)
    n_student <- n_student_middle
    n_teacher <- n_teacher_middle
    n_class <- n_class_middle
    }
  classsize <- (n_student + n_teacher)/(n_class+1)
  npools <- 3
  
   melttestcosts <- melt(data = outcomes, 
                    measure.vars = c("costs_pooled_tests", "costs_followup_tests", "total_costs_testing"), 
                    variable.name = "costtype", value.name = "cost")
  
  melttestcosts$costtype <- factor(melttestcosts$costtype,
                                   levels = c("costs_followup_tests", "costs_pooled_tests", "total_costs_testing"), 
                                   labels=c("Follow-up after positives","Initial pooled tests","Combined"))
  if (ql==10) (SFig_test[[paste0(level,ql)]] <- ggplot(melttestcosts %>% 
                                                         filter( test_frac==.9, test==T, 
                                   (quarantine.length==10 & turnaround.time==1)
                                      ) %>%
           filter(costtype %in% c("Initial pooled tests", "Follow-up after positives")) , 
         aes(x= adult_prob, y=cost, fill=costtype)) + 
    geom_area() + 
    facet_grid(~test_freq) + 
    theme_minimal() +
    labs(x = "Community cases per 100k per day", y = "Testing costs per student per month") + 
    scale_fill_discrete(name = "",  guide = guide_legend(reverse = TRUE)) +
      theme(legend.position = 'bottom')
      
  )
  
  # cost per student if no positives or quarantines
  outcomes %>% summarize("basetestingcost" = 
                            ((npools*(n_class+1)*100 + (n_student + n_teacher)*8)/n_student) *5)
  
  #societal costs
  elemplot[[2]] %>% filter(quarantine.length==10, test_days==1, turnaround.time==1, test==T, notify==T) %>% group_by(adult_prob) %>% select(total_costs)
  middleplot[[2]] %>% filter(quarantine.length==10, test_days==1, turnaround.time==1, test==T, notify==T) %>% group_by(adult_prob) %>% select(total_costs)

  
  # cost for testing to exceed hybrid:
  # outcomes$class_tests/ n_student (npools  * pooled_pcr + classsize * collection) > 400
  tests <- unlist(outcomes %>% filter(adult_prob==10, test==T, notify==T, quarantine.length==10, test_days==1) %>%
    select(class_tests))
  tests / n_student * (3  * 100 + classsize * 8)
  tests / n_student * (3  * 500 + classsize * 8)
  tests / n_student * (3  * 100 + classsize * 60)
  
  
    # Exclude isolation days for non-covid symptoms, conservatively assuming that they’re the same across scenarios – but may actually miss less if testing is available in school
 
  
  meltcarecosts <- melt(data = outcomes,
                       measure.vars = c("costs_planned_care", "costs_unplanned_care", "total_costs_care"), 
                       variable.name = "costtype", value.name = "cost")
  meltcarecosts$costtype <- factor(meltcarecosts$costtype,
                                   levels = c("costs_unplanned_care", "costs_planned_care", "total_costs_care"), 
                                   labels=c("Unplanned","Planned","Combined"))
  if (ql==10) (SFig_care[[paste0(level,ql)]] <- ggplot(meltcarecosts %>% 
                         filter( (quarantine.length==10 & turnaround.time==1),
                                 (notify==T | grp!="A/B"),
                         costtype %in% c("Unplanned", "Planned"),
                               ) , 
                      aes(x= adult_prob, y=cost, fill=costtype)) + 
    geom_area() + 
    facet_grid(grp ~ test_freq) + 
    theme_minimal() +
    labs(x = "Community cases per 100k per day", y = "Childcare costs per student per month") + 
    scale_fill_discrete(name = "") +
      theme(legend.position = 'bottom'))
  
  
  allcosts <- outcomes %>% select(-costs_followup_tests, -costs_pooled_tests, -costs_planned_care, -costs_unplanned_care, -costs_rapidtesting, -total_costs_rapid)
  meltallcosts <- melt(data = allcosts, 
                        measure.vars = c("total_costs_testing", "total_costs_care", "total_costs"), 
                        variable.name = "costtype", value.name = "cost")
  meltallcosts$costtype <- factor(meltallcosts$costtype,
                                   levels = c("total_costs_testing","total_costs_care", "total_costs"), 
                                   labels=c("Testing","Childcare","Combined costs"))
  
  allcosts_rapid <- outcomes %>% select(-costs_followup_tests, -costs_pooled_tests, -costs_planned_care, -costs_unplanned_care, -total_costs_testing, -total_costs)
  meltallcosts_rapid <- melt(data = allcosts_rapid, 
                       measure.vars = c("costs_rapidtesting", "total_costs_care", "total_costs_rapid"), 
                       variable.name = "costtype", value.name = "cost")
  meltallcosts_rapid$costtype <- factor(meltallcosts_rapid$costtype,
                                  levels = c("costs_rapidtesting","total_costs_care", "total_costs_rapid"), 
                                  labels=c("Testing","Childcare","Combined costs"))
  
  
  (Fig2[[paste0(level,ql)]] <- ggplot( meltallcosts %>% 
                     filter( (test==F&quarantine.length==10) | 
                               (test==T & quarantine.length==ql & turnaround.time==1),
                             (grp %in% c("A/B","5-Day") &  notify==T&test==F) | 
                               (grp != "A/B")), 
          aes(x = adult_prob, y = cost, 
              col = grp, lty = test_freq)) + 
    geom_line(lwd=0.7) +
    facet_grid(~costtype) +
    theme_minimal() +
    labs(x = "Community cases per 100,000 population per day", y = "Cost per student per month, $")  +
    scale_color_manual(name="", values = plotcols) + 
      scale_linetype_discrete(name="") + 
    theme(legend.key.width=unit(1,"cm"), legend.position = 'bottom', legend.box = 'vertical')
  )

    (Fig2_rapid[[paste0(level,ql)]] <- ggplot( meltallcosts_rapid %>% 
                                           filter( (test==F&quarantine.length==10) | 
                                                     (test==T & quarantine.length==ql & turnaround.time==1),
                                                   (grp %in% c("A/B","5-Day") &  notify==T&test==F) | 
                                                     (grp != "A/B")), 
                                         aes(x = adult_prob, y = cost, 
                                             col = grp, lty = test_freq)) + 
       geom_line(lwd=0.7) +
       facet_grid(~costtype) +
       theme_minimal() +
       labs(x = "Community cases per 100,000 population per day", y = "Cost per student per month, $")  +
       scale_color_manual(name="", values = plotcols) + 
       scale_linetype_discrete(name="") + 
       theme(legend.key.width=unit(1,"cm"), legend.position = 'bottom', legend.box = 'vertical')
    )
  
  
  (Fig2[[paste0(level,ql,"unlabeled")]] <- ggplot( meltallcosts %>% 
                                         filter( (test==F&quarantine.length==10) | 
                                                   (test==T & quarantine.length==ql & turnaround.time==1),
                                                 (grp %in% c("A/B","5-Day") &  test==F&notify==T) | 
                                                   (grp != "A/B")), 
                                       aes(x = adult_prob, y = cost, 
                                           col = grp, lty = test_freq)) + 
      geom_line(lwd=0.7) +
      facet_grid(~costtype) +
      theme_minimal() +
      labs(x = "", y = "Cost per student per month, $")  +
      scale_color_manual(name="", values = plotcols) + 
      scale_linetype_discrete(name="") + 
      theme(legend.position = 'none')
  )
  (Fig2_rapid[[paste0(level,ql,"unlabeled")]] <- ggplot( meltallcosts_rapid %>% 
                                                     filter( (test==F&quarantine.length==10) | 
                                                               (test==T & quarantine.length==ql & turnaround.time==1),
                                                             (grp %in% c("A/B","5-Day") &  test==F&notify==T) | 
                                                               (grp != "A/B")), 
                                                   aes(x = adult_prob, y = cost, 
                                                       col = grp, lty = test_freq)) + 
      geom_line(lwd=0.7) +
      facet_grid(~costtype) +
      theme_minimal() +
      labs(x = "", y = "Cost per student per month, $")  +
      scale_color_manual(name="", values = plotcols) + 
      scale_linetype_discrete(name="") + 
      theme(legend.position = 'none')
  )
  
  
  ##### cost effectiveness #####
  # Cost per case detected, cost per transmission averted, each compared to 5 days w/o screening
  
  fiveday <- outcomes %>% filter(grp=="5-Day", test_frac==0.9, turnaround.time==1, 
                                 (test==F&quarantine.length==10) | 
                              (test==T & quarantine.length==ql))
  fiveday %>% count(test_freq, adult_prob, quarantine.length)
  hybrid <- outcomes %>% filter(grp=="A/B" &quarantine.length==10)
  compareinc <- acast(fiveday, test_freq ~ adult_prob, value.var = "School_inc") # this is proportion infected per month
  # will multiply by school size to get cases 
  comparedetection <- acast(fiveday, test_freq ~ adult_prob, value.var = "detected2")
  # this is cases detected per month, no adjustment
  comparecost_total <- acast(fiveday, test_freq ~ adult_prob, value.var = "total_costs")
  comparecost_care <- acast(fiveday, test_freq ~ adult_prob, value.var = "total_costs_care")
  comparecost_testing <- acast(fiveday, test_freq ~ adult_prob, value.var = "total_costs_testing")
  comparecost_testing_rapid <- acast(fiveday, test_freq ~ adult_prob, value.var = "costs_rapidtesting")
  comparecost_total_rapid <- acast(fiveday, test_freq ~ adult_prob, value.var = "total_costs_rapid")
  # these are costs per student per month, so multiple by school size
  
  #  per school per month:
  incdiff <-( apply(compareinc, 1, function(x) x - compareinc[1,]) * (n_student+n_teacher) )[,"Testing 1x/week"]
  detectdiff <- apply(comparedetection, 1, function(x) x - comparedetection[1,])[,"Testing 1x/week"]
  costdiff_total <- (apply(comparecost_total, 1, function(x) x - comparecost_total[1,]) * (n_student+n_teacher))[,"Testing 1x/week"]
  costdiff_care <- (apply(comparecost_care, 1, function(x) x - comparecost_care[1,]) * (n_student+n_teacher))[,"Testing 1x/week"]
  costdiff_testing <- (apply(comparecost_testing, 1, function(x) x - comparecost_testing[1,]) * (n_student+n_teacher))[,"Testing 1x/week"]
  costdiff_testing_rapid <- (apply(comparecost_testing_rapid, 1, function(x) x - comparecost_testing_rapid[1,]) * (n_student+n_teacher))[,"Testing 1x/week"]
  costdiff_total_rapid <- (apply(comparecost_total_rapid, 1, function(x) x - comparecost_total_rapid[1,]) * (n_student+n_teacher))[,"Testing 1x/week"]
  
  costeffectiveness <- data.frame(cbind(-costdiff_care/incdiff, -costdiff_testing/incdiff, -costdiff_total/incdiff,
                             costdiff_care/detectdiff, costdiff_testing/detectdiff, costdiff_total/detectdiff))
  costeffectiveness_rapid <- data.frame(cbind(-costdiff_care/incdiff, -costdiff_testing_rapid/incdiff, -costdiff_total_rapid/incdiff,
                                        costdiff_care/detectdiff, costdiff_testing_rapid/detectdiff, costdiff_total_rapid/detectdiff))
  colnames(costeffectiveness) <- colnames(costeffectiveness_rapid) <- c("Child care costs per infection averted",
                                   "Testing costs per infection averted",
                                   "Total.i",
                                   "Child care costs per incremental case detected",
                                   "Testing costs per incremental case detected",
                                   "Total.d")
  costeffectiveness$cases <- as.numeric(rownames(costeffectiveness))
  ce_wide <- gather(costeffectiveness, type, ratio, -Total.i, -Total.d, -cases)
  ce_wide$costtype <- ifelse(startsWith(ce_wide$type, "Child"), "Total incl child care", "Testing")
  ce_wide$costtype <- factor(ce_wide$costtype, levels = c("Total incl child care", "Testing"))
  ce_wide$outcome <- ifelse(endsWith(ce_wide$type, "case detected"), 
                            "Cost per incremental case detected", "Cost per infection directly averted")
  
  ce_wide$label <- paste0(round(ce_wide$ratio/1e3))#,"k")
  ce_wide$labelposition <-  ifelse(ce_wide$costtype=="Testing", ce_wide$ratio,
                                   ifelse(ce_wide$outcome=="Cost per incremental case detected",
                                      ce_wide$Total.d, ce_wide$Total.i))/1000
  
  library(ggrepel)
  ce_wide <- subset(ce_wide, label >= 0)

  ## rapid version:
  costeffectiveness_rapid$cases <- as.numeric(rownames(costeffectiveness_rapid))
  ce_wide_rapid <- gather(costeffectiveness_rapid, type, ratio, -Total.i, -Total.d, -cases)
  ce_wide_rapid$costtype <- ifelse(startsWith(ce_wide_rapid$type, "Child"), "Total incl child care", "Testing")
  ce_wide_rapid$costtype <- factor(ce_wide_rapid$costtype, levels = c("Total incl child care", "Testing"))
  ce_wide_rapid$outcome <- ifelse(endsWith(ce_wide_rapid$type, "case detected"), 
                            "Cost per incremental case detected", "Cost per infection directly averted")
  
  ce_wide_rapid$label <- paste0(round(ce_wide_rapid$ratio/1e3))#,"k")
  ce_wide_rapid$labelposition <-  ifelse(ce_wide_rapid$costtype=="Testing", ce_wide_rapid$ratio,
                                   ifelse(ce_wide_rapid$outcome=="Cost per incremental case detected",
                                          ce_wide_rapid$Total.d, ce_wide_rapid$Total.i))/1000
  ###
  
  library(ggrepel)
  ce_wide <- subset(ce_wide, label >= 0)
  ce_wide_rapid <- subset(ce_wide_rapid, label >= 0)
  
  
  
    # For middle school, where incremental case detection doesn't increase because you reduce cases so much:
  (Fig3[[paste0(level," school, ",ql," day quarantine")]]  <- ggplot(ce_wide %>%
                    filter(outcome=="Cost per infection directly averted"), 
                    aes(x=cases, y=ratio/1000, fill=costtype)) + 
      geom_area() + 
      ggtitle(paste0(level," school, ",ql," day quarantine")) + 
      theme_minimal() + 
      # ylim(c(0,50)) +
      scale_fill_discrete(name = "Cost type",  guide = guide_legend(reverse = TRUE)) +
      theme(legend.position = 'bottom',
            strip.text.x = element_text(size=12)) + 
      ylab("Thousands $ US") + 
      scale_x_continuous(breaks=c(1,5,10, 25,50, 100), limits = c(1,100)) +
      xlab("Community cases/100k/day") +
      geom_text(data = ce_wide %>%
                         filter(outcome=="Cost per infection directly averted", 
                                costtype=="Testing"), 
                       aes(x=cases, y=labelposition, label=round(labelposition))) + 
      # geom_label_repel( aes(x=cases, y=labelposition, label=round(labelposition))) + 
      geom_line(data=ce_wide %>%
                  filter(outcome=="Cost per infection directly averted",  
                         costtype=="Testing"), aes(x=cases, y=Total.i/1000), lty=2, col='coral'))

  
  (Fig3_rapid[[paste0(level," school, ",ql," day quarantine")]]  <- ggplot(ce_wide_rapid %>%
                                                                       filter(outcome=="Cost per infection directly averted"), 
                                                                     aes(x=cases, y=ratio/1000, fill=costtype)) + 
      geom_area() + 
      ggtitle(paste0(level," school, ",ql," day quarantine")) + 
      theme_minimal() + 
      # ylim(c(0,50)) +
      scale_fill_discrete(name = "Cost type",  guide = guide_legend(reverse = TRUE)) +
      theme(legend.position = 'bottom',
            strip.text.x = element_text(size=12)) + 
      ylab("Thousands $ US") + 
      xlab("Community cases/100k/day") +
      scale_x_continuous(breaks=c(1,5,10, 25,50, 100), limits = c(1,100)) +
      geom_text(data = ce_wide_rapid %>%
                  filter(outcome=="Cost per infection directly averted", 
                         costtype=="Testing"), 
                aes(x=cases, y=labelposition, label=round(labelposition))) + 
      # geom_label_repel( aes(x=cases, y=labelposition, label=round(labelposition))) + 
      geom_line(data=ce_wide_rapid %>%
                  filter(outcome=="Cost per infection directly averted",  
                         costtype=="Testing"), aes(x=cases, y=Total.i/1000), lty=2, col='coral'))
  

}



SFig_care['Elementary10']
SFig_care['Middle10']
SFig_test['Elementary10']
SFig_test['Middle10']

fig2 <- ggarrange(
  annotate_figure(Fig2[[paste("Elementary10unlabeled")]],
                    fig.lab="Elementary", fig.lab.size=12, fig.lab.face = 'bold'),
  annotate_figure(Fig2[[paste("Middle10")]],
                    fig.lab="Middle", fig.lab.size=12, fig.lab.face = 'bold'),
          nrow = 2, common.legend = T,legend = 'bottom',
  heights=c(3,4)
  )

pdf(file = "Fig2_schools.pdf", height=7, width=7)
fig2
dev.off()

# rapid version:
pdf(file = "s7fig_schools.pdf", height=7, width=7)
ggarrange(
  annotate_figure(Fig2_rapid[[paste("Elementary10unlabeled")]],
                  fig.lab="Elementary", fig.lab.size=12, fig.lab.face = 'bold'),
  annotate_figure(Fig2_rapid[[paste("Middle10")]],
                  fig.lab="Middle", fig.lab.size=12, fig.lab.face = 'bold'),
  nrow = 2, common.legend = T,legend = 'bottom',
  heights=c(3,4)
)
dev.off()

fig3 <- ggarrange( Fig3[[2]] + ylim(c(0,1000)),
             Fig3[[1]] + ylim(c(0,1000)), 
             Fig3[[4]],
             Fig3[[3]], nrow = 2, ncol=2,
            common.legend = T, legend="bottom")
pdf(file="Fig3_schools.pdf", width=7.5, height=7)
fig3
dev.off()

library(patchwork)
Fig3_rapid[[2]] + xlim(c(0,107)) + ylim(c(0,1500)) | 
Fig3_rapid[[4]]

######### Sensitivity analysis ############

# For all sims, group and calculate 
# (a) difference in incidence between weekly screening and A/B and 
# (b) cost per infection averted (w vs w/o testing, with 5-day schedule)

# elem %>% count(scenario, test, notify, type, test_type, days_inf, quarantine.length, adult_prob, attack_level, 
#                test_frac, test_freq, test_sens, test_days, test_start_day, teacher_susp, sim.x, sim.y) %>% 
#   as_tibble() %>% print(n=Inf)
# 

pooled_pcr = 100
followup_test = 100
collection = 8
npools = 3
planned_day = 35.5
unplanned_day = 85.9

forsens_elem <- elem %>% 
  mutate(tot_days = (n_teacher_elem+n_student_elem)*30*5/7,
         days_missed = (quarantine_days + ifelse(type=="A/B", tot_days/2,0) + 
                                 ifelse(type=="Remote", tot_days,0))/tot_days,
         ) %>% 
  group_by(scenario, test_type, type, quarantine.length, adult_prob, attack_level, days_inf, 
           test_frac, test_freq, test_sens, test_days, test_start_day, teacher_susp, turnaround.time, test, notify) %>% 
  summarize(
    all.mean = mean(as.numeric(all)),
    detected = mean(detected),
    detected2 = mean(detected2),
    class_tests = mean(as.numeric(class_test_ind)),
    quarantine_days = mean(quarantine_days)/tot_days,
    days_missed = mean(days_missed),
    all.median = median(as.numeric(all)),
    teachers.mean = mean(as.numeric(adult)),
    students.mean = mean(as.numeric(children)),
    school.mean = teachers.mean + students.mean,
    family.mean = mean(as.numeric(family)),
    tot = mean(as.numeric(tot))) %>% 
  mutate(School_inc = school.mean/(n_teacher_elem + n_student_elem), 
         adult_prob = as.numeric(adult_prob)/3*100000,
         costs_pooled_tests = class_tests * npools / n_student_elem * pooled_pcr + 
           class_tests * (n_student_elem + n_teacher_elem)/(n_class_elem+1)/n_student_elem * collection, 
         costs_followup_tests = detected / n_student_elem * (n_student_elem + n_teacher_elem)/(n_class_elem+1)/npools * followup_test,
         costs_planned_care = planned_day *30*5/7 * pmax(days_missed- quarantine_days,0) * n_student_elem/(n_student_elem+n_teacher_elem), # only count for students
         costs_unplanned_care = unplanned_day*30*5/7 * quarantine_days * n_student_elem/(n_student_elem+n_teacher_elem),
         total_costs_testing = costs_pooled_tests + costs_followup_tests,
         total_costs_care = costs_planned_care + costs_unplanned_care,
         total_costs = total_costs_care + total_costs_testing,
         schoolinc100k = School_inc/30*1e5,
         schoolinc = School_inc*(n_student_elem + n_teacher_elem),
         total_costs_school = total_costs_testing * (n_student_elem + n_teacher_elem)
         # cases per school per month
         
  ) 
  

# middle school:
forsens_middle <- middle %>% 
  mutate(tot_days = (n_teacher_middle+n_student_middle)*30*5/7,
         days_missed = (quarantine_days + ifelse(type=="A/B", tot_days/2,0) + 
                          ifelse(type=="Remote", tot_days,0))/tot_days,
  ) %>% 
  group_by(scenario,  test_type, type, quarantine.length, adult_prob, attack_level, days_inf, 
           test_frac, test_freq, test_sens, test_days, test_start_day, teacher_susp, turnaround.time, test, notify) %>% 
  summarize(
    all.mean = mean(as.numeric(all)),
    detected = mean(detected),
    detected2 = mean(detected2),
    class_tests = mean(as.numeric(class_test_ind)),
    quarantine_days = mean(quarantine_days)/tot_days,
    days_missed = mean(days_missed),
    all.median = median(as.numeric(all)),
    teachers.mean = mean(as.numeric(adult)),
    students.mean = mean(as.numeric(children)),
    school.mean = teachers.mean + students.mean,
    family.mean = mean(as.numeric(family)),
    tot = mean(as.numeric(tot))) %>% 
  mutate(School_inc = school.mean/(n_teacher + n_student), 
         adult_prob = as.numeric(adult_prob)/3*100000,
         costs_pooled_tests = class_tests * npools / n_student_middle * pooled_pcr + 
           class_tests * (n_student_middle + n_teacher_middle)/(n_class_middle+1)/n_student_middle * collection, 
         costs_followup_tests = detected / n_student_middle * (n_student_middle + n_teacher_middle)/(n_class_middle+1)/npools * followup_test,
         costs_planned_care = planned_day *30*5/7 * pmax(days_missed- quarantine_days,0) * n_student_middle/(n_student_middle+n_teacher_middle), # only count for students
         costs_unplanned_care = unplanned_day*30*5/7 * quarantine_days * n_student_middle/(n_student_middle+n_teacher_middle),
         total_costs_testing = costs_pooled_tests + costs_followup_tests,
         total_costs_care = costs_planned_care + costs_unplanned_care,
         total_costs = total_costs_care + total_costs_testing,
         schoolinc100k = School_inc/30*1e5) %>%
  mutate(     schoolinc = School_inc*(n_student_middle + n_teacher_middle),
              total_costs_school = total_costs_testing * (n_student_middle + n_teacher_middle)
  )

#########3

# incidence differences, weekly screening vs A/B
# Limit scenario a/b (2) to strategy=="Classroom quarantine". compare scenario 1 vs 2.

# if want to do middle school, change this and rerun from here
forsens <- forsens_elem

forsens_screen <- forsens %>% 
  ungroup() %>% 
  filter(scenario==1, test==TRUE, notify==TRUE) %>%
  group_by(adult_prob, attack_level, days_inf, teacher_susp, 
           test_type, test_frac, test_sens, test_days, test_start_day, turnaround.time, quarantine.length) 
  
forsens_none <- forsens %>% 
  ungroup() %>% 
  filter(scenario==1, test==F, notify==T) %>%
  group_by(adult_prob, attack_level, days_inf, teacher_susp) 

forsens_hybrid <- forsens %>% 
  ungroup() %>% 
  filter(scenario==2 & notify==T, test==F) %>%
  group_by(adult_prob, test_type, attack_level, days_inf, teacher_susp) 

# forsens_hybrid_base <- forsens_hybrid %>% filter(adult_prob==25, 
#                                                  attack_level=="Medium",
#                                                  days_inf==5,
#                                                  teacher_susp==1) %>% count(schoolinc, total_costs)

forsens_remote <- forsens %>% 
  ungroup() %>% 
  filter(scenario==3 & notify==F, test==F) %>%
  group_by(adult_prob, test_type, attack_level, days_inf, teacher_susp) 

# forsens_remote_base <- forsens_remote %>% filter(adult_prob==25, 
#                                                  attack_level=="Medium",
#                                                  days_inf==5,
#                                                  teacher_susp==1) %>% count(schoolinc, total_costs)
# 

test_vary <- function(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
                      #parameters that vary in all
                      s.attack_level="Medium",
                      s.adult_prob=25, 
                      s.days_inf=5,
                      s.teacher_susp=0.2, 
                      # parameters that vary only if testing
                      s.test_type=1, # suggest we keep fixed to simplify cost estimation
                      s.test_frac=0.9, 
                      s.test_days=1,
                      s.test_start_day=1, 
                      s.turnaround.time=1,
                      s.quarantine.length=10)
{
  screeningoutcomes <- forsens_screen %>% filter(
    test_type == s.test_type,
    test_frac==s.test_frac, 
    test_days==s.test_days,
    test_start_day ==s.test_start_day, 
    turnaround.time==s.turnaround.time,
    quarantine.length==s.quarantine.length,
    
    attack_level==ifelse(s.days_inf==10, "High", s.attack_level),
    adult_prob==s.adult_prob,
    days_inf == s.days_inf,
    teacher_susp==s.teacher_susp) %>% ungroup() %>% 
    select(schoolinc, total_costs_school)

  noscreeningoutcomes <- forsens_none %>% filter(
    attack_level==ifelse(s.days_inf==10, "High", s.attack_level),
    adult_prob==s.adult_prob,
    days_inf == s.days_inf,
    teacher_susp==s.teacher_susp) %>% ungroup() %>% 
    select(schoolinc, total_costs_school)
  
  hybridoutcomes <- forsens_hybrid %>% filter(
    attack_level==ifelse(s.days_inf==10, "High", s.attack_level),
    adult_prob==s.adult_prob,
    days_inf == s.days_inf,
    teacher_susp==s.teacher_susp) %>% ungroup() %>% 
    select(schoolinc, total_costs_school)

  remoteoutcomes <- forsens_remote %>% filter(
    attack_level==ifelse(s.days_inf==10, "High", s.attack_level),
    adult_prob==s.adult_prob,
    days_inf == s.days_inf,
    teacher_susp==s.teacher_susp) %>% ungroup() %>% 
    select(schoolinc, total_costs_school)
  
  incdiff_schedule = (mean(screeningoutcomes$schoolinc) -
    mean(hybridoutcomes$schoolinc))#/schoolsize
  # incdiff_schedule = (mean(screeningoutcomes$schoolinc) -
  #                       mean(remoteoutcomes$schoolinc))#/schoolsize
  
  # incdiff_schedule_relative = mean((screeningoutcomes$schoolinc -
  #                       remoteoutcomes$schoolinc)/remoteoutcomes$schoolinc)#/schoolsize
  incdiff_schedule_relative = mean((screeningoutcomes$schoolinc -
                                      remoteoutcomes$schoolinc)/remoteoutcomes$schoolinc)#/schoolsize
  
  incdiff_testing = mean(noscreeningoutcomes$schoolinc) - 
    mean(screeningoutcomes$schoolinc)
  
  costdiff_testing = mean(noscreeningoutcomes$total_costs_school) - 
    mean(screeningoutcomes$total_costs_school)
  
  costeff_testing = -costdiff_testing/incdiff_testing  
  
  return(list("incdiff_schedule"= incdiff_schedule, "costeff_testing"=costeff_testing, "incdiff_schedule_relative"= incdiff_schedule_relative,
              "incdiff_testing"=incdiff_testing, "costdiff_testing"=costdiff_testing))
}


test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote)
test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
          s.quarantine.length = 7)
test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
          s.attack_level = "Low")
test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
          s.days_inf = 10)
test_vary(forsens_screen, forsens_none, forsens_hybrid,forsens_remote,
          s.teacher_susp = 0.2)
test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
          s.test_type = 2)
test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
          s.turnaround.time = 2) # for community tests only

testarray <- rbind(
  c("attack_level", "Low", "High","Mitigation (masking, student vaccination, etc)", "Low (1.5x transmission)", "Medium","High (0.5x transmission)"),
  c("adult_prob",10,50, "Community notification rate, per 100k/day","10", "25", "50" ),
  c("days_inf",NA,10, "Mean infectious duration", NA, "5 days","10 days"),
  c("teacher_susp",1,NA, "Teacher vaccine protection", "0","80%",NA),
  # c("test_type",NA,2, "Population tested", NA, "Students and staff", "Staff only"),
  c("test_frac",0.7,NA, "Testing sensitivity (assay sensitivity x coverage)","63%","81%",NA),
  c("test_days",NA,2, "Testing days per week",NA,1,2),
  c("test_start_day",5,3, "Testing day","Friday","Monday","Wednesday"),
  # c("turnaround.time",NA,2, "Test turnaround time",NA,"1 day", "2 days"),
  c("quarantine.length",7,NA, "Quarantine duration if screening", "7 days", "10 days", NA)
  # c(NA,NA,NA, "School level", NA, "Elementary school", "Middle school")
)
sensis <- array(NA, dim=c(1,6))
sensis[1,] <- unlist(c(test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote)[c(1,2,3,1,2,3)]))
for (i in 1:nrow(testarray))
{
  if(!is.na(testarray[i,1])) 
     sensis <- rbind(sensis,
                unlist(as.numeric(c(
                  eval(parse(text = paste0("test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
                                            s.",testarray[i,1],"='",testarray[i,2],"')")))[1:3],
                  eval(parse(text = paste0("test_vary(forsens_screen, forsens_none, forsens_hybrid, forsens_remote,
                                            s.",testarray[i,1],"='",testarray[i,3],"')")))[1:3]
                ))))
  
}
rownames(sensis) <- c('base',testarray[,1])
sensis

tornadoinc <- data.frame(sensis[,c(1,4)])
colnames(tornadoinc) <- c("min","max")
tornadoinc$interp <- c(NA,testarray[,4])
tornadoinc$lowsetting <- c(NA,testarray[,5])
tornadoinc$basesetting <- c(NA,testarray[,6])
tornadoinc$highsetting <- c(NA,testarray[,7])
tornadoinc$names <- c(NA,testarray[,4])
tornadoinc_wide <- pivot_longer(data=tornadoinc, cols=c("min","max"), names_to = 'val', values_to = 'output')
tornadoinc_wide$setting <- ifelse(tornadoinc_wide$val=="min",
                                  tornadoinc_wide$lowsetting, tornadoinc_wide$highsetting)

tornadocost <- data.frame(sensis[,c(2,5)])
colnames(tornadocost) <- c("min","max")
tornadocost$interp <- c(NA,testarray[,4])
tornadocost$lowsetting <- c(NA,testarray[,5])
tornadocost$basesetting <- c(NA,testarray[,6])
tornadocost$highsetting <- c(NA,testarray[,7])
tornadocost$names <- c(NA,testarray[,4])
tornadocost_wide <- pivot_longer(data=tornadocost, cols=c("min","max"), names_to = 'val', values_to = 'output')
tornadocost_wide$setting <- ifelse(tornadocost_wide$val=="min",
                                  tornadocost_wide$lowsetting, tornadocost_wide$highsetting)

# tornadoinc_r <- data.frame(sensis[,c(3,6)])
# colnames(tornadoinc_r) <- c("min","max")
# tornadoinc_r$interp <- c(NA,testarray[,4])
# tornadoinc_r$lowsetting <- c(NA,testarray[,5])
# tornadoinc_r$basesetting <- c(NA,testarray[,6])
# tornadoinc_r$highsetting <- c(NA,testarray[,7])
# tornadoinc_r$names <- c(NA,testarray[,4])
# tornadoinc_r_wide <- pivot_longer(data=tornadoinc_r, cols=c("min","max"), names_to = 'val', values_to = 'output')
# tornadoinc_r_wide$setting <- ifelse(tornadoinc_r_wide$val=="min",
#                                   tornadoinc_r_wide$lowsetting, tornadoinc_r_wide$highsetting)

source('tornadoplot_schools.R')
class(tornadoinc_wide) <- c("tornado", class(tornadoinc_wide))
attr(tornadoinc_wide, "output_name") <- "output"


# tornadoinc_wide$output <- tornadoinc_wide$output*schoolsize
baseline <- tornadoinc_wide[1,'output']
(sensplot_inc <- 
  ggplot_tornado(dat = tornadoinc_wide %>% filter(names != 'base'), 
               baseline_output = tornadoinc_wide[1,'output'])  + 
  theme(legend.position = "none") + 
  geom_text(aes(y=output, label=setting, group=val),
            position = position_dodge(width = 0.6),col='black') +
  geom_text(aes(y=baseline, label=basesetting), 
            col='azure4') +
    # ylim(c(0,2.5)) +
    ylim(c(0,11))+
  ylab("Incidence increase (monthly infections per school),\n5-day in-person with weekly testing vs hybrid schedule")+
    # ggtitle("A. Transmission (testing vs hybrid), Elementary school")
  ggtitle("B. Transmission (testing vs hybrid), Middle school")
)

class(tornadocost_wide) <- c("tornado", class(tornadocost_wide))
attr(tornadocost_wide, "output_name") <- "output"
baseline <- tornadocost_wide[1,'output']
(sensplot_cost <- ggplot_tornado(dat = tornadocost_wide %>% filter(names != 'base'), 
               baseline_output = tornadocost_wide[1,'output'])  + 
  theme(legend.position = "none") + 
  geom_text(aes(y=output, label=setting, group=val),
            position = position_dodge(width = 0.8),col='black') +
  geom_text(aes(y=baseline, label=basesetting), 
            col='azure4') +
  # scale_y_log10(limits=c(-3e4,2.5e5)) + 
  # ylim(c(-3e4,2e5)) +
  ylab("Cost per infection averted, $US,\nno testing versus weekly testing (5-day schedules)")+
    # ggtitle("C. Cost, Elementary school") + ylim(0,180000)
  ggtitle("D. Cost, Middle school") + ylim(-800,21000)
)



