library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(data.table)

#Set mitigation bounds
mitigation_a.low <- 0.2
mitigation_a.mid <- 0.3
mitigation_a.high <- 0.4

mitigation_b.low <- 0.6
mitigation_b.mid <- 0.7
mitigation_b.high <- 0.8

mitigation_c.low <- 0.9
mitigation_c.mid <- 0.95
mitigation_c.high <- 1

plotcols <- brewer.pal(6, 'Dark2')

data <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Full Paper Output/Raw Data/revision_output_final_training.rds"))

data_test <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Full Paper Output/Raw Data/revision_output_final_test.rds"))

data_full <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Full Paper Output/Raw Data/revision_output_final_full.rds"))

setwd("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/JNO Full Paper Output/Results")

#Define potential smoothing functions
specifications <- c(linear = "poly(prob, degree = 1, raw = TRUE)*poly(mitigation, degree = 1, raw = TRUE)",
                    quadratic = "poly(prob, degree = 2, raw = TRUE)*poly(mitigation, degree = 2, raw = TRUE)",
                    cubic = "poly(prob, degree = 3, raw = TRUE)*poly(mitigation, degree = 3, raw = TRUE)",
                    quartic = "poly(prob, degree = 4, raw = TRUE)*poly(mitigation, degree = 4, raw = TRUE)",
                    log = "I(log(prob))*I(log(mitigation))")

predict.cont <- expand.grid(prob = seq(from = 0, to = 50, by = 0.5), mitigation = seq(from = 0, to = 1, by = 0.05))
predict.diff <- expand.grid(prob = seq(from = 0, to = 50, by = 0.5), mitigation.high = c(mitigation_b.low, mitigation_b.mid, mitigation_b.high), mitigation.low = c(mitigation_a.low, mitigation_a.mid, mitigation_a.high)) %>%
  filter((mitigation.high == mitigation_b.low & mitigation.low == mitigation_a.high) | (mitigation.high == mitigation_b.mid & mitigation.low == mitigation_a.mid) | (mitigation.high == mitigation_b.high & mitigation.low == mitigation_a.low))

r2.track <- data.table(expand.grid(outcomes = c("inschool.outbreak_inc.1", "all.overall", "hosp"), variant = unique(data$variant.attack), child.vax.rate = unique(data$child.vax), adult.vax.rate = unique(data$teacher_susp), vax.eff = unique(data$vax_eff), notify.scenario = unique(data$notify.scenario)) %>%
  mutate(group = paste(variant, child.vax.rate, adult.vax.rate, vax.eff, notify.scenario)) %>%
  filter(group %in% unique(data$analysis.groups)) %>%
  mutate(r2 = numeric(nrow(.)), r2.binned = numeric(nrow(.)), r2.binned.small = numeric(nrow(.)), opt.spec = character(nrow(.))))

cutoff.track <- data.table(expand.grid(outcomes = c("inschool.outbreak_inc.1", "all.overall", "hosp"), variant = unique(data$variant.attack), child.vax.rate = unique(data$child.vax), adult.vax.rate = unique(data$teacher_susp), vax.eff = unique(data$vax_eff), notify.scenario = unique(data$notify.scenario), risk.tolerance = c("high", "medium", "low"), mitigation.baseline = c(mitigation_a.high, mitigation_a.mid, mitigation_a.low, mitigation_b.high)) %>%
  mutate(group = paste(variant, child.vax.rate, adult.vax.rate, vax.eff, notify.scenario)) %>%
  filter(group %in% unique(data$analysis.groups)) %>%
  mutate(cutoff.level = case_when(outcomes == "inschool.outbreak_inc.1" & risk.tolerance == "high" ~ 0.75,
                                  outcomes == "inschool.outbreak_inc.1" & risk.tolerance == "medium" ~ 0.5,
                                  outcomes == "inschool.outbreak_inc.1" & risk.tolerance == "low" ~ 0.25,
                                  outcomes == "all.overall" & risk.tolerance == "high" ~ 10,
                                  outcomes == "all.overall" & risk.tolerance == "medium" ~ 5,
                                  outcomes == "all.overall" & risk.tolerance == "low" ~ 3,
                                  outcomes == "hosp" & risk.tolerance == "high" ~ 5,
                                  outcomes == "hosp" & risk.tolerance == "medium" ~ 3,
                                  outcomes == "hosp" & risk.tolerance == "low" ~ 1),
         threshold = numeric(nrow(.))))

truncate.prob <- function(vector){
  return(sapply(sapply(vector, min, 1), max, 0))
}

large.bins <- function(data){
  data.binned1 <- data.table(data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 5), labels = seq(from = 5, to = 60, by = 5)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
  data.binned <- data.binned1[, list(mean.outcome = mean(outcome)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 2.5)
  
  return(data.binned)
}

small.bins <- function(data){
  data.binned1 <- data.table(data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 1), labels = seq(from = 1, to = 60, by = 1)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
  data.binned <- data.binned1[, list(mean.outcome = mean(outcome)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 0.5)
  
  return(data.binned)
}

fit.meta.model <- function(specification, outcome, training.data, test.data, test.data.binned, test.data.binned.small){
  
  if(spec == "log"){
    training.data <- training.data[mitigation != 0]
    test.data <- test.data[mitigation != 0]
  }
  
  sse_total <- sum((test.data$outcome - mean(test.data$outcome))^2)
  sse_total.binned <- sum((test.data.binned$mean.outcome - mean(test.data.binned$mean.outcome))^2)
  sse_total.binned.small <- sum((test.data.binned.small$mean.outcome - mean(test.data.binned.small$mean.outcome))^2)
  
  reg.fit <- glm(as.formula(paste(outcome, specification, sep = " ~ ")), data = training.data, family = gaussian(link = "identity"))
  if(outcome == "inschool.outbreak_inc.1"){
    fit.sse <- sum((truncate.prob(predict(reg.fit, newdata = test.data)) - test.data$outcome)^2)
    fit.sse.binned <- sum((truncate.prob(predict(reg.fit, newdata = test.data.binned)) - test.data.binned$mean.outcome)^2)
    fit.sse.binned.small <- sum((truncate.prob(predict(reg.fit, newdata = test.data.binned.small)) - test.data.binned.small$mean.outcome)^2)
  }else{
    fit.sse <- sum((predict(reg.fit, newdata = test.data) - test.data$outcome)^2)
    fit.sse.binned <- sum((predict(reg.fit, newdata = test.data.binned) - test.data.binned$mean.outcome)^2)
    fit.sse.binned.small <- sum((predict(reg.fit, newdata = test.data.binned.small) - test.data.binned.small$mean.outcome)^2)
  }
  
  return(c(r2 = 1-fit.sse/sse_total, r2.binned = 1-fit.sse.binned/sse_total.binned, r2.binned.small = 1-fit.sse.binned.small/sse_total.binned.small))
  
}

for(outcome in c("inschool.outbreak_inc.1", "all.overall", "hosp")){
  ylimit = ifelse(outcome == "all.overall", 45, ifelse(outcome == "hosp", 10, NA))
  ytitle = ifelse(outcome == "all.overall", "Average Additional Cases per Month in School Community", ifelse(outcome == "hosp", "Average Additional Hospitalizations per 100k per Month in School Community", NA))

  for(adult.vax.rate in unique(data$teacher_susp)){
    print(paste("Working on adult vaccination rate", adult.vax.rate))

    for(notify.scenario in unique(data$notify.scenario)){
      print(paste("Working on notify scenario", notify.scenario))

      for(vax.eff in unique(data$vax_eff)){
        print(paste("Working on vaccine efficacy", vax.eff))

        for(variant in unique(data$variant.attack)){
          print(paste("Working on variant", variant))
          
          variant.title <- unique(data$variant.title[data$variant.attack == variant])
          fig2.data <- data.frame(outcome = numeric(0), mitigation.group = character(0), prob = numeric(0), child.vax = character(0))

          for(child.vax.rate in unique(data$child.vax)){
            
            group.name <- paste(variant, child.vax.rate, adult.vax.rate, vax.eff, notify.scenario)
            
            if(!(group.name %in% data$analysis.groups)){
              next
            }
            
            print(paste("Working on child vaccination rate", child.vax.rate))

            print(paste("Running regressions for outcome", outcome))

            loop.data_training <- data[group.name == analysis.groups] %>% mutate(outcome = !!sym(outcome))
            
            loop.data <- data_test[group.name == analysis.groups] %>% mutate(outcome = !!sym(outcome))
            
            loop.data_full <- data_full[group.name == analysis.groups] %>% mutate(outcome = !!sym(outcome))

            data.binned <- large.bins(loop.data)

            data.binned.small <- small.bins(loop.data)
            
            reg.tracker <- data.frame(spec = c("linear", "quadratic", "cubic", "quartic", "log"),
                                      r2 = NA,
                                      r2.binned = NA,
                                      r2.binned.small = NA)

            for(spec in reg.tracker$spec){
              reg.tracker[reg.tracker$spec == spec,c("r2", "r2.binned", "r2.binned.small")] <- fit.meta.model(specification = specifications[spec],
                                                                                      outcome = outcome,
                                                                                      training.data = loop.data_training,
                                                                                      test.data = loop.data,
                                                                                      test.data.binned = data.binned,
                                                                                      test.data.binned.small = data.binned.small)
            }
            
            best.fit.spec <- reg.tracker$spec[which.max(reg.tracker$r2)]
            best.fit.reg <- glm(as.formula(paste(outcome, specifications[best.fit.spec], sep = " ~ ")), data = loop.data_full, family = gaussian(link = "identity"))
            
            print(best.fit.spec)
            
            r2.track[group == group.name & outcomes == outcome, c("opt.spec", "r2", "r2.binned", "r2.binned.small")] <- reg.tracker[which.max(reg.tracker$r2),]
            
            if(outcome == "inschool.outbreak_inc.1"){
              predict.cont$outcome <- truncate.prob(predict(best.fit.reg, newdata = predict.cont))
              cutoff.track$threshold[cutoff.track$group == group.name & cutoff.track$outcomes == outcome] <- sapply(1:length(cutoff.track$threshold[cutoff.track$group == group.name & cutoff.track$outcomes == outcome]),
                                                                                 function(a){max(predict.cont$prob[round(predict.cont$mitigation, 2) == cutoff.track$mitigation.baseline[cutoff.track$group == group.name & cutoff.track$outcomes == outcome][a] & predict.cont$outcome <= cutoff.track$cutoff.level[cutoff.track$group == group.name & cutoff.track$outcomes == outcome][a]])})
              
              
              plot <- ggplot(data = predict.cont, aes(x = prob, y = mitigation)) +
                geom_raster(aes(fill = outcome), interpolate = TRUE) +
                scale_fill_gradient(low = "blue", high = "yellow", limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c(as.character(0), as.character(0.5), as.character(1)), name = "Transmission Probability") +
                geom_contour(aes(z = outcome, color = factor(..level..)), breaks = c(0, 0.25, 0.5, 0.75), size = 1.25) +
                scale_color_discrete(type = c("0.25" = "green", "0.5" = "orange", "0.75" = "purple")) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_a.high, yend = mitigation_a.high, linetype = "A: Simple ventilation and hand washing"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_a.low, yend = mitigation_a.low, linetype = "A: Simple ventilation and hand washing"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 0, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple ventilation and hand washing"), size = 1.25) +
                geom_segment(aes(x = 50, xend = 50, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple ventilation and hand washing"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_b.low, yend = mitigation_b.low, linetype = "B: A plus universal masking of students\nand educators/staff"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_b.high, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand educators/staff"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 0, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand educators/staff"), size = 1.25) +
                geom_segment(aes(x = 50, xend = 50, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand educators/staff"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_c.low, yend = mitigation_c.low, linetype = "C: 2020-2021 package of interventions"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 50, y = mitigation_c.high, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions"), size = 1.25) +
                geom_segment(aes(x = 0, xend = 0, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions"), size = 1.25) +
                geom_segment(aes(x = 50, xend = 50, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions"), size = 1.25) +
                geom_segment(aes(x = !! cutoff.track[group == group.name & outcomes == outcome & cutoff.level == 0.5 & mitigation.baseline == mitigation_a.mid]$threshold, xend = !! cutoff.track[group == group.name & outcomes == outcome & cutoff.level == 0.5 & mitigation.baseline == mitigation_a.mid]$threshold, y = mitigation_a.mid, yend = mitigation_b.low), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
                #geom_segment(aes(x = !! cutoff.track[group == group.name & outcomes == outcome & cutoff.level == 0.5 & mitigation.baseline == mitigation_b.high]$threshold, xend = !! cutoff.track[group == group.name & outcomes == outcome & cutoff.level == 0.5 & mitigation.baseline == mitigation_b.high]$threshold, y = 0.8, yend = 0.9), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
                theme_minimal() + #theme_opts +
                theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5)) +
                labs(x = "Observed Local Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = ""), color = "Probability Thresholds", linetype = "Mitigation Interventions")
              
              saveRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = ""))
            }else{
              
              predict.diff$outcome <- predict(best.fit.reg, newdata = predict.diff %>% rename(mitigation = mitigation.low)) - predict(best.fit.reg, newdata = predict.diff %>% rename(mitigation = mitigation.high))
              
              cutoff.track$threshold[cutoff.track$group == group.name & cutoff.track$outcomes == outcome] <- sapply(1:length(cutoff.track$threshold[cutoff.track$group == group.name & cutoff.track$outcomes == outcome]),
                                                                                                                    function(a){max(predict.diff$prob[round(predict.diff$mitigation.low, 2) == cutoff.track$mitigation.baseline[cutoff.track$group == group.name & cutoff.track$outcomes == outcome][a] & predict.diff$outcome <= cutoff.track$cutoff.level[cutoff.track$group == group.name & cutoff.track$outcomes == outcome][a]])})
              
              fig2.data <- rbind(fig2.data, data.frame(outcome = predict.diff$outcome, mitigation.group = paste(predict.diff$mitigation.high*100, "% to ", predict.diff$mitigation.low*100, "% mitigation effectiveness", sep = ""), prob = predict.diff$prob, child.vax = paste(child.vax.rate*100, "%", sep = "")))
            }
            

          }
          if(outcome != "inschool.outbreak_inc.1"){
            plot <- ggplot(fig2.data,
                                             aes(x = prob, y = outcome,
                                                 group = as.factor(child.vax), col = as.factor(child.vax))) +
              geom_line() + #arrow = arrow(ends = "last", type = "closed", length = unit(0.125, "inches"), angle = 20)
              scale_color_discrete(name = "Student Vaccination Rate") +
              facet_grid(cols = vars(mitigation.group)) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 12)) +
              labs(x = "Observed Local Incidence (cases/100K/day)", y = ytitle, title = variant.title) + ylim(0, ylimit)
            
            saveRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = ""))
          }
        }
      }
    }
  }
}

write.csv(r2.track, file = "smoothing_fits_r2.csv")
write.csv(cutoff.track, file = "smoothed_cutoffs.csv")

for(adult.vax.rate in c(0.5, 0.7, 0.9)){
  for(notify.scenario in c("yes.wo.test", "yes.w.test90")){
    for(variant in c(0.07, 0.035, 0.02)){
      for(vax.eff in c(0.25, 0.5, 0.7, 0.9)){
        if(!(paste(adult.vax.rate, notify.scenario, variant, vax.eff) %in%
           paste(cutoff.track$adult.vax.rate, cutoff.track$notify.scenario, cutoff.track$variant, cutoff.track$vax.eff))) next
          cutoff.subset <- as.data.frame(cutoff.track)[cutoff.track$adult.vax.rate == adult.vax.rate & cutoff.track$notify.scenario == notify.scenario & cutoff.track$variant == variant & cutoff.track$vax.eff == vax.eff,]
        write.csv(cutoff.subset, file = paste(paste("variant", variant, "adultvax", adult.vax.rate, notify.scenario, "vaxeff", vax.eff, sep = "_"), ".csv", sep = ""))
      }
    }
  }
}

#Figure 1, Main Text
adult.vax.rate <- 0.7
vax.eff <- 0.7
notify.scenario <- "yes.wo.test"
outcome <- "inschool.outbreak_inc.1"
plot.list <- list()
i <- 0

for(variant in c(0.07, 0.035, 0.02)){
  plot.list.var <- list()
  j <- 0
  i <- i + 1
  for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
    j <- j + 1
    plot.list.var[[j]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = ""))
  } 
  var.plot <- ggarrange(plotlist =  plot.list.var, nrow = 1, ncol = 4, legend = 'none')
  plot.list[[i]] <- annotate_figure(var.plot, top = text_grob(label = ifelse(variant == 0.07, "Delta variant", ifelse(variant == 0.035, "Alpha variant", ifelse(variant == 0.02, "Wild-type", "ERROR"))), hjust = 0, size = 13, x = 0))
}

combined.plot <- ggarrange(plotlist =  plot.list, nrow = 3, common.legend = T, legend = 'right', legend.grob = get_legend(plot.list.var[[1]]))

ggsave("figure1_main_text.pdf", combined.plot, width = 18.12, height = 13, units = "in")
ggsave("figure1_main_text.jpg", combined.plot, width = 18.12, height = 13, units = "in")

#Figure 2, Main Text
adult.vax.rate <- 0.7
vax.eff <- 0.7
child.vax.rate <- 0.9
notify.scenario <- "yes.wo.test"
outcome <- "all.overall"
plot.list <- list()
i <- 0

for(variant in c(0.07, 0.035, 0.02)){
  i <- i + 1
  plot.list[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = ""))
}

combined.plot <- ggarrange(plotlist =  plot.list, nrow = 3, ncol = 1, common.legend = T, legend = 'right')

ggsave("figure2_main_text.pdf", combined.plot, width = 14, height = 18.12, units = "in")
ggsave('figure2_main_text.jpg', combined.plot, width = 14, height = 18.12, units = "in")

#Figure 3, Main Text
adult.vax.rate <- 0.7
child.vax.rate <- 0.9
variant <- 0.07
outcome <- "all.overall"
plot.list <- list()

vax.eff <- 0.7
notify.scenario <- "yes.w.test90"
plot.list[[1]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "Weekly screening, 90% uptake")

vax.eff <- 0.5
notify.scenario <- "yes.wo.test"
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "50% vaccine efficacy")

combined.plot <- ggarrange(plotlist =  plot.list, nrow = 2, ncol = 1, common.legend = T, legend = 'right', labels = "AUTO")

ggsave("figure3_main_text.pdf", combined.plot, width = 18.12, height = 13, units = "in")
ggsave("figure3_main_text.jpg", combined.plot, width = 18.12, height = 13, units = "in")



#eFigure 1, Supplement
adult.vax.rate <- 0.7
vax.eff <- 0.7
child.vax.rate <- 0.9
notify.scenario <- "yes.wo.test"
outcome <- "hosp"
plot.list <- list()
i <- 0

for(variant in c(0.07, 0.035, 0.02)){
  i <- i + 1
  plot.list[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = ""))
}

combined.plot <- ggarrange(plotlist =  plot.list, nrow = 3, ncol = 1, common.legend = T, legend = 'right')

ggsave("efigure1_supplement.pdf", combined.plot, width = 14, height = 18.12, units = "in")
ggsave('efigure1_supplement.jpg', combined.plot, width = 14, height = 18.12, units = "in")

#eFigure 2, Supplement
variant <- 0.07
adult.vax.rate <- 0.5
vax.eff <- 0.7
notify.scenario <- "yes.wo.test"
plot.list <- list()

outcome <- "inschool.outbreak_inc.1"
plot.list.panel <- list()
i <- 0

for(child.vax.rate in c(0, 0.25, 0.5)){
  i <- i + 1
  plot.list.panel[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = "")) + theme(plot.margin = unit(c(1,1,1,0.5), "cm"))
} 

plot.list[[1]] <- ggarrange(plotlist =  plot.list.panel, nrow = 1, ncol = 3, common.legend = T, legend = 'right')

outcome <- "all.overall"
child.vax.rate <- 0.9
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

plot.list[[3]] <- NULL

outcome <- "hosp"
plot.list[[4]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

combined.plot <- ggarrange(plotlist = plot.list, nrow = 4, ncol = 1, common.legend = F, labels = c("A: Probability of at least one in-school transmission per month", "B: Average Additional Cases per Month in School Community", "", "C: Average Additional Hospitalizations per 100k per Month in School Community"), vjust = 1, hjust = 0, font.label = list(size = 14), heights = c(1,1,0.05,1))

ggsave("efigure2_supplement.pdf", combined.plot, width = 16, height = 18.25, units = "in")
ggsave("efigure2_supplement.jpg", combined.plot, width = 16, height = 18.25, units = "in")

#eFigure 3, Supplement
variant <- 0.07
adult.vax.rate <- 0.7
vax.eff <- 0.7
notify.scenario <- "yes.w.test90"
plot.list <- list()

outcome <- "inschool.outbreak_inc.1"
plot.list.panel <- list()
i <- 0

for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
  i <- i + 1
  plot.list.panel[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = "")) + theme(plot.margin = unit(c(1,1,1,0.5), "cm"))
} 

plot.list[[1]] <- ggarrange(plotlist =  plot.list.panel, nrow = 1, ncol = 4, common.legend = T, legend = 'right')

child.vax.rate <- 0.9
outcome <- "hosp"
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

combined.plot <- ggarrange(plotlist = plot.list, nrow = 2, ncol = 1, common.legend = F, labels = c("A: Probability of at least one in-school transmission per month", "B: Average Additional Hospitalizations per 100k per Month in School Community"), vjust = 1, hjust = 0, font.label = list(size = 14))

ggsave("efigure3_supplement.pdf", combined.plot, width = 23, height = 13.5, units = "in")
ggsave("efigure3_supplement.jpg", combined.plot, width = 23, height = 13.5, units = "in")

#eFigure 4, Supplement
variant <- 0.07
adult.vax.rate <- 0.7
vax.eff <- 0.5
notify.scenario <- "yes.wo.test"
plot.list <- list()

outcome <- "inschool.outbreak_inc.1"
plot.list.panel <- list()
i <- 0

for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
  i <- i + 1
  plot.list.panel[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = "")) + theme(plot.margin = unit(c(1,1,1,0.5), "cm"))
} 

plot.list[[1]] <- ggarrange(plotlist =  plot.list.panel, nrow = 1, ncol = 4, common.legend = T, legend = 'right')

child.vax.rate <- 0.9
outcome <- "hosp"
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

combined.plot <- ggarrange(plotlist = plot.list, nrow = 2, ncol = 1, common.legend = F, labels = c("A: Probability of at least one in-school transmission per month", "B: Average Additional Hospitalizations per 100k per Month in School Community"), vjust = 1, hjust = 0, font.label = list(size = 14))

ggsave("efigure4_supplement.pdf", combined.plot, width = 23, height = 13.5, units = "in")
ggsave("efigure4_supplement.jpg", combined.plot, width = 23, height = 13.5, units = "in")

#eFigure 5, Supplement
variant <- 0.07
adult.vax.rate <- 0.7
vax.eff <- 0.25
notify.scenario <- "yes.wo.test"
plot.list <- list()

outcome <- "inschool.outbreak_inc.1"
plot.list.panel <- list()
i <- 0

for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
  i <- i + 1
  plot.list.panel[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = "")) + theme(plot.margin = unit(c(1,1,1,0.5), "cm"))
} 

plot.list[[1]] <- ggarrange(plotlist =  plot.list.panel, nrow = 1, ncol = 4, common.legend = T, legend = 'right')

outcome <- "all.overall"
child.vax.rate <- 0.9
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

plot.list[[3]] <- NULL

outcome <- "hosp"
plot.list[[4]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

combined.plot <- ggarrange(plotlist = plot.list, nrow = 4, ncol = 1, common.legend = F, labels = c("A: Probability of at least one in-school transmission per month", "B: Average Additional Cases per Month in School Community", "", "C: Average Additional Hospitalizations per 100k per Month in School Community"), vjust = 1, hjust = 0, font.label = list(size = 14), heights = c(1,1,0.05,1))

ggsave("efigure5_supplement.pdf", combined.plot, width = 16, height = 18.25, units = "in")
ggsave("efigure5_supplement.jpg", combined.plot, width = 16, height = 18.25, units = "in")

#eFigure 6, Supplement
variant <- 0.07
adult.vax.rate <- 0.7
vax.eff <- 0.9
notify.scenario <- "yes.wo.test"
plot.list <- list()

outcome <- "inschool.outbreak_inc.1"
plot.list.panel <- list()
i <- 0

for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
  i <- i + 1
  plot.list.panel[[i]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = paste(child.vax.rate*100, "% student vaccination coverage", sep = "")) + theme(plot.margin = unit(c(1,1,1,0.5), "cm"))
} 

plot.list[[1]] <- ggarrange(plotlist =  plot.list.panel, nrow = 1, ncol = 4, common.legend = T, legend = 'right')

outcome <- "all.overall"
child.vax.rate <- 0.9
plot.list[[2]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

plot.list[[3]] <- NULL

outcome <- "hosp"
plot.list[[4]] <- readRDS(plot, file=paste(paste(outcome, "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, sep = "_"), ".rds", sep = "")) + labs(title = "")

combined.plot <- ggarrange(plotlist = plot.list, nrow = 4, ncol = 1, common.legend = F, labels = c("A: Probability of at least one in-school transmission per month", "B: Average Additional Cases per Month in School Community", "", "C: Average Additional Hospitalizations per 100k per Month in School Community"), vjust = 1, hjust = 0, font.label = list(size = 14), heights = c(1,1,0.05,1))

ggsave("efigure6_supplement.pdf", combined.plot, width = 16, height = 18.25, units = "in")
ggsave("efigure6_supplement.jpg", combined.plot, width = 16, height = 18.25, units = "in")
