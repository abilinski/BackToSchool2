library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(data.table)

plotcols <- brewer.pal(6, 'Dark2')

data <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/Revisions Output/revision_output_final.rds"))

data_test <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/Revisions Output/revision_output_final_test.rds"))

data_full <- data.table(readRDS("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/Revisions Output/revision_output_final_full.rds"))

setwd("C:/Users/johnc/Dropbox (Harvard University)/Documents/Research/COVID-19/BackToSchool2/4 - Output/Paper 3/Revisions Output/Results")

#Set parameters
mitigation_a.low <- 0.2
mitigation_a.high <- 0.4

mitigation_b.low <- 0.7
mitigation_b.high <- 0.8

mitigation_c.low <- 0.9
mitigation_c.high <- 0.9

#Define potential smoothing functions
reg.linear <- "poly(prob, degree = 1, raw = TRUE)*poly(mitigation, degree = 1, raw = TRUE)"
reg.quadratic <- "poly(prob, degree = 2, raw = TRUE)*poly(mitigation, degree = 2, raw = TRUE)"
reg.cubic <- "poly(prob, degree = 3, raw = TRUE)*poly(mitigation, degree = 3, raw = TRUE)"
reg.quartic <- "poly(prob, degree = 4, raw = TRUE)*poly(mitigation, degree = 4, raw = TRUE)"
reg.log <- "I(log(prob))*I(log(mitigation))"

predict.df <- expand.grid(prob = seq(from = 0.1, to = 50, by = 0.1), mitigation = seq(from = 0.01, to = 1, by = 0.01))

r2.track <- data.frame(outcome = character(), adult.vax = numeric(), child.vax = numeric(), variant = numeric(), fxn = character(), r2 = numeric(), rmse.total = numeric(), rmse.residual = numeric(), mae.total = numeric(), mae.residual = numeric())
r2.track.binned <- data.frame(outcome = character(), adult.vax = numeric(), child.vax = numeric(), variant = numeric(), fxn = character(), r2 = numeric(), rmse.total = numeric(), rmse.residual = numeric(), mae.total = numeric(), mae.residual = numeric())
r2.track.binned.small <- data.frame(outcome = character(), adult.vax = numeric(), child.vax = numeric(), variant = numeric(), fxn = character(), r2 = numeric(), rmse.total = numeric(), rmse.residual = numeric(), mae.total = numeric(), mae.residual = numeric())
cutoff.track <- data.frame(outcome = character(), mitigation.level = numeric(), adult.vax = numeric(), child.vax = numeric(), variant = numeric(), cutoff.level = numeric(), threshold = numeric())

truncate.prob <- function(vector){
  return(sapply(sapply(vector, min, 1), max, 0))
}

for(outcome in c("inschool.outbreak_inc.1")){

  for(adult.vax.rate in c(0.7, 0.5)){
    print(paste("Working on adult vaccination rate", adult.vax.rate))

    for(notify.scenario in c("yes.wo.test")){
      print(paste("Working on notify scenario", notify.scenario))

      for(vax.eff in c(0.7)){
        print(paste("Working on vaccine efficacy", vax.eff))

        min.prob <- 1
        max.prob <- 0

        plot.list <- list()
        plot.list.raw <- list()
        plot.list.raw.small <- list()

        i <- 0

        for(variant in c(0.02, 0.035, 0.07)){
          print(paste("Working on variant", variant))

          for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }
            
            print(paste("Working on child vaccination rate", child.vax.rate))

            print(paste("Running regressions for outcome", outcome))

            loop.data_training <- data[variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            loop.data <- data_test[variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]

            data.binned1 <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 5), labels = seq(from = 5, to = 60, by = 5)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned <- data.binned1[, list(mean.outcome = mean(inschool.outbreak_inc.1)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 2.5)

            data.binned1.small <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 1), labels = seq(from = 1, to = 60, by = 1)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned.small <- data.binned1.small[, list(mean.outcome = mean(inschool.outbreak_inc.1)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 0.5)

            sse_total <- sum((unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome])))^2)
            sse_total.binned <- sum((unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome)))^2)
            sse_total.binned.small <- sum((unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome)))^2)
            sae_total <- sum(abs(unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome]))))
            sae_total.binned <- sum(abs(unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome))))
            sae_total.binned.small <- sum(abs(unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome))))

            reg.linear.fit <- glm(as.formula(paste(outcome, reg.linear, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            linear.sse <- sum((truncate.prob(predict(reg.linear.fit, newdata = loop.data)) - loop.data[,..outcome])^2)
            linear.sse.binned <- sum((truncate.prob(predict(reg.linear.fit, newdata = data.binned)) - data.binned$mean.outcome)^2)
            linear.sse.binned.small <- sum((truncate.prob(predict(reg.linear.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome)^2)
            linear.sae <- sum(abs(truncate.prob(predict(reg.linear.fit, newdata = loop.data)) - loop.data[,..outcome]))
            linear.sae.binned <- sum(abs(truncate.prob(predict(reg.linear.fit, newdata = data.binned)) - data.binned$mean.outcome))
            linear.sae.binned.small <- sum(abs(truncate.prob(predict(reg.linear.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome))

            reg.quadratic.fit <- glm(as.formula(paste(outcome, reg.quadratic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quadratic.sse <- sum((truncate.prob(predict(reg.quadratic.fit, newdata = loop.data)) - loop.data[,..outcome])^2)
            quadratic.sse.binned <- sum((truncate.prob(predict(reg.quadratic.fit, newdata = data.binned)) - data.binned$mean.outcome)^2)
            quadratic.sse.binned.small <- sum((truncate.prob(predict(reg.quadratic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome)^2)
            quadratic.sae <- sum(abs(truncate.prob(predict(reg.quadratic.fit, newdata = loop.data)) - loop.data[,..outcome]))
            quadratic.sae.binned <- sum(abs(truncate.prob(predict(reg.quadratic.fit, newdata = data.binned)) - data.binned$mean.outcome))
            quadratic.sae.binned.small <- sum(abs(truncate.prob(predict(reg.quadratic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome))
            
            reg.cubic.fit <- glm(as.formula(paste(outcome, reg.cubic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            cubic.sse <- sum((truncate.prob(predict(reg.cubic.fit, newdata = loop.data)) - loop.data[,..outcome])^2)
            cubic.sse.binned <- sum((truncate.prob(predict(reg.cubic.fit, newdata = data.binned)) - data.binned$mean.outcome)^2)
            cubic.sse.binned.small <- sum((truncate.prob(predict(reg.cubic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome)^2)
            cubic.sae <- sum(abs(truncate.prob(predict(reg.cubic.fit, newdata = loop.data)) - loop.data[,..outcome]))
            cubic.sae.binned <- sum(abs(truncate.prob(predict(reg.cubic.fit, newdata = data.binned)) - data.binned$mean.outcome))
            cubic.sae.binned.small <- sum(abs(truncate.prob(predict(reg.cubic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome))

            reg.quartic.fit <- glm(as.formula(paste(outcome, reg.quartic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quartic.sse <- sum((truncate.prob(predict(reg.quartic.fit, newdata = loop.data)) - loop.data[,..outcome])^2)
            quartic.sse.binned <- sum((truncate.prob(predict(reg.quartic.fit, newdata = data.binned)) - data.binned$mean.outcome)^2)
            quartic.sse.binned.small <- sum((truncate.prob(predict(reg.quartic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome)^2)
            quartic.sae <- sum(abs(truncate.prob(predict(reg.quartic.fit, newdata = loop.data)) - loop.data[,..outcome]))
            quartic.sae.binned <- sum(abs(truncate.prob(predict(reg.quartic.fit, newdata = data.binned)) - data.binned$mean.outcome))
            quartic.sae.binned.small <- sum(abs(truncate.prob(predict(reg.quartic.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome))

            reg.log.fit <- glm(as.formula(paste(outcome, reg.log, sep = " ~ ")), data = loop.data_training[mitigation != 0], family = gaussian(link = "identity"))
            log.sse <- sum((truncate.prob(predict(reg.log.fit, newdata = loop.data[mitigation != 0])) - loop.data[mitigation != 0,..outcome])^2)
            log.sse.binned <- sum((truncate.prob(predict(reg.log.fit, newdata = data.binned)) - data.binned$mean.outcome)^2)
            log.sse.binned.small <- sum((truncate.prob(predict(reg.log.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome)^2)
            log.sae <- sum(abs(truncate.prob(predict(reg.log.fit, newdata = loop.data[mitigation != 0])) - loop.data[mitigation != 0,..outcome]))
            log.sae.binned <- sum(abs(truncate.prob(predict(reg.log.fit, newdata = data.binned)) - data.binned$mean.outcome))
            log.sae.binned.small <- sum(abs(truncate.prob(predict(reg.log.fit, newdata = data.binned.small)) - data.binned.small$mean.outcome))
            
            min.sse <- min(linear.sse, quadratic.sse, cubic.sse, quartic.sse, log.sse)

            if(linear.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.linear.fit)

              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Linear")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse/sse_total, sse_total/nrow(loop.data), linear.sse/nrow(loop.data), sae_total/nrow(loop.data), linear.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(linear.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), linear.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(linear.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), linear.sae.binned.small/nrow(data.binned.small))
              
            }

            if(quadratic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quadratic.fit)

              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quadratic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse/sse_total, sse_total/nrow(loop.data), quadratic.sse/nrow(loop.data), sae_total/nrow(loop.data), quadratic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quadratic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quadratic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quadratic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quadratic.sae.binned.small/nrow(data.binned.small))
              
            }

            if(cubic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.cubic.fit)

              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Cubic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse/sse_total, sse_total/nrow(loop.data), cubic.sse/nrow(loop.data), sae_total/nrow(loop.data), cubic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(cubic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), cubic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(cubic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), cubic.sae.binned.small/nrow(data.binned.small))
              
            }

            if(quartic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quartic.fit)

              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quartic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse/sse_total, sse_total/nrow(loop.data), quartic.sse/nrow(loop.data), sae_total/nrow(loop.data), quartic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quartic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quartic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quartic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quartic.sae.binned.small/nrow(data.binned.small))
              
            }

            if(log.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.log.fit)

              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Log")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse/sse_total, sse_total/nrow(loop.data), log.sse/nrow(loop.data), sae_total/nrow(loop.data), log.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(log.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), log.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(log.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), log.sae.binned.small/nrow(data.binned.small))
              
            }

            predict.df$predicted.outbreak <- truncate.prob(predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = predict.df))

            min.prob <- min(min.prob, min(predict.df$predicted.outbreak))
            max.prob <- max(max.prob, max(predict.df$predicted.outbreak))

          }
        }

        for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){

          for(variant in c(0.02, 0.035, 0.07)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }

            i <- i+1

            print(paste("Working on variant", variant))

            variant.title <- unique(data[which(data$variant.attack == variant),]$variant.title)

            print(paste("Working on child vaccination rate", child.vax.rate))

            print(paste("Running heatmap for outcome", outcome))

            predict.df$predicted.outbreak <- truncate.prob(predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = predict.df))

            binned.data <- data[prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]

            binned.data1 = binned.data[,`:=`(mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE), prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 5), labels = seq(from = 5, to = 50, by = 5)))]

            binned.data2 = binned.data1[,list(predicted.outbreak = mean(inschool.outbreak_inc.1)),
                                       by = .(mitigation.binned, prob.binned)]

            binned.data3 = binned.data2[,`:=`(prob.binned = as.numeric(as.character(prob.binned)), mitigation.binned = as.numeric(as.character(mitigation.binned)))] %>%
              mutate(prob.binned = prob.binned - 2.5, mitigation.binned = mitigation.binned - 0.05, prob = prob.binned, mitigation = mitigation.binned)

            binned.data3$predicted.outbreak = unlist(lapply(unlist(lapply(binned.data3$predicted.outbreak, min, 1)), max, 0))

            binned.data.small <- data[prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]

            binned.data1.small = binned.data.small[,`:=`(mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE), prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 1), labels = seq(from = 1, to = 50, by = 1)))]

            binned.data2.small = binned.data1.small[,list(predicted.outbreak = mean(inschool.outbreak_inc.1)),
                                       by = .(mitigation.binned, prob.binned)]

            binned.data3.small = binned.data2.small[,`:=`(prob.binned = as.numeric(as.character(prob.binned)), mitigation.binned = as.numeric(as.character(mitigation.binned)))] %>%
              mutate(prob.binned = prob.binned - 0.5, mitigation.binned = mitigation.binned - 0.05, prob = prob.binned, mitigation = mitigation.binned)

            binned.data3.small$predicted.outbreak = unlist(lapply(unlist(lapply(binned.data3.small$predicted.outbreak, min, 1)), max, 0))

            low.mitigation.50 <- mean(predict.df[which(round(predict.df$mitigation, 2) == mitigation_a.high & predict.df$predicted.outbreak < 0.51 & predict.df$predicted.outbreak > 0.49), "prob"])
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, mitigation_a.high, adult.vax.rate, child.vax.rate, variant, 0.5, low.mitigation.50)
            

            med.mitigation.50 <- mean(predict.df[which(round(predict.df$mitigation, 2) == mitigation_b.high & predict.df$predicted.outbreak < 0.51 & predict.df$predicted.outbreak > 0.49), "prob"])
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, mitigation_b.high, adult.vax.rate, child.vax.rate, variant, 0.5, med.mitigation.50)

            plot <- ggplot(data = predict.df, aes(x = prob, y = mitigation)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = TRUE) +
              scale_fill_gradient(low = "blue", high = "yellow", limits = c(min.prob, max.prob), breaks = c(min.prob, (max.prob + min.prob)/2, max.prob), labels = c(as.character(round(min.prob, 2)), as.character(round((max.prob + min.prob)/2, 2)), as.character(round(max.prob, 2))), name = "Transmission Probability") +
              geom_contour(aes(z = predicted.outbreak, color = factor(..level..)), breaks = c(0, 0.25, 0.5, 0.75), size = 1.25) +
              scale_color_discrete(type = c("0.25" = "green", "0.5" = "orange", "0.75" = "purple")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_a.high, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_a.low, yend = mitigation_a.low, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_b.low, yend = mitigation_b.low, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_b.high, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_c.low, yend = mitigation_c.low, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_c.high, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = !! low.mitigation.50, xend = !! low.mitigation.50, y = mitigation_a.high, yend = 0.7), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
              geom_segment(aes(x = !! med.mitigation.50, xend = !! med.mitigation.50, y = 0.8, yend = 0.9), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5)) +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = ""), color = "Probability Thresholds", linetype = "Mitigation Interventions")

            plot.raw <- ggplot(binned.data3, aes(x = prob.binned, y = mitigation.binned)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = FALSE) +
              scale_fill_gradient(low = "blue", high = "yellow", limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), name = "Outbreak Probability") +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5))

            plot.raw.small <- ggplot(binned.data3.small, aes(x = prob.binned, y = mitigation.binned)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = FALSE) +
              scale_fill_gradient(low = "blue", high = "yellow", limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"), name = "Outbreak Probability") +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5))

            plot.list[[i]] <- plot
            plot.list.raw[[i]] <- plot.raw
            plot.list.raw.small[[i]] <- plot.raw.small

            remove(list = paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
            gc()

          }
        }
        
        if(adult.vax.rate == 0.7){
          combined.plot <- ggarrange(plotlist =  plot.list, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot, file=paste(paste("heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw <- ggarrange(plotlist = plot.list.raw, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw, file=paste(paste("raw_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw.small <- ggarrange(plotlist = plot.list.raw.small, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw.small, file=paste(paste("raw_small_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
        }else{
          combined.plot <- ggarrange(plotlist =  plot.list, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot, file=paste(paste("heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw <- ggarrange(plotlist = plot.list.raw, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw, file=paste(paste("raw_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw.small <- ggarrange(plotlist = plot.list.raw.small, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw.small, file=paste(paste("raw_small_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
        }
        
        remove(combined.plot)
        remove(combined.plot.raw)
        remove(combined.plot.raw.small)
        gc()

      }
    }

  }
}

for(outcome in c("overall.absolute.diff")){

  for(adult.vax.rate in c(0.7, 0.5)){
    print(paste("Working on adult vaccination rate", adult.vax.rate))

    for(notify.scenario in c("yes.wo.test")){
      print(paste("Working on notify scenario", notify.scenario))

      for(vax.eff in c(0.7)){
        print(paste("Working on vaccine efficacy", vax.eff))

        plot.list <- list()
        plot.list.raw <- list()
        plot.list.raw.small <- list()
        plot.list.reg <- list()

        i <- 0

        min.outbreak <- 1000
        max.outbreak <- -1000

        for(variant in c(0.02, 0.035, 0.07)){
          print(paste("Working on variant", variant))

          for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }
            
            print(paste("Working on child vaccination rate", child.vax.rate))

            print(paste("Running regressions for outcome", outcome))
            
            loop.data_training <- data[which(data$variant.attack == variant & data$child.vax == child.vax.rate & data$teacher_susp == adult.vax.rate & data$notify.scenario == notify.scenario & data$vax_eff == vax.eff),]

            loop.data <- data_test[which(data$variant.attack == variant & data$child.vax == child.vax.rate & data$teacher_susp == adult.vax.rate & data$notify.scenario == notify.scenario & data$vax_eff == vax.eff),]

            data.binned1 <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 5), labels = seq(from = 5, to = 60, by = 5)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned <- data.binned1[, list(mean.outcome = mean(overall.absolute.diff)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 2.5)
            
            data.binned1.small <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 1), labels = seq(from = 1, to = 60, by = 1)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned.small <- data.binned1.small[, list(mean.outcome = mean(overall.absolute.diff)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 0.5)
            
            sse_total <- sum((unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome])))^2)
            sse_total.binned <- sum((unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome)))^2)
            sse_total.binned.small <- sum((unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome)))^2)
            sae_total <- sum(abs(unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome]))))
            sae_total.binned <- sum(abs(unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome))))
            sae_total.binned.small <- sum(abs(unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome))))
            
            reg.linear.fit <- glm(as.formula(paste(outcome, reg.linear, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            linear.sse <- sum((predict(reg.linear.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            linear.sse.binned <- sum((predict(reg.linear.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            linear.sse.binned.small <- sum((predict(reg.linear.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            linear.sae <- sum(abs(predict(reg.linear.fit, newdata = loop.data) - loop.data[,..outcome]))
            linear.sae.binned <- sum(abs(predict(reg.linear.fit, newdata = data.binned) - data.binned$mean.outcome))
            linear.sae.binned.small <- sum(abs(predict(reg.linear.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.quadratic.fit <- glm(as.formula(paste(outcome, reg.quadratic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quadratic.sse <- sum((predict(reg.quadratic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            quadratic.sse.binned <- sum((predict(reg.quadratic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            quadratic.sse.binned.small <- sum((predict(reg.quadratic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            quadratic.sae <- sum(abs(predict(reg.quadratic.fit, newdata = loop.data) - loop.data[,..outcome]))
            quadratic.sae.binned <- sum(abs(predict(reg.quadratic.fit, newdata = data.binned) - data.binned$mean.outcome))
            quadratic.sae.binned.small <- sum(abs(predict(reg.quadratic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.cubic.fit <- glm(as.formula(paste(outcome, reg.cubic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            cubic.sse <- sum((predict(reg.cubic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            cubic.sse.binned <- sum((predict(reg.cubic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            cubic.sse.binned.small <- sum((predict(reg.cubic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            cubic.sae <- sum(abs(predict(reg.cubic.fit, newdata = loop.data) - loop.data[,..outcome]))
            cubic.sae.binned <- sum(abs(predict(reg.cubic.fit, newdata = data.binned) - data.binned$mean.outcome))
            cubic.sae.binned.small <- sum(abs(predict(reg.cubic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.quartic.fit <- glm(as.formula(paste(outcome, reg.quartic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quartic.sse <- sum((predict(reg.quartic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            quartic.sse.binned <- sum((predict(reg.quartic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            quartic.sse.binned.small <- sum((predict(reg.quartic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            quartic.sae <- sum(abs(predict(reg.quartic.fit, newdata = loop.data) - loop.data[,..outcome]))
            quartic.sae.binned <- sum(abs(predict(reg.quartic.fit, newdata = data.binned) - data.binned$mean.outcome))
            quartic.sae.binned.small <- sum(abs(predict(reg.quartic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.log.fit <- glm(as.formula(paste(outcome, reg.log, sep = " ~ ")), data = loop.data_training[mitigation != 0], family = gaussian(link = "identity"))
            log.sse <- sum((predict(reg.log.fit, newdata = loop.data[mitigation != 0]) - loop.data[mitigation != 0,..outcome])^2)
            log.sse.binned <- sum((predict(reg.log.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            log.sse.binned.small <- sum((predict(reg.log.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            log.sae <- sum(abs(predict(reg.log.fit, newdata = loop.data[mitigation != 0]) - loop.data[mitigation != 0,..outcome]))
            log.sae.binned <- sum(abs(predict(reg.log.fit, newdata = data.binned) - data.binned$mean.outcome))
            log.sae.binned.small <- sum(abs(predict(reg.log.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            min.sse <- min(linear.sse, quadratic.sse, cubic.sse, quartic.sse, log.sse)
            
            if(linear.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.linear.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Linear")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse/sse_total, sse_total/nrow(loop.data), linear.sse/nrow(loop.data), sae_total/nrow(loop.data), linear.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(linear.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), linear.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(linear.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), linear.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(quadratic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quadratic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quadratic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse/sse_total, sse_total/nrow(loop.data), quadratic.sse/nrow(loop.data), sae_total/nrow(loop.data), quadratic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quadratic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quadratic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quadratic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quadratic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(cubic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.cubic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Cubic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse/sse_total, sse_total/nrow(loop.data), cubic.sse/nrow(loop.data), sae_total/nrow(loop.data), cubic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(cubic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), cubic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(cubic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), cubic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(quartic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quartic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quartic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse/sse_total, sse_total/nrow(loop.data), quartic.sse/nrow(loop.data), sae_total/nrow(loop.data), quartic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quartic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quartic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quartic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quartic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(log.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.log.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Log")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse/sse_total, sse_total/nrow(loop.data), log.sse/nrow(loop.data), sae_total/nrow(loop.data), log.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(log.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), log.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(log.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), log.sae.binned.small/nrow(data.binned.small))
              
            }

            predict.df$predicted.outbreak <- predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = predict.df)

            min.outbreak <- min(min.outbreak, predict.df$predicted.outbreak)
            max.outbreak <- max(max.outbreak, predict.df$predicted.outbreak)

          }
        }

        for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){

          for(variant in c(0.02, 0.035, 0.07)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }

            i <- i+1

            print(paste("Working on variant", variant))

            variant.title <- unique(data[which(data$variant.attack == variant),]$variant.title)

            print(paste("Working on child vaccination rate", child.vax.rate))

            print(paste("Running heatmap for outcome", outcome))

            predict.df$predicted.outbreak <- unlist(lapply(unlist(lapply(predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = predict.df), min, 10.3)), max, 0))

            low.mitigation.10 <- mean(predict.df[which(round(predict.df$mitigation, 2) == mitigation_a.high & predict.df$predicted.outbreak < 10.1 & predict.df$predicted.outbreak > 9.9), "prob"])
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, mitigation_a.high, adult.vax.rate, child.vax.rate, variant, 10, low.mitigation.10)

            low.mitigation.5 <- mean(predict.df[which(round(predict.df$mitigation, 2) == mitigation_a.high & predict.df$predicted.outbreak < 5.1 & predict.df$predicted.outbreak > 4.9), "prob"])
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, mitigation_a.high, adult.vax.rate, child.vax.rate, variant, 5, low.mitigation.5)

            med.mitigation.10 <- mean(predict.df[which(round(predict.df$mitigation, 2) == mitigation_b.high & predict.df$predicted.outbreak < 10.1 & predict.df$predicted.outbreak > 9.9), "prob"])
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, mitigation_b.high, adult.vax.rate, child.vax.rate, variant, 10, med.mitigation.10)


            binned.data <- data[prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]

            binned.data1 = binned.data[,`:=`(mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE), prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 5), labels = seq(from = 5, to = 50, by = 5)))]

              binned.data2 = binned.data1[,list(predicted.outbreak = mean(overall.absolute.diff)),
                                         by = .(mitigation.binned, prob.binned)]

              binned.data3 = binned.data2[,`:=`(prob.binned = as.numeric(as.character(prob.binned)), mitigation.binned = as.numeric(as.character(mitigation.binned)))] %>%
                mutate(prob.binned = prob.binned - 2.5, mitigation.binned = mitigation.binned - 0.05, prob = prob.binned, mitigation = mitigation.binned)

            binned.data3$predicted.outbreak = unlist(lapply(binned.data3$predicted.outbreak, min, 10.3))
            
            binned.data3$predicted.reg.outbreak = unlist(lapply(predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = binned.data3), min, 10.3))

              binned.data.small <- data[prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]

              binned.data1.small = binned.data.small[,`:=`(mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE), prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 1), labels = seq(from = 1, to = 50, by = 1)))]

              binned.data2.small = binned.data1.small[,list(predicted.outbreak = mean(overall.absolute.diff)),
                                          by = .(mitigation.binned, prob.binned)]

              binned.data3.small = binned.data2.small[,`:=`(prob.binned = as.numeric(as.character(prob.binned)), mitigation.binned = as.numeric(as.character(mitigation.binned)))] %>%
                mutate(prob.binned = prob.binned - 0.5, mitigation.binned = mitigation.binned - 0.05, prob = prob.binned, mitigation = mitigation.binned)

              binned.data3.small$predicted.outbreak = unlist(lapply(binned.data3.small$predicted.outbreak, min, 10.3))
              binned.data3$predicted.reg.outbreak = unlist(lapply(predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = binned.data3), min, 10.3))



            plot <- ggplot(data = predict.df, aes(x = prob, y = mitigation)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = TRUE) +
              scale_fill_gradient(low = "blue", high = "yellow", name = "Number of additional cases", limits = c(0, 10.4), breaks = c(0, 5, 10), labels = c("0", "5", "10+")) +
              geom_contour(aes(z = predicted.outbreak, color = factor(..level..)), breaks = c(3, 5, 10), size = 1.25) +
              scale_color_discrete(type = c("3" = "green", "5" = "orange", "10" = "purple")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_a.high, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_a.low, yend = mitigation_a.low, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_a.low, yend = mitigation_a.high, linetype = "A: Simple Ventilation and Hand Washing")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_b.low, yend = mitigation_b.low, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_b.high, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_b.low, yend = mitigation_b.high, linetype = "B: A plus universal masking of students\nand unvaccinated educators/staff")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_c.low, yend = mitigation_c.low, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 0, xend = 50, y = mitigation_c.high, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 0, xend = 0, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = 50, xend = 50, y = mitigation_c.low, yend = mitigation_c.high, linetype = "C: 2020-2021 package of interventions")) +
              geom_segment(aes(x = !! low.mitigation.10, xend = !! low.mitigation.10, y = mitigation_a.high, yend = mitigation_b.low), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
              geom_segment(aes(x = !! med.mitigation.10, xend = !! med.mitigation.10, y = mitigation_b.high, yend = mitigation_c.low), arrow = arrow(length = unit(0.125, "in")), size = 1.5) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5)) +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = ""), color = "Case thresholds", linetype = "Mitigation Interventions")

            plot.raw <- ggplot(binned.data3, aes(x = prob.binned, y = mitigation.binned)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = FALSE) +
              scale_fill_gradient(low = "blue", high = "yellow", name = "Number of additional cases", limits = c(-2, 10.4), breaks = c(0, 5, 10), labels = c("0", "5", "+10")) +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5))

            plot.raw.small <- ggplot(binned.data3.small, aes(x = prob.binned, y = mitigation.binned)) +
              geom_raster(aes(fill = predicted.outbreak), interpolate = FALSE) +
              scale_fill_gradient(low = "blue", high = "yellow", name = "Number of additional cases", limits = c(-2, 10.4), breaks = c(0, 5, 10), labels = c("0", "5", "+10")) +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5))

            plot.binned.reg <- ggplot(binned.data3, aes(x = prob.binned, y = mitigation.binned)) +
              geom_raster(aes(fill = predicted.reg.outbreak), interpolate = FALSE) +
              scale_fill_gradient(low = "blue", high = "yellow", name = "Number of additional cases", limits = c(-2, 10.4), breaks = c(0, 5, 10), labels = c("0", "5", "+10")) +
              labs(x = "Observed Community Incidence (cases/100k/day)", y = "Mitigation Effectiveness", title = paste(variant.title, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) +
              theme_minimal() + #theme_opts +
              theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5))

            plot.list[[i]] <- plot
            plot.list.raw[[i]] <- plot.raw
            plot.list.raw.small[[i]] <- plot.raw.small
            plot.list.reg[[i]] <- plot.binned.reg

          }
        }
        
        if(adult.vax.rate == 0.7){
          combined.plot <- ggarrange(plotlist =  plot.list, nrow = 4, ncol = 3, common.legend = T,legend = 'right', legend.grob = get_legend(plot.list[[4]]))
          
          saveRDS(combined.plot, file=paste(paste("heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw <- ggarrange(plotlist = plot.list.raw, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw, file=paste(paste("raw_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw.small <- ggarrange(plotlist = plot.list.raw.small, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw.small, file=paste(paste("raw_small_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.reg <- ggarrange(plotlist = plot.list.reg, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.reg, file=paste(paste("reg_binned_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = "")) 
        }else{
          combined.plot <- ggarrange(plotlist =  plot.list, nrow = 3, ncol = 3, common.legend = T,legend = 'right', legend.grob = get_legend(plot.list[[4]]))
          
          saveRDS(combined.plot, file=paste(paste("heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw <- ggarrange(plotlist = plot.list.raw, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw, file=paste(paste("raw_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.raw.small <- ggarrange(plotlist = plot.list.raw.small, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.raw.small, file=paste(paste("raw_small_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
          
          combined.plot.reg <- ggarrange(plotlist = plot.list.reg, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
          
          saveRDS(combined.plot.reg, file=paste(paste("reg_binned_heatmap", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = "")) 
        }

        remove(combined.plot)
        remove(combined.plot.raw)
        remove(combined.plot.raw.small)
        remove(combined.plot.reg)
        gc()

      }
    }

  }
}

predict.mitigation.diff <- data.table(expand_grid(prob = seq(from = 0, to = 50, by = 0.1), mitigation = c(0.2, 0.3, 0.4, 0.7, 0.75, 0.8), child.vax = c(0, 0.25, 0.5, 0.7)))

for(outcome in c("all.overall")){
  
  for(adult.vax.rate in c(0.7, 0.5)){
    print(paste("Working on adult vaccination rate", adult.vax.rate))
    
    for(notify.scenario in c("yes.wo.test")){
      print(paste("Working on notify scenario", notify.scenario))
      
      for(vax.eff in c(0.7)){
        print(paste("Working on vaccine efficacy", vax.eff))
        
        facet_graph_data <- data.frame()
        facet_graph_data.binned <- data.frame()
        facet_graph_data.binned.small <- data.frame()
        
        i <- 0
        
        min.outbreak <- 1000
        max.outbreak <- -1000
        
        for(variant in c(0.02, 0.035, 0.07)){
          print(paste("Working on variant", variant))
          
          for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }
            
            print(paste("Working on child vaccination rate", child.vax.rate))
            
            print(paste("Running regressions for outcome", outcome))
            
            loop.data_training <- data[which(data$variant.attack == variant & data$child.vax == child.vax.rate & data$teacher_susp == adult.vax.rate & data$notify.scenario == notify.scenario & data$vax_eff == vax.eff),]
            
            loop.data <- data_test[which(data$variant.attack == variant & data$child.vax == child.vax.rate & data$teacher_susp == adult.vax.rate & data$notify.scenario == notify.scenario & data$vax_eff == vax.eff),]
            
            data.binned1 <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 5), labels = seq(from = 5, to = 60, by = 5)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned <- data.binned1[, list(mean.outcome = mean(all.overall)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 2.5)
            
            data.binned1.small <- data.table(loop.data)[,`:=`(prob.binned = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 60, by = 1), labels = seq(from = 1, to = 60, by = 1)))), mitigation.binned = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 0.1), labels = seq(from = 0.1, to = 1, by = 0.1), include.lowest = TRUE))))]
            data.binned.small <- data.binned1.small[, list(mean.outcome = mean(all.overall)), by = .(mitigation.binned, prob.binned)] %>% rename(mitigation = mitigation.binned, prob = prob.binned) %>% mutate(mitigation = mitigation - 0.05, prob = prob - 0.5)
            
            sse_total <- sum((unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome])))^2)
            sse_total.binned <- sum((unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome)))^2)
            sse_total.binned.small <- sum((unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome)))^2)
            sae_total <- sum(abs(unlist(loop.data[,..outcome]) - mean(unlist(loop.data[,..outcome]))))
            sae_total.binned <- sum(abs(unlist(data.binned$mean.outcome) - mean(unlist(data.binned$mean.outcome))))
            sae_total.binned.small <- sum(abs(unlist(data.binned.small$mean.outcome) - mean(unlist(data.binned.small$mean.outcome))))
            
            reg.linear.fit <- glm(as.formula(paste(outcome, reg.linear, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            linear.sse <- sum((predict(reg.linear.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            linear.sse.binned <- sum((predict(reg.linear.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            linear.sse.binned.small <- sum((predict(reg.linear.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            linear.sae <- sum(abs(predict(reg.linear.fit, newdata = loop.data) - loop.data[,..outcome]))
            linear.sae.binned <- sum(abs(predict(reg.linear.fit, newdata = data.binned) - data.binned$mean.outcome))
            linear.sae.binned.small <- sum(abs(predict(reg.linear.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.quadratic.fit <- glm(as.formula(paste(outcome, reg.quadratic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quadratic.sse <- sum((predict(reg.quadratic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            quadratic.sse.binned <- sum((predict(reg.quadratic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            quadratic.sse.binned.small <- sum((predict(reg.quadratic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            quadratic.sae <- sum(abs(predict(reg.quadratic.fit, newdata = loop.data) - loop.data[,..outcome]))
            quadratic.sae.binned <- sum(abs(predict(reg.quadratic.fit, newdata = data.binned) - data.binned$mean.outcome))
            quadratic.sae.binned.small <- sum(abs(predict(reg.quadratic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.cubic.fit <- glm(as.formula(paste(outcome, reg.cubic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            cubic.sse <- sum((predict(reg.cubic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            cubic.sse.binned <- sum((predict(reg.cubic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            cubic.sse.binned.small <- sum((predict(reg.cubic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            cubic.sae <- sum(abs(predict(reg.cubic.fit, newdata = loop.data) - loop.data[,..outcome]))
            cubic.sae.binned <- sum(abs(predict(reg.cubic.fit, newdata = data.binned) - data.binned$mean.outcome))
            cubic.sae.binned.small <- sum(abs(predict(reg.cubic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.quartic.fit <- glm(as.formula(paste(outcome, reg.quartic, sep = " ~ ")), data = loop.data_training, family = gaussian(link = "identity"))
            quartic.sse <- sum((predict(reg.quartic.fit, newdata = loop.data) - loop.data[,..outcome])^2)
            quartic.sse.binned <- sum((predict(reg.quartic.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            quartic.sse.binned.small <- sum((predict(reg.quartic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            quartic.sae <- sum(abs(predict(reg.quartic.fit, newdata = loop.data) - loop.data[,..outcome]))
            quartic.sae.binned <- sum(abs(predict(reg.quartic.fit, newdata = data.binned) - data.binned$mean.outcome))
            quartic.sae.binned.small <- sum(abs(predict(reg.quartic.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            reg.log.fit <- glm(as.formula(paste(outcome, reg.log, sep = " ~ ")), data = loop.data_training[mitigation != 0], family = gaussian(link = "identity"))
            log.sse <- sum((predict(reg.log.fit, newdata = loop.data[mitigation != 0]) - loop.data[mitigation != 0,..outcome])^2)
            log.sse.binned <- sum((predict(reg.log.fit, newdata = data.binned) - data.binned$mean.outcome)^2)
            log.sse.binned.small <- sum((predict(reg.log.fit, newdata = data.binned.small) - data.binned.small$mean.outcome)^2)
            log.sae <- sum(abs(predict(reg.log.fit, newdata = loop.data[mitigation != 0]) - loop.data[mitigation != 0,..outcome]))
            log.sae.binned <- sum(abs(predict(reg.log.fit, newdata = data.binned) - data.binned$mean.outcome))
            log.sae.binned.small <- sum(abs(predict(reg.log.fit, newdata = data.binned.small) - data.binned.small$mean.outcome))
            
            min.sse <- min(linear.sse, quadratic.sse, cubic.sse, quartic.sse, log.sse)
            
            if(linear.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.linear.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Linear")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse/sse_total, sse_total/nrow(loop.data), linear.sse/nrow(loop.data), sae_total/nrow(loop.data), linear.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(linear.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), linear.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Linear", 1-linear.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(linear.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), linear.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(quadratic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quadratic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quadratic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse/sse_total, sse_total/nrow(loop.data), quadratic.sse/nrow(loop.data), sae_total/nrow(loop.data), quadratic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quadratic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quadratic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quadratic", 1-quadratic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quadratic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quadratic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(cubic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.cubic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Cubic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse/sse_total, sse_total/nrow(loop.data), cubic.sse/nrow(loop.data), sae_total/nrow(loop.data), cubic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(cubic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), cubic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Cubic", 1-cubic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(cubic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), cubic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(quartic.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.quartic.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Quartic")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse/sse_total, sse_total/nrow(loop.data), quartic.sse/nrow(loop.data), sae_total/nrow(loop.data), quartic.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(quartic.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), quartic.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Quartic", 1-quartic.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(quartic.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), quartic.sae.binned.small/nrow(data.binned.small))
              
            }
            
            if(log.sse == min.sse){
              assign(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), reg.log.fit)
              
              print(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"))
              print("Log")
              
              r2.track[nrow(r2.track)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse/sse_total, sse_total/nrow(loop.data), log.sse/nrow(loop.data), sae_total/nrow(loop.data), log.sae/nrow(loop.data))
              r2.track.binned[nrow(r2.track.binned)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned/sse_total.binned, sqrt(sse_total.binned/nrow(data.binned)), sqrt(log.sse.binned/nrow(data.binned)), sae_total.binned/nrow(data.binned), log.sae.binned/nrow(data.binned))
              r2.track.binned.small[nrow(r2.track.binned.small)+1,] <- c(outcome, adult.vax.rate, child.vax.rate, variant, "Log", 1-log.sse.binned.small/sse_total.binned.small, sqrt(sse_total.binned.small/nrow(data.binned.small)), sqrt(log.sse.binned.small/nrow(data.binned.small)), sae_total.binned.small/nrow(data.binned.small), log.sae.binned.small/nrow(data.binned.small))
              
            }

          }
        }
        
        for(variant in c(0.02, 0.035, 0.07)){
          
          graph.data <- data.frame()
          binned.group.data <- data.frame()
          binned.group.data.small <- data.frame()
          
          i <- i + 1
          
          print(paste("Working on variant", variant))
          
          variant.title <- unique(data[which(data$variant.attack == variant),]$variant.title)
          
          for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
            
            if(adult.vax.rate == 0.5 & child.vax.rate == 0.7){
              next
            }
            
            print(paste("Working on child vaccination rate", child.vax.rate))
            
            print(paste("Running heatmap for outcome", outcome))
            
            predict.mitigation.diff[child.vax == child.vax.rate, c("predicted.outbreak")] <- predict(get(paste("opt.fit", "variant", variant, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_")), newdata = predict.mitigation.diff[child.vax == child.vax.rate])
            
            middle.mitigation.title <- "Moderate Mitigation Decrease (from 75% to 30%) (e.g., switch from masks + ventilation [moderate effectiveness estimate] to ventilation only [moderate effectiveness estimate])"
            lower.mitigation.title <- "Smaller Mitigation Decrease (from 70% to 40%) (e.g., switch from masks + ventilation [lower bound effectiveness estimate] to ventilation only [upper bound effectiveness estimate])"
            upper.mitigation.title <- "Larger Mitigation Decrease (from 80% to 20%) (e.g., switch from masks + ventilation [upper bound effectiveness estimate] to ventilation only [lower bound effectiveness estimate])"
            
            
            predict.diff.middle <- predict.mitigation.diff[child.vax == child.vax.rate & (mitigation == 0.3 | mitigation == 0.75)]
            
            predict.diff.middle <- predict.diff.middle[order(mitigation)]
            
            predict.diff.middle <- predict.diff.middle[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            predict.diff.middle <- predict.diff.middle[mitigation == 0.3]
            
            predict.diff.middle$group <- middle.mitigation.title
            
            med.mitigation.5 <- mean(predict.diff.middle[mitigation.diff < 5.1 & mitigation.diff > 4.9]$prob)
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, 0.3, adult.vax.rate, child.vax.rate, variant, 5, med.mitigation.5)
            
            predict.diff.lower <- predict.mitigation.diff[child.vax == child.vax.rate & (mitigation == 0.4 | mitigation == 0.7)]
            
            predict.diff.lower <- predict.diff.lower[order(mitigation)]
            
            predict.diff.lower <- predict.diff.lower[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            predict.diff.lower <- predict.diff.lower[mitigation == 0.4]
            
            predict.diff.lower$group <- lower.mitigation.title
            
            high.mitigation.5 <- mean(predict.diff.lower[mitigation.diff < 5.1 & mitigation.diff > 4.9]$prob)
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, 0.4, adult.vax.rate, child.vax.rate, variant, 5, high.mitigation.5)
            
            predict.diff.upper <- predict.mitigation.diff[child.vax == child.vax.rate & (mitigation == 0.2 | mitigation == 0.8)]
            
            predict.diff.upper <- predict.diff.upper[order(mitigation)]
            
            predict.diff.upper <- predict.diff.upper[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            predict.diff.upper <- predict.diff.upper[mitigation == 0.2]
            
            predict.diff.upper$group <- upper.mitigation.title
            
            low.mitigation.5 <- mean(predict.diff.upper[mitigation.diff < 5.1 & mitigation.diff > 4.9]$prob)
            cutoff.track[nrow(cutoff.track)+1,] <- c(outcome, 0.2, adult.vax.rate, child.vax.rate, variant, 5, low.mitigation.5)
            
            graph.data <- bind_rows(graph.data, predict.diff.lower, predict.diff.middle, predict.diff.upper)
            
            
            binned.data <- data[(round(mitigation, 4) == 0.3 | round(mitigation, 4) == 0.75) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1 = binned.data[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 5), labels = seq(from = 5, to = 50, by = 5)))]
            
            binned.data2 = binned.data1[,list(predicted.outbreak = mean(all.overall)),
                                        by = .(mitigation, prob.binned)]
            
            binned.data3 = binned.data2 %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 2.5, prob = prob.binned)
            
            binned.data4 <- binned.data3[order(mitigation), -c("prob.binned")]
            
            binned.data5 <- binned.data4[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6 <- binned.data5[mitigation == 0.3]
            
            binned.data6$child.vax <- child.vax.rate
            
            binned.data6$group <- middle.mitigation.title
            
            binned.group.data <- bind_rows(binned.group.data, binned.data6)
            
            
            binned.data <- data[(round(mitigation, 4) == 0.4 | round(mitigation, 4) == 0.7) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1 = binned.data[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 5), labels = seq(from = 5, to = 50, by = 5)))]
            
            binned.data2 = binned.data1[,list(predicted.outbreak = mean(all.overall)),
                                        by = .(mitigation, prob.binned)]
            
            binned.data3 = binned.data2 %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 2.5, prob = prob.binned)
            
            binned.data4 <- binned.data3[order(mitigation), -c("prob.binned")]
            
            binned.data5 <- binned.data4[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6 <- binned.data5[mitigation == 0.4]
            
            binned.data6$child.vax <- child.vax.rate
            
            binned.data6$group <- lower.mitigation.title
            
            binned.group.data <- bind_rows(binned.group.data, binned.data6)
            
            
            binned.data <- data[(round(mitigation, 4) == 0.2 | round(mitigation, 4) == 0.8) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1 = binned.data[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 5), labels = seq(from = 5, to = 50, by = 5)))]
            
            binned.data2 = binned.data1[,list(predicted.outbreak = mean(all.overall)),
                                        by = .(mitigation, prob.binned)]
            
            binned.data3 = binned.data2 %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 2.5, prob = prob.binned)
            
            binned.data4 <- binned.data3[order(mitigation), -c("prob.binned")]
            
            binned.data5 <- binned.data4[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6 <- binned.data5[mitigation == 0.2]
            
            binned.data6$child.vax <- child.vax.rate
            
            binned.data6$group <- upper.mitigation.title
            
            binned.group.data <- bind_rows(binned.group.data, binned.data6)
            
            
            binned.data.small <- data[(round(mitigation, 4) == 0.3 | round(mitigation, 4) == 0.75) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1.small = binned.data.small[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 1), labels = seq(from = 1, to = 50, by = 1)))]
            
            binned.data2.small = binned.data1.small[,list(predicted.outbreak = mean(all.overall)),
                                        by = .(mitigation, prob.binned)]
            
            binned.data3.small = binned.data2.small %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 0.5, prob = prob.binned)
            
            binned.data4.small <- binned.data3.small[order(mitigation), -c("prob.binned")]
            
            binned.data5.small <- binned.data4.small[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6.small <- binned.data5.small[mitigation == 0.3]
            
            binned.data6.small$child.vax <- child.vax.rate
            
            binned.data6.small$group <- middle.mitigation.title
            
            binned.group.data.small <- bind_rows(binned.group.data.small, binned.data6.small)
            
            
            binned.data.small <- data[(round(mitigation, 4) == 0.4 | round(mitigation, 4) == 0.7) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1.small = binned.data.small[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 1), labels = seq(from = 1, to = 50, by = 1)))]
            
            binned.data2.small = binned.data1.small[,list(predicted.outbreak = mean(all.overall)),
                                                    by = .(mitigation, prob.binned)]
            
            binned.data3.small = binned.data2.small %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 0.5, prob = prob.binned)
            
            binned.data4.small <- binned.data3.small[order(mitigation), -c("prob.binned")]
            
            binned.data5.small <- binned.data4.small[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6.small <- binned.data5.small[mitigation == 0.4]
            
            binned.data6.small$child.vax <- child.vax.rate
            
            binned.data6.small$group <- lower.mitigation.title
            
            binned.group.data.small <- bind_rows(binned.group.data.small, binned.data6.small)
            
            
            binned.data.small <- data[(round(mitigation, 4) == 0.2 | round(mitigation, 4) == 0.8) & prob <= 50 & variant.attack == variant & child.vax == child.vax.rate & teacher_susp == adult.vax.rate & notify.scenario == notify.scenario & vax_eff == vax.eff]
            
            binned.data1.small = binned.data.small[,`:=`(prob.binned = cut(round(prob, 4), breaks = seq(from = 0, to = 50, by = 1), labels = seq(from = 1, to = 50, by = 1)))]
            
            binned.data2.small = binned.data1.small[,list(predicted.outbreak = mean(all.overall)),
                                                    by = .(mitigation, prob.binned)]
            
            binned.data3.small = binned.data2.small %>%
              mutate(prob.binned = as.numeric(as.character(prob.binned)) - 0.5, prob = prob.binned)
            
            binned.data4.small <- binned.data3.small[order(mitigation), -c("prob.binned")]
            
            binned.data5.small <- binned.data4.small[,`:=`(mitigation.diff = c(-1,1)*diff(predicted.outbreak)), by = .(prob)]
            
            binned.data6.small <- binned.data5.small[mitigation == 0.2]
            
            binned.data6.small$child.vax <- child.vax.rate
            
            binned.data6.small$group <- upper.mitigation.title
            
            binned.group.data.small <- bind_rows(binned.group.data.small, binned.data6.small)
          }
          
          graph.data$variant.title <- variant.title
          binned.group.data$variant.title <- variant.title
          binned.group.data.small$variant.title <- variant.title
          
          facet_graph_data <- bind_rows(facet_graph_data, graph.data)
          facet_graph_data.binned <- bind_rows(facet_graph_data.binned, binned.group.data)
          facet_graph_data.binned.small <- bind_rows(facet_graph_data.binned.small, binned.group.data.small)
          
        }
        
        max.outbreak <- max(facet_graph_data$mitigation.diff)
        max.outbreak.binned <- max(max.outbreak, facet_graph_data.binned$mitigation.diff)
        max.outbreak.binned.small <- max(max.outbreak, facet_graph_data.binned.small$mitigation.diff)
        
        facet_graph_data$child.vax <- paste(facet_graph_data$child.vax*100, "%", sep ="")
        facet_graph_data.binned$child.vax <- paste(facet_graph_data.binned$child.vax*100, "%", sep ="")
        facet_graph_data.binned.small$child.vax <- paste(facet_graph_data.binned.small$child.vax*100, "%", sep ="")
        
        facet_graph_data$variant.title <- factor(facet_graph_data$variant.title, levels = c("Wild-Type", "Alpha Variant", "Delta Variant"), labels = c("Wild-Type", "Alpha Variant", "Delta Variant"))
        facet_graph_data.binned$variant.title <- factor(facet_graph_data.binned$variant.title, levels = c("Wild-Type", "Alpha Variant", "Delta Variant"), labels = c("Wild-Type", "Alpha Variant", "Delta Variant"))
        facet_graph_data.binned.small$variant.title <- factor(facet_graph_data.binned.small$variant.title, levels = c("Wild-Type", "Alpha Variant", "Delta Variant"), labels = c("Wild-Type", "Alpha Variant", "Delta Variant"))
        
        facet_graph_data$group <- factor(facet_graph_data$group, levels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title), labels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title))
        facet_graph_data.binned$group <- factor(facet_graph_data.binned$group, levels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title), labels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title))
        facet_graph_data.binned.small$group <- factor(facet_graph_data.binned.small$group, levels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title), labels = c(lower.mitigation.title, middle.mitigation.title, upper.mitigation.title))
        
        combined.plot.moderate <- ggplot(filter(facet_graph_data, group == middle.mitigation.title),
                                 aes(x = prob, y = mitigation.diff,
                                     group = as.factor(child.vax), col = as.factor(child.vax))) +
          geom_line(arrow = arrow(ends = "last", type = "closed", length = unit(0.125, "inches"), angle = 20)) +
          scale_color_discrete(name = "Student Vaccination Rate") +
          facet_grid(cols = vars(variant.title)) +
          theme_minimal() + #theme_opts +
          theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 11)) +
          labs(x = "Observed Community incidence (cases/100K/day)", y = "Average Additional Cases per Month in School Community", title = middle.mitigation.title) + ylim(0, 45)
        
        combined.plot.upper <- ggplot(filter(facet_graph_data, group == upper.mitigation.title),
                                         aes(x = prob, y = mitigation.diff,
                                             group = as.factor(child.vax), col = as.factor(child.vax))) +
          geom_line(arrow = arrow(ends = "last", type = "closed", length = unit(0.125, "inches"), angle = 20)) +
          scale_color_discrete(name = "Student Vaccination Rate") +
          facet_grid(cols = vars(variant.title)) +
          theme_minimal() + #theme_opts +
          theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 11)) +
          labs(x = "Observed Community incidence (cases/100K/day)", y = "Average Additional Cases per Month in School Community", title = upper.mitigation.title) + ylim(0, 45)
        
        combined.plot.lower <- ggplot(filter(facet_graph_data, group == lower.mitigation.title),
                                      aes(x = prob, y = mitigation.diff,
                                          group = as.factor(child.vax), col = as.factor(child.vax))) +
          geom_line(arrow = arrow(ends = "last", type = "closed", length = unit(0.125, "inches"), angle = 20)) +
          scale_color_discrete(name = "Student Vaccination Rate") +
          facet_grid(cols = vars(variant.title)) +
          theme_minimal() + #theme_opts +
          theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 11)) +
          labs(x = "Observed Community incidence (cases/100K/day)", y = "Average Additional Cases per Month in School Community", title = lower.mitigation.title) + ylim(0, 45)
        
        combined.plot <- ggarrange(plotlist = list(combined.plot.lower, combined.plot.moderate, combined.plot.upper), nrow = 3, common.legend = T,legend = 'right')
        
        combined.plot.raw <- ggplot() +
          geom_line(data = facet_graph_data,
                    aes(x = prob, y = mitigation.diff,
                        group = as.factor(child.vax), col = as.factor(child.vax))) +
          geom_line(data = facet_graph_data.binned, aes(x = prob, y = mitigation.diff, group = as.factor(child.vax), col = as.factor(child.vax)), linetype = "dotted") +
          scale_color_discrete(name = "Student Vaccination Rate") +
          facet_grid(rows = vars(group), cols = vars(variant.title)) +
          theme_minimal() + #theme_opts +
          theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 11)) +
          labs(x = "Observed Community incidence (cases/100K/day)", y = "Average Additional Cases per Month in School Community") + guides(linetype = "none") + ylim(0, max.outbreak.binned)
        
        combined.plot.raw.small <- ggplot() +
          geom_line(data = facet_graph_data,
                    aes(x = prob, y = mitigation.diff,
                        group = as.factor(child.vax), col = as.factor(child.vax))) +
          geom_line(data = facet_graph_data.binned.small, aes(x = prob, y = mitigation.diff, group = as.factor(child.vax), col = as.factor(child.vax)), linetype = "dotted") +
          scale_color_discrete(name = "Student Vaccination Rate") +
          facet_grid(rows = vars(group), cols = vars(variant.title)) +
          theme_minimal() + #theme_opts +
          theme(  text = element_text(size = 11), title =element_text(size=11, hjust = 0.5), strip.text = element_text(size = 11)) +
          labs(x = "Observed Community incidence (cases/100K/day)", y = "Average Additional Cases per Month in School Community") + guides(linetype = "none") + ylim(0, max.outbreak.binned.small)
        
        saveRDS(combined.plot, file=paste(paste("mitigation_lineplot", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
        
        saveRDS(combined.plot.raw, file=paste(paste("raw_mitigation_lineplot", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
        
        saveRDS(combined.plot.raw.small, file=paste(paste("raw_small_mitigation_lineplot", "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, outcome, sep = "_"), ".rds", sep = ""))
        
        remove(combined.plot)
        remove(combined.plot.raw)
        remove(combined.plot.raw.small)
        gc()
        
      }
    }
    
  }
}



write.csv(r2.track, file = "smoothing_fits_r2.csv")
write.csv(r2.track.binned, file = "smoothing_fits_r2_binned.csv")
write.csv(r2.track.binned.small, file = "smoothing_fits_r2_binned_small.csv")
write.csv(cutoff.track, file = "smoothed_cutoffs.csv")



pal = c("#457b9d", "#449187", "#52bdd3", "#a8ccb4")
theme_opts = theme_minimal() + theme(text = element_text(family = "sans"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "darkgrey"))

bt = data[prob <= 50]

bt = bt[, `:=`(prob.binned = cut(prob, breaks = seq(from = 0, to = 50, by = 50/10)),
                  var = factor(variant.title, levels = c("Wild-Type", "Alpha Variant", "Delta Variant")),
                  mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 1/10)),
                 prob.numeric = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 50, by = 50/10), labels = seq(from = 5, to = 50, by = 50/10)))) - 2.5,
                 mitigation.numeric = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 1/10), labels = seq(from = 0.1, to = 1, by = 1/10)))) - 0.05)]

bt2 = bt[, `:=`(avg.case.diff = mean(overall.absolute.diff),
                hosp.100k = mean(hosp.diff.100k),
                q5 = quantile(overall.absolute.diff, 0.05), q25 = quantile(overall.absolute.diff, 0.25), q50 = median(overall.absolute.diff),
                q75 = quantile(overall.absolute.diff, 0.75), q95 = quantile(overall.absolute.diff, 0.95)),
         by = .(var, variant.attack, child.vax, teacher_susp, prob.binned, mitigation.binned)]

bt2 = bt2[, `:=`(mitigation.binned = factor(mitigation.binned, levels = c("(0.9,1]", "(0.8,0.9]", "(0.7,0.8]", "(0.6,0.7]", "(0.5,0.6]", "(0.4,0.5]", "(0.3,0.4]", "(0.2,0.3]", "(0.1,0.2]", "(0,0.1]")))]

bt3 = bt2[, `:=`(prob = prob.numeric, mitigation = mitigation.numeric)]

bt.small = data[prob <= 50]

bt.small = bt.small[, `:=`(prob.binned = cut(prob, breaks = seq(from = 0, to = 50, by = 50/50)),
               var = factor(case_when(variant.attack==0.02~"Wild-type", variant.attack==0.035~"Alpha Variant",
                                      variant.attack==0.07~"Delta Variant"), levels = c("Wild-type", "Alpha Variant", "Delta Variant")),
               mitigation.binned = cut(mitigation, breaks = seq(from = 0, to = 1, by = 1/10)),
               prob.numeric = as.numeric(as.character(cut(prob, breaks = seq(from = 0, to = 50, by = 50/50), labels = seq(from = 1, to = 50, by = 50/50)))) - 0.5,
               mitigation.numeric = as.numeric(as.character(cut(mitigation, breaks = seq(from = 0, to = 1, by = 1/10), labels = seq(from = 0.1, to = 1, by = 1/10)))) - 0.05)]

bt2.small = bt.small[, `:=`(avg.case.diff = mean(overall.absolute.diff),
                hosp.100k = mean(hosp.diff.100k),
                q5 = quantile(overall.absolute.diff, 0.05), q25 = quantile(overall.absolute.diff, 0.25), q50 = median(overall.absolute.diff),
                q75 = quantile(overall.absolute.diff, 0.75), q95 = quantile(overall.absolute.diff, 0.95)),
         by = .(var, variant.attack, child.vax, teacher_susp, prob.binned, mitigation.binned)]

bt2.small = bt2.small[, `:=`(mitigation.binned = factor(mitigation.binned, levels = c("(0.9,1]", "(0.8,0.9]", "(0.7,0.8]", "(0.6,0.7]", "(0.5,0.6]", "(0.4,0.5]", "(0.3,0.4]", "(0.2,0.3]", "(0.1,0.2]", "(0,0.1]")))]

bt3.small = bt2.small[, `:=`(prob = prob.numeric, mitigation = mitigation.numeric)]

for(adult.vax.rate in c(0.7, 0.5)){
  plot.list <- list()
  plot.list.small <- list()
  i <- 0
  for(child.vax.rate in c(0, 0.5)){
    for(variant in  c("Wild-type", "Alpha Variant", "Delta Variant")){
      i <- i+1

      variant.attack <- ifelse(variant == "Wild-type", 0.02, ifelse(variant == "Alpha Variant", 0.035, ifelse(variant == "Delta Variant", 0.07)))

      bt3$binned.fitted <- predict(get(paste("opt.fit", "variant", variant.attack, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, "overall.absolute.diff", sep = "_")), newdata = bt3)

      bt3.small$binned.fitted <- predict(get(paste("opt.fit", "variant", variant.attack, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, "overall.absolute.diff", sep = "_")), newdata = bt3.small)

      plot.list[[i]] <- ggplot(bt3 %>% filter(teacher_susp == adult.vax.rate & child.vax == child.vax.rate & var == variant),
                               aes(x = prob.binned, y = binned.fitted,
                                   group = mitigation.binned, col = mitigation.binned)) +
        geom_line(aes(linetype = "Smoothed Bin Estimates")) +
        geom_line(aes(y = avg.case.diff, linetype = "Raw Bin Average")) +
        scale_linetype_discrete(name = "Empirical vs. Smoothed Estimates") +
        scale_color_discrete(name = "Mitigation Effectiveness") +
        theme_minimal() + #theme_opts +
        theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5)) +
        labs(x = "Observed Community incidence (cases/100K/day)", y = "Additional Cases Over Remote", title = paste(variant, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) + ylim(min(bt2[teacher_susp == adult.vax.rate]$avg.case.diff), max(bt2[teacher_susp == adult.vax.rate]$avg.case.diff))

      plot.list.small[[i]] <- ggplot(bt3.small %>% filter(teacher_susp == adult.vax.rate & child.vax == child.vax.rate & var == variant),
                               aes(x = prob, y = binned.fitted,
                                   group = mitigation.binned, col = mitigation.binned)) +
        geom_line(aes(linetype = "Smoothed Bin Estimates")) +
        geom_line(aes(y = avg.case.diff, linetype = "Raw Bin Average")) +
        scale_linetype_discrete(name = "Empirical vs. Smoothed Estimates") +
        scale_color_discrete(name = "Mitigation Effectiveness") +
        theme_minimal() + #theme_opts +
        theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5)) +
        labs(x = "Observed Community incidence (cases/100K/day)", y = "Additional Cases Over Remote", title = paste(variant, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) + ylim(min(bt3.small[teacher_susp == adult.vax.rate]$avg.case.diff), max(bt3.small[teacher_susp == adult.vax.rate]$avg.case.diff))

      remove(list = paste("opt.fit", "variant", variant.attack, "childvax", child.vax.rate, "adultvax", adult.vax.rate, "notify", notify.scenario, "vaxeff", vax.eff, "overall.absolute.diff", sep = "_"))
      gc()
    }
  }
  combined.lineplot.cases <- ggarrange(plotlist =  plot.list, nrow = 2, ncol = 3, common.legend = T,legend = 'right')

  saveRDS(combined.lineplot.cases, file=paste(paste("lineplot", "adultvax", adult.vax.rate, "notify", "yes.wo.test", "vaxeff", "0.9", "overall.absolute.diff", sep = "_"), ".rds", sep = ""))

  combined.lineplot.cases.small <- ggarrange(plotlist =  plot.list.small, nrow = 2, ncol = 3, common.legend = T,legend = 'right')

  saveRDS(combined.lineplot.cases.small, file=paste(paste("lineplot_small", "adultvax", adult.vax.rate, "notify", "yes.wo.test", "vaxeff", "0.9", "overall.absolute.diff", sep = "_"), ".rds", sep = ""))

  remove(combined.lineplot.cases)
  remove(combined.lineplot.cases.small)
  gc()
}

bt = data_full[prob <= 50]

bt = bt[, `:=`(prob.binned = cut(prob, breaks = seq(from = 0, to = 50, by = 50/10)),
               var = factor(case_when(variant.attack==0.02~"Wild-type", variant.attack==0.035~"Alpha Variant",
                                      variant.attack==0.07~"Delta Variant"), levels = c("Wild-type", "Alpha Variant", "Delta Variant")),
               mitigation.binned = cut(round(mitigation, 4), breaks = seq(from = 0, to = 1, by = 1/5), include.lowest = TRUE))]

bt2 = bt[, .(hosp.100k = mean(hosp.diff.100k)),
         by = .(var, variant.attack, child.vax, teacher_susp, prob.binned, mitigation.binned)]

for(adult.vax.rate in c(0.7, 0.5)){
  plot.list <- list()
  i <- 0
  for(child.vax.rate in c(0, 0.25, 0.5, 0.7)){
    if(child.vax.rate == 0.7 & adult.vax.rate == 0.5){next}
    for(variant in  c("Wild-type", "Alpha Variant", "Delta Variant")){
      i <- i+1
      plot.list[[i]] <- ggplot(bt2 %>% filter(teacher_susp == adult.vax.rate & child.vax == child.vax.rate & var == variant),
                               aes(x = prob.binned, y = hosp.100k,
                                   group = mitigation.binned, col = mitigation.binned)) +
        geom_line() +
        scale_color_discrete(name = "Mitigation Effectiveness") +
        theme_minimal() + #theme_opts +
        theme(  text = element_text(size = 9), title =element_text(size=11, hjust = 0.5)) +
        labs(x = "Observed Community incidence (cases/100K/day)", y = "Hospitalizations per 100k Over Remote", title = paste(variant, ", ", child.vax.rate*100, "%", " Child Vaccination Rate", sep = "")) + ylim(min(bt2[teacher_susp == adult.vax.rate]$hosp.100k), max(bt2[teacher_susp == adult.vax.rate]$hosp.100k))
    }
  }
  if(adult.vax.rate == 0.7){
    combined.lineplot.hosp <- ggarrange(plotlist =  plot.list, nrow = 4, ncol = 3, common.legend = T,legend = 'right')
  }else{
    combined.lineplot.hosp <- ggarrange(plotlist =  plot.list, nrow = 3, ncol = 3, common.legend = T,legend = 'right')
  }
  
  saveRDS(combined.lineplot.hosp, file=paste(paste("lineplot", "adultvax", adult.vax.rate, "notify", "yes.wo.test", "vaxeff", "0.7", "hosp.100k", sep = "_"), ".rds", sep = ""))
  
  remove(combined.lineplot.hosp)
  gc()
}
