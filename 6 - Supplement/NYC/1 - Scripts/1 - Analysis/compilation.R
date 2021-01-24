#************************************ SCHOOLS ANALYSIS ************************************#

#### SETUP ####
setwd("~/Dropbox/Schools/NYC/0 - Data")
library(tidyverse)
library(lubridate)
library(jsonlite)

#### IMPORT DATA ####

# schools
high = read.csv("2020_DOE_High_School_Directory.csv" )
middle = read.csv("2020_DOE_Middle_School_Directory.csv")

# testing
df = jsonlite::fromJSON(txt = "results.json", simplifyDataFrame = T) %>%
  # filter out schools tbd
  filter(DBN!="ZZZZZZ") %>%
  # demarcate elementary/middle/high schools
  mutate(high = DBN%in%high$dbn,
         middle = DBN%in%middle$schooldbn,
         
         # neither middle nor high
         elem = !high & !middle, 
         
         # elementary school by key words
         elem2 = grepl("P.S.|ELEMENTARY", SCHOOL, ignore.case = T) & elem,
         
         # date
         date = as.Date(substring(DATE_OF_TEST, 1, 10))) 
write.csv(df, "processed.csv")
df = df %>%
  gather(var, value, 7:9, 12:14) %>%
  mutate(value = as.numeric(value)) %>%
  spread(var, value)

# reasonability checks
table(df$high); length(unique(df$DBN[df$high]))
dim(high)
table(df$middle); length(unique(df$DBN[df$middle]))
dim(middle)

#### OVERALL ####
k1 = df %>% 
  group_by(isoweek(date)) %>%
  summarize(total = sum(POSTITVE_IN_LAST_SURVERY),
         student = sum(POSITIVE_STUDENT_IN_LAST_SURVEY),
         staff = sum(POSITIVE_EMPLOYEE_IN_LAST_SURVEY),
         denom_total = sum(TOTAL_TESTED),
         denom_student = sum(TOTAL_STUDENT),
         denom_staff = sum(TOTAL_EMPLOYEE),
         prev_total =total/denom_total,
         prev_student = student/denom_student,
         prev_staff = staff/denom_staff)

View(k1)

k2 = df %>% 
  gather(var, value, 7:9, 12:14) %>%
  mutate(value = as.numeric(value)) %>%
  spread(var, value) %>%
  group_by(high) %>%
  summarize(total = sum(POSTITVE_IN_LAST_SURVERY),
            student = sum(POSITIVE_STUDENT_IN_LAST_SURVEY),
            staff = sum(POSITIVE_EMPLOYEE_IN_LAST_SURVEY),
            denom_total = sum(TOTAL_TESTED),
            denom_student = sum(TOTAL_STUDENT),
            denom_staff = sum(TOTAL_EMPLOYEE),
            prev_total =total/denom_total,
            prev_student = student/denom_student,
            prev_staff = staff/denom_staff)

View(k2)


k2 = df %>% filter(elem2 | high) %>%
  group_by(elem2, month(date)) %>%
  summarize(total = sum(POSTITVE_IN_LAST_SURVERY),
            student = sum(POSITIVE_STUDENT_IN_LAST_SURVEY),
            staff = sum(POSITIVE_EMPLOYEE_IN_LAST_SURVEY),
            denom_total = sum(TOTAL_TESTED),
            denom_student = sum(TOTAL_STUDENT),
            denom_staff = sum(TOTAL_EMPLOYEE),
            prev_total =total/denom_total,
            prev_student = student/denom_student,
            prev_staff = staff/denom_staff)

View(k2)


k2 = df %>% filter(elem | high) %>%
  group_by(elem) %>%
  summarize(total = sum(POSTITVE_IN_LAST_SURVERY),
            student = sum(POSITIVE_STUDENT_IN_LAST_SURVEY),
            staff = sum(POSITIVE_EMPLOYEE_IN_LAST_SURVEY),
            denom_total = sum(TOTAL_TESTED),
            denom_student = sum(TOTAL_STUDENT),
            denom_staff = sum(TOTAL_EMPLOYEE),
            prev_total =total/denom_total,
            prev_student = student/denom_student,
            prev_staff = staff/denom_staff)

View(k2)



