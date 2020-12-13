#*************************************** Tests ********************************************#
#                                                                                          #
#                                                                                          #
# This file runs tests on the demographic data output.                                     #
#******************************************************************************************#

### RAN ON OLDER VERSION OF CODE -- NEED TO UPDATE FUNCTION NAMES ###

# load files
load("~/Dropbox/Schools/synthMD.RData")

# source code
source("~/Dropbox/Schools/abm6.R")

#### BASIC CHECKS ####

# examine files
head(out)
dim(out)

# check number of adults/kids
table(out$adult, useNA = "always")

# check number of classes
table(out$class, useNA = "always")


# check groups by siblings
table(out$group[!out$adult], useNA = "always")
table(out$group_quarter[!out$adult], useNA = "always")

table(out$class[!out$adult], out$group[!out$adult], useNA = "always")
table(out$class[!out$adult], out$group_quarter[!out$adult], useNA = "always")

table(out$HH_id[!out$adult], out$group[!out$adult], useNA = "always")
table(out$HH_id[!out$adult], out$group_quarter[!out$adult], useNA = "always")

# check class groupings
# this doesn't keep siblings in same group necessarily
table(out$class, out$class_grp2, useNA = "always")
table(out$class, out$class_grp4, useNA = "always")

#### SCHEDULE CHECKS ####

# make non-teacher adults
other_adults = data.frame(HH_id = rep(0, 30), age = 0, class = 0, adult = T, class_grp2 = 5, class_grp4 = 5, group = 99, group_quarter = 99)
n = nrow(out) + nrow(other_adults)
c = max(out$class)

# initialize data frame
df = bind_rows(out, other_adults) %>% mutate(id = row_number())

# make schedules

# base
z1 = make_schedule(time = 30, df = df, type = "base")
ggplot(z1, aes(x = t, y = as.character(id), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# On/off
# one day
z2 = make_schedule(time = 30, df = df, type = "On/off", total_days = 1)
ggplot(z2, aes(x = t, y = as.character(id), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# two days
z3 = make_schedule(time = 30, df = df, type = "On/off", total_days = 2)
ggplot(z3, aes(x = t, y = as.character(id), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# three days
z4 = make_schedule(time = 30, df = df, type = "On/off", total_days = 3)
ggplot(z4, aes(x = t, y = as.character(id), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# five days
z5 = make_schedule(time = 30, df = df, type = "On/off", total_days = 5)
ggplot(z5, aes(x = t, y = as.character(id), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# A/B
# one day
z6 = make_schedule(time = 30, df = df, type = "A/B", total_days = 1)
ggplot(z6, aes(x = t, y = reorder(as.character(id), group_quarter), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# two days
z7 = make_schedule(time = 30, df = df, type = "A/B", total_days = 2)
ggplot(z7, aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# two days
z8 = make_schedule(time = 30, df = df, type = "A/B", total_days = 2.2)
ggplot(z8, aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

# two days
z9 = make_schedule(time = 30, df = df, type = "A/B", total_days = 5)
ggplot(z9, aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")


#### RUN TESTS ####

# base
out = mult_runs(N = 1, n_contacts = 20, n_contacts_brief = 50, run_specials = T)
model = cbind(out[[2]][[1]] %>%
                select(id, class, adult, group, group_quarter), out[[2]][[4]]) %>%
  gather(var, value, -id, -class, -adult, -group, -group_quarter)

ggplot(model, aes(x = var, y = factor(id), fill = value)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

ggplot(out[[4]], aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")


# on/off
out = mult_runs(N = 1, n_contacts = 20, n_contacts_brief = 50, run_specials = T, type = "On/off",
                total_days = 5)
model = cbind(out[[2]][[1]] %>%
                select(id, class, adult, group, group_quarter), out[[2]][[4]]) %>%
  gather(var, value, -id, -class, -adult, -group, -group_quarter)

ggplot(model %>% filter(class == 1), aes(x = as.numeric(var), y = factor(id), fill = value)) +
  geom_tile() + facet_wrap(.~class, scales = "free")

ggplot(out[[4]], aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")


# A/B
out = mult_runs(N = 1, n_contacts = 20, n_contacts_brief = 50, run_specials = T,
                type = "A/B",
                total_days = 1)

model = cbind(out[[2]][[1]] %>%
                select(id, class, adult, group, group_quarter), out[[2]][[4]]) %>%
  gather(var, value, -id, -class, -adult, -group, -group_quarter)

ggplot(model %>% filter(class == 1), aes(x = as.numeric(var), y = reorder(factor(id), group_quarter), fill = value)) +
  geom_tile() + facet_wrap(.~class, scales = "free") + geom_text(aes(label = group_quarter))

ggplot(out[[4]], aes(x = t, y = reorder(as.character(id), group_quarter), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")



# A/B
out = mult_runs(N = 1, n_contacts = 20, n_contacts_brief = 50, run_specials = T,
                type = "A/B",
                total_days = 5)
model = cbind(out[[2]][[1]] %>%
                select(id, class, adult, group, group_quarter), out[[2]][[4]]) %>%
  gather(var, value, -id, -class, -adult, -group, -group_quarter)

ggplot(model %>% filter(class == 1), aes(x = as.numeric(var), y = reorder(factor(id), group), fill = value)) +
  geom_tile() + facet_wrap(.~class, scales = "free") + geom_text(aes(label = group))

ggplot(out[[4]], aes(x = t, y = reorder(as.character(id), group), fill = present)) +
  geom_tile() + facet_wrap(.~class, scales = "free")




