#***************************** RUN BASE MODELS *****************************####
# Note that I'm continuously updating this file
# But it should give an idea of how to run things

df= make_df(n_tot = 2, n_class = 16, high_school = T)
df = make_df(n_tot = 100, n_class = 16, high_school = T, attack = .03,
                rel_trans = 1/6, n_HH = 2,
                start_type = "child", notify = F, test = F, scenario = "Base case")

synthpop = synthpop_HS
i = 1


N = 500; n_other_adults = 30; n_contacts = 10; n_contacts_brief = 0; rel_trans_HH = 1;
rel_trans = 1/8; rel_trans_brief = 1/50; rel_trans_CC = 2; rel_trans_adult = 2; p_asymp_adult = .4; child_prob = 0.05; adult_prob = 0.01;
p_asymp_child = .8; attack = .01; child_trans = 1; child_susp = .5;
teacher_trans = 1; teacher_susp = 1; disperse_transmission = T; n_staff_contact = 0; n_HH = 0; num_adults = 2;
n_start = 1; time_seed_inf = NA; days_inf = 6; mult_asymp = 1; seed_asymp = F; isolate = T; dedens = 0; run_specials_now = F;
time = 30; notify = F; test = F; test_sens =  .7; test_frac = .9; test_days = "week"; test_type = "all"; quarantine.length = 10; quarantine.grace = 3;
type = "base"; total_days = 5; includeFamily = T; synthpop = synthpop; class = NA; n_class = 4; high_school = F; nper = 8; start_mult = 1; start_type = "mix";
bubble = F; no_weekends = F; p_subclin_adult = 0; p_subclin_child = 0


N = df$n_tot[i]; n_contacts = df$n_contacts[i]; n_staff_contact = df$n_staff_contact[i]; 
run_specials_now = df$run_specials_now[i]; start_mult = df$start_mult[i]; high_school = df$high_school[i];
attack = df$attack[i]; child_susp = df$child_susp[i]; time = df$time[i]; synthpop = synthpop;
rel_trans = df$rel_trans[i]; n_other_adults = df$n_other_adults[i]; n_class = df$n_class[i];
class = NA; notify = df$notify[i]; test = df$test[i]; dedens = df$dedens[i]; 
start_type = df$start_type[i]; child_trans = df$child_trans[i]; type = df$type[i];
days_inf = df$days_inf[i]; disperse_transmission = df$disperse_transmission[i]; n_start = df$n_start[i]; 
total_days = df$total_days[i];  teacher_susp = df$teacher_susp[i]; mult_asymp = df$mult_asymp[i]; 
isolate = df$isolate[i]; test_sens = df$test_sens[i]; test_frac = df$test_frac[i]; 
p_subclin_adult = df$p_subclin_adult[i]; p_subclin_child = df$p_subclin_child[i];
test_days = df$test_days[i]; test_type = df$test_type[i]; rel_trans_HH = .04/df$attack[i]; n_HH = df$n_HH[i]

time = time; notify = notify; test = test; df = school; sched = sched;
test_sens = test_sens; test_frac = test_frac; test_days = test_days; days_inf = days_inf;
mult_asymp = mult_asymp; seed_asymp = seed_asymp; n_HH = n_HH; n_staff_contact = n_staff_contact;
n_start = n_start; time_seed_inf = time_seed_inf; high_school = high_school; nper = nper; 
start_mult = start_mult; start_type = start_type; child_prob = child_prob; adult_prob = adult_prob; test_type = test_type;
rel_trans_CC = rel_trans_CC; rel_trans_adult = rel_trans_adult; quarantine.length = quarantine.length; quarantine.grace = quarantine.grace; 
num_adults = num_adults; bubble = bubble; no_weekends = no_weekends

