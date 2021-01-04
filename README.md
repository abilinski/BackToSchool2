# COVID-19 Agent-Based Elementary School Model

 <font size="4"> This code implements an agent-based model of COVID-19 spread in elementary schools in an R package called BackToSchool, stored in this repository.  Package documentation is available [here](https://github.com/abilinski/BackToSchool2/blob/master/1%20-%20R%20package/BackToSchool_0.0.0.9000.pdf).  The package remains under development.  Sample parallelized code is available [here](https://github.com/abilinski/BackToSchool2/blob/master/3%20-%20Scripts/base_script.R).
  
The model includes students (organized into households with siblings and parents).  Each elementary student is assigned to a classroom with a primary teacher.  There are other adults in the school, some of who circulate between classrooms (e.g. music, art, and special education) and some of whom do not (e.g. administrators, counselors, and pull-out special education).  In addition to in-classroom interactions, the user can specify The base model is parameterized to the population distribution of Maryland, but this can be modified by adjusting the input file passed to the synthpop variable in the function make_class().  Users pass parameters to the function mult_runs(), which will call the model multiple times, allowing full representation of model stochasticity.
 
 Options for interventions include:
 1. Reducing the classroom attack rate through masking, distancing, and other mitigation measures
 2. Reducing class size
 3. Alternative schedules
 4. Teacher vaccination
 5. Screening
 
 <img src="https://github.com/abilinski/BackToSchool2/blob/master/5%20-%20Figures/model2.png" width="200" height="200" />
