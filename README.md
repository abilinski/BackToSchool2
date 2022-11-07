# COVID-19 Agent-Based K-12 School Model

 <font size="4"> This code implements an agent-based model of COVID-19 spread in elementary schools in an R package called BackToSchool, stored in this repository.  Package documentation is available [here](https://github.com/abilinski/BackToSchool2/blob/master/1%20-%20R%20package/BackToSchool_0.0.0.9000.pdf).  Version 1 of this package was used to write [this paper](https://www.acpjournals.org/doi/10.7326/M21-0600), published in Annals of Internal Medicine. An updated version of this package was also used for [a paper on masking decisions](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2789005), published in JAMA Network Open and on [testing](https://jamanetwork.com/journals/jamapediatrics/fullarticle/2791525), published in JAMA Pediatrics.
 
To install the package locally, run the following code:

```
library(devtools)
install_github("abilinski/BacktoSchool2/1 - R package/BackToSchool")
```
  
  <img src="https://github.com/abilinski/BackToSchool2/blob/master/4%20-%20Output/Paper%201/Saved%20figures/Fig1.png" width="800" class="center"/>

The model includes students (organized into households with siblings and parents).  Each elementary student is assigned to a classroom with a primary teacher.  There are other adults in the school, some of who circulate between classrooms (e.g. music, art, and special education) and some of whom do not (e.g. administrators, counselors, and pull-out special education).  In addition to in-classroom interactions, the user can specify The base model is parameterized to the population distribution of Maryland, but this can be modified by adjusting the input file passed to the synthpop variable in the function make_class().  Users pass parameters to the function mult_runs(), which will call the model multiple times, allowing full representation of model stochasticity.
 
 Options for interventions include:
 1. Reducing the classroom attack rate through masking, distancing, and other mitigation measures
 2. Reducing class size
 3. Isolation and quarantine
 4. Alternative schedules
 5. Teacher vaccination
 6. Screening
