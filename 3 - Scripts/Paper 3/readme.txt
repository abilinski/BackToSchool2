To replicate model results, the files should be run in the following order:
1. paper3_script_corrections.R (Generates model runs, calling functions from abm_correction.R)
2. bind_results_corrections.R (Summarizes and binds model runs into a single data frame)
3. main_code_revisions_draft.R (Analyzes model runs and generates figure and table output)

Note: The paper3_script_corrections.R is designed to be run as part of a job array on a computing cluster. The job.number variable should be set to "0" and the nsamp variable appropriately increased if the model is being run locally.
