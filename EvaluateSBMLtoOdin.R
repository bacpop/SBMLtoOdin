

# read in file names from all models that were translated from SBML to R to js
translated_models <- read.table("~/Documents/PhD_Project/Code/BioModelsProject/Evaluation_SBMLtoOdin/model_names.txt", quote="\"", comment.char="")

# sample 10 of these
set.seed(1)
translated_models_sample <- sample(translated_models$V1, 10)
# [1] "BIOMD0000001058.js" "BIOMD0000000854.js" "BIOMD0000000172.js" "BIOMD0000000644.js" "BIOMD0000000595.js" "BIOMD0000000373.js" "BIOMD0000000343.js" "BIOMD0000000236.js"
#[9] "BIOMD0000000381.js" "BIOMD0000000754.js"
