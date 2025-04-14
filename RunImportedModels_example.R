# you can find the models using the BioModels ID on https://www.ebi.ac.uk/biomodels/

# we need odin to run the models
# documentation: https://mrc-ide.github.io/odin/
# install.packages("odin")
library(odin)
# install.packages("devtools")
library(devtools)
load_all() # loads functions from SBMLtoOdin package

### example 1

# SIR model example (Malaysia)
importSBMLfromBioModels("BIOMD0000000982","../LawModel.R")

model_generator <- odin::odin("../LawModel.R") # creates model
LawModel <- model_generator$new() #creates object, with parameters if needed
LawModel_res <- LawModel$run(0:150) # runs model forward in time, for specified time
# plot simulation
matplot(LawModel_res[, 1], LawModel_res[, c(3,4)], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = c(I = "#E69F00", R = "#56B4E9"), lty = 1, ylim = c(0,7000), lwd = 3)
legend(x = 50, y =4000, lwd = 3, col = c("#E69F00", "#56B4E9"), legend = c("I", "R"), bty = "n")



cols <- c(S = "#000000", I = "#E69F00", R = "#56B4E9") # defines colors for plot

# S is not shown in this version of the plot because it is very large compared to I and R


### example 2
# Model for Intracellular Signalling
importSBMLfromBioModels("MODEL2307110001","../MothesModel.R") # imports model using SBMLtoOdin package
# Error in parse(file = x, keep.source = TRUE) :
#../MothesModel.R:40:11: unexpected input
#39: deriv(IKK_active) <- 0 + 1 * k14 / (k14 + A20) - k15 * IKK_active + stimulus * k13 * exp(-A20)
#40:  default1 _

# quick fix: remove white space before _
model_generator_mothes <- odin::odin("../MothesModel.R",verbose = FALSE) # creates model
MothesModel <- model_generator_mothes$new() #creates object, with parameters if needed
MothesModel_res <- MothesModel$run(0:4000) # runs model forward in time, for specified time


cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
matplot(MothesModel_res[, 1], MothesModel_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = cols, lty = 1, ylim = c(0,40))
legend("topright", lwd = 1, col = cols, legend = colnames(MothesModel_res)[-1], bty = "n")

# testing whether increasing time is a problem in odin / R
# no.
# It must be a problem in js

cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# example 3
#BIOMD0000000012

# import model using SBMLtoOdin package
SBMLtoOdin::importSBMLfromBioModels("BIOMD0000000012","../Repressilator.R")

# run model
model_generator_rep <- odin::odin("../Repressilator.R") # creates model
RepModel <- model_generator_rep$new() #creates object, with parameters if needed
RepModel_res <- RepModel$run(0:300) # runs model forward in time, for specified time

# run model
model_generator <- odin::odin("../Repressilator.R") # creates model
RepModel <- model_generator$new() #generates executable model
RepModel_res <- RepModel$run(0:300) # runs model forward in time

# create plot
matplot(RepModel_res[, 1], RepModel_res[, -1], xlab = "Time", ylab = "Species",
        type = "l", col = cols, lty = 1, lwd = 3)
legend("topleft", lwd = 3, col = cols, legend = colnames(RepModel_res)[-1], bty = "n")


### more examples
example_model_ids <- c("BIOMD0000000002", "BIOMD0000000003", "BIOMD0000000004","BIOMD0000000005","BIOMD0000000006","BIOMD0000000008","BIOMD0000000011","BIOMD0000000012","BIOMD0000000014","BIOMD0000000017")

for (id in example_model_ids) {
  output_file <- paste("../",id, ".R", sep = "")
  importSBMLfromBioModels(id,output_file)
  model_generator <- odin::odin(output_file)

  model <- model_generator$new() #creates object, with parameters if needed
  model_res <- model$run(0:400) # runs model forward in time, for specified time

  cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  matplot(model_res[, 1], model_res[, -1], xlab = "Time", ylab = "Number of individuals",
          type = "l", col = cols, lty = 1)
  legend("topright", lwd = 1, col = cols, legend = colnames(model_res)[-1], bty = "n")
}

#Error in SBMLtoOdin::getSpeciesRule(model, i, dic_react) :
#object 'func_def_dict' not found

