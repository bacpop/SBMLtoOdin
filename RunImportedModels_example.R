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


cols <- c(S = "#000000", I = "#E69F00", R = "#56B4E9") # defines colors for plot
matplot(LawModel_res[, 1], LawModel_res[, c(3,4)], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = cols[c(2,3)], lty = 1, ylim = c(0,7000))
legend("topright", lwd = 1, col = cols, legend = c("S", "I", "R"), bty = "n")
# S is not shown in this version of the plot because it is very large compared to I and R


### example 2
# Model for Intracellular Signalling
importSBMLfromBioModels("MODEL2307110001","../MothesModel.R") # imports model using SBMLtoOdin package
model_generator_mothes <- odin::odin("../MothesModel.R",verbose = FALSE) # creates model
MothesModel <- model_generator_mothes$new() #creates object, with parameters if needed
MothesModel_res <- MothesModel$run(0:400) # runs model forward in time, for specified time


cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
matplot(MothesModel_res[, 1], MothesModel_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = cols, lty = 1, ylim = c(0,40))
legend("topright", lwd = 1, col = cols, legend = colnames(MothesModel_res)[-1], bty = "n")

