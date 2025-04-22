# SBMLtoOdin
An R package for translating ODE models from the Systems Biology Markup Language (SBML, https://sbml.org/) into the domain-specific language odin (https://github.com/mrc-ide/odin).

SBMLtoOdin was mainly developed to translate SBML models from the BioModels database (https://www.ebi.ac.uk/biomodels/), which hosts thousands of computational models for biology, but it can be run on any SBML file.

## Installation
```R
# install devtools which allows users to install SBMLtoOdin
install.packages("devtools")
library(devtools)

# install libSBML
# this requires a few manual steps
# download R bindings (libSBML-5.20.2 src archive) from here https://github.com/sbmlteam/libsbml/releases/tag/v5.20.2
# then go to "Packages" or "Packages and Data" section in R Studio --> Install --> from Package Archive File (tar.gz) --> choose tar.gz file that has just been downloaded
# more information on libSBML installation here: https://sbml.org/software/libsbml/libsbml-docs/installation/

# install SBMLtoOdin from our github repository
install_github("bacpop/SBMLtoOdin")
library(SBMLtoOdin)
```

## Usage (with model example)
### Importing models
```R
# import model using BioModels ID, which directly downloads the model and translates it using SBMLtoOdin, e.g.
SBMLtoOdin::importSBMLfromBioModels("BIOMD0000000012","Repressilator.R")

# alternatively, run SBMLtoOdin on local file, e.g.
SBMLtoOdin::importSBMLfromFile("tests/testthat/BIOMD0000000012_url.xml","Repressilator.R")
```

### Running models
```R
# need to install odin for this
install.packages("odin")
# more information on odin installation here: https://github.com/mrc-ide/odin
library(odin)

model_generator <- odin::odin("Repressilator.R") # creates model
RepModel <- model_generator$new() #generates executable model
RepModel_res <- RepModel$run(0:300) # runs model forward in time

# create plot
matplot(RepModel_res[, 1], RepModel_res[, -1], xlab = "Time", ylab = "Species",
        type = "l", lty = 1, lwd = 3)
legend("topleft", lwd = 3, col = seq_len(ncol(RepModel_res)-1), legend = colnames(RepModel_res)[-1], bty = "n")
```
