library(odin)
# install.packages("devtools")
library(devtools)
load_all() # loads functions from SBMLtoOdin package
#install.packages("rjson")
library("rjson")

json_data <- fromJSON(file="../all_BioModels.json")
all_biomod_ids <- json_data[[2]]

# the curated models on BioModels are the first models in the list (model 1 - 1073 at the moment)
# alternative to this script: use python script to download all curated BioModels models. Then iterate over them.

ImportError <- 0
OdinError <- 0
ImportError_ids <- rep(NA,1073)
OdinError_ids <- rep(NA,1073)
for (i in 1:1073) {
  tryCatch( { importSBMLfromBioModels(all_biomod_ids[i],"../TestModel.R") }, error = function(w) { print("ImportSBML error"); ImportError <<- ImportError + 1; ImportError_ids[ImportError] <<-  all_biomod_ids[i]}, warning = function(w) { print("ImportSBML warning") })

  tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(w) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i] })
}
ImportError # 155
OdinError # 482
ImportError_ids[1:ImportError]
OdinError_ids[1:OdinError]

# import fails in 15% of the cases
# running odin fails in 45% of the cases
ImportError_messages <- rep(NA, ImportError)
for (i in 1:ImportError) {
  tryCatch( { importSBMLfromBioModels(ImportError_ids[i],"../TestModel.R") }
            , error = function(m) {ImportError_messages[i] <<- as.character((conditionMessage(m))) })
}

# common error messages:
# "Function 'pow' is not in the derivatives table" --> problem was in SpeciesRules where I calculate the derivative for the species
# needed to add a call to translate_pow to it and then solved a number of issue
### issue resolved
# (also others not in the derivates table: GK_219, 'goldbeter_koshland', 'floor', 'gt')
# "Input null"

ImportError_messages

tryCatch( { importSBMLfromBioModels(ImportError_ids[2],"../TestModel.R") }
          , error = function(m) { message(conditionMessage(m)) })

importSBMLfromBioModels(ImportError_ids[7],"../TestModel.R")
