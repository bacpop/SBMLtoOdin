library(odin)
# install.packages("devtools")
library(devtools)
load_all() # loads functions from SBMLtoOdin package
#install.packages("rjson")
library("rjson")

json_data <- fromJSON(file="../all_BioModels.json")
all_biomod_ids <- json_data[[2]]

# the curated models on BioModels are the first models in the list (model 1 - 1073 at the moment)

ImportError <- 0
OdinError <- 0
ImportError_ids <- rep(NA,1073)
OdinError_ids <- rep(NA,1073)
for (i in 1:1073) {
  tryCatch( { importSBMLfromBioModels(all_biomod_ids[i],"../TestModel.R") }, error = function(w) { print("ImportSBML error"); ImportError <<- ImportError + 1; ImportError_ids[ImportError] <<-  all_biomod_ids[i]}, warning = function(w) { print("ImportSBML warning") })

  tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(w) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i] })
}
ImportError
OdinError
ImportError_ids[1:3]
OdinError_ids[1:32]

