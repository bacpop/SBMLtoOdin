library(odin)
# install.packages("devtools")
library(devtools)
load_all() # loads functions from SBMLtoOdin package
#install.packages("rjson")
#library("rjson")

json_data <- jsonlite::fromJSON(txt ="../all_BioModels.json")
all_biomod_ids <- json_data[[2]]

# the curated models on BioModels are the first models in the list (model 1 - 1073 at the moment)
# alternative to this script: use python script to download all curated BioModels models. Then iterate over them.

ImportError <- 0
OdinError <- 0
noImportError <- 0
ImportError_ids <- rep(NA,1073)
OdinError_ids <- rep(NA,1073)
non_error_ids <- rep(NA,1073)
for (i in 1:1073) {
  tryCatch( { importSBMLfromBioModels(all_biomod_ids[i],"../TestModel.R"); noImportError <- noImportError +1; non_error_ids[noImportError] <- all_biomod_ids[i] }, error = function(w) { print("ImportSBML error"); ImportError <<- ImportError + 1; ImportError_ids[ImportError] <<-  all_biomod_ids[i]}, warning = function(w) { print("ImportSBML warning") })

  #tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(w) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i] })
}
ImportError # 155
OdinError # 482
ImportError_ids[1:ImportError]
OdinError_ids[1:OdinError]

ImportError2 <- 0
OdinError2 <- 0
non_import_error_models <- all_biomod_ids[1:1073][! all_biomod_ids[1:1073] %in% ImportError_ids[1:ImportError]]
ImportError_ids2 <- rep(NA,1073)
OdinError <- rep(NA,1073)
for (i in 1:length(non_import_error_models)) {

  tryCatch( { importSBMLfromBioModels(non_import_error_models[i],"../TestModel.R") }, error = function(w) { print("ImportSBML error"); ImportError2 <<- ImportError2 + 1; ImportError_ids2[ImportError2] <<-  non_import_error_models[i]}, warning = function(w) { print("ImportSBML warning") })

  tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(w) { print("odin error"); OdinError2 <<- OdinError2 +1; OdinError_ids[OdinError2] <<-  non_import_error_models[i] })
}
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
# but not for other functions! --> try to find an autograd / automatic differentiation tool that can deal with non-diff. functions
# 1  3  7  8  9 10 11 12 13 21 29 31
# 1 solved, the rest still needs to be solved:
# 3 gt "BIOMD0000000173"
# 7 8 9 10 11 12 13 goldbeter_koshland "BIOMD0000000306" "BIOMD0000000307" "BIOMD0000000309" "BIOMD0000000310" "BIOMD0000000311" "BIOMD0000000312" "BIOMD0000000351"
### --> add getFunctionForRule somewhere else in the code.
# 21 floor "BIOMD0000000678"
# 29 geq "BIOMD0000001030"
# 31 GK "BIOMD0000001044"

# (also others not in the derivates table: GK_219, 'goldbeter_koshland', 'floor', 'gt')

# "Input null"
# length(which(ImportError_messages == "Input null"))
# [1] 85
# after fixing the particular error with getFunctionOutput: [1] 12 :-)
# but still some remaining: 24 26 27 28 30 32 33 34 35 36

# still problems with 32, 34, 35, 36
# model 32 "BIOMD0000001046" is not an ode (constraint-based modelling). But I am not sure that this information is given anywhere
# model 34 "BIOMD0000001062" is a metabolic network
# model 35 "BIOMD0000001063" is a metabolic network
# model 36 "BIOMD0000001064" is a constraint-based model

# embedded nul in string
#grep('embedded nul in string', ImportError_messages)
#[1] 37 38 39 40 41 42 43 44 45 46
# Model ids: "BIOMD0000001066", "BIOMD0000001067", "BIOMD0000001068", "BIOMD0000001069", "BIOMD0000001070", "BIOMD0000001071", "BIOMD0000001073", "BIOMD0000001074", "BIOMD0000001075", "BIOMD0000001076"
# all of them are onnx or zip files. This is not supported by SBMLtoOdin

# --> reduced it down to 38 error messages (3.5%)
# 10 of those aer onnx or zip files and 4 non-odes (1.3%)
# that is a pretty decent result
# implementing a solution for the floor function, geq, and finding the issue for the golbeter function might still be worth it though

ImportError_messages

# still have warning messages for ImportError_ids[7] (BIOMD0000000174) (same params defined twice with different variables)
# I have that issue with many models. I really need to check the priorities of local and global parameter definitions.

# similarly, "Warning: Initial amount and concentration not defined for ..."

# floor function does not have a derivative...

############
# just for testing whichever model/error I am trying to solve at the moment
# basically let's me run the R package bit by bit which helps with trouble shooting and finding the exact error

tryCatch( { importSBMLfromBioModels(ImportError_ids[7],"../TestModel.R") }
          , error = function(m) { message(conditionMessage(m)) })

importSBMLfromBioModels(ImportError_ids[7],"../TestModel.R")
model_id <- ImportError_ids[7]
res1 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/files/", model_id, sep = ""))
data = jsonlite::fromJSON(rawToChar(res1$content))
filename = data$main[,"name"]
filename = gsub(" ", "%20", filename)
res2 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/download/", model_id, "?filename=", filename, sep = ""))
doc = libSBML::readSBMLFromString(rawToChar(res2$content))
model = libSBML::SBMLDocument_getModel(doc)

file_str <-""

dic_react <- c()
# add initial values for species
print("Fetching Species")
for (i in seq_len(libSBML::Model_getNumSpecies(model))) {
  species = libSBML::Model_getSpecies(model, i-1)
  id = libSBML::Species_getId(species)
  dic_react[id] <- 0
  found_initial <- FALSE
  if(!is.na(libSBML::Species_getInitialAmount(species))){
    conc = libSBML::Species_getInitialAmount(species)
    conc = paste("user(",conc, ")",sep = "")
    found_initial <- TRUE
  }
  else if(!is.na(libSBML::Species_getInitialConcentration(species))){
    conc = libSBML::Species_getInitialConcentration(species)
    conc = paste("user(",conc, ")",sep = "")
    found_initial <- TRUE
  }
  if(libSBML::Model_getNumRules(model)>0){
    for (j in 1:libSBML::Model_getNumRules(model)) {
      if(libSBML::Rule_getVariable(libSBML::Model_getRule(model,j-1)) == id){
        if(libSBML::Rule_getType(libSBML::Model_getRule(model,j-1)) == "RULE_TYPE_SCALAR"){
          conc = libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(model,j-1)))
          found_initial <- TRUE
        }
      }
    }
  }
  if(libSBML::Model_getNumInitialAssignments(model)>0){
    for (j in 1:libSBML::Model_getNumInitialAssignments(model)) {
      if(libSBML::InitialAssignment_getSymbol(libSBML::Model_getInitialAssignment(model,j-1)) == id)
        conc = libSBML::formulaToString(libSBML::InitialAssignment_getMath(libSBML::Model_getInitialAssignment(model,j-1)))
      found_initial <- TRUE
    }
  }
  if(!found_initial){
    print(paste("Warning: Initial amount and concentration not defined for ", as.character(id)))
  }
  file_str <- paste(file_str, paste("initial(",id,") <- ", id, "_init",sep = ""), paste(id, "_init <- ", conc, sep = ""), sep = "\n")
}
# add rules to file
print("Fetching Rules")
for (i in seq_len(libSBML::Model_getNumRules(model))) {
  print(i)
  if(is.element(libSBML::Rule_getId(libSBML::Model_getRule(model,i-1)),names(dic_react))){
    dic_react <- SBMLtoOdin::getSpeciesRule(model, i, dic_react)
  }
  else{
    file_str <- SBMLtoOdin::getRule(file_str, model, i)
    # this does not seem right!
  }
}
# collect reactions
print("Fetching Reactions")
param_lib <- c()
reserved_names_lib <- c()
reserved_names_lib[c("i", "j", "k", "l", "i5", "i6", "i7", "i8")] <- c("i_", "j_", "k_", "l_", "i5_", "i6_", "i7_", "i8_")
for (i in seq_len(libSBML::Model_getNumReactions(model))){
  print(paste("Reaction",i))
  for (j in seq_len(libSBML::Reaction_getNumProducts(libSBML::Model_getReaction(model, i-1)))) {
    print(paste("Product",j))
    id_prod <- libSBML::Species_getSpeciesType(libSBML::Reaction_getProduct(libSBML::Model_getReaction(model, i-1),j-1))

    dic_react[id_prod] <- paste(dic_react[id_prod],  " + ", libSBML::KineticLaw_getFormula(libSBML::Reaction_getKineticLaw(libSBML::Model_getReaction(model, i-1))), sep = "")
    # former version. I will have to test what happens when reaction does not behave according to kinetic law
    #dic_react[id_prod] <- paste(dic_react[id_prod],  " + ", SBMLtoOdin::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")
  }
  for (j in seq_len(libSBML::Reaction_getNumReactants(libSBML::Model_getReaction(model, i-1)))) {
    print(paste("Reactant",j))
    id_reac = libSBML::Species_getSpeciesType(libSBML::Reaction_getReactant(libSBML::Model_getReaction(model, i-1),j-1))
    dic_react[id_reac] <- paste(dic_react[id_reac], " - ", libSBML::KineticLaw_getFormula(libSBML::Reaction_getKineticLaw(libSBML::Model_getReaction(model, i-1))), sep = "")
    # former version. I will have to test what happens when reaction does not behave according to kinetic law
    #dic_react[id_reac] <- paste(dic_react[id_reac], " - ", SBMLtoOdin::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")

  }
  # get Parameters for reaction
  file_str <- SBMLtoOdin::getFunctionParams(file_str, libSBML::Model_getReaction(model, i-1),param_lib,reserved_names_lib)
  param_lib <- SBMLtoOdin::AddToParamLib(libSBML::Model_getReaction(model, i-1), param_lib,reserved_names_lib)
}
### special case: there are no reactions in the model.
# or maybe there are reactions that are defined through rules rather than reactions.
# add reactions, one per product
for (i in names(dic_react)){
  file_str <- paste(file_str, paste("deriv(",i,")", " <- ", dic_react[i], sep = ""), sep = "\n")
}
#print(file_str)
# Call function that replaces pow() by ^ if necessary
if(grepl("pow\\(",file_str)){
  file_str <- translate_pow(file_str)
}

