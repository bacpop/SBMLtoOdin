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
ImportError_ids <- rep(NA,1073)
OdinError_ids <- rep(NA,1073)
OdinError_messages <- rep(NA, 1073)
#for (i in 1:1073) {
for (i in 1:100){
  print(paste("Model", i))
  tryCatch( { importSBMLfromBioModels(all_biomod_ids[i],"../TestModel.R")}, error = function(w) { print("ImportSBML error"); ImportError <<- ImportError + 1; ImportError_ids[ImportError] <<-  all_biomod_ids[i]}, warning = function(w) { print("ImportSBML warning") })
  tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(m) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i]; OdinError_messages[OdinError] <<- as.character(conditionMessage(m))})
  #tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(w) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i] })
}
ImportError # 155
OdinError # 482
ImportError_ids[1:ImportError]
OdinError_ids[1:OdinError]


# 17.02.2025
# new series of testing while changing SBMLtoOdin script to a more list-based code, with parsing to string only at the end
# BIOMD0000000007 throws OdinError:
# Error: Self referencing expressions not allowed (except for arrays)
#kp <- if(SPF >=  0.1) kp / 2 else 3.25 # (line 116)
# but this is apparently not new
# for models 1:100 I have odin 21 errors
# "BIOMD0000000007" "BIOMD0000000016" "BIOMD0000000020" "BIOMD0000000024" "BIOMD0000000034" "BIOMD0000000036" "BIOMD0000000047" "BIOMD0000000050" "BIOMD0000000054" "BIOMD0000000055" "BIOMD0000000056" "BIOMD0000000059" "BIOMD0000000069" "BIOMD0000000075" "BIOMD0000000077" "BIOMD0000000081" "BIOMD0000000087" "BIOMD0000000088" "BIOMD0000000095" "BIOMD0000000096" "BIOMD0000000097"
# all except 5 (20, 50, 59, 75, 81) are errors I had before
# Let's check out these 5 first
# 20 is fixed - that was a problem with non-constant parameters that depend on some other parameter value but do not have an initial value
# 50 is fixed - that was a problem with parameters/species that start with a number or an underscore
# 59 is now working as well
# 75 working as well - that was a problem when parameters appear locally and globally (assuming local definition is more important), is overriding the global parameters
# 81 was working before but was not correct. Instead, I need to introduce features to recognise "modifier species" (species that do not change, i.e. do not have their own ODE but play a role in some of the equations (similar to parameters))
# oxoM_EX <- if(t >=  3 && t <= 8) 10 else if(t >=  8) 0 else 0
# still working on this. trying to catch duplicated events, continue on line 1300
# this is more difficult than expected. leaving it for now

# 24.02.2025
# ImportError
# "BIOMD0000000232" "BIOMD0000000234" "BIOMD0000000246" "BIOMD0000000256" "BIOMD0000000280"
# "BIOMD0000000471"
# "BIOMD0000000988" "BIOMD0000001061" "BIOMD0000001062" "BIOMD0000001063" "BIOMD0000001064" "BIOMD0000001066" "BIOMD0000001067" "BIOMD0000001068" "BIOMD0000001069" "BIOMD0000001070" "BIOMD0000001071" "BIOMD0000001073" "BIOMD0000001074" "BIOMD0000001075" "BIOMD0000001076"
# fixed models up to BIOMD0000000988

# 25.02.25
# odin errors for models 1-100 (40!)
# [1] "BIOMD0000000006" "BIOMD0000000007" "BIOMD0000000013" "BIOMD0000000015" "BIOMD0000000017" "BIOMD0000000018" "BIOMD0000000019" "BIOMD0000000023" "BIOMD0000000024" "BIOMD0000000025"
#[11] "BIOMD0000000027" "BIOMD0000000029" "BIOMD0000000031" "BIOMD0000000033" "BIOMD0000000034" "BIOMD0000000037" "BIOMD0000000038" "BIOMD0000000046" "BIOMD0000000047" "BIOMD0000000049"
#[21] "BIOMD0000000051" "BIOMD0000000054" "BIOMD0000000055" "BIOMD0000000056" "BIOMD0000000061" "BIOMD0000000063" "BIOMD0000000064" "BIOMD0000000067" "BIOMD0000000068" "BIOMD0000000069"
#[31] "BIOMD0000000070" "BIOMD0000000071" "BIOMD0000000076" "BIOMD0000000077" "BIOMD0000000081" "BIOMD0000000087" "BIOMD0000000088" "BIOMD0000000095" "BIOMD0000000096" "BIOMD0000000097"
# fixed "BIOMD0000000006" (problem with boundary conditions)
# "BIOMD0000000007" self referencing not allowed - not quite sure how to fix that
# fixed "BIOMD0000000013" problem with boundary conditions that are constant without being defined as constant
# "BIOMD0000000015", "BIOMD0000000017" - BIOMD0000000023 were fine then
# "BIOMD0000000024" broken because of delay() must be the only call on the rhs
# "BIOMD0000000025" too
# "BIOMD0000000034" too
# fixed "BIOMD0000000027" - was issue with constant species
# "BIOMD0000000029" "BIOMD0000000031" "BIOMD0000000033" "BIOMD0000000037" "BIOMD0000000038" "BIOMD0000000046" "BIOMD0000000049" were fine then
# fixed "BIOMD0000000047" (also translate Time to t now - I hope that's correct?)
# "BIOMD0000000051" still not working Unknown variable txt
# definition of time variable ..> defined in math section of assignment rule, then csymbol but I do not know how to access that


#151 (1-495), 70 (500-750)
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

# 06.10.2024
#ImportError # 36
#OdinError # 281
# with 9 models in the intersect
# so, 308 models result in error in total
# i.e. 704 can be translated and run successfully
# that is 70%

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




### OdinErrors
# common ones seem to be:
# Self referencing expressions not allowed
# Unknown variable pi (and other unknown variables)
# delay() must be the only call on the rhs
# Reserved name 'i' for lhs

# "BIOMD0000000001" Unknown variables

#> OdinErrors for the first 300 models (...)
#[1] 127
#> OdinError_ids[1:OdinError]
#[1] "BIOMD0000000007" "BIOMD0000000016" "BIOMD0000000019" "BIOMD0000000020" "BIOMD0000000024" "BIOMD0000000025"
#[7] "BIOMD0000000034" "BIOMD0000000036" "BIOMD0000000047" "BIOMD0000000050" "BIOMD0000000054" "BIOMD0000000055"
#[13] "BIOMD0000000056" "BIOMD0000000057" "BIOMD0000000059" "BIOMD0000000063" "BIOMD0000000064" "BIOMD0000000069"
#[19] "BIOMD0000000071" "BIOMD0000000073" "BIOMD0000000074" "BIOMD0000000075" "BIOMD0000000077" "BIOMD0000000078"
#[25] "BIOMD0000000081" "BIOMD0000000083" "BIOMD0000000087" "BIOMD0000000088" "BIOMD0000000095" "BIOMD0000000096"
#[31] "BIOMD0000000097" "BIOMD0000000101" "BIOMD0000000104" "BIOMD0000000110" "BIOMD0000000111" "BIOMD0000000113"
#[37] "BIOMD0000000118" "BIOMD0000000119" "BIOMD0000000120" "BIOMD0000000121" "BIOMD0000000122" "BIOMD0000000124"
#[43] "BIOMD0000000125" "BIOMD0000000126" "BIOMD0000000127" "BIOMD0000000128" "BIOMD0000000129" "BIOMD0000000130"
#[49] "BIOMD0000000131" "BIOMD0000000132" "BIOMD0000000133" "BIOMD0000000134" "BIOMD0000000135" "BIOMD0000000136"
#[55] "BIOMD0000000137" "BIOMD0000000138" "BIOMD0000000139" "BIOMD0000000140" "BIOMD0000000141" "BIOMD0000000142"
#[61] "BIOMD0000000144" "BIOMD0000000148" "BIOMD0000000149" "BIOMD0000000150" "BIOMD0000000152" "BIOMD0000000153"
#[67] "BIOMD0000000154" "BIOMD0000000155" "BIOMD0000000157" "BIOMD0000000158" "BIOMD0000000161" "BIOMD0000000162"
#[73] "BIOMD0000000163" "BIOMD0000000164" "BIOMD0000000165" "BIOMD0000000171" "BIOMD0000000174" "BIOMD0000000179"
#[79] "BIOMD0000000180" "BIOMD0000000183" "BIOMD0000000188" "BIOMD0000000189" "BIOMD0000000190" "BIOMD0000000195"
#[85] "BIOMD0000000196" "BIOMD0000000201" "BIOMD0000000202" "BIOMD0000000211" "BIOMD0000000213" "BIOMD0000000214"
#[91] "BIOMD0000000216" "BIOMD0000000226" "BIOMD0000000227" "BIOMD0000000232" "BIOMD0000000234" "BIOMD0000000235"
#[97] "BIOMD0000000237" "BIOMD0000000238" "BIOMD0000000239" "BIOMD0000000241" "BIOMD0000000243" "BIOMD0000000244"
#[103] "BIOMD0000000245" "BIOMD0000000247" "BIOMD0000000248" "BIOMD0000000250" "BIOMD0000000255" "BIOMD0000000256"
#[109] "BIOMD0000000259" "BIOMD0000000260" "BIOMD0000000261" "BIOMD0000000265" "BIOMD0000000266" "BIOMD0000000268"
#[115] "BIOMD0000000269" "BIOMD0000000273" "BIOMD0000000279" "BIOMD0000000280" "BIOMD0000000281" "BIOMD0000000285"
#[121] "BIOMD0000000286" "BIOMD0000000287" "BIOMD0000000288" "BIOMD0000000290" "BIOMD0000000293" "BIOMD0000000297"
#[127] "BIOMD0000000300"

#OdinError_messages

# tackling issues with tanh function e.g. OdinError_messages[117] "BIOMD0000000279"
# replaced tanh with if statement
# will have to check whether odin solver can deal with this and whether I can still compute derivatives (where necessary)

# tackling issues with using 'default' where it is not allowed [109][110][111] ("BIOMD0000000261")
# fixed the default error

# unexpected symbol  e.g. in 17 (BIOMD0000000064: ADP_init <- (SUM_P - (((1 +  F6P / KmPFK F6PP) and 19 (BIOMD0000000071: unexpected symbol
#37: ATPc_init <- (Pc * (1 - 4 * KeqAK) - sumAc + (((PEPc / (0.34 * (1 + ADPc / 0.57 + ATPc / 0.64)))^(n12) * ADPc / K12ADP / ((1 + (PEPc / (0.34 * (1 + ADPc / 0.57 + ATPc / 0.64)))^(n12)) * (1 + A
 # possibly connected to power function (yes!)                                                                                                                                                                                          38: deriv)

#R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P)
#get translated into
#(1 +  F6P / KmPFK F6PP +  ATP /  KmPFK ATPP +  gR * ( F6P / KmPFK F6PP) * ( ATP /  KmPFK ATPP))

#[1] 114


# "BIOMD0000000019"
# nk5 <- piecewise(1.55, lt(C, 3100), 0.2, gt(C, 100000), C * -1.35e-5 + 1.55)\n
# has a step function that three segments. I can split this out into if else if else but the parsing might be annoying


# 114 of 300 models have errors
# unexpected numeric constant e.g. 95, 97, 103 ("BIOMD0000000241", "BIOMD0000000244", "BIOMD0000000266")
# unknown variable 113 ("BIOMD0000000293")
# Self referencing expressions not allowed (except for arrays) 34 42("BIOMD0000000118" "BIOMD0000000127")

# "BIOMD0000000266" fixed but apparently this type of error has different causes (...)

# "BIOMD0000000244"
#problem gets introduced in events
# if(geq(t, 3600 * shift1)NA > NANA) GLC_1 else NA\nACT <- if(geq(t, 3600 * shift1)NA > NANA) ACT_1 else NA\nBM <- if(geq(t, 3600 * shift1)NA > NANA) BM_1 else NA\nGLC <- if(geq(t, 3600 * shift2)NA > NANA) GLC_2 else NA\nACT <- if(geq(t, 3600 * shift2)NA > NANA) ACT_2 else NA\nBM <- if(geq(t, 3600 * shift2)NA > NANA) BM_2 else NA"
### hmm, this is a model with two tirggers, where the odes are re-set to specific values. Not sure if I am going to make that work...

# "BIOMD0000000241"
# has a similar issue
# actually throws "self referencing" error now
# events seem to be difficult for odin :-/

# "BIOMD0000000293" fixed (issue with parameters that were defined as constant=FALSE but were, in effect, constant...)

#> OdinError_ids[21]
#[1] "BIOMD0000000356"
#> OdinError_messages[21]
#[1] "Self referencing expressions not allowed (except for arrays)\n\tG_p <- EGP + Ra - E - U_ii - k_1 * G_p + k_2 * G_t # (line 168)"
# solved :-)

#> OdinError_ids[15]
#[1] "BIOMD0000000348"
#> OdinError_messages[15]
#[1] "Self referencing expressions not allowed (except for arrays)\n\tG3P_init <- G3P_init # (line 72)"

# looking at models 501-600
# [3] "../TestModel.R:2:10: unexpected numeric constant\n1: \n2: initial(_2\n
# "BIOMD0000000510" "BIOMD0000000511" "BIOMD0000000513" "BIOMD0000000514" "BIOMD0000000515" "BIOMD0000000516"
# the problem seems to be that variable names start with _number

# looking at model 601-700
# "BIOMD0000000670" problem with reserved param l
#solved
# models BIOMD0000000603 - BIOMD0000000607 unexpected ')' always close to if statement
# solved (issue with if else if statements)
# "BIOMD0000000622" and "BIOMD0000000630" Unhandled expression deri(v) on lhs
# solved through one of the previous steps
# BIOMD0000000669, BIOMD0000000672 unexpected numeric constant
# again a problem with if statements, this time there seem to be two conditions (if (a and b))
# solved

# looking at models 701-800
# 23 odin errors (0 import errors!)
# [18] "Reserved name 'k' for lhs\n\tk <- f * exp(-a * t) # (line 33)" "BIOMD0000000764"
# solved
# [23] "Reserved name 'do' for lhs\n\tdo <- user(0.01) # (line 15)" "BIOMD0000000801"
# added "do" and a couple of others to reserved_names_lib
# [21] "Self referencing expressions not allowed "BIOMD0000000789"
# not sure that the paper https://link.springer.com/article/10.1007/s11538-018-0424-4 (equations) and the sbml model really are the same
# anyway, this example made clear that there are persisting and non-persiting triggers
# the first type will always be one once triggered
# the second type will be ON when triggered and then turn back off
# this might be worth implementing
# this specific model though might be tricky because it actually sets one of the species (ode variables) is set to some value.
# does odin even allow that?
# maybe if I integrate if statements directly in the rhs of deriv

# [6] "Unsupported function: function_for_actCycACdk2_1 BIOMD0000000723
# there are no commas in the function call of "function_for_actCycACdk2_1". really weird. need to investigate this.
# [7] "Unsupported functions: Function_for_Reaction_6 BIOMD0000000726
# both had the same problem (another function in the model had the same parameter set and already replaced the comma-separated parameters by the function output (but no the name))

# [16] "Reserved name 't' for lhs\n\tt <- user(3e+05) # (line 10)"  BIOMD0000000760
# I added "t" to the reserved_names_lib and pushed replacing time by t to the very end of the script

# now just 12 odin errors for models 701-800

# looking at model 801-900 today (12.08)
# to start: 20 odin errors
# "BIOMD0000000805" "BIOMD0000000806" "BIOMD0000000816" "BIOMD0000000817" "BIOMD0000000818" "BIOMD0000000820" "BIOMD0000000825" "BIOMD0000000826" "BIOMD0000000830" "BIOMD0000000833" "BIOMD0000000835" "BIOMD0000000841" "BIOMD0000000849" "BIOMD0000000856" "BIOMD0000000860" "BIOMD0000000862" "BIOMD0000000864" "BIOMD0000000872" "BIOMD0000000899" "BIOMD0000000901"
# Self referencing expressions not allowed (except for arrays) [an error I've been getting over and over - if these are things that are supposed to be updated in every step, it might be difficult to find a solution but let's see]
# here: [14] BIOMD0000000856 [16] BIOMD0000000862 [20] BIOMD0000000901
# model BIOMD0000000901: the problem seems to be that there is an event that changes the value of a species, not of a param - (I had that before, right? solution?)
# model BIOMD0000000856: possible solution might be to implement _help variable (but I am not sure about what gets evaluated when)
# model BIOMD0000000862: actually missing else case. But, essentially, also event on species and odin complains that it is not within initial or deriv

# checking whether I can find an easier self-ref example in models 700-800
# BIOMD0000000711, BIOMD0000000718, BIOMD0000000727, BIOMD0000000789
# all of them are events for species
# and BIOMD0000000706, BIOMD0000000734, BIOMD0000000735, BIOMD0000000736 are "variables on lhs must be within deriv() or initial()" errors

# idea for solving this: introduce delay variables
# e.g. for model BIOMD0000000789
#V1lag <- delay(V1, 2)
#V2lag <- delay(V2, 4)
#deriv(V1) <- 0 + tme * ( a  *   di  *   I ) - tme * dv * V1
#deriv(V2) <- 0 + tme * ( a  *   di  *   I ) - tme * dv * V2
#deriv(V) <- if(t >=  2) V1lag else if(t >=  4) V2lag else tme * ( a  *   di  *   I ) - tme * dv * V

# this does not really work for species that are set to a specific value when they (or another variable other than time) are at a specific value
# this is the case for model BIOMD0000000718 Ini
# hm

############
# just for testing whichever model/error I am trying to solve at the moment
# basically let's me run the R package bit by bit which helps with trouble shooting and finding the exact error

tryCatch( { importSBMLfromBioModels("BIOMD0000000001","../TestModel.R") }
          , error = function(m) { message(conditionMessage(m)) })

tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(m) { message(conditionMessage(m))})



importSBMLfromBioModels("BIOMD0000000001","../TestModel.R")
model_id <- "BIOMD0000000001"
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


# test exporting sbml models
importSBMLfromFile("../../models/BIOMD0000000809.xml","../../testmodel_output.R")
doc = libSBML::readSBMLFromFile("../../models/BIOMD0000000809.xml")
libSBML::writeSBMLToFile(doc, "../../testmodel_output2.xml")
#libSBML::writeSBMLToString(doc)
# yes, that works easily.
# but I'd ideally want a tool that creates SBML based on R code...?
sbml_document <- SBMLDocument(3, 1)
model <- SBMLDocument_createModel(sbml_document)
# Create a compartment
compartment <- Model_createCompartment(model)
Compartment_setId(compartment, "c1")
Compartment_setSize(compartment, 1.0)
Compartment_setUnits(compartment, "litre")

# Create species S1
species1 <- Model_createSpecies(model)
Species_setId(species1, "S1")
Species_setCompartment(species1, "c1")
Species_setInitialAmount(species1, 10.0)
Species_setBoundaryCondition(species1, FALSE)

# Create species S2
species2 <- Model_createSpecies(model)
Species_setId(species2, "S2")
Species_setCompartment(species2, "c1")
Species_setInitialAmount(species2, 0.0)
Species_setBoundaryCondition(species2, FALSE)

# Create a reaction R1: S1 -> S2
reaction <- Model_createReaction(model)
Reaction_setId(reaction, "R1")
Reaction_setReversible(reaction, FALSE)

# Add S1 as a reactant
reactant <- Reaction_createReactant(reaction)
SimpleSpeciesReference_setSpecies(reactant, "S1")

# Add S2 as a product
product <- Reaction_createProduct(reaction)
SimpleSpeciesReference_setSpecies(product, "S2")

# Set stoichiometries
#SimpleSpeciesReference_setStoichiometry(reactant, 1.0)
#SimpleSpeciesReference_setStoichiometry(product, 1.0)
# these two did not work --> let's check whether this breaks SBML requirements

# Create a kinetic law for the reaction: k * S1
kinetic_law <- Reaction_createKineticLaw(reaction)
math <- parseFormula("k * S1")
KineticLaw_setMath(kinetic_law, math)

# Add a parameter 'k' to the kinetic law
parameter <- KineticLaw_createParameter(kinetic_law)
Parameter_setId(parameter, "k")
Parameter_setValue(parameter, 0.1)

# Write the SBML document to an XML file
writeSBML(sbml_document, "../../minimal_model.xml")
doc = libSBML::readSBMLFromFile("../../minimal_model.xml")
SBMLDocument_checkConsistency(doc) # zero errors!
