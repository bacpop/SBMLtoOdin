# script from https://github.com/bacpop/odinviewer

# install.packages("odin")
library(odin)
# install.packages("odin.js")
library(odin.js)
# install.packages("devtools")
library(devtools)
library(stringr)

load_all() # loads functions from SBMLtoOdin package

id <- "BIOMD0000000002" # change with the id of the model you want
id <- "BIOMD0000000700" # change with the id of the model you want

output_file <- paste("../",id, ".R", sep = "")
importSBMLfromBioModels(id,output_file)

js_code <- odin::odin_js_bundle(output_file)
code <- js_code$model$code
code[1] <- paste("export",code[1])
splited_code <- str_split_1(code[1]," ")
splited_code[3] <- "model"
code[1] <- paste(splited_code, collapse = " ")
output_file <- paste("../",id, ".js", sep = "")
writeLines(code, con = output_file)
