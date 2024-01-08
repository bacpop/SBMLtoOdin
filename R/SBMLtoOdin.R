
#' Title
#'
#' @param file_content A string.
#' @param m A libSBML model object.
#' @param i An integer.
#'
#' @importFrom libSBML Rule_getType Model_getRule Rule_getId formulaToString Rule_getMath
#'
#' @return A string (updated file odin file content)
#' @export
#'
#' @examples
#' getRule("initial(S1) <- S1_init", model, 1)
getRule <- function(file_content, m, i){
  rule_type <- libSBML::Rule_getType(libSBML::Model_getRule(m,i-1))
  if(rule_type == "RULE_TYPE_SCALAR"){
    file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
  }
  else if(rule_type == "RULE_TYPE_RATE"){
    if(libSBML::Rule_isParameter(libSBML::Model_getRule(m,i-1))){
      # this is a rule for a parameter, not for a species
      file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
      #file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), "1 + 0.5 * t"), collapse = " <- "), sep = "\n")
    }
    else{
      print("Warning: I cannot deal with rate rules yet\n")
      file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
    }

  }
  else{
    print("I do not recognise this rule type")
  }
  # I should also add the third option "algebraic" (even though I think it's probably used very rarely)
  return(file_content)
}


#' Title
#'
#' @param m
#' @param i
#' @param species_dic
#'
#' @importFrom libSBML formulaToString Model_getRule Rule_getMath Rule_getId
#'
#' @return
#' @export
#'
#' @examples
#' getSpeciesRule(model,1,dictionary)
getSpeciesRule <- function(m, i, species_dic){
  if(Rule_getType(libSBML::Model_getRule(m,i-1)) == "RULE_TYPE_SCALAR"){
    deriv_of_rule <- D(parse(text = libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)))
    species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))] <- paste(species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))], deriv_of_rule ,sep = " + ")
  }
  else{
    species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))] <- paste(species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))], libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1))) ,sep = " + ")
  }
  return(species_dic)
}

#' Title
#'
#' @param m A libSBML model object.
#' @param ind An integer.
#' @param r An integer.
#'
#' @importFrom libSBML Reaction_isSetKineticLaw Reaction_getKineticLaw KineticLaw_isSetMath KineticLaw_getMath Model_getNumFunctionDefinitions FunctionDefinition_getId Model_getFunctionDefinition FunctionDefinition_getBody FunctionDefinition_getArgument formulaToString
#' @importFrom stringr str_trim
#'
#' @return
#' @export
#'
#' @examples
getFunctionOutput <- function(m, ind, r){
  if (libSBML::Reaction_isSetKineticLaw(r)) {
    k = libSBML::Reaction_getKineticLaw(r);
    if (libSBML::KineticLaw_isSetMath(k)) {
      formula = libSBML::formulaToString(libSBML::KineticLaw_getMath(k));
    }
    for (n in seq_len(libSBML::Model_getNumFunctionDefinitions(m))){
      #Test whether the formula contains a function call
      if(grepl(libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1)), formula)){
        func_id <- libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1))
        function_def <- libSBML::formulaToString(libSBML::FunctionDefinition_getBody(libSBML::Model_getFunctionDefinition(m,n-1)))
        formula0 <- strsplit(formula, func_id)[[1]][length(strsplit(formula, func_id)[[1]])]
        formula1 <- strsplit(formula0,"\\(|\\)")[[1]][2]
        function_call_vars <- stringr::str_trim(strsplit(formula1, ",")[[1]])
        for (i in 1:length(function_call_vars)) {
          function_def <- gsub(libSBML::formulaToString(libSBML::FunctionDefinition_getArgument(libSBML::Model_getFunctionDefinition(m,n-1),i-1)), function_call_vars[i], function_def)
        }
        formula2 <- paste(func_id,"\\(",formula1,"\\)", sep="")
        formula <- gsub(formula2, function_def, formula)
      }
    }
  }
  formula
}

#' Title
#'
#' @param param_name A string.
#' @param reserved_lib A dictionary.
#'
#' @return A string.
#' @export
#'
#' @examples
in_reserved_lib <- function(param_name, reserved_lib){
  if(is.element(param_name, names(reserved_lib))){
    param_name <- as.character(reserved_lib[param_name])
  }
  param_name
}



#' Title
#'
#' @param r An integer.
#' @param local_param_lib A dictionary.
#' @param reserved_lib A dictionary.
#'
#' @importFrom libSBML Reaction_isSetKineticLaw Reaction_getKineticLaw KineticLaw_getNumParameters Parameter_getId KineticLaw_getParameter Parameter_getValue
#'
#' @return A dictionary.
#' @export
#'
#' @examples
AddToParamLib <- function(r, local_param_lib,reserved_lib){
  if (libSBML::Reaction_isSetKineticLaw(r)) {
    k = libSBML::Reaction_getKineticLaw(r);
    if(libSBML::KineticLaw_getNumParameters(k) > 0){
      for (i in seq_len(libSBML::KineticLaw_getNumParameters(k))) {
        id <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(k,i-1))
        id <- SBMLtoOdin::in_reserved_lib(id, reserved_lib)
        value <- libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(k,i-1))
        #local_counter <- 2
        #id_new <- id
        #while(is.element(id_new, names(local_param_lib))) {
        #  id_new <- paste(id,"_",as.character(local_counter),sep = "")
        #  print(id_new)
        #  local_counter <- local_counter + 1
        #}
        #local_param_lib[id_new] <- value
        if(is.element(id, names(local_param_lib))){
          if(value != local_param_lib[id]){
            print(paste("Warning: ", id, "is defined twice with values ", value , " and ", local_param_lib[id], ". Assuming value ", local_param_lib[id], sep = ""))
          }
        }
        else{
          local_param_lib[id] <- value
        }

      }
    }
  }
  local_param_lib
}


#' Title
#'
#' @param file_content A string (file content of odin model file).
#' @param r An integer.
#' @param param_lib A dictionary.
#' @param reserved_lib A dictionary.
#'
#' @importFrom libSBML Reaction_isSetKineticLaw Reaction_getKineticLaw KineticLaw_getNumParameters Parameter_getId KineticLaw_getParameter Parameter_getValue
#'
#' @return A string.
#' @export
#'
#' @examples
getFunctionParams <- function(file_content, r, param_lib, reserved_lib){
  if (libSBML::Reaction_isSetKineticLaw(r)) {
    k = libSBML::Reaction_getKineticLaw(r);
    if(libSBML::KineticLaw_getNumParameters(k) > 0){
      for (i in seq_len(libSBML::KineticLaw_getNumParameters(k))) {
        id <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(k,i-1))
        id <- SBMLtoOdin::in_reserved_lib(id, reserved_lib)
        value <- libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(k,i-1))
        if(!is.element(id, names(param_lib))){
          param_def <- paste(id, " <- ", value, sep = "")
          file_content <- paste(file_content, param_def, sep = "\n")
        }
      }
    }
  }
  file_content
}


#' Title
#'
#' @param file_content A string.
#'
#' @importFrom stringi stri_split_fixed
#'
#' @return
#' @export
#'
#' @examples
translate_pow <- function(file_content){
  while(grepl("pow\\(",file_content)){
    pow_expr <- stringi::stri_split_fixed(str = file_content, pattern = "pow(", n = 2)[[1]][2]
    pow_expr1 <- stringi::stri_split_fixed(str = pow_expr, pattern = ")", n = 2)[[1]][1]
    pow_rest <- ""
    if(length(stringi::stri_split_fixed(str = pow_expr, pattern = ")", n = 2)[[1]])>1){
      pow_rest <- stringi::stri_split_fixed(str = pow_expr, pattern = ")", n = 2)[[1]][2]
    }
    pow_base <- strsplit(pow_expr1,",")[[1]][1]
    pow_exponent <- strsplit(pow_expr1,",")[[1]][2]
    new_pow_expr <- paste("(",pow_base, ")^(", pow_exponent,")", sep="")
    file_content <- paste(stringi::stri_split_fixed(str = file_content, pattern = "pow", n = 2)[[1]][1], new_pow_expr, pow_rest, sep = "")
  }
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @importFrom stringi stri_split_fixed
#'
#' @return
#' @export
#'
#' @examples
translate_piecewise <- function(file_content){
  piece_expr <- strsplit(file_content,"piecewise")[[1]]
  new_piece_expr <- ""
  for (i in 2:length(piece_expr)) {
    piece_expr0 <- stringi::stri_split_fixed(str = piece_expr[i], pattern = "(", n = 3)[[1]]
    val1 <- strsplit(piece_expr0[2],",")[[1]][1]
    piece_expr1 <- stringi::stri_split_fixed(str = piece_expr0[3], pattern = ")", n = 3)[[1]]
    # I should probably write a more general version of this function
    # potentially one that checks whether lt is part of the expression
    cond1 <- strsplit(piece_expr1[1],",")[[1]][1]
    cond2 <- strsplit(piece_expr1[1],",")[[1]][2]
    val2 <- strsplit(piece_expr1[2],",")[[1]][2]
    rest1 <- ""
    if(length(piece_expr1)>2){
      rest1 <- piece_expr1[[3]]
    }
    y_expan <- as.character((as.numeric(val2) - as.numeric(val1))/2)
    y_shift <- as.character(as.numeric(val1) + as.numeric(y_expan))

    new_piece_expr <-  paste(new_piece_expr, " ", y_shift, " + ", y_expan, " * tanh( 20 * ((", cond1, ") - (", cond2, "))) ", rest1 ,sep="")
  }
  file_content <- paste(strsplit(file_content,"piecewise")[[1]][1], new_piece_expr)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @importFrom stringi stri_split_fixed
#'
#' @return A string.
#' @export
#'
#' @examples
sub_factorial <- function(file_content){
  fact_expr <- strsplit(file_content,"factorial")[[1]]
  new_fact_expr <- ""
  for (i in 2:length(fact_expr)) {
    fact_expr0 <- stringi::stri_split_fixed(str = fact_expr[i], pattern = "(", n = 2)[[1]][2]
    new_fact_expr <-  paste(new_fact_expr, "gamma( 1 + ", fact_expr0, sep="")
  }
  file_content <- paste(strsplit(file_content,"factorial")[[1]][1], new_fact_expr)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @return A string.
#' @export
#'
#' @examples
sub_ceil <- function(file_content){
  ceil_expr <- strsplit(file_content,"ceil")[[1]]
  new_ceil_expr <- ""
  for (i in 2:length(ceil_expr)) {
    new_ceil_expr <-  paste(new_ceil_expr, "ceiling", ceil_expr[i], sep="")
    print(new_ceil_expr)
  }
  file_content <- paste(strsplit(file_content,"ceil")[[1]][1], new_ceil_expr)
  file_content

}

#' Title
#'
#' @param model A libSBML model.
#' @param path_to_output A string (location of odin model file to be created).
#'
#' @importFrom libSBML Model_getNumSpecies Species_getId Species_getInitialAmount Species_getInitialConcentration Model_getNumRules Reaction_getNumProducts Rule_getId Model_getRule Model_getNumReactions Reaction_getNumProducts Model_getNumParameters Parameter_getConstant Model_getParameter Parameter_getId Parameter_getValue Model_getNumCompartments Model_getCompartment Compartment_getId Compartment_getSize
#'
#' @return
#' @export
#'
#' @examples
#' SBMLtoOdin("/usr/models/my_SBML_model.xml","/usr/models/my_odin_model.R")
SBML_to_odin <- function(model, path_to_output){
  ### test by using
  # library(devtools)
  # devtools::document()
  # load_all()
  # importSBMLfromFile("../testmodel_00001-sbml-l3v1.xml","../testmodel_output.R")
  # or
  # importSBMLfromBioModels("MODEL2210070001","../testmodel_output.R")
  # devtools::document()
  # devtools::test()

  # does not work anymore:
  # SBMLtoOdin::SBML_to_odin("../testmodel_00001-sbml-l3v1.xml","../testmodel_output.R")
  # instead call either the importSBMLfromFile function or the importSBMLfromBioModels function

  #model = SBMLtoOdin::importSBML(path_to_input)

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
    if(is.element(libSBML::Rule_getId(libSBML::Model_getRule(model,i-1)),names(dic_react))){
      dic_react <- SBMLtoOdin::getSpeciesRule(model, i, dic_react)
    }
    else{
      file_str <- SBMLtoOdin::getRule(file_str, model, i)
    }
  }

  # collect reactions
  print("Fetching Reactions")
  param_lib <- c()
  reserved_names_lib <- c()
  reserved_names_lib[c("i", "j", "k", "l", "i5", "i6", "i7", "i8")] <- c("i_", "j_", "k_", "l_", "i5_", "i6_", "i7_", "i8_")
  for (i in seq_len(libSBML::Model_getNumReactions(model))){
    for (j in seq_len(libSBML::Reaction_getNumProducts(libSBML::Model_getReaction(model, i-1)))) {
      id_prod = libSBML::Species_getSpeciesType(libSBML::Reaction_getProduct(libSBML::Model_getReaction(model, i-1),j-1))

      dic_react[id_prod] <- paste(dic_react[id_prod],  " + ", SBMLtoOdin::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")

    }
    for (j in seq_len(libSBML::Reaction_getNumReactants(libSBML::Model_getReaction(model, i-1)))) {
      id_reac = libSBML::Species_getSpeciesType(libSBML::Reaction_getReactant(libSBML::Model_getReaction(model, i-1),j-1))

      dic_react[id_reac] <- paste(dic_react[id_reac], " - ", SBMLtoOdin::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")

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
  # add parameter values
  print("Fetching Parameter Values")
  for (i in seq_len(libSBML::Model_getNumParameters(model))) {
    if(libSBML::Parameter_getConstant(libSBML::Model_getParameter(model, i-1))){
      param_id <- libSBML::Parameter_getId(libSBML::Model_getParameter(model, i-1))
      param_id <- SBMLtoOdin::in_reserved_lib(param_id, reserved_names_lib)
      file_str <- paste(file_str, paste(param_id, " <- user(", libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1)) , ")", sep = ""), sep = "\n")
    }
  }
  # add compartments
  #print(file_str)
  print("Fetching Compartments")
  for (i in seq_len(libSBML::Model_getNumCompartments(model))) {
    comp = (libSBML::Model_getCompartment(model,i-1))
    file_str <- paste(file_str, paste(libSBML::Compartment_getId(comp), " <- ", libSBML::Compartment_getSize(comp), sep = ""), sep = "\n")
  }
  # Call function that replaces piecewise() by a differentiable approximation using tanh(20*x)
  #print(file_str)
  if(grepl("piecewise",file_str)){
    file_str <- SBMLtoOdin::translate_piecewise(file_str)
  }
  #substitute all mentions of time by t
  file_str <- gsub("time", "t", file_str)
  for (reserved_param in names(reserved_names_lib)) {
    file_str <- gsub(paste(" ", reserved_param, " ", sep = ""), paste(" ", reserved_names_lib[reserved_param], " ", sep = ""), file_str)
  }
  # substitute factorial by gamma function
  if(grepl("factorial",file_str)){
    file_str <- SBMLtoOdin::sub_factorial(file_str)
  }
  # substitute ceil by ceiling
  if(grepl("ceil",file_str)){
    file_str <- SBMLtoOdin::sub_ceil(file_str)
  }
  # write information into odin.dust file
  writeLines(file_str, path_to_output,sep = "")
}

#' Title
#'
#' @param path_to_input A string (Path to an SBML model file).
#' @param path_to_output A string (Path to the output file).
#'
#' @importFrom libSBML readSBMLFromFile SBMLDocument_getModel
#' @return
#' @export
#'
#' @examples
#' importSBMLfromFile("/usr/model_files/my_model.xml")
importSBMLfromFile <- function(path_to_input, path_to_output = "odinModel.R"){
  doc = libSBML::readSBMLFromFile(path_to_input)
  model = libSBML::SBMLDocument_getModel(doc)
  SBMLtoOdin::SBML_to_odin(model,path_to_output)
}

#' Title
#'
#' @param model_id A string (a valid BioModels id)
#' @param path_to_output A string (Path to the output file).
#'
#' @importFrom libSBML readSBMLFromString SBMLDocument_getModel
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @return
#' @export
#'
#' @examples
importSBMLfromBioModels <- function(model_id, path_to_output = "odinModel.R"){
  ### still need to add try.. catch stuff and error messages. If the model does not exist etc.
  res1 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/files/", model_id, sep = ""))
  data = jsonlite::fromJSON(rawToChar(res1$content))
  filename = data$main[,"name"]
  filename = gsub(" ", "%20", filename)
  res2 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/download/", model_id, "?filename=", filename, sep = ""))
  doc = libSBML::readSBMLFromString(rawToChar(res2$content))
  model = libSBML::SBMLDocument_getModel(doc)
  SBMLtoOdin::SBML_to_odin(model,path_to_output)
}
