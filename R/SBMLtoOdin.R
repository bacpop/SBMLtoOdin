### usage:
# install libSBML package by (source: libSBML documentation)
# download R bindings from https://github.com/sbmlteam/libsbml/releases/v5.20.2
# Using the R GUI
#From the R menu Packages or Packages & Data (depending on your operating system), select the Package Installer item. You will be presented with an interface that lets you install a local binary package. Use that interface to navigate to the directory where you copied the libSBML archive file, and select the archive file for installation.
# more info and other options: https://sbml.org/software/libsbml/libsbml-docs/installation/

# library(devtools)
# devtools::document()
# load_all()
# importSBMLfromFile("../testmodel_00001-sbml-l3v1.xml","../testmodel_output.R")
# or
# importSBMLfromBioModels("MODEL2210070001","../testmodel_output.R")
# devtools::document()
# devtools::test()


#' Title
#'
#' @param file_content A string.
#' @param m A libSBML model object.
#' @param i An integer.
#' @param var_params_dict_used dictionary that saves param usage
#' @param var_params_dict dictionary that has parameter (initial) values
#'
#' @importFrom libSBML Rule_getType Model_getRule Rule_getId formulaToString Rule_getMath
#'
#' @return A string (updated file odin file content)
#'
#' @examples
#' getRule("initial(S1) <- S1_init", model, 1)
getRule <- function(file_content, m, i, var_params_dict_used, var_params_dict, reserved_names_lib){
  rule_type <- libSBML::Rule_getType(libSBML::Model_getRule(m,i-1))
  if(rule_type == "RULE_TYPE_SCALAR"){
    param_id <- SBMLtoOdin:::in_reserved_lib(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), reserved_names_lib)
    #print(paste(c(param_id, libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "))
    file_content <- paste(file_content, paste(c(param_id, libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
    var_params_dict_used[param_id] <- TRUE
  }
  else if(rule_type == "RULE_TYPE_RATE"){
    if(libSBML::Rule_isParameter(libSBML::Model_getRule(m,i-1))){
      # this is a rule for a parameter, not for a species
      param_id <- SBMLtoOdin:::in_reserved_lib(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), reserved_names_lib)
      file_content <- paste(file_content, paste(c(paste("deriv(",param_id,")",sep = ""), libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
      file_content <- paste(file_content, "\n", "initial(", param_id, ") <- ", var_params_dict[param_id], sep = "")
      #file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), "1 + 0.5 * t"), collapse = " <- "), sep = "\n")
      var_params_dict_used[param_id] <- TRUE
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
  return(list(file_content, var_params_dict_used))
}


#' Title
#'
#' @param m A libSBML model object.
#' @param i An integer for the index of the rule.
#' @param species_dic The dictionary containing all species.
#' @param func_def_dict The dictionary containing all function definitions.
#'
#' @importFrom libSBML formulaToString Model_getRule Rule_getMath Rule_getId
#'
#' @return species dictionary.
#'
#' @examples
#' getSpeciesRule(model,1,dictionary)
getSpeciesRule <- function(m, i, species_dic, func_def_dict){
  rhs <- libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))
  #if(grepl("leq",rhs)){
  #  rhs <- SBMLtoOdin:::sub_leq(rhs)
  #  print(rhs)
  #}
  #rhs <- gsub("time", "t", rhs)
  if(libSBML::Rule_getType(libSBML::Model_getRule(m,i-1)) == "RULE_TYPE_SCALAR"){
    # need to calculate derivative
    # if this is a custom function, I need to first determine the real rhs (from the function call)
    for (cust_func in func_def_dict) {
      if(grepl(cust_func, rhs)){
        rhs <- SBMLtoOdin:::getFunctionOutputForRules(m, rhs, cust_func)
      }
    }
    #if(is.element(strsplit(rhs,"\\(")[[1]][1],func_def_dict)){
    #  rhs <- SBMLtoOdin:::getFunctionOutputForRules(m, rhs, strsplit(rhs,"\\(")[[1]][1])

      if(grepl("piecewise",rhs)){
        rhs <- SBMLtoOdin:::translate_piecewise(rhs)
      }
      if(grepl("pow\\(",rhs)){
        rhs <- SBMLtoOdin:::translate_pow(rhs)
      }
      if(grepl("root\\(",rhs)){
        rhs <- SBMLtoOdin:::translate_root(rhs)
      }

    #else{
    #  if(grepl("piecewise",rhs)){
    #    rhs <- SBMLtoOdin:::translate_piecewise(rhs)
    #  }
    #  if(grepl("pow\\(",rhs)){
    #    rhs <- translate_pow(rhs)
    #  }
    #  if(grepl("root\\(",rhs)){
    #    rhs <- translate_root(rhs)
    #  }
      deriv_of_rule <- D(parse(text = rhs), libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)))
      species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))] <- paste(species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))], deriv_of_rule ,sep = " + ")
  }
  else{
    species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))] <- paste(species_dic[libSBML::Rule_getId(libSBML::Model_getRule(m,i-1))], rhs ,sep = " + ")
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
#' @return function output as string
#'
#' @examples
#' getFunctionOutput(model, index, reaction)
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
#' @param m A libSBML model object.
#' @param ind A string.
#' @param r A string.
#'
#' @importFrom libSBML Reaction_isSetKineticLaw Reaction_getKineticLaw KineticLaw_isSetMath KineticLaw_getMath Model_getNumFunctionDefinitions FunctionDefinition_getId Model_getFunctionDefinition FunctionDefinition_getBody FunctionDefinition_getArgument formulaToString
#'
#' @return function output of rule as string
#'
#' @examples
#' getFunctionOutputForRules(model, formula, index)
getFunctionOutputForRules <- function(m, formula, func_id){
  #if (libSBML::Reaction_isSetKineticLaw(r)) {
  #  k = libSBML::Reaction_getKineticLaw(r);
  #  if (libSBML::KineticLaw_isSetMath(k)) {
  #    formula = libSBML::formulaToString(libSBML::KineticLaw_getMath(k));
  #  }
    for (n in seq_len(libSBML::Model_getNumFunctionDefinitions(m))){
      #find the correct function definition
      #if(grepl(libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1)), func_id)){
      if((libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1)) == func_id)){
        #func_id <- libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1))
        function_def <- libSBML::formulaToString(libSBML::FunctionDefinition_getBody(libSBML::Model_getFunctionDefinition(m,n-1)))

        func_args_dict <- rep(NA, libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))
        args_call <- strsplit(formula,"\\(|\\)")[[1]][2]
        #print(formula)
        #print(args_call)
        if(args_call != ""){
          func_args_dict <- stringr::str_split_fixed(args_call,",",libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))[1,]
          for (i in seq_len(libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))) {
            names(func_args_dict)[i] <- libSBML::formulaToString(libSBML::FunctionDefinition_getArgument(libSBML::Model_getFunctionDefinition(m,n-1),i-1))
          }

          #formula0 <- strsplit(formula, func_id)[[1]][length(strsplit(formula, func_id)[[1]])]
          #formula1 <- strsplit(formula0,"\\(|\\)")[[1]][2]
          #function_call_vars <- stringr::str_trim(strsplit(formula1, ",")[[1]])
          for (i in 1:length(func_args_dict)) {
            #function_def <- gsub(names(func_args_dict)[i], func_args_dict[i], function_def)
            function_def <- gsub(paste("\\b",names(func_args_dict)[i], "\\b", sep = ''), paste(" ", func_args_dict[i], " ", sep = ""), function_def, perl = TRUE)
          }
          #formula2 <- paste(func_id,"\\(",formula1,"\\)", sep="")
          #formula <- gsub(formula2, function_def, formula)
          formula <- gsub(paste("\\Q",func_id,"(",args_call,")\\E",sep = ""), paste("(",function_def,")",sep=""), formula)
          #print(formula)
          }
        else{
          formula <- function_def
          #print(formula)
        }
        }

    }
  #function_def
  formula
}

#' Title
#'
#' @param param_name A string.
#' @param reserved_lib A dictionary.
#'
#' @return A string.
#'
#' @examples
#' in_reserved_lib("a", c("a"="a1", "b"="b1"))
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
#'
#' @examples
#' AddToParamLib(reaction, param_dictionary, reserved_names_dictionary)
AddToParamLib <- function(r, local_param_lib,reserved_lib){
  if (libSBML::Reaction_isSetKineticLaw(r)) {
    k = libSBML::Reaction_getKineticLaw(r);
    if(libSBML::KineticLaw_getNumParameters(k) > 0){
      for (i in seq_len(libSBML::KineticLaw_getNumParameters(k))) {
        id <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(k,i-1))
        id <- SBMLtoOdin:::in_reserved_lib(id, reserved_lib)
        value <- libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(k,i-1))
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
#'
#' @examples
#' getFunctionParams(file_content, reaction, param_dictionary, reserved_names_dictionary)
getFunctionParams <- function(file_content, r, param_lib, reserved_lib){
  if (libSBML::Reaction_isSetKineticLaw(r)) {
    k = libSBML::Reaction_getKineticLaw(r);
    if(libSBML::KineticLaw_getNumParameters(k) > 0){
      for (i in seq_len(libSBML::KineticLaw_getNumParameters(k))) {
        id <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(k,i-1))
        id <- SBMLtoOdin:::in_reserved_lib(id, reserved_lib)
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
#' @return file content as string
#'
#' @examples
#' translate_pow("pow(2,5)")
translate_pow <- function(file_content){
  while(grepl("pow\\(",file_content)){
    pow_expr <- regmatches(file_content, gregexpr("pow(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]] # find power expression
    #print(pow_expr)
    for (i in 1:length(pow_expr)) {
    pow_expr0 <- stringi::stri_split_fixed(str = pow_expr[i], pattern = "pow(", n = 2)[[1]][2] # cut of "pow(" at beginning
    pow_expr1 <- strsplit(pow_expr0, ",\\s*(?=[^,]+$)", perl=TRUE) # split expression into base and exponent
    #pow_expr1 <- stringi::stri_split_fixed(str = pow_expr0, pattern = ")", n = 2)[[1]][1]

    pow_base <- pow_expr1[[1]][1] # assign base
    pow_exponent <- strsplit(pow_expr1[[1]][2],"\\)\\s*(?=[^)]*$)", perl=TRUE)[[1]][1] # assign exponent
    new_pow_expr <- paste("(",pow_base, ")^(", pow_exponent,")", sep="") # put expression together

    in_front_pow <- stringi::stri_split_fixed(str = file_content, pow_expr[i], n=2)[[1]][1] # find part that was in front of power expression
    pow_rest <- stringi::stri_split_fixed(str = file_content, pow_expr[i], n=2)[[1]][2]
    file_content <- paste(in_front_pow, new_pow_expr, pow_rest, sep = "") # put everything together
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
#' @return file content as string
#'
#' @examples
#' translate_root("root(2,4)")
translate_root <- function(file_content){
  while(grepl("root\\(",file_content)){
    root_expr <- regmatches(file_content, gregexpr("root(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]] # find root expression
    for (i in 1:length(root_expr)) {
      root_expr0 <- stringi::stri_split_fixed(str = root_expr[i], pattern = "root(", n = 2)[[1]][2] # cut of "root(" at beginning
      root_expr1 <- strsplit(root_expr0, ",\\s*(?=[^,]+$)", perl=TRUE) # split expression into base and exponent
      #root_expr1 <- stringi::stri_split_fixed(str = root_expr0, pattern = ")", n = 2)[[1]][1]

      root_base <- root_expr1[[1]][1] # assign base
      root_exponent <- strsplit(root_expr1[[1]][2],"\\)\\s*(?=[^)]*$)", perl=TRUE)[[1]][1] # assign exponent
      new_root_expr <- paste("(",root_exponent, ")^(1/(", root_base,"))", sep="") # put expression together

      in_front_root <- stringi::stri_split_fixed(str = file_content, root_expr[i], n=2)[[1]][1] # find part that was in front of root expression
      root_rest <- stringi::stri_split_fixed(str = file_content, root_expr[i], n=2)[[1]][2]
      file_content <- paste(stringi::stri_split_fixed(str = file_content, pattern = "root", n = 2)[[1]][1], new_root_expr, root_rest, sep = "") # put everything together
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
#' @return file content as string
#'
#' @examples
#' translate_piecewise("piecewise(1.55, lt(C, 3100), 0.2)")
translate_piecewise <- function(file_content){
  while(grepl("piecewise\\(",file_content)){
    piece_expr_all <- regmatches(file_content, gregexpr("piecewise(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1] # find piecewise expression
    #print(piece_expr_all)
    #piece_expr <- strsplit(file_content,"piecewise")[[1]]
  # (\\(([^()]|(?1))*\\))
  piece_expr <- strsplit(piece_expr_all,"piecewise\\(")[[1]]
  piece_expr[2] <- gsub('.{1}$', '', piece_expr[2]) # remove closing bracket at the end
  piece_expr_all_new <- piece_expr_all
  i_test <- 0
  while(length(piece_expr) > 2 && i_test<10){
    i_test <- i_test +1
    #print(length(piece_expr))
    #print(piece_expr[length(piece_expr)])
    last_piece <- translate_piecewise(paste("piecewise(", piece_expr[length(piece_expr)], sep = ""))
    #print(last_piece)
    piece_expr_all_new <- gsub(paste("piecewise(",piece_expr[length(piece_expr)],sep = ""), last_piece, piece_expr_all_new, fixed = TRUE)
    #print(piece_expr_all_new)
    piece_expr <- strsplit(piece_expr_all_new,"piecewise\\(")[[1]]
  }
  new_piece_expr <- ""
    while(grepl("geq",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_geq(piece_expr[2])
    }
    while(grepl("leq",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_leq(piece_expr[2])
    }
    while(grepl("lt",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_lt(piece_expr[2])
    }
    while(grepl("gt",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_gt(piece_expr[2])
    }
    while(grepl("neq",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_neq_for_comp(piece_expr[2])
    }
  #print(piece_expr[2])
    while(grepl("eq",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_eq_for_comp(piece_expr[2])
    }
    while(grepl("and",piece_expr[2])){
     piece_expr[2] <- SBMLtoOdin:::sub_and(piece_expr[2])
   }
    # to do: add neq
    #print(piece_expr[2])
    #piece_expr0 <- strsplit(x = piece_expr[2], split = ",")[[1]]
    #piece_expr0 <- strsplit(piece_expr[2], ",(?![^(]*\\))", perl = TRUE)[[1]]
    piece_expr0 <- strsplit(piece_expr[2], ",(?![^(]*\\))", perl = TRUE)[[1]]
    ### this is not working
    # missing exceptions for ")" towards end of piece_expr
    #print(piece_expr0)
    #if(length(piece_expr0)){
    #  if_part <- paste(piece_expr0[2:(length(piece_expr0)-1)], sep = "")
    #  print(if_part)
    #}
    if_part <- piece_expr0[2]
    #print(if_part)
    new_piece_expr <- paste("(",piece_expr[1], "if(", if_part, ") ", piece_expr0[1], " else ", piece_expr0[length(piece_expr0)], ")" ,sep="" )
    #print(piece_expr_all)
    #print(new_piece_expr)
  file_content <- gsub(piece_expr_all, new_piece_expr, file_content, fixed = TRUE)
  }
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @importFrom stringi stri_split_fixed
#'
#' @return A string.
#'
#' @examples
#' sub_factorial("a + factorial(4)")
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
#'
#' @examples
#' sub_ceil("ceil(1.2)")
sub_ceil <- function(file_content){
  ceil_expr <- strsplit(file_content,"ceil")[[1]]
  new_ceil_expr <- ""
  for (i in 2:length(ceil_expr)) {
    new_ceil_expr <-  paste(new_ceil_expr, "ceiling", ceil_expr[i], sep="")
    #print(new_ceil_expr)
  }
  file_content <- paste(strsplit(file_content,"ceil")[[1]][1], new_ceil_expr)
  file_content

}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content
#'
#' @examples
#' sub_and("if(and(b,c))")
sub_and <- function(file_content){
  and_expr <- regmatches(file_content, gregexpr("and(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  #print(and_expr)
  and_expr1 <- strsplit(and_expr, ",")[[1]]
  and_expr1[1] <- gsub("and(", "(", and_expr1[1], fixed = TRUE)
  new_and <- paste(and_expr1[1], and_expr1[2], sep = " && ")
  #print(new_and)
  file_content <- gsub(and_expr, new_and, file_content, fixed = TRUE)
  file_content
}


#' Title
#'
#' @param file_content A string.
#'
#' @return file content
#'
#' @examples
#' sub_leq("if(leq(a,2))")
sub_leq <- function(file_content){
  leq_expr0 <- regmatches(file_content, gregexpr("leq(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  leq_expr <- strsplit(leq_expr0,"leq\\(")[[1]]
  leq_expr1 <- stringi::stri_split_fixed(str = leq_expr[2], pattern = ")", n = 2)[[1]]
  leq_expr2 <- strsplit(leq_expr1[1],",")[[1]]
  leq_expr3 <- leq_expr2[1]
  leq_expr4 <- leq_expr2[2]
  new_str <- paste(leq_expr3, " <= ", leq_expr4,leq_expr1[2], sep = "")
  #file_content <- paste(leq_expr[1],new_str,sep = "")
  file_content <- gsub(leq_expr0, new_str, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content
#'
#' @examples
#' sub_neq_for_comp("if(neq(a,2))")
sub_neq_for_comp <- function(file_content){
  leq_expr0 <- regmatches(file_content, gregexpr("neq(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  leq_expr <- strsplit(leq_expr0,"neq\\(")[[1]]
  leq_expr1 <- stringi::stri_split_fixed(str = leq_expr[2], pattern = ")", n = 2)[[1]]
  leq_expr2 <- strsplit(leq_expr1[1],",")[[1]]
  leq_expr3 <- leq_expr2[1]
  leq_expr4 <- leq_expr2[2]
  new_str <- paste(leq_expr3, " != ", leq_expr4,leq_expr1[2], sep = "")
  #file_content <- paste(leq_expr[1],new_str,sep = "")
  file_content <- gsub(leq_expr0, new_str, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content
#'
#' @examples
#' sub_eq_for_comp("if(eq(a,2))")
sub_eq_for_comp <- function(file_content){
  leq_expr0 <- regmatches(file_content, gregexpr("eq(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  leq_expr <- strsplit(leq_expr0,"eq\\(")[[1]]
  #leq_expr <- strsplit(file_content,"eq\\(")[[1]]
  leq_expr1 <- stringi::stri_split_fixed(str = leq_expr[2], pattern = ")", n = 2)[[1]]
  leq_expr2 <- strsplit(leq_expr1[1],",")[[1]]
  leq_expr3 <- leq_expr2[1]
  leq_expr4 <- leq_expr2[2]
  new_str <- paste(leq_expr3, " == ", leq_expr4,leq_expr1[2], sep = "")
  #file_content <- paste(leq_expr[1],new_str,sep = "")
  file_content <- gsub(leq_expr0, new_str, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#'
#' @importFrom stringr str_count
#'
#' @return file content A string.
#'
#' @examples
#' sub_lt("if(lt(a,2))")
sub_lt <- function(file_content){
  lt_expr0 <- regmatches(file_content, gregexpr("lt(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  lt_expr <- strsplit(lt_expr0,"lt\\(")[[1]]
  #lt_expr <- strsplit(file_content,"lt\\(")[[1]]
  lt_expr1 <- stringi::stri_split_fixed(str = lt_expr[2], pattern = ")", n = 2)[[1]]
  lt_expr2 <- strsplit(lt_expr1[1],",")[[1]]
  lt_expr3 <- lt_expr2[1]
  lt_expr4 <- lt_expr2[2]
  new_str <- paste(lt_expr3, " < ", lt_expr4,lt_expr1[2], sep = "")
  #file_content <- paste(lt_expr[1],new_str,sep = "")
  file_content <- gsub(lt_expr0, new_str, file_content, fixed = TRUE)
  file_content
}
# this does not work for all cases but I didn't manage to get the new version working yet
# example e.g. "x * lt(a,b) + lt(b,c) + 2"
#sub_lt <- function(file_content){
  #lt_expr <- strsplit(file_content,"lt\\(|\\)")[[1]]
#  lt_expr <- strsplit(file_content,"lt\\(")[[1]]
#  new_str <- lt_expr[1]
#  for (i in 1:stringr::str_count(file_content, pattern = "lt\\(")) {
#    lt_expr0 <- paste("(",lt_expr[2],sep = "")
#    strsplit(lt_expr0,"\\(|\\)")[[1]]
#    lt_expr1 <- strsplit(lt_expr[i+1],",")[[1]]
#    lt_expr2 <- lt_expr1[1]
#    lt_expr3 <- lt_expr1[2]
#    new_str <- paste(new_str, lt_expr2, " < ", lt_expr3, sep = "")
#  }
#  if(length(lt_expr) > (1 + stringr::str_count(file_content, pattern = "lt\\("))){
#    new_str <- paste(new_str, lt_expr[2 + stringr::str_count(file_content, pattern = "lt\\(")], sep = "")
#  }
  #lt_expr1 <- strsplit(lt_expr[2],",")[[1]]
  #stringi::stri_split_fixed(file_content, "lt(", n = 2)[[1]]
  #lt_expr0 <- paste("(",lt_expr[2],sep = "")
  #lt_expr1 <- strsplit(lt_expr0,"\\(|\\)")[[1]][2]
  #lt_expr1 <- stringi::stri_split_fixed(str = lt_expr[2], pattern = ")", n = 2)[[1]]
  #lt_expr2 <- strsplit(lt_expr1[1],",")[[1]]
  #lt_expr3 <- lt_expr2[1]
  #lt_expr4 <- lt_expr2[2]
  #new_str <- paste(lt_expr3, " < ", lt_expr4,lt_expr1[2], sep = "")
  #file_content <- paste(lt_expr[1],new_str,sep = "")
#  file_content <- new_str
#  file_content
#}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content A string.
#'
#' @examples
#' sub_gt("if(gt(a,2))")
sub_gt <- function(file_content){
  gt_expr0 <- regmatches(file_content, gregexpr("gt(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  gt_expr <- strsplit(gt_expr0,"gt\\(")[[1]]
  #gt_expr <- strsplit(file_content,"gt\\(")[[1]]
  gt_expr1 <- stringi::stri_split_fixed(str = gt_expr[2], pattern = ")", n = 2)[[1]]
  gt_expr2 <- strsplit(gt_expr1[1],",")[[1]]
  gt_expr3 <- gt_expr2[1]
  gt_expr4 <- gt_expr2[2]
  new_str <- paste(gt_expr3, " > ", gt_expr4,gt_expr1[2], sep = "")
  #file_content <- paste(gt_expr[1],new_str,sep = "")
  file_content <- gsub(gt_expr0, new_str, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content A string.
#'
#'
#' @examples
#' sub_geq("if(geq(a,2))")
sub_geq <- function(file_content){
  gt_expr0 <- regmatches(file_content, gregexpr("geq(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  gt_expr <- strsplit(gt_expr0,"geq\\(")[[1]]
  #gt_expr <- strsplit(file_content,"geq\\(")[[1]]
  gt_expr1 <- stringi::stri_split_fixed(str = gt_expr[2], pattern = ")", n = 2)[[1]]
  gt_expr2 <- strsplit(gt_expr1[1],",")[[1]]
  gt_expr3 <- gt_expr2[1]
  gt_expr4 <- gt_expr2[2]
  new_str <- paste(gt_expr3, " >= ", gt_expr4,gt_expr1[2], sep = "")
  #file_content <- paste(gt_expr[1],new_str,sep = "")
  file_content <- gsub(gt_expr0, new_str, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param model A libSBML model.
#' @param path_to_output A string (location of odin model file to be created).
#'
#' @importFrom libSBML Model_getNumSpecies Species_getId Species_getInitialAmount Species_getInitialConcentration Model_getNumRules Reaction_getNumProducts Rule_getId Model_getRule Model_getNumReactions Reaction_getNumProducts Model_getNumParameters Parameter_getConstant Model_getParameter Parameter_getId Parameter_getValue Model_getNumCompartments Model_getCompartment Compartment_getId Compartment_getSize KineticLaw_getFormula Reaction_getKineticLaw
#'
#' @return no return
#'
#'
#' @examples
#' SBMLtoOdin("/usr/models/my_SBML_model.xml","/usr/models/my_odin_model.R")
SBML_to_odin <- function(model, path_to_output, input_str){
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
  # SBMLtoOdin:::SBML_to_odin("../testmodel_00001-sbml-l3v1.xml","../testmodel_output.R")
  # instead call either the importSBMLfromFile function or the importSBMLfromBioModels function

  #model = SBMLtoOdin:::importSBML(path_to_input)
  reserved_names_lib <- c()
  reserved_names_lib[c("i", "j", "k", "l", "i5", "i6", "i7", "i8", "t", "auto", "break", "case", "char", "const", "continue", "default",
                       "do", "double", "enum", "extern", "float", "for", "goto",
                       "inline", "int", "long", "register", "restrict", "return",
                       "short", "signed", "sizeof", "static", "struct", "switch", "typedef",
                       "union", "unsigned", "void", "volatile", "while")] <- c("ixi", "jxj", "kxk", "lxl", "i5xi5", "i6xi6", "i7xi7", "i8xi8", "txt", "auto1", "break1", "case1", "char1", "const1", "continue1", "default1",
                                                                               "do1", "double1", "enum1", "extern1", "float1", "for1", "goto1",
                                                                               "inline1", "int1", "long1", "register1", "restrict1", "return1",
                                                                               "short1", "signed1", "sizeof1", "static1", "struct1", "switch1", "typedef1",
                                                                               "union1", "unsigned1", "void1", "volatile1", "while1")

  # find out how time is defined in the model (if that is defined, I think it's just a feature of SBML3)
  #input_str
  if(grepl("definitionURL=\"http://www.sbml.org/sbml/symbols/time\">",input_str, fixed = TRUE)){
    time_def_1 <- strsplit(input_str,"definitionURL=\"http://www.sbml.org/sbml/symbols/time\"> ", fixed = TRUE)[[1]][2]
    time_def <- strsplit(time_def_1," </csymbol>", fixed = TRUE)[[1]][1]

    reserved_names_lib[time_def] <- "t" # if time is defined as "t" there is no translation
    # if it defined as something else, "t" will be translated to txt, and whatever time is defined as will be translated to "t"
    #print(reserved_names_lib)
  }

  file_str <-""

  # add parameter values
  # dict for non-constant params
  # var_params_dict <- c()
  # var_params_dict_used <- c() # stores whether value has been used somewhere (usually events or rules), otherwise prints it out in the end
  # print("Fetching Parameter Values")
  # for (i in seq_len(libSBML::Model_getNumParameters(model))) {
  #   if(libSBML::Parameter_getConstant(libSBML::Model_getParameter(model, i-1))){
  #     param_id <- libSBML::Parameter_getId(libSBML::Model_getParameter(model, i-1))
  #     param_id <- SBMLtoOdin:::in_reserved_lib(param_id, reserved_names_lib)
  #     if(grepl("_init",param_id)){
  #       param_id <- paste(param_id, "1",sep = "")
  #     }
  #     file_str <- paste(file_str, paste(param_id, " <- user(", libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1)) , ")", sep = ""), sep = "\n")
  #   }
  #   else{
  #     param_id <- libSBML::Parameter_getId(libSBML::Model_getParameter(model, i-1))
  #     #print("down")
  #     #print(param_id)
  #     param_id <- SBMLtoOdin:::in_reserved_lib(param_id, reserved_names_lib)
  #     #print(param_id)
  #     param_val <- libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1))
  #     if(grepl("_init",param_id)){
  #       param_id <- paste(param_id, "1",sep = "")
  #     }
  #     var_params_dict[param_id] <- param_val
  #     var_params_dict_used[param_id] <- FALSE
  #   }
  # }

  dic_react <- c()
  bad_names <- c()
  boundCond <- c()
  # add initial values for species
  print("Fetching Species")
  species_list <- list()
  boundary_cond_list <- list()
  boundary_cond_rule_list <- list()
  for (i in seq_len(libSBML::Model_getNumSpecies(model))) {
    species <- libSBML::Model_getSpecies(model, i-1)
    # find initial concentration
    spec_conc <- NA
    conc_found <- FALSE
    if(!is.na(libSBML::Species_getInitialAmount(species))){
      #print(libSBML::Species_getId(species))
      #print(libSBML::Species_getInitialAmount(species))
      spec_conc <- libSBML::Species_getInitialAmount(species)
      conc_found <- TRUE
    }
    if(!is.na(libSBML::Species_getInitialConcentration(species))){
      if(!conc_found){
        spec_conc = libSBML::Species_getInitialConcentration(species)
        conc_found <- TRUE
      }
      else{ # also found a non-NA value in InitialAmount. Take the one that is non-zero for now
        if(spec_conc == 0){
          spec_conc = libSBML::Species_getInitialConcentration(species)
          conc_found <- TRUE
        }
      }
    }
    #print(libSBML::Species_getInitialConcentration(species))
    is_mod <- FALSE
    #print(libSBML::Species_getId(species))
    # hmm, modifier species are species. But I need to test whether it is a modifier.
    # not sure that there is a function
    # I could iterate separately of modifiers and mark them here in the list
    #print(libSBML::Species_getConstant(species))
    species_list[[libSBML::Species_getId(species)]] <- list(
      name = libSBML::Species_getId(species),
      compartment = libSBML::Species_getCompartment(species),
      initialAmount = spec_conc,
      initial_found = conc_found,
      is_modifier = is_mod,
      is_constant = libSBML::Species_getConstant(species)
    )
    #print(libSBML::Species_getBoundaryCondition(species))
    boundary_cond_list[[libSBML::Species_getId(species)]] <- libSBML::Species_getBoundaryCondition(species)
    if(libSBML::Species_getBoundaryCondition(species)){
      boundary_cond_rule_list[[libSBML::Species_getId(species)]] <- list(hasRule = FALSE, theRule = NA, hasRateRule = FALSE, theRateRule = NA)
    }
  }
  #print(species_list)

  #print(names(species_list))
  # find all initial species assignments that I might have missed
  if(libSBML::Model_getNumInitialAssignments(model)>0){
        for (j in seq_len(libSBML::Model_getNumInitialAssignments(model))) {
          init_assign <- libSBML::Model_getInitialAssignment(model,j-1)
          var_id <- libSBML::InitialAssignment_getSymbol(init_assign)
          conc = libSBML::formulaToString(libSBML::InitialAssignment_getMath(init_assign))
          if(grepl("_init",conc)){
            conc <- paste(conc, "1",sep="")
          }
          if(is.element(var_id, names(species_list))){
            #print(conc)
            species_list[[var_id]]$initialAmount = conc
            species_list[[var_id]]$initial_found  <- TRUE
          }
        }
  }
  #print(names(species_list))

  # for (i in seq_len(libSBML::Model_getNumSpecies(model))) {
  #   species = libSBML::Model_getSpecies(model, i-1)
  #   id = libSBML::Species_getId(species)
  #   boundCond[id] = libSBML::Species_getBoundaryCondition(species)
  #   #print("species id")
  #   #print(id)
  #   if(grepl("^\\_[0-9+]",id, perl = TRUE)){
  #     bad_names[id] <- paste("p", id, sep = "")
  #   }
  #   else if(grepl("^[0-9+]",id, perl = TRUE)){
  #     bad_names[id] <- paste("p", id, sep = "")
  #   }
  #   dic_react[id] <- 0
  #   found_initial <- FALSE
  #   conc <- ""
  #   if(!is.na(libSBML::Species_getInitialAmount(species))){
  #     conc = libSBML::Species_getInitialAmount(species)
  #     conc = paste("user(",conc, ")",sep = "")
  #     found_initial <- TRUE
  #   }
  #   else if(!is.na(libSBML::Species_getInitialConcentration(species))){
  #     conc = libSBML::Species_getInitialConcentration(species)
  #     conc = paste("user(",conc, ")",sep = "")
  #     found_initial <- TRUE
  #   }
  #   if(libSBML::Model_getNumRules(model)>0){
  #     for (j in 1:libSBML::Model_getNumRules(model)) {
  #       if(libSBML::Rule_getVariable(libSBML::Model_getRule(model,j-1)) == id){
  #         if(libSBML::Rule_getType(libSBML::Model_getRule(model,j-1)) == "RULE_TYPE_SCALAR"){
  #           conc = libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(model,j-1)))
  #           if(grepl("_init",conc)){
  #             conc <- paste(conc, "1",sep="")
  #           }
  #           found_initial <- TRUE
  #         }
  #       }
  #     }
  #   }
  #   if(libSBML::Model_getNumInitialAssignments(model)>0){
  #     for (j in 1:libSBML::Model_getNumInitialAssignments(model)) {
  #       if(libSBML::InitialAssignment_getSymbol(libSBML::Model_getInitialAssignment(model,j-1)) == id){
  #         conc = libSBML::formulaToString(libSBML::InitialAssignment_getMath(libSBML::Model_getInitialAssignment(model,j-1)))
  #         if(grepl("_init",conc)){
  #           conc <- paste(conc, "1",sep="")
  #         }
  #         found_initial <- TRUE
  #       }
  #     }
  #   }
  #   if(!found_initial){
  #     print(paste("Warning: Initial amount and concentration not defined for ", as.character(id)))
  #   }
  #   id_tr <- SBMLtoOdin:::in_reserved_lib(id, reserved_names_lib)
  #   file_str <- paste(file_str, paste("initial(",id_tr,") <- ", id_tr, "_init",sep = ""), paste(id_tr, "_init <- ", conc, sep = ""), sep = "\n")
  # }

  # build library of function definitions
  func_def_dict <- c()
  for (i in seq_len(libSBML::Model_getNumFunctionDefinitions(model))) {
    func_def_dict[i] <- libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(model,i-1))
  }
  #print(var_params_dict_used)
  # parameters
  parameter_list <- list()
  parameter_used_list <- list()
  for (i in seq_len(libSBML::Model_getNumParameters(model))) {
    parameter <- libSBML::Model_getParameter(model, i-1)
    param_id_orig <- libSBML::Parameter_getId(parameter)
    param_id <- SBMLtoOdin:::in_reserved_lib(param_id_orig, reserved_names_lib)
    if(grepl("_init",param_id)){
      param_id <- paste(param_id, "1",sep = "")
    }
    if(libSBML::Parameter_getConstant(parameter)){
      parameter_list[[param_id]] <- list(
        value = libSBML::Parameter_getValue(parameter),
        const = TRUE,
        math = NA,
        has_init = FALSE,
        orig_name = param_id_orig)
      parameter_used_list[[param_id]] <- FALSE
    }
    else{
      #print(libSBML::Parameter_getValue(parameter))
      parameter_list[[param_id]] <- list(
        value = libSBML::Parameter_getValue(parameter),
        const = FALSE,
        math = NA,
        has_init = FALSE,
        orig_name = param_id_orig)
      parameter_used_list[[param_id]] <- FALSE
    }

    #add option for non constant parameter

    # if(libSBML::Parameter_getConstant(parameter)){
    #   parameter_list[[param_id]] <- libSBML::Parameter_getValue(parameter)
    #   parameter_used_list[[param_id]] <- FALSE
    # }
    # else{
    #   param_val <- libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1))
    # }
    #
  }

  # save rules
  print("Fetching Rules")
  rule_list <- list()
  #print(libSBML::Model_getNumRules(model))
  #print(names(parameter_list))
  if(libSBML::Model_getNumRules(model) > 0){
    for (i in 1:(libSBML::Model_getNumRules(model))) {
      rule <- libSBML::Model_getRule(model, i-1)
      if (libSBML::Rule_isRate(rule)) {
        param_id <- SBMLtoOdin:::in_reserved_lib(libSBML::Rule_getVariable(rule), reserved_names_lib)
        #print(param_id)
        if(libSBML::Rule_isParameter(rule) || is.element(param_id, parameter_list)){
          #print(param_id)
          # this is a rule for a parameter, not for a species

          #param_id <- SBMLtoOdin:::in_reserved_lib(libSBML::Rule_getId(rule), reserved_names_lib)

          #file_content <- paste(file_content, paste(c(paste("deriv(",param_id,")",sep = ""), libSBML::formulaToString(libSBML::Rule_getMath(libSBML::Model_getRule(m,i-1)))), collapse = " <- "), sep = "\n")
          #file_content <- paste(file_content, "\n", "initial(", param_id, ") <- ", var_params_dict[param_id], sep = "")
          #file_content <- paste(file_content, paste(c(libSBML::Rule_getId(libSBML::Model_getRule(m,i-1)), "1 + 0.5 * t"), collapse = " <- "), sep = "\n")
          #var_params_dict_used[param_id] <- TRUE
          math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
          #init <- parameter_list[[param_id]]
          parameter_list[[param_id]]$math <- math
          parameter_list[[param_id]]$has_init = TRUE
          #parameter_list[[param_id]]$value <- init
          #parameter_used_list[[param_id]] <- TRUE
        }
        #variable <- Rule_getVariable(rule)
        #math <- formulaToString(Rule_getMath(rule))
        #rule_list[[variable]] <- list(math = math,

        else if(boundary_cond_list[[param_id]]){ #if species is boundary condition, i.e. does not depend on reactions
          boundary_cond_rule_list[[param_id]]$hasRateRule <- TRUE
          math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
          boundary_cond_rule_list[[param_id]]$theRateRule <- math
          #print(math)
        }
        # cannot deal with other rate rules yet
      }
      else if(libSBML::Rule_isAssignment(rule)){
        variable <- Rule_getVariable(rule)
        # if it is a piecewise expression
        #print(libSBML::formulaToString(Rule_getMath(rule)))
        math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
        if (grepl("piecewise",math)) {
          math <- SBMLtoOdin:::translate_piecewise(math)
        }

        # If species has no initial amount but has an assignment rule, use the rule for initialization
        if (variable %in% names(species_list) && (is.null(species_list[[variable]]$initialAmount) || is.na(species_list[[variable]]$initialAmount))) {
        # actually, initialAssignment overrides initialAmount and initialConcentration if species is not constant
        #if (variable %in% names(species_list) && !species_list[[variable]]$is_constant){
          species_list[[variable]]$initialAmount <- math
        }
        else if(variable %in% names(parameter_list)){
          parameter_list[[variable]]$value = math
          #print(libSBML::Rule_getMath(rule))
        }
        if(is.element(variable, names(boundary_cond_list))){
          boundary_cond_rule_list[[variable]]$hasRule <- TRUE
          boundary_cond_rule_list[[variable]]$theRule <- math
        }
      }
      else if(libSBML::Rule_isScalar(rule)){
        var_id <- libSBML::Rule_getVariable(rule)
        conc = libSBML::formulaToString(libSBML::Rule_getMath(rule))
        if(grepl("_init",conc)){
                    conc <- paste(conc, "1",sep="")
        }
        #print(conc)
        species_list[[var_id]]$initialAmount = conc
        species_list[[var_id]]$initial_found  <- TRUE
      }
    }
  }
  # for (i in seq_len(libSBML::Model_getNumRules(model))) {
  #   if(is.element(libSBML::Rule_getId(libSBML::Model_getRule(model,i-1)),names(dic_react))){
  #     dic_react <- SBMLtoOdin:::getSpeciesRule(model, i, dic_react, func_def_dict)
  #   }
  #   else{
  #     return_list <- SBMLtoOdin:::getRule(file_str, model, i, var_params_dict_used, var_params_dict, reserved_names_lib)
  #     file_str <- return_list[[1]]
  #     var_params_dict_used <- return_list[[2]]
  #     # this does not seem right!
  #   }
  # }
  #print(var_params_dict_used)


  #for (i in seq_len(libSBML::Model_getNumParameters(model))) {
    #   if(libSBML::Parameter_getConstant(libSBML::Model_getParameter(model, i-1))){
    #     param_id <- libSBML::Parameter_getId(libSBML::Model_getParameter(model, i-1))
    #     param_id <- SBMLtoOdin:::in_reserved_lib(param_id, reserved_names_lib)
    #     if(grepl("_init",param_id)){
    #       param_id <- paste(param_id, "1",sep = "")
    #     }
    #     file_str <- paste(file_str, paste(param_id, " <- user(", libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1)) , ")", sep = ""), sep = "\n")
    #   }
    #   else{
    #     param_id <- libSBML::Parameter_getId(libSBML::Model_getParameter(model, i-1))
    #     #print("down")
    #     #print(param_id)
    #     param_id <- SBMLtoOdin:::in_reserved_lib(param_id, reserved_names_lib)
    #     #print(param_id)
    #     param_val <- libSBML::Parameter_getValue(libSBML::Model_getParameter(model, i-1))
    #     if(grepl("_init",param_id)){
    #       param_id <- paste(param_id, "1",sep = "")
    #     }
    #     var_params_dict[param_id] <- param_val
    #     var_params_dict_used[param_id] <- FALSE
    #   }
    # }

  # collect reactions
  print("Fetching Reactions")
  reaction_list <- list()
  for (i in seq_len(libSBML::Model_getNumReactions(model))) {
    reaction <- libSBML::Model_getReaction(model, i-1)

    # reactants
    reactant_list <- list()
    for (j in seq_len(libSBML::Reaction_getNumReactants(reaction, i-1))) {
      reactant <- libSBML::Reaction_getReactant(reaction, j-1)
      reactant_list[[libSBML::Species_getSpeciesType(reactant)]] <- libSBML::SpeciesReference_getStoichiometry(reactant)
    }
    # products
    product_list <- list()
    for (k in seq_len(libSBML::Reaction_getNumProducts(reaction, i-1))) {
      product <- libSBML::Reaction_getProduct(reaction, k-1)
      product_list[[libSBML::Species_getSpeciesType(product)]] <- libSBML::SpeciesReference_getStoichiometry(product)
    }

    # extract local parameters
    local_parameter_list <- list()
    law <- libSBML::Reaction_getKineticLaw(reaction)
    #print(libSBML::KineticLaw_getNumLocalParameters(law))
    if (!is.null(law)) {
      if(libSBML::KineticLaw_getNumParameters(law) > 0){
        for (j in 1:(libSBML::KineticLaw_getNumParameters(law))) {
          local_param_id_orig <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(law, j-1))
          #print(local_param_id)
          local_param_id <- SBMLtoOdin:::in_reserved_lib(local_param_id_orig, reserved_names_lib)
          #print(libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(law,j-1)))
          #print(local_param)
          parameter_list[[local_param_id]] <- list(
            value = libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(law,j-1)),
            const = TRUE,
            math = NA,
            has_init = FALSE,
            orig_name = local_param_id_orig)
          parameter_used_list[[local_param_id]] <- FALSE
        }
      }
      if(libSBML::KineticLaw_getNumLocalParameters(law) > 0){
        for (j in 1:(libSBML::KineticLaw_getNumLocalParameters(law))) {
          local_param_id <- libSBML::Parameter_getId(libSBML::KineticLaw_getLocalParameter(law, j-1))
          local_param_id <- SBMLtoOdin:::in_reserved_lib(local_param_id, reserved_names_lib)
          #print(libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(law,j-1)))
          #print(local_param)
          local_parameter_list[[local_param_id]] <- libSBML::Parameter_getValue(libSBML::KineticLaw_getLocalParameter(law,j-1))
          parameter_used_list[[local_param_id]] <- FALSE
        }
      }
    }

    # extract kinetic law
    law <- libSBML::Reaction_getKineticLaw(reaction)
    math <- if (!is.null(law)) {
      formula <- formulaToString(KineticLaw_getMath(law))
      gsub("\n", "", formula)
    } else {
      "0"
    }
    # for (param_id in names(parameter_list)) {
    #   orig_param_id <- parameter_list[[param_id]]$orig_name
    #   if(grepl(paste(" ",orig_param_id, " ", sep = ""),file_str, fixed = TRUE) || grepl(paste("(",orig_param_id, ",", sep = ""),file_str, fixed = TRUE) || grepl(paste(" ",orig_param_id, ")", sep = ""),file_str, fixed = TRUE)){
    #     math <-
    #   }
    # }

    #print(math)
    reaction_list[[libSBML::Reaction_getId(reaction)]] <- list(
      name = libSBML::Reaction_getName(reaction),
      reactants = reactant_list,
      products = product_list,
      rate = math,
      local_parameters = local_parameter_list
    )

    # # extract modifier (special case of species that do not need an ode)
    # So, modifier are more complicated than that. Species can be modifiers in some reactions and reactants/products in others.
    # Later

    # for (j in seq_len(libSBML::Reaction_getNumModifiers(reaction))) {
    #   mod <- libSBML::Reaction_getModifier(reaction, j-1)
    #   #mod_name <- libSBML::ModifierSpeciesReference_getElementName(mod)
    #   #print(mod_name)
    #   mod_id <- libSBML::Species_getSpeciesType(mod)
    #   #print(mod_id)
    #   #print(names(species_list))
    #   if(is.element(mod_id, names(species_list))){
    #     #print("found a modifier")
    #     print(mod_id)
    #     species_list[[mod_id]]$is_modifier <- TRUE
    #   }
    #   #else{
    #     #print(paste("Not sure what kind of a modifier this is:", mod_id))
    #   #}
    # }
  }
  # param_lib <- c()
  # for (i in seq_len(libSBML::Model_getNumReactions(model))){
  #   for (j in seq_len(libSBML::Reaction_getNumProducts(libSBML::Model_getReaction(model, i-1)))) {
  #     id_prod = libSBML::Species_getSpeciesType(libSBML::Reaction_getProduct(libSBML::Model_getReaction(model, i-1),j-1))
  #     #print(libSBML::SpeciesReference_getStoichiometry(libSBML::Reaction_getProduct(libSBML::Model_getReaction(model, i-1),j-1)))
  #     if(!boundCond[id_prod]){
  #       dic_react[id_prod] <- paste(dic_react[id_prod],  " + ", libSBML::SpeciesReference_getStoichiometry(libSBML::Reaction_getProduct(libSBML::Model_getReaction(model, i-1),j-1)), " * ", libSBML::KineticLaw_getFormula(libSBML::Reaction_getKineticLaw(libSBML::Model_getReaction(model, i-1))), sep = "")
  #     }
  #     #print(id_prod)
  #     #print(libSBML::KineticLaw_getFormula(libSBML::Reaction_getKineticLaw(libSBML::Model_getReaction(model, i-1))))
  #     # former version. I will have to test what happens when reaction does not behave according to kinetic law
  #     #dic_react[id_prod] <- paste(dic_react[id_prod],  " + ", SBMLtoOdin:::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")
  #
  #   }
  #   for (j in seq_len(libSBML::Reaction_getNumReactants(libSBML::Model_getReaction(model, i-1)))) {
  #     id_reac = libSBML::Species_getSpeciesType(libSBML::Reaction_getReactant(libSBML::Model_getReaction(model, i-1),j-1))
  #     if(!boundCond[id_reac]){
  #     dic_react[id_reac] <- paste(dic_react[id_reac], " - ", libSBML::SpeciesReference_getStoichiometry(libSBML::Reaction_getReactant(libSBML::Model_getReaction(model, i-1),j-1)), " * ", libSBML::KineticLaw_getFormula(libSBML::Reaction_getKineticLaw(libSBML::Model_getReaction(model, i-1))), sep = "")
  #     }
  #     #print(id_reac)
  #     #print(dic_react[id_reac])
  #     # former version. I will have to test what happens when reaction does not behave according to kinetic law
  #     #dic_react[id_reac] <- paste(dic_react[id_reac], " - ", SBMLtoOdin:::getFunctionOutput(model, i, libSBML::Model_getReaction(model, i-1)), sep = "")
  #
  #   }
  #   # get Parameters for reaction
  #   file_str <- SBMLtoOdin:::getFunctionParams(file_str, libSBML::Model_getReaction(model, i-1),param_lib,reserved_names_lib)
  #   param_lib <- SBMLtoOdin:::AddToParamLib(libSBML::Model_getReaction(model, i-1), param_lib,reserved_names_lib)
  # }

  ### special case: there are no reactions in the model.
  # or maybe there are reactions that are defined through rules rather than reactions.
  # add reactions, one per product
  # for (i in names(dic_react)){
  #   i_tr <- SBMLtoOdin:::in_reserved_lib(i, reserved_names_lib)
  #   #print(i_tr)
  #   #print(dic_react[i])
  #   file_str <- paste(file_str, paste("deriv(",i_tr,")", " <- ", paste(dic_react[i], " ", sep = ""), sep = ""), sep = "\n")
  # }



  ### this one does not catch nuances like in all_biomod_ids[1]
  # that some initial parameters depend on other parameters
  # something like (if t >0 ) 1 else 2
  # please make sure to add that later

  #print(file_str)

  # add events
  print("Fetching Events")
  #print(parameter_list)
  #print(species_list)
  event_list <- list()
  if(libSBML::Model_getNumEvents(model) > 0){
    for (i in 1:(libSBML::Model_getNumEvents(model))) {
      event <- libSBML::Model_getEvent(model, i-1)
      trigger <- libSBML::formulaToString(libSBML::Trigger_getMath(libSBML::Event_getTrigger(event)))
      #trigger <- gsub("time", "t", trigger)
      assignment_list <- list()
      for (j in seq_len(libSBML::Event_getNumEventAssignments(event))) {
        assign <- libSBML::Event_getEventAssignment(event, j-1)
        assign_id <- libSBML::EventAssignment_getId(assign)
        event_var <- libSBML::EventAssignment_getVariable(assign)
        event_var <- SBMLtoOdin:::in_reserved_lib(event_var, reserved_names_lib)
        event_val <- libSBML::formulaToString(libSBML::EventAssignment_getMath(assign))
        #print(event_var)
        if(is.element(event_var, names(parameter_list))){
          non_event_val <- parameter_list[[event_var]]$value
          parameter_used_list[[event_var]] <- TRUE
        }
        else if(is.element(event_var, names(species_list))){
          non_event_val <- species_list[[event_var]]$initialAmount
        }
        assignment_list[[assign_id]] <- list(trigger = trigger, event_val = event_val, event_var = event_var, non_event_val = non_event_val, is_duplicated = FALSE)
      }
      event_list[[libSBML::Event_getId(event)]] <- assignment_list

    }
  }
  #print((event_list))
  #print("all event vars")
  #print(event_list[[]]$event_var)
  #print(unlist(lapply(event_list, "[[", "event_var")))
  #event_vars_vec <- unlist(lapply(event_list, "[[", "event_var"))
  #print(event_vars_vec[duplicated(event_vars_vec)])
  #print(which(duplicated(event_vars_vec)))

  # find events that apply to the same event_var
  # Later
  # dupl_events <- which(duplicated(event_vars_vec))
  # if(length(dupl_events)>0){
  #   for (i in dupl_events) {
  #     dupl_event_var <- event_list[[i]]$event_var
  #     #print(which(event_vars_vec == dupl_event_var))
  #     event_list[[which(event_vars_vec == dupl_event_var)[1]]]$is_duplicated = TRUE
  #     event_list[[which(event_vars_vec == dupl_event_var)[2]]]$is_duplicated = TRUE
  #     #print(event_list[[which(event_vars_vec == dupl_event_var)[1]]]$trigger)
  #     #print(event_list[[which(event_vars_vec == dupl_event_var)[2]]]$trigger)
  #   }
  # } # do the rest when parsing to file?

  # this is currently only focused on a very specific type of event (trigger by time >=x and only changes param values). Need to generalise this later.
  #print(libSBML::Model_getNumEvents(model))
  # if(libSBML::Model_getNumEvents(model) > 0){
  #   for (i in seq_len(libSBML::Model_getNumEvents(model))) {
  #     event <- libSBML::Model_getEvent(model,i-1)
  #     event_trigger <- libSBML::formulaToString( libSBML::Trigger_getMath(libSBML::Event_getTrigger(event)))
  #     #print(event_trigger)
  #     if(grepl("gt",event_trigger)){
  #       event_trigger <- SBMLtoOdin:::sub_gt(event_trigger)
  #     }
  #     else if(grepl("geq",event_trigger)){
  #       event_trigger <- SBMLtoOdin:::sub_geq(event_trigger)
  #     }
  #     #event_trigger <- gsub("time", "t", event_trigger)
  #     for (j in seq_len(libSBML::Event_getNumEventAssignments(event))) {
  #       event_assign <- libSBML::Event_getEventAssignment(event, j-1)
  #       event_var <- libSBML::EventAssignment_getVariable(event_assign)
  #       event_var <- SBMLtoOdin:::in_reserved_lib(event_var, reserved_names_lib)
  #       event_val <- libSBML::formulaToString(libSBML::EventAssignment_getMath(event_assign))
  #       non_event_val <- var_params_dict[event_var]
  #       var_params_dict_used[event_var] <- TRUE
  #       var_formula <- paste(event_var, " <- if(", event_trigger, ") ", event_val, " else ",  non_event_val, sep = "")
  #       file_str <- paste(file_str, var_formula, sep = "\n")
  #     }
  #   }
  #   #libSBML::Model_getEvent(model,0)
  #   #libSBML::formulaToString( libSBML::Trigger_getMath(libSBML::Event_getTrigger(libSBML::Model_getEvent(model,0))))
  #
  #   #libSBML::Event_getNumEventAssignments(libSBML::Model_getEvent(model,0))
  #   #libSBML::Event_getEventAssignment(libSBML::Model_getEvent(model,0), 0)
  #   #libSBML::EventAssignment_getVariable(libSBML::Event_getEventAssignment(libSBML::Model_getEvent(model,0), 0))
  #   #libSBML::formulaToString(libSBML::EventAssignment_getMath(libSBML::Event_getEventAssignment(libSBML::Model_getEvent(model,0), 0)))
  # }
  #print(file_str)
  # add compartments
  #print(file_str)
  print("Fetching Compartments")
  compartment_list <- list()
  for (i in seq_len(libSBML::Model_getNumCompartments(model))) {
    comp = (libSBML::Model_getCompartment(model,i-1))
    compartment_list[[libSBML::Compartment_getId(comp)]] <- libSBML::Compartment_getSize(comp)
  #  file_str <- paste(file_str, paste(libSBML::Compartment_getId(comp), " <- ", libSBML::Compartment_getSize(comp), sep = ""), sep = "\n")
  }

  ### add info from dictionaries to file string
  file_str <- paste(file_str, "# Initial Conditions", sep = "\n")

  # Initial conditions
  #print(species_list)
  for (id in names(species_list)) {
    #print(id)
    if(species_list[[id]]$initial_found){
      if(!species_list[[id]]$is_modifier && !boundary_cond_list[[id]] && !species_list[[id]]$is_constant){
        init_val <- species_list[[id]]$initialAmount
        file_str <- paste(file_str, paste0("initial(", id, ") <- ", id, "_init"), sep = "\n")
        file_str <- paste(file_str, paste0(id, "_init <- user(", init_val, ")"), sep = "\n")
      }
    }
    else if(!boundary_cond_list[[id]]){
      print(paste("No initial amount found for variable", id))
    }
  }

  # Differential equations (reactions)
  file_str <- paste(file_str, "# Differential equations", sep = "\n")
  deriv_equations <- list()
  for (id in names(species_list)) {
    if (!boundary_cond_list[[id]] && !species_list[[id]]$is_constant) {
      deriv_equations[[id]] <- "0"
    }
  }

  # for (id in names(species_list)) {
  #   deriv_equations[[id]] <- "0"
  # }

  for (reaction in reaction_list) {
    rate <- reaction$rate
    for (reactant in names(reaction$reactants)) {
      stoich <- reaction$reactants[[reactant]]
      if (!boundary_cond_list[[reactant]]) {
        deriv_equations[[reactant]] <- paste0(deriv_equations[[reactant]], " - ", stoich, " * ", rate)
      }
    }
    for (product in names(reaction$products)) {
      stoich <- reaction$products[[product]]
      if (!boundary_cond_list[[product]]) {
      deriv_equations[[product]] <- paste0(deriv_equations[[product]], " + ", stoich, " * ", rate)
      }
    }
  }
  # add rate rules to output
  for (variable in names(rule_list)) {
    deriv_equations[[variable]] <- rule_list[[variable]]
  }
  #print(deriv_equations)
  for (id in names(deriv_equations)) {
    file_str <- paste(file_str, paste0("deriv(", id, ") <- ", deriv_equations[[id]]), sep = "\n")
  }
  # add parameters to output
  # local
  # BE CAREFUL: I assume here that a local parameter has a unique name (i.e. there is no other local or global parameter with the same name)
  # this might not always be true
  # in odin, there are no local and global parameters
  # So I would need to change names if I want to make sure there are no names with more than one meaning / value
  #print(reaction_list)
  for (reaction in reaction_list) {
    for (param_id in names(reaction$local_parameters)) {
      #print(param_id)
      if(!is.element(param_id, names(parameter_used_list))){
        value <- reaction$local_parameters[[param_id]]
        file_str <- paste(file_str, paste0(param_id, " <- ", value, " # Local parameter in ", reaction$name), sep = "\n")
      }
      else if(!parameter_used_list[[param_id]]){
        value <- reaction$local_parameters[[param_id]]
        file_str <- paste(file_str, paste0(param_id, " <- ", value, " # Local parameter in ", reaction$name), sep = "\n")
        parameter_used_list[[param_id]] <- TRUE
      }
    }
  }
  # boundary conditions / constant species
  for (id in names(boundary_cond_list)) {
    if(boundary_cond_list[[id]]){
      if(boundary_cond_rule_list[[id]]$hasRule){
        file_str <- paste(file_str, paste0(id, " <- ", boundary_cond_rule_list[[id]]$theRule), sep = "\n")
        #print(boundary_cond_rule_list[[id]]$theRule)
      }
      else if(species_list[[id]]$is_constant){
        #print(species_list[[id]]$initialAmount)
        file_str <- paste(file_str, paste0(id, " <- ", species_list[[id]]$initialAmount), sep = "\n")
      }
      else if(boundary_cond_rule_list[[id]]$hasRateRule){
        if(species_list[[id]]$initial_found){
            init_val <- species_list[[id]]$initialAmount
            file_str <- paste(file_str, paste0("initial(", id, ") <- ", id, "_init"), sep = "\n")
            file_str <- paste(file_str, paste0(id, "_init <- user(", init_val, ")"), sep = "\n")

            file_str <- paste(file_str, paste0("deriv(", id, ") <- ", boundary_cond_rule_list[[id]]$theRateRule), sep = "\n")
          }
      }
      else if(species_list[[id]]$initial_found){ # since the "constant" attribute is optional, species might be constant without being declared to be constant
        # so this is the case when I only find an initial amount and nothing else about the boundary condition
        file_str <- paste(file_str, paste0(id, " <- ", species_list[[id]]$initialAmount), sep = "\n")
      }
    }
  }
  # add constant species
  for (id in names(species_list)) {
    #print(id)
    if(species_list[[id]]$initial_found && species_list[[id]]$is_constant){
      if(!species_list[[id]]$is_modifier && !boundary_cond_list[[id]]){
        file_str <- paste(file_str, paste0(id, " <- ", species_list[[id]]$initialAmount), sep = "\n")
      }
    }
  }

  # global
  #print(parameter_list)
  #print(parameter_used_list)
  file_str <- paste(file_str, "# Parameters", sep = "\n")
  for (id in names(parameter_list)) {
    #print(id)
    #print(parameter_used_list[[id]])
    if(!parameter_used_list[[id]]){
      if(parameter_list[[id]]$const){
        value <- parameter_list[[id]]$value
        file_str <- paste(file_str, paste0(id, " <- user(", value, ")"), sep = "\n")
      }
      else if(parameter_list[[id]]$has_init){# non-constant parameters with rule
        init <- parameter_list[[id]]$value
        file_str <- paste(file_str, "\n", "initial(", id, ") <- ", init, sep = "")
        file_str <- paste(file_str, paste(c(paste("deriv(",id,")",sep = ""), parameter_list[[id]]$math), collapse = " <- "), sep = "\n")
      }
      else{ # parameters that are non-constant but actually do not change the value (i.e. they do not have a defined start value)
        #print(id)
        #print(parameter_list[[id]]$value)
        value <- parameter_list[[id]]$value
        file_str <- paste(file_str, paste0(id, " <- ", value), sep = "\n")
      }
    }
    if(id != parameter_list[[id]]$orig_name){
      file_str <- paste(file_str, paste0("# parameter \'", id, "\' is parameter \'", parameter_list[[id]]$orig_name, "\' in the original model. But this name is reserved in odin."), sep = "\n")
    }
  }


  # add events to output
  file_str <- paste(file_str, "# Events", sep = "\n")
  for (event in event_list) {
    for (assign in event) {
      var_formula <- paste(assign$event_var, " <- if(", assign$trigger, ") ", assign$event_val, " else ",  assign$non_event_val, sep = "")
      file_str <- paste(file_str, var_formula, sep = "\n")
    }
  }

  #print(file_str)
  # for (event in event_list) {
  #   if(!event$is_duplicated){
  #     var_formula <- paste(event$event_var, " <- if(", event$trigger, ") ", event$event_val, " else ",  event$non_event_val, sep = "")
  #     file_str <- paste(file_str, var_formula, sep = "\n")
  #   }
  #   else{ # more complex events
  #     dupl_event_var <- event$event_var
  #     event_1_name <- names(event_list)[which(event_vars_vec == dupl_event_var)[1]]
  #     event_1 <- event_list[[event_1_name]]
  #     event_2_name <- names(event_list)[which(event_vars_vec == dupl_event_var)[2]]
  #     event_2 <- event_list[[event_2_name]]
  #     var_formula <- paste(event$event_var, " <- if(", event_1$trigger, "&& !", event_2$trigger, ") ", event_1$event_val," else if(", event_2$trigger, "&& !", event_1$trigger, ") ", event_2$event_val,  " else ", event_1$non_event_val, sep = "")
  #     file_str <- paste(file_str, var_formula, sep = "\n")
  #     # almost, need to remove this so it does not appear a second time
  #   }
  # }



  # add compartments to output
  file_str <- paste(file_str, "# Compartments", sep = "\n")
  for (id in names(compartment_list)) {
    size <- compartment_list[[id]]
    file_str <- paste(file_str, paste0(id, " <- ", size), sep = "\n")
  }

  #print(file_str)
  # Call function that replaces pow() by ^ if necessary
  if(grepl("pow\\(",file_str)){
    file_str <- translate_pow(file_str)
  }
  if(grepl("root\\(",file_str)){
    file_str <- translate_root(file_str)
  }
  # Call function that replaces piecewise() by if statement
  #print(file_str)
  if(grepl("piecewise",file_str)){
    file_str <- SBMLtoOdin:::translate_piecewise(file_str)
  }
  #print(file_str)
  # substitute factorial by gamma function
  if(grepl("factorial",file_str)){
    file_str <- SBMLtoOdin:::sub_factorial(file_str)
  }
  # substitute ceil by ceiling
  if(grepl("ceil",file_str)){
    file_str <- SBMLtoOdin:::sub_ceil(file_str)
  }
  # substitute leq by <=
  if(grepl("leq",file_str)){
    file_str <- SBMLtoOdin:::sub_leq(file_str)
  }
  # substitute geq by >=
  if(grepl("geq",file_str)){
    file_str <- SBMLtoOdin:::sub_geq(file_str)
  }
  # substitute lt by <
  if(grepl("lt\\(",file_str)){
    file_str <- SBMLtoOdin:::sub_lt(file_str)
  }
  # substitute gt by >
  if(grepl("gt\\(",file_str)){
    file_str <- SBMLtoOdin:::sub_gt(file_str)
  }
  # substitute custom functions
  #print(file_str)
  #print(func_def_dict)
  #print(grep("v1sub(",file_str, fixed = TRUE))
  for (cust_func in func_def_dict) {
    if(grepl(paste(cust_func, "(", sep = ""), file_str, fixed = TRUE)){
      #print(cust_func)
      new_str <- strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][1]
      for (i in 2:length(strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]])) {
      #for (i in 2:3) {
        #print("found")
        #print(strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i])
        #print(paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""))
        replaced_func <- SBMLtoOdin:::getFunctionOutputForRules(model, paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""), cust_func)
        #print(replaced_func)
        #print("to be replaced")
        #print(paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""))
        #print(cust_func)
        #print(regmatches(paste(cust_func,strsplit(file_str,cust_func)[[1]][i], sep = ""), gregexpr(paste(cust_func,"(\\(([^()]|(?1))*\\))",sep = ""), paste(cust_func,strsplit(file_str,cust_func)[[1]][i], sep = ""), perl=TRUE))[[1]][1])
        #print(regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1])
        #print("replaced")
        #print(replaced_func)
        #print("regmatch")
        #print(regmatches(strsplit(file_str,cust_func)[[1]][i], gregexpr("(\\(([^()]|(?1))*\\))", strsplit(file_str,cust_func)[[1]][i], perl=TRUE))[[1]][1])
        #print(regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1])
        new_str_part <- gsub(paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""), replaced_func, paste(cust_func,"(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), fixed = TRUE)

        #print(new_str_part)
        #new_str <- paste(new_str, replaced_func, sep = "")
        new_str <- paste(new_str, new_str_part, sep = "")
      }
      file_str <- new_str
    }
  }
  #print(file_str)
  # for (p in names(var_params_dict_used)) {
  #   if(!(var_params_dict_used[p])){
  #     file_str <- paste(file_str, paste(p, " <- user(", var_params_dict[p] , ")", sep = ""), sep = "\n")
  #     var_params_dict_used[p] <- TRUE
  #   }
  # }

  # Call function that replaces pow() by ^ if necessary
  if(grepl("pow\\(",file_str)){
    file_str <- translate_pow(file_str)
  }

  # replace parameter and species names that start with a number or an underscore
  for (param_name in c(names(parameter_list), names(species_list))) {
    if(grepl("^\\_[0-9+]",param_name, perl = TRUE)){
      bad_names[param_name] <- paste("p", param_name, sep = "")
    }
    else if(grepl("^[0-9+]",param_name, perl = TRUE)){
      bad_names[param_name] <- paste("p", param_name, sep = "")
    }
  }
  for (param_name in names(bad_names)) {
    #print(param_name)
    #print(bad_names[param_name])
    file_str <- gsub(param_name,bad_names[param_name],file_str)
    #print(file_str)
  }
  # this is not very elegant. maybe I should better check whether pi is one of the parameters
  if(grepl("pi", file_str)){
    file_str <- paste("pi <- 3.141593", file_str, sep = "\n")
  }
  if(grepl("piecewise",file_str)){
    file_str <- SBMLtoOdin:::translate_piecewise(file_str)
  }
  for (reserved_param in names(reserved_names_lib)) {
    #print(reserved_param)
    file_str <- gsub(paste(" ", reserved_param, " ", sep = ""), paste(" ", reserved_names_lib[reserved_param], " ", sep = ""), file_str)
    file_str <- gsub(paste("\n", reserved_param, " ", sep = ""), paste("\n", reserved_names_lib[reserved_param], " ", sep = ""), file_str)
    file_str <- gsub(paste(" ", reserved_param, "\\)", sep = ""), paste(" ", reserved_names_lib[reserved_param], "\\)", sep = ""), file_str)
    file_str <- gsub(paste("\\(", reserved_param, " ", sep = ""), paste("\\(", reserved_names_lib[reserved_param], " ", sep = ""), file_str)
    file_str <- gsub(paste("\\(", reserved_param, "\\)", sep = ""), paste("\\(", reserved_names_lib[reserved_param], "\\)", sep = ""), file_str)
  }
  file_str <- gsub("default ", paste(" ", reserved_names_lib["default"], " ", sep = ""), file_str)
  file_str <- gsub("default\n", paste(" ", reserved_names_lib["default"], "\n", sep = ""), file_str)
  #substitute all mentions of time by t
  file_str <- gsub("time", "t", file_str)
  file_str <- gsub("Time", "t", file_str)
  # write information into odin.dust file
  writeLines(file_str, path_to_output,sep = "")
}

#' Title
#'
#' @param path_to_input A string (Path to an SBML model file).
#' @param path_to_output A string (Path to the output file).
#'
#' @importFrom libSBML readSBMLFromFile SBMLDocument_getModel
#' @importFrom SBMLtoOdin SBML_to_odin
#' @return no return
#' @export
#'
#' @examples
#' importSBMLfromFile("/usr/model_files/my_model.xml")
importSBMLfromFile <- function(path_to_input, path_to_output = "odinModel.R"){
  model_string <- readChar(path_to_input, file.info(path_to_input)$size)
  doc = libSBML::readSBMLFromFile(path_to_input)
  model = libSBML::SBMLDocument_getModel(doc)
  SBMLtoOdin:::SBML_to_odin(model,path_to_output, model_string)
}

#' Title
#'
#' @param model_id A string (a valid BioModels id)
#' @param path_to_output A string (Path to the output file).
#'
#' @importFrom libSBML readSBMLFromString SBMLDocument_getModel
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom SBMLtoOdin SBML_to_odin
#' @return no return
#' @export
#'
#' @examples
#' importSBMLfromBioModels(biomodels_id, output_directory)
importSBMLfromBioModels <- function(model_id, path_to_output = "odinModel.R"){
  ### still need to add try.. catch stuff and error messages. If the model does not exist etc.
  #res1 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/files/", model_id, sep = ""))
  #data_1 = res1$content
  #data_2 = data_1[!data_1=='00']
  #data_2 = as.raw(res1$content)
  #data = jsonlite::fromJSON(rawToChar(data_2))
  #data = jsonlite::fromJSON(rawToChar(res1$content))

  #filename = data$main[,"name"]
  #filename = gsub(" ", "%20", filename)
  #res2 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/download/", model_id, "?filename=", filename, sep = ""))
  #data_1 = res2$content
  #data_2 = data_1[!data_1=='00']
  #data_2 = as.raw(res2$content)
  #doc = libSBML::readSBMLFromString(data_2)
  #doc = libSBML::readSBMLFromString(res2$content)
  #model = libSBML::SBMLDocument_getModel(doc)
  #SBMLtoOdin:::SBML_to_odin(model,path_to_output)

  res1 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/files/", model_id, sep = ""))
  data = jsonlite::fromJSON(rawToChar(res1$content))
  filename = data$main[,"name"]
  filename = URLencode(filename, reserved = TRUE)
  #filename = gsub(" ", "%20", filename)

  #print(filename)
  # check whether model is in sbml file format, otherwise abort
  file_ext <- strsplit(filename,"\\.")[[1]][length(strsplit(filename,"\\.")[[1]])]
  if (file_ext != "xml"){
    stop(paste("Model is of file format", file_ext, "instead of xml. SBMLtoOdin only imports sbml files."))
  }
  res2 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/download/", model_id, "?filename=", filename, sep = ""))
  model_string = (rawToChar(res2$content))
  doc = libSBML::readSBMLFromString(rawToChar(res2$content))
  model = libSBML::SBMLDocument_getModel(doc)
  SBMLtoOdin:::SBML_to_odin(model,path_to_output, model_string)
}
