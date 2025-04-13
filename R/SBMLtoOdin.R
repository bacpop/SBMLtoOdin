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
      if((libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(m,n-1)) == func_id)){
        function_def <- libSBML::formulaToString(libSBML::FunctionDefinition_getBody(libSBML::Model_getFunctionDefinition(m,n-1)))

        func_args_dict <- rep(NA, libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))
        args_call <- strsplit(formula,"\\(|\\)")[[1]][2]
        if(args_call != ""){
          func_args_dict <- stringr::str_split_fixed(args_call,",",libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))[1,]
          for (i in seq_len(libSBML::FunctionDefinition_getNumArguments(libSBML::Model_getFunctionDefinition(m,n-1)))) {
            names(func_args_dict)[i] <- libSBML::formulaToString(libSBML::FunctionDefinition_getArgument(libSBML::Model_getFunctionDefinition(m,n-1),i-1))
          }

          for (i in 1:length(func_args_dict)) {
            function_def <- gsub(paste("\\b",names(func_args_dict)[i], "\\b", sep = ''), paste(" ", func_args_dict[i], " ", sep = ""), function_def, perl = TRUE)
          }
          formula <- gsub(paste("\\Q",func_id,"(",args_call,")\\E",sep = ""), paste("(",function_def,")",sep=""), formula)
          }
        else{
          formula <- function_def
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
  piece_expr <- strsplit(piece_expr_all,"piecewise\\(")[[1]]
  piece_expr[2] <- gsub('.{1}$', '', piece_expr[2]) # remove closing bracket at the end
  piece_expr_all_new <- piece_expr_all
  i_test <- 0
  while(length(piece_expr) > 2 && i_test<10){
    i_test <- i_test +1
    last_piece <- translate_piecewise(paste("piecewise(", piece_expr[length(piece_expr)], sep = ""))
    piece_expr_all_new <- gsub(paste("piecewise(",piece_expr[length(piece_expr)],sep = ""), last_piece, piece_expr_all_new, fixed = TRUE)
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
    while(grepl("eq",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_eq_for_comp(piece_expr[2])
    }
    while(grepl("and",piece_expr[2])){
     piece_expr[2] <- SBMLtoOdin:::sub_and(piece_expr[2])
    }
    while (grepl("xor",piece_expr[2])) {
      stop("SBMLtoOdin cannot handle xor logic.")
    }
    while(grepl("or",piece_expr[2])){
      piece_expr[2] <- SBMLtoOdin:::sub_or(piece_expr[2])
    }
    piece_expr0 <- strsplit(piece_expr[2], ",(?![^(]*\\))", perl = TRUE)[[1]]
    if_part <- piece_expr0[2]
    new_piece_expr <- paste("(",piece_expr[1], "if(", if_part, ") ", piece_expr0[1], " else ", piece_expr0[length(piece_expr0)], ")" ,sep="" )
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
  #print(fact_expr)
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

  and_content <- strsplit(and_expr, "and(", fixed = TRUE)[[1]][2]
  and_content2 <- strsplit(and_content, ")", fixed = TRUE)[[1]][1]
  and_expr1 <- strsplit(and_content2, ",")[[1]]
  new_expr <- and_expr1[1]
  for (i in 2:length(and_expr1)) {
    new_expr <- paste(new_expr, and_expr1[i], sep = " && ")
  }
  new_expr <- paste(" (", new_expr, ")", sep = "")
  file_content <- gsub(and_expr, new_expr, file_content, fixed = TRUE)
  file_content
}

#' Title
#'
#' @param file_content A string.
#'
#' @return file content
#'
#' @examples
#' sub_or("if(or(b,c))")
sub_or <- function(file_content){
  or_expr <- regmatches(file_content, gregexpr("or(\\(([^()]|(?1))*\\))", file_content, perl=TRUE))[[1]][1]
  or_content <- strsplit(or_expr, "or(", fixed = TRUE)[[1]][2]
  or_content2 <- strsplit(or_content, ")", fixed = TRUE)[[1]][1]
  or_expr1 <- strsplit(or_content2, ",")[[1]]
  new_expr <- or_expr1[1]
  for (i in 2:length(or_expr1)) {
    new_expr <- paste(new_expr, or_expr1[i], sep = " || ")
  }
  new_expr <- paste("(", new_expr, ")", sep = "")
  file_content <- gsub(or_expr, new_expr, file_content, fixed = TRUE)
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


  # build library of function definitions
  func_def_dict <- c()
  for (i in seq_len(libSBML::Model_getNumFunctionDefinitions(model))) {
    func_def_dict[i] <- libSBML::FunctionDefinition_getId(libSBML::Model_getFunctionDefinition(model,i-1))
  }

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
        has_initAssign = FALSE,
        math_initAssign = NA,
        orig_name = param_id_orig)
      parameter_used_list[[param_id]] <- FALSE
    }
    else{
      parameter_list[[param_id]] <- list(
        value = libSBML::Parameter_getValue(parameter),
        const = FALSE,
        math = NA,
        has_init = FALSE,
        has_initAssign = FALSE,
        math_initAssign = NA,
        orig_name = param_id_orig)
      parameter_used_list[[param_id]] <- FALSE
    }
  }
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

    is_mod <- FALSE

    species_list[[libSBML::Species_getId(species)]] <- list(
      name = libSBML::Species_getId(species),
      compartment = libSBML::Species_getCompartment(species),
      initialAmount = spec_conc,
      initial_found = conc_found,
      has_initAssign = FALSE,
      is_modifier = is_mod,
      is_constant = libSBML::Species_getConstant(species),
      hasRateRule = FALSE,
      theRateRule = NA,
      hasScalarRule = FALSE,
      theScalarRule = NA
    )
    boundary_cond_list[[libSBML::Species_getId(species)]] <- libSBML::Species_getBoundaryCondition(species)
    if(libSBML::Species_getBoundaryCondition(species)){
      boundary_cond_rule_list[[libSBML::Species_getId(species)]] <- list(hasRule = FALSE, theRule = NA, hasRateRule = FALSE, theRateRule = NA)
    }
  }

  # find all initial species assignments that I might have missed
  if(libSBML::Model_getNumInitialAssignments(model)>0){
    #print("found initial Assignment")
        for (j in seq_len(libSBML::Model_getNumInitialAssignments(model))) {
          init_assign <- libSBML::Model_getInitialAssignment(model,j-1)
          var_id <- libSBML::InitialAssignment_getSymbol(init_assign)
          conc = libSBML::formulaToString(libSBML::InitialAssignment_getMath(init_assign))
          if(is.element(var_id, names(species_list))){
            if(grepl("_init",conc)){
              conc <- paste(conc, "1",sep="")
            }
            species_list[[var_id]]$initialAmount <- conc
            species_list[[var_id]]$initial_found  <- TRUE
            species_list[[var_id]]$has_initAssign  <- TRUE
          }
          else if(is.element(var_id, names(parameter_list))){
            parameter_list[[var_id]]$has_init <- TRUE
            parameter_list[[var_id]]$math_initAssign <- conc
            parameter_list[[var_id]]$has_initAssign <- TRUE
          }
      }
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
          math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
          parameter_list[[param_id]]$math <- math
          parameter_list[[param_id]]$has_init = TRUE
        }
        else if(is.element(param_id, names(boundary_cond_list)) && boundary_cond_list[[param_id]]){ #if species is boundary condition, i.e. does not depend on reactions
          boundary_cond_rule_list[[param_id]]$hasRateRule <- TRUE
          math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
          boundary_cond_rule_list[[param_id]]$theRateRule <- math
          #print(math)
        }
        else if(is.element(param_id, names(species_list))){ # if species is a "normal species"
          species_list[[param_id]]$hasRateRule <- TRUE
          math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
          species_list[[param_id]]$theRateRule <- math
        }
      }
      else if(libSBML::Rule_isAssignment(rule)){
        variable <- libSBML::Rule_getVariable(rule)
        # if it is a piecewise expression
        math <- libSBML::formulaToString(libSBML::Rule_getMath(rule))
        if (grepl("piecewise",math)) {
          math <- SBMLtoOdin:::translate_piecewise(math)
        }
        if(is.element(variable, names(boundary_cond_list)) && boundary_cond_list[[variable]]){
          boundary_cond_rule_list[[variable]]$hasRule <- TRUE
          boundary_cond_rule_list[[variable]]$theRule <- math
        }
        else if(variable %in% names(parameter_list)){
          parameter_list[[variable]]$value = math
          #print(libSBML::Rule_getMath(rule))
        }
        else if(variable %in% names(species_list)){
          stop("SBMLtoOdin does not support Assignment Rules for species yet if they are not for boundary conditions.")
        }
      }
      else if(libSBML::Rule_isScalar(rule)){
        var_id <- libSBML::Rule_getVariable(rule)
        conc = libSBML::formulaToString(libSBML::Rule_getMath(rule))
        if(grepl("_init",conc)){
          conc <- paste(conc, "1",sep="")
          species_list[[var_id]]$initialAmount = conc
          species_list[[var_id]]$initial_found  <- TRUE
        }
        #print(conc)
        else if(is.element(var_id, names(species_list))){
          for (cust_func in func_def_dict) {
            if(grepl(cust_func, conc)){
              conc <- SBMLtoOdin:::getFunctionOutputForRules(m, conc, cust_func)
            }
          }
          if(grepl("piecewise",conc)){
            conc <- SBMLtoOdin:::translate_piecewise(conc)
          }
          if(grepl("pow\\(",conc)){
            conc <- SBMLtoOdin:::translate_pow(conc)
          }
          if(grepl("root\\(",conc)){
            conc <- SBMLtoOdin:::translate_root(conc)
          }
          deriv_of_rule <- D(parse(text = conc), var_id)
          species_list[[var_id]]$hasScalarRule <- TRUE
          species_list[[var_id]]$theScalarRule <- paste(var_id, deriv_of_rule ,sep = " + ")
        }
      }
      else if(libSBML::Rule_isAlgebraic(rule)){
        stop("SBMLtoOdin does not handle algebraic rules yet.")
      }
      else{
        stop("SBMLtoOdin does not recognise this rule type.")
      }
    }
  }

  # collect reactions
  print("Fetching Reactions")
  reaction_list <- list()
  for (i in seq_len(libSBML::Model_getNumReactions(model))) {
    reaction <- libSBML::Model_getReaction(model, i-1)
    is_fast <- libSBML::Reaction_isSetFast(reaction, i-1)
    if(is_fast){
      stop("SBMLtoOdin does not support fast reactions yet.")
    }
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
    if (!is.null(law)) {
      if(libSBML::KineticLaw_getNumLocalParameters(law) > 0){
        for (j in 1:(libSBML::KineticLaw_getNumLocalParameters(law))) {
          local_param_id <- libSBML::Parameter_getId(libSBML::KineticLaw_getLocalParameter(law, j-1))
          local_param_id_reac <- paste0(local_param_id, "_", libSBML::Reaction_getId(reaction))
          local_parameter_list[[local_param_id]] <- list(value = libSBML::Parameter_getValue(libSBML::KineticLaw_getLocalParameter(law,j-1)),
                                                        local_name = local_param_id_reac,
                                                        parameter_used = FALSE)
        }
      }
      if(libSBML::KineticLaw_getNumParameters(law) > 0){
        for (j in 1:(libSBML::KineticLaw_getNumParameters(law))) {

          local_param_id_orig <- libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(law, j-1))

          local_param_id <- SBMLtoOdin:::in_reserved_lib(local_param_id_orig, reserved_names_lib)
          if(! libSBML::Parameter_getId(libSBML::KineticLaw_getParameter(law, j-1)) %in% names(local_parameter_list)){
            parameter_list[[local_param_id]] <- list(
              value = libSBML::Parameter_getValue(libSBML::KineticLaw_getParameter(law,j-1)),
              const = TRUE,
              math = NA,
              has_init = FALSE,
              has_initAssign = FALSE,
              orig_name = local_param_id_orig)
            parameter_used_list[[local_param_id]] <- FALSE
          }
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

    reaction_list[[libSBML::Reaction_getId(reaction)]] <- list(
      name = libSBML::Reaction_getName(reaction),
      reactants = reactant_list,
      products = product_list,
      rate = math,
      local_parameters = local_parameter_list
    )
  }

  # add events
  print("Fetching Events")
  event_list <- list()
  if(libSBML::Model_getNumEvents(model) > 0){
    for (i in 1:(libSBML::Model_getNumEvents(model))) {
      event <- libSBML::Model_getEvent(model, i-1)
      trigger <- libSBML::formulaToString(libSBML::Trigger_getMath(libSBML::Event_getTrigger(event)))
      assignment_list <- list()
      for (j in seq_len(libSBML::Event_getNumEventAssignments(event))) {
        assign <- libSBML::Event_getEventAssignment(event, j-1)
        assign_id <- libSBML::EventAssignment_getId(assign)
        event_var <- libSBML::EventAssignment_getVariable(assign)
        event_var <- SBMLtoOdin:::in_reserved_lib(event_var, reserved_names_lib)
        event_val <- libSBML::formulaToString(libSBML::EventAssignment_getMath(assign))
        if(is.element(event_var, names(parameter_list))){
          non_event_val <- parameter_list[[event_var]]$value
          parameter_used_list[[event_var]] <- TRUE
        }
        else if(is.element(event_var, names(species_list))){
          non_event_val <- species_list[[event_var]]$initialAmount
        }
        assignment_list[[assign_id]] <- list(trigger = trigger, event_val = event_val, event_var = event_var, non_event_val = non_event_val, is_duplicated = FALSE)
        if(grepl(event_var, trigger, fixed = TRUE)){
          stop("SBMLtoOdin cannot handle events with self referencing yet because odin does not allow that.")
        }
        if(is.element(event_var, names(species_list))){
          stop("SBMLtoOdin cannot handle events for species yet.")
        }
      }
      event_list[[libSBML::Event_getId(event)]] <- assignment_list
    }
  }

  print("Fetching Compartments")
  compartment_list <- list()
  for (i in seq_len(libSBML::Model_getNumCompartments(model))) {
    comp = (libSBML::Model_getCompartment(model,i-1))
    compartment_list[[libSBML::Compartment_getId(comp)]] <- libSBML::Compartment_getSize(comp)
    if (is.na(libSBML::Compartment_getSize(comp)) | libSBML::Compartment_getSize(comp) != 1){
      stop("SMBLtoOdin does not support compartment sizes unequal to 1 yet.")
    }
  }

  ### add info from dictionaries to file string
  file_str <- paste(file_str, "# Initial Conditions", sep = "\n")

  # Initial conditions
  for (id in names(species_list)) {
    if(species_list[[id]]$initial_found){
      if(!species_list[[id]]$is_modifier && !boundary_cond_list[[id]] && !species_list[[id]]$is_constant){
        init_val <- species_list[[id]]$initialAmount
        file_str <- paste(file_str, paste0("initial(", id, ") <- ", id, "_init"), sep = "\n")
        if(!species_list[[id]]$has_initAssign){
          file_str <- paste(file_str, paste0(id, "_init <- user(", init_val, ")"), sep = "\n")
        }
        else{ # is initial Assignment. This might contain other variables and can therefore not be within user()
          file_str <- paste(file_str, paste0(id, "_init <- ", init_val), sep = "\n")
        }
      }
    }
    else if(!boundary_cond_list[[id]]){
      #print(paste("No initial amount found for variable", id))
      stop(paste("No initial amount found for variable", id))
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

  for (reaction in reaction_list) {
    rate <- reaction$rate
    for (param_id in names(reaction$local_parameters)) {
      value <- reaction$local_parameters[[param_id]]$value
      local_name <- reaction$local_parameters[[param_id]]$local_name
      if(!reaction$local_parameters[[param_id]]$parameter_used){
        file_str <- paste(file_str, paste0(local_name, " <- ", value, " # Local parameter in ", reaction$name, ", original name is ", param_id), sep = "\n")
        reaction$local_parameters[[param_id]]$parameter_used <- TRUE
      }
      rate <- gsub(param_id, local_name, rate, fixed = TRUE)
    }


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
  for (variable in names(species_list)) {
    if(species_list[[variable]]$hasRateRule){
      deriv_equations[[variable]] <- paste(deriv_equations[[variable]], species_list[[variable]]$theRateRule, sep = " + ")
    }
    if(species_list[[variable]]$hasScalarRule){
      deriv_equations[[variable]] <- paste(deriv_equations[[variable]], species_list[[variable]]$theScalarRule, sep = " + ")
    }
  }
  #print(deriv_equations)
  for (id in names(deriv_equations)) {
    file_str <- paste(file_str, paste0("deriv(", id, ") <- ", deriv_equations[[id]]), sep = "\n")
  }

  # boundary conditions / constant species
  for (id in names(boundary_cond_list)) {
    if(boundary_cond_list[[id]]){
      if(boundary_cond_rule_list[[id]]$hasRule){
        file_str <- paste(file_str, paste0(id, " <- ", boundary_cond_rule_list[[id]]$theRule), sep = "\n")
        #print(boundary_cond_rule_list[[id]]$theRule)
      }
      else if(species_list[[id]]$is_constant){
        file_str <- paste(file_str, paste0("initial(", id, ") <- ", id, "_init"), sep = "\n")
        file_str <- paste(file_str, paste0(id, "_init <- user(", species_list[[id]]$initialAmount, ")"), sep = "\n")
        file_str <- paste(file_str, paste0("deriv(", id, ") <- 0"), sep = "\n")
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
        # will write "empty" ode, so that species appears in output (and plots) of model
        file_str <- paste(file_str, paste0("initial(", id, ") <- ", id, "_init"), sep = "\n")
        if(!species_list[[id]]$has_initAssign){
          file_str <- paste(file_str, paste0(id, "_init <- user(", species_list[[id]]$initialAmount, ")"), sep = "\n")
        }
        else{
          file_str <- paste(file_str, paste0(id, "_init <- ", species_list[[id]]$initialAmount), sep = "\n")
        }
        file_str <- paste(file_str, paste0("deriv(", id, ") <- 0"), sep = "\n")
        #file_str <- paste(file_str, paste0(id, " <- ", species_list[[id]]$initialAmount), sep = "\n")
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

  # global parameters
  file_str <- paste(file_str, "# Parameters", sep = "\n")
  for (id in names(parameter_list)) {
    if(!parameter_used_list[[id]]){
      if(parameter_list[[id]]$const && !parameter_list[[id]]$has_initAssign){
        value <- parameter_list[[id]]$value
        file_str <- paste(file_str, paste0(id, " <- user(", value, ")"), sep = "\n")
      }
      else if(parameter_list[[id]]$has_init && !parameter_list[[id]]$has_initAssign){# non-constant parameters with rule
        init <- parameter_list[[id]]$value
        file_str <- paste(file_str, "\n", "initial(", id, ") <- ", init, sep = "")
        file_str <- paste(file_str, paste(c(paste("deriv(",id,")",sep = ""), parameter_list[[id]]$math), collapse = " <- "), sep = "\n")
      }
      else if(parameter_list[[id]]$has_initAssign){
        init <- parameter_list[[id]]$math_initAssign
        file_str <- paste(file_str, "\n", id, " <- ", init, sep = "")
      }
      else{ # parameters that are non-constant but actually do not change the value (i.e. they do not have a defined start value)
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

  # add compartments to output
  file_str <- paste(file_str, "# Compartments", sep = "\n")
  for (id in names(compartment_list)) {
    size <- compartment_list[[id]]
    file_str <- paste(file_str, paste0(id, " <- ", size), sep = "\n")
    if(size != 1){
      print("Warning: compartment sizes unequal to 1 are currently not supported")
      #tryCatch( { size == 1 }, error = function(m) { print("compartment size error")})
    }
  }

  # substitute custom functions
  for (cust_func in func_def_dict) {
    if(grepl(paste(cust_func, "(", sep = ""), file_str, fixed = TRUE)){
      #print(cust_func)
      new_str <- strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][1]
      for (i in 2:length(strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]])) {
        replaced_func <- SBMLtoOdin:::getFunctionOutputForRules(model, paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""), cust_func)
        new_str_part <- gsub(paste(cust_func, regmatches(paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), gregexpr("(\\(([^()]|(?1))*\\))", paste("(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), perl=TRUE))[[1]][1], sep = ""), replaced_func, paste(cust_func,"(",strsplit(file_str,paste(cust_func, "(", sep = ""), fixed = TRUE)[[1]][i], sep = ""), fixed = TRUE)
        new_str <- paste(new_str, new_str_part, sep = "")
      }
      file_str <- new_str
    }
  }
  # Call function that replaces pow() by ^ if necessary
  if(grepl("pow\\(",file_str)){
    file_str <- translate_pow(file_str)
  }
  if(grepl("root\\(",file_str)){
    file_str <- translate_root(file_str)
  }
  # Call function that replaces piecewise() by if statement
  if(grepl("piecewise",file_str)){
    file_str <- SBMLtoOdin:::translate_piecewise(file_str)
  }
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
    file_str <- gsub(param_name,bad_names[param_name],file_str)
  }
  # this is not very elegant. maybe I should better check whether pi is one of the parameters
  if(grepl("pi", file_str)){
    file_str <- paste("pi <- 3.141593", file_str, sep = "\n")
  }
  if(grepl("piecewise",file_str)){
    file_str <- SBMLtoOdin:::translate_piecewise(file_str)
  }
  for (reserved_param in names(reserved_names_lib)) {
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
#' @return no return
#' @export
#'
#' @examples
#' importSBMLfromFile("/usr/model_files/my_model.xml", "/usr/model_files/my_model.R")
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
#' @return no return
#' @export
#'
#' @examples
#' importSBMLfromBioModels("BIOMD0000000012", "/usr/model_files/my_model.xml")
importSBMLfromBioModels <- function(model_id, path_to_output = "odinModel.R"){

  res1 = httr::GET(paste("https://www.ebi.ac.uk/biomodels/model/files/", model_id, sep = ""))
  data = jsonlite::fromJSON(rawToChar(res1$content))
  filename = data$main[,"name"]
  filename = URLencode(filename, reserved = TRUE)

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
