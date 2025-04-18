test_that("check presence in reserved library works", {
  expect_equal(in_reserved_lib("a",c("a"="a_1","b"="b_1")), "a_1")
  expect_equal(in_reserved_lib("c",c("a"="a_1","b"="b_1")), "c")
})
test_that("parsing power function works", {
  expect_equal(translate_pow("pow(a,b)"), "(a)^(b)")
  expect_equal(translate_pow("pow(a+3,b)"), "(a+3)^(b)")
  expect_equal(translate_pow("c <- pow(a+3,b)\nd <- pow(10,2+x)"), "c <- (a+3)^(b)\nd <- (10)^(2+x)")
})
test_that("in_reserved_lib function works", {
  expect_equal(in_reserved_lib("a", c("a"=1, "b" =2)), "1")
  expect_equal(in_reserved_lib("a", c("c"=1, "b" =2)), "a")
})

# missing test functions:
# importSBMLfromFile
# importSBMLfromBioModels
# getRule
# getSpeciesRule
# getFunctionOutput
# getFunctionOutputForRules
# AddToParamLib
# getFunctionParams
# translate_root
# translate_piecewise
# sub_factorial
# sub_ceil
# sub_and
# sub_leq
# sub_neq_for_comp
# sub_eq_for_comp
# sub_lt
# sub_gt
# sub_geq
# SBML_to_odin



test_that("importing model works", {
  sbml_test <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n<model metaid=\"_case00001\" id=\"case00001\" name=\"case00001\" timeUnits=\"time\">\n<listOfUnitDefinitions>\n<unitDefinition id=\"volume\">\n<listOfUnits>\n<unit kind=\"litre\" exponent=\"1\" scale=\"0\" multiplier=\"1\"/>\n</listOfUnits>\n</unitDefinition>\n<unitDefinition id=\"substance\">\n<listOfUnits>\n<unit kind=\"mole\" exponent=\"1\" scale=\"0\" multiplier=\"1\"/>\n</listOfUnits>\n</unitDefinition>\n<unitDefinition id=\"time\">\n<listOfUnits>\n<unit kind=\"second\" exponent=\"1\" scale=\"0\" multiplier=\"1\"/>\n</listOfUnits>\n</unitDefinition>\n</listOfUnitDefinitions>\n<listOfCompartments>\n<compartment id=\"compartment\" name=\"compartment\" spatialDimensions=\"3\" size=\"1\" units=\"volume\" constant=\"true\"/>\n</listOfCompartments>\n<listOfSpecies>\n<species id=\"S1\" name=\"S1\" compartment=\"compartment\" initialAmount=\"0.00015\" substanceUnits=\"substance\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n<species id=\"S2\" name=\"S2\" compartment=\"compartment\" initialAmount=\"0\" substanceUnits=\"substance\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n</listOfSpecies>\n<listOfParameters>\n<parameter id=\"k1\" name=\"k1\" value=\"1\" constant=\"true\"/>\n</listOfParameters>\n<listOfReactions>\n<reaction id=\"reaction1\" name=\"reaction1\" reversible=\"false\" fast=\"false\">\n<listOfReactants>\n<speciesReference species=\"S1\" stoichiometry=\"1\" constant=\"true\"/>\n</listOfReactants>\n<listOfProducts>\n<speciesReference species=\"S2\" stoichiometry=\"1\" constant=\"true\"/>\n</listOfProducts>\n<kineticLaw>\n<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n<apply>\n<times/>\n<ci> compartment </ci>\n<ci> k1 </ci>\n<ci> S1 </ci>\n</apply>\n</math>\n</kineticLaw>\n</reaction>\n</listOfReactions>\n</model>\n</sbml>"
  doc_test = libSBML::readSBMLFromString(sbml_test)
  test_model = libSBML::SBMLDocument_getModel(doc_test)

  expect_equal(test_model, test_model)
  # not working yet. seems to compare the pointers, not the content
  # still working on sensible test functions here.
  #expect_equal(importSBML("../testmodel_00001-sbml-l3v1.xml"), test_model)
})



