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
for (i in 1:1073){
  tryCatch( { importSBMLfromBioModels(all_biomod_ids[i],"../TestModel.R")}, error = function(w) { print("ImportSBML error"); ImportError <<- ImportError + 1; ImportError_ids[ImportError] <<-  all_biomod_ids[i]}, warning = function(w) { print("ImportSBML warning") })
  tryCatch( { model_generator <- odin::odin("../TestModel.R") }, error = function(m) { print("odin error"); OdinError <<- OdinError +1; OdinError_ids[OdinError] <<-  all_biomod_ids[i]; OdinError_messages[OdinError] <<- as.character(conditionMessage(m))})
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
 # possibly connected to power function                                                                                                                                                                                              38: deriv)


"\ninitial(GLCi) <- GLCi_init\nGLCi_init <- user(0)\ninitial(G6P) <- G6P_init\nG6P_init <- user(0)\ninitial(F6P) <- F6P_init\nF6P_init <- user(0)\ninitial(F16P) <- F16P_init\nF16P_init <- user(0)\ninitial(TRIO) <- TRIO_init\nTRIO_init <- user(0)\ninitial(BPG) <- BPG_init\nBPG_init <- user(0)\ninitial(P3G) <- P3G_init\nP3G_init <- user(0)\ninitial(P2G) <- P2G_init\nP2G_init <- user(0)\ninitial(PEP) <- PEP_init\nPEP_init <- user(0)\ninitial(PYR) <- PYR_init\nPYR_init <- user(0)\ninitial(ACE) <- ACE_init\nACE_init <- user(0)\ninitial(P) <- P_init\nP_init <- user(0)\ninitial(NAD) <- NAD_init\nNAD_init <- user(0)\ninitial(NADH) <- NADH_init\nNADH_init <- user(0)\ninitial(Glyc) <- Glyc_init\nGlyc_init <- user(0)\ninitial(Trh) <- Trh_init\nTrh_init <- user(0)\ninitial(CO2) <- CO2_init\nCO2_init <- user(0)\ninitial(SUCC) <- SUCC_init\nSUCC_init <- user(0)\ninitial(GLCo) <- GLCo_init\nGLCo_init <- user(0)\ninitial(ETOH) <- ETOH_init\nETOH_init <- user(0)\ninitial(GLY) <- GLY_init\nGLY_init <- user(0)\ninitial(ATP) <- ATP_init\nATP_init <- (P - ADP) / 2\ninitial(ADP) <- ADP_init\nADP_init <- (SUM_P - ((P)^(2) * (1 - 4 * KeqAK) + 2 * SUM_P * P * (4 * KeqAK - 1) + (SUM_P)^(2))^(0.5)) / (1 - 4 * KeqAK)\ninitial(AMP) <- AMP_init\nAMP_init <- SUM_P - ATP - ADP\ninitial(SUM_P) <- SUM_P_init\nSUM_P_init <- user(0)\ninitial(F26BP) <- F26BP_init\nF26BP_init <- user(0)\nVmGLK <- 226.452\nKmGLKGLCi <- 0.08\nKmGLKATP <- 0.15\nKeqGLK <- 3800\nKmGLKG6P <- 30\nKmGLKADP <- 0.23\nVmPGI_2 <- 339.677\nKmPGIG6P_2 <- 1.4\nKeqPGI_2 <- 0.314\nKmPGIF6P_2 <- 0.3\nKGLYCOGEN_3 <- 6\nKTREHALOSE <- 2.4\nVmPFK <- 182.903\nVmALD <- 322.258\nKmALDF16P <- 0.3\nKeqALD <- 0.069\nKmALDGAP <- 2\nKmALDDHAP <- 2.4\nKmALDGAPi <- 10\nVmGAPDHf <- 1184.52\nKmGAPDHGAP <- 0.21\nKmGAPDHNAD <- 0.09\nVmGAPDHr <- 6549.8\nKmGAPDHBPG <- 0.0098\nKmGAPDHNADH <- 0.06\nVmPGK <- 1306.45\nKmPGKP3G <- 0.53\nKmPGKATP <- 0.3\nKeqPGK <- 3200\nKmPGKBPG <- 0.003\nKmPGKADP <- 0.2\nVmPGM <- 2525.81\nKmPGMP3G <- 1.2\nKeqPGM <- 0.19\nKmPGMP2G <- 0.08\nVmENO <- 365.806\nKmENOP2G <- 0.04\nKeqENO <- 6.7\nKmENOPEP <- 0.5\nVmPYK <- 1088.71\nKmPYKPEP <- 0.14\nKmPYKADP <- 0.53\nKeqPYK <- 6500\nKmPYKPYR <- 21\nKmPYKATP <- 1.5\nVmPDC <- 174.194\nnPDC <- 1.9\nKmPDCPYR <- 4.33\nKSUCC <- 21.4\nVmGLT <- 97.264\nKmGLTGLCo <- 1.1918\nKeqGLT <- 1\nKmGLTGLCi <- 1.1918\nVmADH <- 810\nKiADHNAD <- 0.92\nKmADHETOH <- 17\nKeqADH <- 6.9e-05\nKmADHNAD <- 0.17\nKmADHNADH <- 0.11\nKiADHNADH <- 0.031\nKmADHACE <- 1.11\nKiADHACE <- 1.1\nKiADHETOH <- 90\nVmG3PDH <- 70.15\nKmG3PDHDHAP <- 0.4\nKmG3PDHNADH <- 0.023\nKeqG3PDH <- 4300\nKmG3PDHGLY <- 1\nKmG3PDHNAD <- 0.93\nKATPASE <- 33.7\nderiv(GLCi) <- 0 - cytosol * VmGLK / (KmGLKGLCi * KmGLKATP) * (GLCi * ATP - G6P * ADP / KeqGLK) / ((1 + GLCi / KmGLKGLCi + G6P / KmGLKG6P) * (1 + ATP / KmGLKATP + ADP / KmGLKADP)) + VmGLT / KmGLTGLCo * (GLCo - GLCi / KeqGLT) / (1 + GLCo / KmGLTGLCo + GLCi / KmGLTGLCi + 0.91 * GLCo * GLCi / (KmGLTGLCo * KmGLTGLCi))\nderiv(G6P) <- 0 + cytosol * VmGLK / (KmGLKGLCi * KmGLKATP) * (GLCi * ATP - G6P * ADP / KeqGLK) / ((1 + GLCi / KmGLKGLCi + G6P / KmGLKG6P) * (1 + ATP / KmGLKATP + ADP / KmGLKADP)) - cytosol * VmPGI_2 / KmPGIG6P_2 * (G6P - F6P / KeqPGI_2) / (1 + G6P / KmPGIG6P_2 + F6P / KmPGIF6P_2) - cytosol * KGLYCOGEN_3 - cytosol * KTREHALOSE\n

deriv(F6P) <- 0 + cytosol * VmPGI_2 / KmPGIG6P_2 * (G6P - F6P / KeqPGI_2) / (1 + G6P / KmPGIG6P_2 + F6P / KmPGIF6P_2) - cytosol * VmPFK * gR * (F6P / KmPFKF6P) * (ATP / KmPFKATP) * R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P) / ((R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P))^(2) + L_PFK(Lzero, CiPFKATP, KiPFKATP, CPFKAMP, KPFKAMP, CPFKF26BP, KPFKF26BP, CPFKF16BP, KPFKF16BP, ATP, AMP, F16P, F26BP) * (T_PFK(CPFKATP, KmPFKATP, ATP))^(2))

R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P)
get translated into
(1 +  F6P / KmPFK F6PP +  ATP /  KmPFK ATPP +  gR * ( F6P / KmPFK F6PP) * ( ATP /  KmPFK ATPP))

\nderiv(F16P) <- 0 + cytosol * VmPFK * gR * (F6P / KmPFKF6P) * (ATP / KmPFKATP) * R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P) / ((R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P))^(2) + L_PFK(Lzero, CiPFKATP, KiPFKATP, CPFKAMP, KPFKAMP, CPFKF26BP, KPFKF26BP, CPFKF16BP, KPFKF16BP, ATP, AMP, F16P, F26BP) * (T_PFK(CPFKATP, KmPFKATP, ATP))^(2)) - cytosol * VmALD / KmALDF16P * (F16P - KeqTPI / (1 + KeqTPI) * TRIO * (1 / (1 + KeqTPI)) * TRIO / KeqALD) / (1 + F16P / KmALDF16P + KeqTPI / (1 + KeqTPI) * TRIO / KmALDGAP + 1 / (1 + KeqTPI) * TRIO / KmALDDHAP + KeqTPI / (1 + KeqTPI) * TRIO * (1 / (1 + KeqTPI)) * TRIO / (KmALDGAP * KmALDDHAP) + F16P * (KeqTPI / (1 + KeqTPI)) * TRIO / (KmALDGAPi * KmALDF16P))\nderiv(TRIO) <- 0 + cytosol * VmALD / KmALDF16P * (F16P - KeqTPI / (1 + KeqTPI) * TRIO * (1 / (1 + KeqTPI)) * TRIO / KeqALD) / (1 + F16P / KmALDF16P + KeqTPI / (1 + KeqTPI) * TRIO / KmALDGAP + 1 / (1 + KeqTPI) * TRIO / KmALDDHAP + KeqTPI / (1 + KeqTPI) * TRIO * (1 / (1 + KeqTPI)) * TRIO / (KmALDGAP * KmALDDHAP) + F16P * (KeqTPI / (1 + KeqTPI)) * TRIO / (KmALDGAPi * KmALDF16P)) - cytosol * (VmGAPDHf * (KeqTPI / (1 + KeqTPI)) * TRIO * NAD / (KmGAPDHGAP * KmGAPDHNAD) - VmGAPDHr * BPG * NADH / (KmGAPDHBPG * KmGAPDHNADH)) / ((1 + KeqTPI / (1 + KeqTPI) * TRIO / KmGAPDHGAP + BPG / KmGAPDHBPG) * (1 + NAD / KmGAPDHNAD + NADH / KmGAPDHNADH)) - cytosol * VmG3PDH / (KmG3PDHDHAP * KmG3PDHNADH) * (1 / (1 + KeqTPI) * TRIO * NADH - GLY * NAD / KeqG3PDH) / ((1 + 1 / (1 + KeqTPI) * TRIO / KmG3PDHDHAP + GLY / KmG3PDHGLY) * (1 + NADH / KmG3PDHNADH + NAD / KmG3PDHNAD))\nderiv(BPG) <- 0 + cytosol * (VmGAPDHf * (KeqTPI / (1 + KeqTPI)) * TRIO * NAD / (KmGAPDHGAP * KmGAPDHNAD) - VmGAPDHr * BPG * NADH / (KmGAPDHBPG * KmGAPDHNADH)) / ((1 + KeqTPI / (1 + KeqTPI) * TRIO / KmGAPDHGAP + BPG / KmGAPDHBPG) * (1 + NAD / KmGAPDHNAD + NADH / KmGAPDHNADH)) - cytosol * VmPGK / (KmPGKP3G * KmPGKATP) * (KeqPGK * BPG * ADP - P3G * ATP) / ((1 + BPG / KmPGKBPG + P3G / KmPGKP3G) * (1 + ATP / KmPGKATP + ADP / KmPGKADP))\nderiv(P3G) <- 0 + cytosol * VmPGK / (KmPGKP3G * KmPGKATP) * (KeqPGK * BPG * ADP - P3G * ATP) / ((1 + BPG / KmPGKBPG + P3G / KmPGKP3G) * (1 + ATP / KmPGKATP + ADP / KmPGKADP)) - cytosol * VmPGM / KmPGMP3G * (P3G - P2G / KeqPGM) / (1 + P3G / KmPGMP3G + P2G / KmPGMP2G)\nderiv(P2G) <- 0 + cytosol * VmPGM / KmPGMP3G * (P3G - P2G / KeqPGM) / (1 + P3G / KmPGMP3G + P2G / KmPGMP2G) - cytosol * VmENO / KmENOP2G * (P2G - PEP / KeqENO) / (1 + P2G / KmENOP2G + PEP / KmENOPEP)\nderiv(PEP) <- 0 + cytosol * VmENO / KmENOP2G * (P2G - PEP / KeqENO) / (1 + P2G / KmENOP2G + PEP / KmENOPEP) - cytosol * VmPYK / (KmPYKPEP * KmPYKADP) * (PEP * ADP - PYR * ATP / KeqPYK) / ((1 + PEP / KmPYKPEP + PYR / KmPYKPYR) * (1 + ATP / KmPYKATP + ADP / KmPYKADP))\nderiv(PYR) <- 0 + cytosol * VmPYK / (KmPYKPEP * KmPYKADP) * (PEP * ADP - PYR * ATP / KeqPYK) / ((1 + PEP / KmPYKPEP + PYR / KmPYKPYR) * (1 + ATP / KmPYKATP + ADP / KmPYKADP)) - cytosol * VmPDC * ((PYR)^(nPDC) / (KmPDCPYR)^(nPDC)) / (1 + (PYR)^(nPDC) / (KmPDCPYR)^(nPDC))\nderiv(ACE) <- 0 + cytosol * VmPDC * ((PYR)^(nPDC) / (KmPDCPYR)^(nPDC)) / (1 + (PYR)^(nPDC) / (KmPDCPYR)^(nPDC)) - cytosol * KSUCC * ACE - -cytosol * (VmADH / (KiADHNAD * KmADHETOH) * (NAD * ETOH - NADH * ACE / KeqADH) / (1 + NAD / KiADHNAD + KmADHNAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * ACE / (KiADHNADH * KmADHACE) + NADH / KiADHNADH + NAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * NAD * ACE / (KiADHNAD * KiADHNADH * KmADHACE) + KmADHNAD * ETOH * NADH / (KiADHNAD * KmADHETOH * KiADHNADH) + NADH * ACE / (KiADHNADH * KmADHACE) + NAD * ETOH * ACE / (KiADHNAD * KmADHETOH * KiADHACE) + ETOH * NADH * ACE / (KiADHETOH * KiADHNADH * KmADHACE)))\nderiv(P) <- 0 - cytosol * VmGLK / (KmGLKGLCi * KmGLKATP) * (GLCi * ATP - G6P * ADP / KeqGLK) / ((1 + GLCi / KmGLKGLCi + G6P / KmGLKG6P) * (1 + ATP / KmGLKATP + ADP / KmGLKADP)) - cytosol * KGLYCOGEN_3 - cytosol * KTREHALOSE - cytosol * VmPFK * gR * (F6P / KmPFKF6P) * (ATP / KmPFKATP) * R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P) / ((R_PFK(KmPFKF6P, KmPFKATP, gR, ATP, F6P))^(2) + L_PFK(Lzero, CiPFKATP, KiPFKATP, CPFKAMP, KPFKAMP, CPFKF26BP, KPFKF26BP, CPFKF16BP, KPFKF16BP, ATP, AMP, F16P, F26BP) * (T_PFK(CPFKATP, KmPFKATP, ATP))^(2)) + cytosol * VmPGK / (KmPGKP3G * KmPGKATP) * (KeqPGK * BPG * ADP - P3G * ATP) / ((1 + BPG / KmPGKBPG + P3G / KmPGKP3G) * (1 + ATP / KmPGKATP + ADP / KmPGKADP)) + cytosol * VmPYK / (KmPYKPEP * KmPYKADP) * (PEP * ADP - PYR * ATP / KeqPYK) / ((1 + PEP / KmPYKPEP + PYR / KmPYKPYR) * (1 + ATP / KmPYKATP + ADP / KmPYKADP)) - cytosol * KSUCC * ACE - cytosol * KATPASE * ATP\nderiv(NAD) <- 0 - cytosol * (VmGAPDHf * (KeqTPI / (1 + KeqTPI)) * TRIO * NAD / (KmGAPDHGAP * KmGAPDHNAD) - VmGAPDHr * BPG * NADH / (KmGAPDHBPG * KmGAPDHNADH)) / ((1 + KeqTPI / (1 + KeqTPI) * TRIO / KmGAPDHGAP + BPG / KmGAPDHBPG) * (1 + NAD / KmGAPDHNAD + NADH / KmGAPDHNADH)) - cytosol * KSUCC * ACE + -cytosol * (VmADH / (KiADHNAD * KmADHETOH) * (NAD * ETOH - NADH * ACE / KeqADH) / (1 + NAD / KiADHNAD + KmADHNAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * ACE / (KiADHNADH * KmADHACE) + NADH / KiADHNADH + NAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * NAD * ACE / (KiADHNAD * KiADHNADH * KmADHACE) + KmADHNAD * ETOH * NADH / (KiADHNAD * KmADHETOH * KiADHNADH) + NADH * ACE / (KiADHNADH * KmADHACE) + NAD * ETOH * ACE / (KiADHNAD * KmADHETOH * KiADHACE) + ETOH * NADH * ACE / (KiADHETOH * KiADHNADH * KmADHACE))) + cytosol * VmG3PDH / (KmG3PDHDHAP * KmG3PDHNADH) * (1 / (1 + KeqTPI) * TRIO * NADH - GLY * NAD / KeqG3PDH) / ((1 + 1 / (1 + KeqTPI) * TRIO / KmG3PDHDHAP + GLY / KmG3PDHGLY) * (1 + NADH / KmG3PDHNADH + NAD / KmG3PDHNAD))\nderiv(NADH) <- 0 + cytosol * (VmGAPDHf * (KeqTPI / (1 + KeqTPI)) * TRIO * NAD / (KmGAPDHGAP * KmGAPDHNAD) - VmGAPDHr * BPG * NADH / (KmGAPDHBPG * KmGAPDHNADH)) / ((1 + KeqTPI / (1 + KeqTPI) * TRIO / KmGAPDHGAP + BPG / KmGAPDHBPG) * (1 + NAD / KmGAPDHNAD + NADH / KmGAPDHNADH)) + cytosol * KSUCC * ACE - -cytosol * (VmADH / (KiADHNAD * KmADHETOH) * (NAD * ETOH - NADH * ACE / KeqADH) / (1 + NAD / KiADHNAD + KmADHNAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * ACE / (KiADHNADH * KmADHACE) + NADH / KiADHNADH + NAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * NAD * ACE / (KiADHNAD * KiADHNADH * KmADHACE) + KmADHNAD * ETOH * NADH / (KiADHNAD * KmADHETOH * KiADHNADH) + NADH * ACE / (KiADHNADH * KmADHACE) + NAD * ETOH * ACE / (KiADHNAD * KmADHETOH * KiADHACE) + ETOH * NADH * ACE / (KiADHETOH * KiADHNADH * KmADHACE))) - cytosol * VmG3PDH / (KmG3PDHDHAP * KmG3PDHNADH) * (1 / (1 + KeqTPI) * TRIO * NADH - GLY * NAD / KeqG3PDH) / ((1 + 1 / (1 + KeqTPI) * TRIO / KmG3PDHDHAP + GLY / KmG3PDHGLY) * (1 + NADH / KmG3PDHNADH + NAD / KmG3PDHNAD))\nderiv(Glyc) <- 0 + cytosol * KGLYCOGEN_3\nderiv(Trh) <- 0 + cytosol * KTREHALOSE\nderiv(CO2) <- 0 + cytosol * VmPDC * ((PYR)^(nPDC) / (KmPDCPYR)^(nPDC)) / (1 + (PYR)^(nPDC) / (KmPDCPYR)^(nPDC))\nderiv(SUCC) <- 0 + cytosol * KSUCC * ACE\nderiv(GLCo) <- 0 - VmGLT / KmGLTGLCo * (GLCo - GLCi / KeqGLT) / (1 + GLCo / KmGLTGLCo + GLCi / KmGLTGLCi + 0.91 * GLCo * GLCi / (KmGLTGLCo * KmGLTGLCi))\nderiv(ETOH) <- 0 + -cytosol * (VmADH / (KiADHNAD * KmADHETOH) * (NAD * ETOH - NADH * ACE / KeqADH) / (1 + NAD / KiADHNAD + KmADHNAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * ACE / (KiADHNADH * KmADHACE) + NADH / KiADHNADH + NAD * ETOH / (KiADHNAD * KmADHETOH) + KmADHNADH * NAD * ACE / (KiADHNAD * KiADHNADH * KmADHACE) + KmADHNAD * ETOH * NADH / (KiADHNAD * KmADHETOH * KiADHNADH) + NADH * ACE / (KiADHNADH * KmADHACE) + NAD * ETOH * ACE / (KiADHNAD * KmADHETOH * KiADHACE) + ETOH * NADH * ACE / (KiADHETOH * KiADHNADH * KmADHACE)))\nderiv(GLY) <- 0 + cytosol * VmG3PDH / (KmG3PDHDHAP * KmG3PDHNADH) * (1 / (1 + KeqTPI) * TRIO * NADH - GLY * NAD / KeqG3PDH) / ((1 + 1 / (1 + KeqTPI) * TRIO / KmG3PDHDHAP + GLY / KmG3PDHGLY) * (1 + NADH / KmG3PDHNADH + NAD / KmG3PDHNAD))\nderiv(ATP) <- 0 + 0\nderiv(ADP) <- 0 + 0\nderiv(AMP) <- 0 + 0\nderiv(SUM_P) <- 0\nderiv(F26BP) <- 0\ngR <- user(5.12)\nKmPFKF6P <- user(0.1)\nKmPFKATP <- user(0.71)\nLzero <- user(0.66)\nCiPFKATP <- user(100)\nKiPFKATP <- user(0.65)\nCPFKAMP <- user(0.0845)\nKPFKAMP <- user(0.0995)\nCPFKF26BP <- user(0.0174)\nKPFKF26BP <- user(0.000682)\nCPFKF16BP <- user(0.397)\nKPFKF16BP <- user(0.111)\nCPFKATP <- user(3)\nKeqAK <- user(0.45)\nKeqTPI <- user(0.045)\nextracellular <- 1\ncytosol <- 1"



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

