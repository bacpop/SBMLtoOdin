library(odin)

# SIR model example (Malaysia)
importSBMLfromBioModels("BIOMD0000000982","../LawModel.R")

model_generator <- odin::odin("../LawModel.R")
LawModel <- model_generator$new()
LawModel_res <- LawModel$run(0:150)


par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
cols <- c(S = "#000000", I = "#E69F00", R = "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
matplot(LawModel_res[, 1], LawModel_res[, c(3,4)], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = cols[c(2,3)], lty = 1, ylim = c(0,7000))
legend("topright", lwd = 1, col = cols, legend = c("S", "I", "R"), bty = "n")


importSBMLfromBioModels("MODEL2307110001","../MothesModel.R")
model_generator_mothes <- odin::odin("../MothesModel.R",verbose = FALSE)
MothesModel <- model_generator_mothes$new()
MothesModel_res <- MothesModel$run(0:400)



par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
cols <- c(S = "#000000", I = "#E69F00", R = "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
matplot(MothesModel_res[, 1], MothesModel_res[, -1], xlab = "Time", ylab = "Number of individuals",
        type = "l", col = cols, lty = 1, ylim = c(0,40))
legend("topright", lwd = 1, col = cols, legend = colnames(MothesModel_res)[-1], bty = "n")

