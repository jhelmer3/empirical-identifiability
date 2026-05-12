
# Source: https://naep-research.airprojects.org/portals/0/edsurvey_a_users_guide/_book/index.html
library(EdSurvey)

downloadECLS_K(root = here::here("..", "Data", "ECLS-K"),
               years = 2011)

# I did have to extract the .dat file from the zipped folder and put it in the "2011" folder before this step

eclsk11 <- readECLS_K2011(path = here::here("..", "Data", "ECLS-K", "ECLS_K", "2011"))
eclsk <- getData(data = eclsk11, varnames = colnames(eclsk11))
saveRDS(eclsk, here::here("..", "Data", "ECLS-K", "ECLS-K.rds"))
save(eclsk, file = "eclsk2011.RData")