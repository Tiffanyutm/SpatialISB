# [1] Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
# [2] Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., Hornik, K.(2025).  cluster: Cluster Analysis Basics and Extensions. R package version 2.1.8.1.
# [3] Kassambara A, Mundt F (2020). _factoextra: Extract and Visualize the Results of Multivariate Data Analyses_. doi:10.32614/CRAN.package.factoextra <https://doi.org/10.32614/CRAN.package.factoextra>, R package version 1.0.7, <https://CRAN.R-project.org/package=factoextra>.
# [4] Larmarange J (2025). _ggstats: Extension to 'ggplot2' for Plotting Stats_. doi:10.32614/CRAN.package.ggstats <https://doi.org/10.32614/CRAN.package.ggstats>, R package version 0.11.0, <https://CRAN.Rproject.org/package=ggstats>.
# [5] Hennig C (2024). _fpc: Flexible Procedures for Clustering_. doi:10.32614/CRAN.package.fpc <https://doi.org/10.32614/CRAN.package.fpc>, R package version 2.2-13, <https://CRAN.R-project.org/package=fpc>.
# [6] Pedersen T (2025). _patchwork: The Composer of Plots_. doi:10.32614/CRAN.package.patchwork <https://doi.org/10.32614/CRAN.package.patchwork>, R package version 1.3.2, <https://CRAN.Rproject.org/package=patchwork>.
# [7] Kuhn, M. (2008). Building Predictive Models in R Using the caretnPackage. Journal of Statistical Software, 28(5), 1–26. https://doi.org/10.18637/jss.v028.i05
# [8] R Core Team (2025). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.<https://www.R-project.org/>.
# [9] Kassambara A (2023). _rstatix: Pipe-Friendly Framework for Basic Statistical Tests_. doi:10.32614/CRAN.package.rstatix <https://doi.org/10.32614/CRAN.package.rstatix>, R package version 0.7.2, <https://CRAN.R-project.org/package=rstatix>.

#### Re-do ###### with "don't know"

library(tidyverse) #[1]

# import 2024 Great Lakes Regional Poll data
survey <- read_csv("2024_ICJ_Telephone_Poll.csv")

# subset data to only include demographic and geographic variables, perceptions, behavioural, recreational engagement
survey <- survey[,c(1,3,53, 54, 55, 56, 57, 58, 59, 117, 118)]
glimpse(survey) # see datatypes

# Rename columns
col_names <- c("Zipcode", "JURISDICTION",
               "Q18M1","Q18M2","Q18M3","Q18M4","Q18M5","Q18M6",
               "Q19",
               "D1",
               "D2"
)
colnames(survey) <- col_names

# Collapse oldest age categories to 55+
survey$D1[survey$D1 == "55-64"] <- "55+"
survey$D1[survey$D1 == "65-74"] <- "55+"
survey$D1[survey$D1 == "75 & older"] <- "55+"

# Encode data function (source: R-bloggers Anindya Mozumdar) [2]
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

# Jurisdiction
survey$JURISDICTION <- encode_ordinal(survey$JURISDICTION,
                                      order = c("Illinois", "Indiana", "Michigan", "Minnesota", "New York", "Ohio", "Ontario", "Pennsylvania", "Wisconsin"))
# Q19
survey$Q19 <- encode_ordinal(survey$Q19,
                             order = c("Don't know", "Never", "Rarely", "Sometimes","Most of the time","Always"))-1

# D1
survey$D1 <- encode_ordinal(survey$D1,
                            order = c("Refused", "18-34", "35-44", "45-54","55+"))-2
# D2
survey$D2 <- encode_ordinal(survey$D2,
                            order = c("Refused", "Some high school or less", "Graduated high school", "Some post secondary (college, university)","Graduated university / college"))-2

# Replace zeros with NA 
survey[survey == -1] <- NA # replace -1 with NA

# Create binary columns where 0: minimal/no contact with water, 1: contact/indirect contact with water for each Q18M
survey$Q18_B1 <- survey$Q18M1
survey$Q18_B1[survey$Q18_B1 == "Birdwatching"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Walking / hiking"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Hunting"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Tent camping / RV camping"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Foraging"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Wild rice harvesting"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Cultural activities"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Political action"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "With friends/family/pets"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Don't know"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "-9998.00"] <- 0 
survey$Q18_B1[survey$Q18_B1 == "Swimming"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Fishing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Paddleboarding"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Sailing / windsurfing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Motorboating"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Kayaking"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Boating"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "Canoeing"] <- 1 
survey$Q18_B1[survey$Q18_B1 == "#NULL!"] <- 0 

survey$Q18_B2 <- survey$Q18M2
survey$Q18_B2[survey$Q18_B2 == "Birdwatching"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Walking / hiking"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Hunting"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Tent camping / RV camping"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Foraging"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Wild rice harvesting"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Cultural activities"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Political action"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "With friends/family/pets"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Don't know"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "-9998.00"] <- 0 
survey$Q18_B2[survey$Q18_B2 == "Swimming"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Fishing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Paddleboarding"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Sailing / windsurfing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Motorboating"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Kayaking"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Boating"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "Canoeing"] <- 1 
survey$Q18_B2[survey$Q18_B2 == "#NULL!"] <- 0 

survey$Q18_B3 <- survey$Q18M3
survey$Q18_B3[survey$Q18_B3 == "Birdwatching"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Walking / hiking"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Hunting"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Tent camping / RV camping"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Foraging"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Wild rice harvesting"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Cultural activities"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Political action"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "With friends/family/pets"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Don't know"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "-9998.00"] <- 0 
survey$Q18_B3[survey$Q18_B3 == "Swimming"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Fishing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Paddleboarding"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Sailing / windsurfing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Motorboating"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Kayaking"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Boating"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "Canoeing"] <- 1 
survey$Q18_B3[survey$Q18_B3 == "#NULL!"] <- 0 

survey$Q18_B4 <- survey$Q18M4
survey$Q18_B4[survey$Q18_B4 == "Birdwatching"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Walking / hiking"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Hunting"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Tent camping / RV camping"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Foraging"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Wild rice harvesting"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Cultural activities"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Political action"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "With friends/family/pets"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Don't know"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "-9998.00"] <- 0 
survey$Q18_B4[survey$Q18_B4 == "Swimming"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Fishing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Paddleboarding"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Sailing / windsurfing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Motorboating"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Kayaking"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Boating"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "Canoeing"] <- 1 
survey$Q18_B4[survey$Q18_B4 == "#NULL!"] <- 0 

survey$Q18_B5 <- survey$Q18M5
survey$Q18_B5[survey$Q18_B5 == "Birdwatching"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Walking / hiking"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Hunting"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Tent camping / RV camping"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Foraging"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Wild rice harvesting"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Cultural activities"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Political action"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "With friends/family/pets"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Don't know"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "-9998.00"] <- 0 
survey$Q18_B5[survey$Q18_B5 == "Swimming"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Fishing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Paddleboarding"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Sailing / windsurfing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Motorboating"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Kayaking"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Boating"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "Canoeing"] <- 1 
survey$Q18_B5[survey$Q18_B5 == "#NULL!"] <- 0 

survey$Q18_B6 <- survey$Q18M6
survey$Q18_B6[survey$Q18_B6 == "Birdwatching"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Walking / hiking"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Hunting"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Tent camping / RV camping"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Foraging"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Wild rice harvesting"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Cultural activities"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Political action"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "With friends/family/pets"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Don't know"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "-9998.00"] <- 0 
survey$Q18_B6[survey$Q18_B6 == "Swimming"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Fishing"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Paddleboarding"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Sailing / windsurfing"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Motorboating"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Kayaking"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Boating"] <- 1 
survey$Q18_B6[survey$Q18_B6 == "Canoeing"] <- 1
survey$Q18_B6[survey$Q18_B6 == "#NULL!"] <- 0 

# Save a csv file
write.csv(survey, file="Re-doChpt2_Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# import cleaned data
survey <- read_csv("Re-doChpt2_Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# combine all columns into one yes/no binary
survey$Q18_B1 <- as.numeric(survey$Q18_B1)
survey$Q18_B3 <- as.numeric(survey$Q18_B3)
survey$Q18_B4 <- as.numeric(survey$Q18_B4)
survey$Q18_B5 <- as.numeric(survey$Q18_B5)
survey$Q18_B6 <- as.numeric(survey$Q18_B6)
survey$Q18_B <- rowSums(survey[c(13, 14, 15, 16, 17, 18)])
survey$Q18_B <- with(survey, ifelse(survey$Q18_B > 0, 1, 0))

# categorize recreational activities
survey$Q18_C1 <- survey$Q18M1
survey$Q18_C1[survey$Q18_C1 == "Birdwatching"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Walking / hiking"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Hunting"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Tent camping / RV camping"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Foraging"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Wild rice harvesting"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Cultural activities"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Political action"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "With friends/family/pets"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Don't know"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "-9998.00"] <- 0 
survey$Q18_C1[survey$Q18_C1 == "Swimming"] <- 1 
survey$Q18_C1[survey$Q18_C1 == "Fishing"] <- 2 
survey$Q18_C1[survey$Q18_C1 == "Paddleboarding"] <- 3 
survey$Q18_C1[survey$Q18_C1 == "Sailing / windsurfing"] <- 3 
survey$Q18_C1[survey$Q18_C1 == "Motorboating"] <- 4 
survey$Q18_C1[survey$Q18_C1 == "Kayaking"] <- 4
survey$Q18_C1[survey$Q18_C1 == "Boating"] <- 4
survey$Q18_C1[survey$Q18_C1 == "Canoeing"] <- 4 
survey$Q18_C1[survey$Q18_C1 == "#NULL!"] <- 0 

survey$Q18_C2 <- survey$Q18M2
survey$Q18_C2[survey$Q18_C2 == "Birdwatching"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Walking / hiking"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Hunting"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Tent camping / RV camping"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Foraging"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Wild rice harvesting"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Cultural activities"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Political action"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "With friends/family/pets"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Don't know"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "-9998.00"] <- 0 
survey$Q18_C2[survey$Q18_C2 == "Swimming"] <- 1 
survey$Q18_C2[survey$Q18_C2 == "Fishing"] <- 2 
survey$Q18_C2[survey$Q18_C2 == "Paddleboarding"] <- 3 
survey$Q18_C2[survey$Q18_C2 == "Sailing / windsurfing"] <- 3 
survey$Q18_C2[survey$Q18_C2 == "Motorboating"] <- 4 
survey$Q18_C2[survey$Q18_C2 == "Kayaking"] <- 4
survey$Q18_C2[survey$Q18_C2 == "Boating"] <- 4
survey$Q18_C2[survey$Q18_C2 == "Canoeing"] <- 4 
survey$Q18_C2[survey$Q18_C2 == "#NULL!"] <- 0 

survey$Q18_C3 <- survey$Q18M3
survey$Q18_C3[survey$Q18_C3 == "Birdwatching"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Walking / hiking"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Hunting"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Tent camping / RV camping"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Foraging"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Wild rice harvesting"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Cultural activities"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Political action"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "With friends/family/pets"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Don't know"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "-9998.00"] <- 0 
survey$Q18_C3[survey$Q18_C3 == "Swimming"] <- 1 
survey$Q18_C3[survey$Q18_C3 == "Fishing"] <- 2 
survey$Q18_C3[survey$Q18_C3 == "Paddleboarding"] <- 3 
survey$Q18_C3[survey$Q18_C3 == "Sailing / windsurfing"] <- 3 
survey$Q18_C3[survey$Q18_C3 == "Motorboating"] <- 4 
survey$Q18_C3[survey$Q18_C3 == "Kayaking"] <- 4
survey$Q18_C3[survey$Q18_C3 == "Boating"] <- 4
survey$Q18_C3[survey$Q18_C3 == "Canoeing"] <- 4 
survey$Q18_C3[survey$Q18_C3 == "#NULL!"] <- 0 

survey$Q18_C4 <- survey$Q18M4
survey$Q18_C4[survey$Q18_C4 == "Birdwatching"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Walking / hiking"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Hunting"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Tent camping / RV camping"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Foraging"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Wild rice harvesting"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Cultural activities"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Political action"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "With friends/family/pets"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Don't know"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "-9998.00"] <- 0 
survey$Q18_C4[survey$Q18_C4 == "Swimming"] <- 1 
survey$Q18_C4[survey$Q18_C4 == "Fishing"] <- 2 
survey$Q18_C4[survey$Q18_C4 == "Paddleboarding"] <- 3 
survey$Q18_C4[survey$Q18_C4 == "Sailing / windsurfing"] <- 3 
survey$Q18_C4[survey$Q18_C4 == "Motorboating"] <- 4 
survey$Q18_C4[survey$Q18_C4 == "Kayaking"] <- 4
survey$Q18_C4[survey$Q18_C4 == "Boating"] <- 4
survey$Q18_C4[survey$Q18_C4 == "Canoeing"] <- 4 
survey$Q18_C4[survey$Q18_C4 == "#NULL!"] <- 0 

survey$Q18_C5 <- survey$Q18M5
survey$Q18_C5[survey$Q18_C5 == "Birdwatching"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Walking / hiking"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Hunting"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Tent camping / RV camping"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Foraging"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Wild rice harvesting"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Cultural activities"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Political action"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "With friends/family/pets"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Don't know"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "-9998.00"] <- 0 
survey$Q18_C5[survey$Q18_C5 == "Swimming"] <- 1 
survey$Q18_C5[survey$Q18_C5 == "Fishing"] <- 2 
survey$Q18_C5[survey$Q18_C5 == "Paddleboarding"] <- 3 
survey$Q18_C5[survey$Q18_C5 == "Sailing / windsurfing"] <- 3 
survey$Q18_C5[survey$Q18_C5 == "Motorboating"] <- 4 
survey$Q18_C5[survey$Q18_C5 == "Kayaking"] <- 4
survey$Q18_C5[survey$Q18_C5 == "Boating"] <- 4
survey$Q18_C5[survey$Q18_C5 == "Canoeing"] <- 4 
survey$Q18_C5[survey$Q18_C5 == "#NULL!"] <- 0 

survey$Q18_C6 <- survey$Q18M6
survey$Q18_C6[survey$Q18_C6 == "Birdwatching"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Walking / hiking"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Hunting"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Tent camping / RV camping"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Foraging"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Wild rice harvesting"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Cultural activities"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Political action"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "With friends/family/pets"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Don't know"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Scuba diving / snorkeling"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "-9998.00"] <- 0 
survey$Q18_C6[survey$Q18_C6 == "Swimming"] <- 1 
survey$Q18_C6[survey$Q18_C6 == "Fishing"] <- 2 
survey$Q18_C6[survey$Q18_C6 == "Paddleboarding"] <- 3 
survey$Q18_C6[survey$Q18_C6 == "Sailing / windsurfing"] <- 3 
survey$Q18_C6[survey$Q18_C6 == "Motorboating"] <- 4 
survey$Q18_C6[survey$Q18_C6 == "Kayaking"] <- 4
survey$Q18_C6[survey$Q18_C6 == "Boating"] <- 4
survey$Q18_C6[survey$Q18_C6 == "Canoeing"] <- 4 
survey$Q18_C6[survey$Q18_C6 == "#NULL!"] <- 0 


# binary yes/no for each type of water based recreational activity in question 18
survey$Q18_swimming <- with(survey, ifelse(Q18_C1 == 1 | Q18_C2 == 1 | Q18_C3 == 1 | Q18_C4 == 1 | Q18_C5 == 1 | Q18_C6 == 1, 1, 0))
survey$Q18_fishing <- with(survey, ifelse(Q18_C1 == 2 | Q18_C2 == 2 | Q18_C3 == 2 | Q18_C4 == 2 | Q18_C5 == 2 | Q18_C6 == 2, 1, 0))
survey$Q18_boarding <- with(survey, ifelse(Q18_C1 == 3 | Q18_C2 == 3 | Q18_C3 == 3 | Q18_C4 == 3  | Q18_C5 == 3 | Q18_C6 == 3, 1, 0))
survey$Q18_boating <- with(survey, ifelse(Q18_C1 == 4 | Q18_C2 == 4 | Q18_C3 == 4 | Q18_C4 == 4 | Q18_C5 == 4 | Q18_C6 == 4, 1, 0))

# subset data to remove the Q18s I don't need
survey <- survey[,c(1,2,3,10,11,12,19,26,27,28,29)]

write.csv(survey, file="Re-doChpt2_Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")
survey <- read_csv("Re-doChpt2_Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# subset data to only include post/zip code, information-seeking behaviours
surveyUS <- survey[,c(3,4,5,6,7,8,9,10,11,12)]
survey <- survey[,c(3,4,5,6,7,8,9,10,11,12)]

# Filter by Canadian/US Data
survey <- survey[complete.cases(survey),]
survey <- survey[survey$JURISDICTION == 7,]

surveyUS <- surveyUS[complete.cases(surveyUS),]
surveyUS <- surveyUS[surveyUS$JURISDICTION != 7, ]

# Re-name Canadian data
# Rename columns
col_names <- c("POSTAL_CODE","JURISDICTION","Q19",
               "D1", "D2", 
               "Q18_B",
               "Q18_swimming","Q18_fishing","Q18_boarding","Q18_boating"
               
)
colnames(survey) <- col_names

col_names <- c("Zip_Code","JURISDICTION","Q19",
               "D1", "D2", 
               "Q18_B",
               "Q18_swimming","Q18_fishing","Q18_boarding","Q18_boating"
               
)
colnames(surveyUS) <- col_names

# Save a csv file 
write.csv(survey, file="Chp2redo_Data_CAD.csv")
write.csv(surveyUS, file="Chp2redo_Data_US.csv")

# import postal/zip code data
fsa <- read_csv("CanadianPostalCodes202403.csv")
zipc <- read_csv("USZIPCodes202506.csv")

fsa <- distinct(fsa, POSTAL_CODE,.keep_all = TRUE)
zipc <- distinct(zipc, Zip_Code,.keep_all = TRUE)
zipc$Zip_Code <- as.numeric(zipc$Zip_Code)

# merge postal/zip code data for survey with longitude and latitude data

survey_combined <- merge(survey, fsa,
                         by.x = "POSTAL_CODE",
                         by.y = "POSTAL_CODE",
                         all.x = TRUE)

names(surveyUS)[names(surveyUS) == 'POSTAL_CODE'] <- 'Zip_Code'
survey_combined_US <- merge(surveyUS, zipc,
                            by.x = "Zip_Code",
                            by.y = "Zip_Code",
                            all.x = TRUE)

write.csv(survey_combined, file="Chp2redo_Data_CAD2.csv")
write.csv(survey_combined_US, file="Chp2redo_Data_US2.csv")

# re-combine the datasets
survey_combined_US <- survey_combined_US[,c(1,3,4,5,7,8,9,10,18,19)]
survey_combined <- survey_combined[,c(1,3,4,5,7,8,9,10,14,15)]

col_names <- c("POSTAL_ZIP", "Q19", "D1", "D2", 
               "Q18_swimming", "Q18_fishing", "Q18_boarding", "Q18_boating", 
              "Latitude","Longitude") # Rename columns
colnames(survey_combined_US) <- col_names
colnames(survey_combined) <- col_names

survey2 <- rbind(survey_combined_US, survey_combined)

## k-medoids clustering starts here ##

library(cluster) #[2]
library(factoextra) #[3]
library(ggstats) #[4]
library(fpc) #[5]
library(patchwork) #[6]

# subset data to only include demographics(age, education), ISB, lat/long
survey_d <- survey2[,c(2,3,4,9,10)]

# remove NAs - demographic
survey_d <- na.omit(survey_d)

# convert data types
survey_d$Q19 <- as.ordered(survey_d$Q19)
survey_d$D1 <- as.ordered(survey_d$D1)
survey_d$D2 <- as.ordered(survey_d$D2)
glimpse(survey_d)

# standardize and distance metric - demographics
gower_dis_d <- daisy(survey_d, metric = "gower",
                     type = list(ordered = 1:3,
                                 numeric = 4:5))

# pamk - silhouette and Calinski Harabasz index 
set.seed(123) # replicate 
k_med_d_asw <- pamk(data = gower_dis_d,
                    krange = 2:10,
                    criterion = "asw",
                    usepam = TRUE,
                    diss = TRUE)

set.seed(123) # replicate 
k_med_d_ch <- pamk(data = gower_dis_d,
                   krange = 2:10,
                   criterion = "ch",
                   usepam = TRUE,
                   diss = TRUE)

# Run Partitioning Around Medoids (PAM) - 2 to 10 - demographics D1, D2, Q19, Lat/Long
set.seed(123) # replicate 
k_med_d2 <- pam(x = gower_dis_d,
                k = 2,
                diss = TRUE)
set.seed(123)
k_med_d3 <- pam(x = gower_dis_d,
                k = 3,
                diss = TRUE)
set.seed(123)
k_med_d4 <- pam(x = gower_dis_d,
                k = 4,
                diss = TRUE)
set.seed(123)
k_med_d5 <- pam(x = gower_dis_d,
                k = 5,
                diss = TRUE)
set.seed(123)
k_med_d6 <- pam(x = gower_dis_d,
                k = 6,
                diss = TRUE)
set.seed(123)
k_med_d7 <- pam(x = gower_dis_d,
                k = 7,
                diss = TRUE)
set.seed(123)
k_med_d8 <- pam(x = gower_dis_d,
                k = 8,
                diss = TRUE)
set.seed(123)
k_med_d9 <- pam(x = gower_dis_d,
                k = 9,
                diss = TRUE)
set.seed(123)
k_med_d10 <- pam(x = gower_dis_d,
                 k = 10,
                 diss = TRUE)

# store data in a dataframe
silho_d <- data.frame(clusterr = 1:10)
silho_d$avgs <- k_med_d2$silinfo$avg.width
silho_d$avgs[3] <- k_med_d3$silinfo$avg.width
silho_d$avgs[4] <- k_med_d4$silinfo$avg.width
silho_d$avgs[5] <- k_med_d5$silinfo$avg.width
silho_d$avgs[6] <- k_med_d6$silinfo$avg.width
silho_d$avgs[7] <- k_med_d7$silinfo$avg.width
silho_d$avgs[8] <- k_med_d8$silinfo$avg.width
silho_d$avgs[9] <- k_med_d9$silinfo$avg.width
silho_d$avgs[10] <- k_med_d10$silinfo$avg.width
silho_d$avgs[1] <- 0

# Calinhara
set.seed(123) # replicate 
ch_d2 <- calinhara(gower_dis_d,k_med_d2$clustering)

set.seed(123) # replicate 
ch_d3 <- calinhara(gower_dis_d,k_med_d3$clustering)

set.seed(123) # replicate 
ch_d4 <- calinhara(gower_dis_d,k_med_d4$clustering)

set.seed(123) # replicate 
ch_d5 <- calinhara(gower_dis_d,k_med_d5$clustering)

set.seed(123) # replicate 
ch_d6 <- calinhara(gower_dis_d,k_med_d6$clustering)

set.seed(123) # replicate 
ch_d7 <- calinhara(gower_dis_d,k_med_d7$clustering)

set.seed(123) # replicate 
ch_d8 <- calinhara(gower_dis_d,k_med_d8$clustering)

set.seed(123) # replicate 
ch_d9 <- calinhara(gower_dis_d,k_med_d9$clustering)

set.seed(123) # replicate 
ch_d10 <- calinhara(gower_dis_d,k_med_d10$clustering)

# store data in a dataframe - demographic
ch_d <- data.frame(clusterr = 1:10)
ch_d$score <- ch_d2
ch_d$score[3] <- ch_d3
ch_d$score[4] <- ch_d4
ch_d$score[5] <- ch_d5
ch_d$score[6] <- ch_d6
ch_d$score[7] <- ch_d7
ch_d$score[8] <- ch_d8
ch_d$score[9] <- ch_d9
ch_d$score[10] <- ch_d10
ch_d$score[1] <- 0

# Plot avg silhouette, ch and cluster plot
d <- ggplot(silho_d, aes(x = clusterr, y = avgs)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) # avg silhouette

d2 <- ggplot(ch_d, aes(x = clusterr, y = score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Calinski-Harabasz Index"
  ) # ch

patchwork_dd <- d / d2
plot_annotation(tag_levels = 'a')

ggsave("patchwork_redo_dd.png", patchwork_dd,
       width = 10,
       height = 10,
       dpi = 300)

sil_d2 <- silhouette(k_med_d2$clustering, dist = gower_dis_d)
fviz_silhouette(sil_d2) # silhouette cluster plot

# store cluster in data set
survey_d$CLUSTER2 <- k_med_d2$clustering
survey_d$CLUSTER2 <- as.factor(survey_d$CLUSTER2)

# visualize clusters  - demographic

# bar charts
d1_d <- ggplot(survey_d, aes(x = D1, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Age Category",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("18-34", "35-44", "45-54", "55+")) +
  theme(legend.position = "bottom") # age

d2_d <- ggplot(survey_d, aes(x = D2, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Education Level",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Some high school or less", "Graduated High school", "Some post secondary", "Graduated university / college")) +
  theme(axis.text.x = element_text(size = 6, hjust = 0.5)) +
  theme(legend.position = "bottom") # education

Q19_d <- ggplot(survey_d, aes(x = Q19, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Frequency of Information-Seeking Behaviour",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Don't Know", "Never", "Rarely", "Sometimes", "Most of the Time", "Always")) +
  theme(legend.position = "bottom") # information-seeking behaviour

#box plots
lat_d <- ggplot(survey_d, aes(x = CLUSTER2, y = Latitude)) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Latitude"
  ) # latitude

long_d <- ggplot(survey_d, aes(x = CLUSTER2, y = Longitude)) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Longitude"
  ) # longitude

patchwork_d <- (d1_d + d2_d) / Q19_d / (lat_d + long_d) + 
  plot_annotation(tag_levels = 'a')

ggsave("patchwork_d_redo.png", patchwork_d,
       width = 10,
       height = 10,
       dpi = 300)

# save file for maps and join-count
write.csv(survey_d, file="Chp2redo_Data_kd.csv")

## repeat for recreation ##

# subset data to only include demographic D1, D2, jurisdiction, lat/long, recreation
survey_r <- survey2[,c(2,3,4,5,6,7,8,9,10)]

# remove NAs - demographic
survey_r <- na.omit(survey_r)
# convert data types
survey_r$Q19 <- as.ordered(survey_r$Q19)
survey_r$D1 <- as.ordered(survey_r$D1)
survey_r$D2 <- as.ordered(survey_r$D2)
survey_r$Q18_swimming <- as.factor(survey_r$Q18_swimming)
survey_r$Q18_fishing <- as.factor(survey_r$Q18_fishing)
survey_r$Q18_boarding <- as.factor(survey_r$Q18_boarding)
survey_r$Q18_boating <- as.factor(survey_r$Q18_boating)
glimpse(survey_r)

# standardize and distance metric - demographics
gower_dis_r <- daisy(survey_r, metric = "gower",
                     type = list(ordered = 1:3,
                                 symm = 4:7,
                                 numeric = 8:9))

# pamk - silhouette and Calinski Harabasz index 
set.seed(123) # replicate 
k_med_r_asw <- pamk(data = gower_dis_r,
                    krange = 2:10,
                    criterion = "asw",
                    usepam = TRUE,
                    diss = TRUE)

set.seed(123) # replicate 
k_med_r_ch <- pamk(data = gower_dis_r,
                   krange = 2:10,
                   criterion = "ch",
                   usepam = TRUE,
                   diss = TRUE)

# Run Partitioning Around Medoids (PAM) - 2 to 10 - demographics D1, D2, Lat/Long

set.seed(123) # replicate 
k_med_r2 <- pam(x = gower_dis_r,
                k = 2,
                diss = TRUE)
set.seed(123) # replicate
k_med_r3 <- pam(x = gower_dis_r,
                k = 3,
                diss = TRUE)
set.seed(123) # replicate
k_med_r4 <- pam(x = gower_dis_r,
                k = 4,
                diss = TRUE)
set.seed(123) # replicate
k_med_r5 <- pam(x = gower_dis_r,
                k = 5,
                diss = TRUE)
set.seed(123) # replicate
k_med_r6 <- pam(x = gower_dis_r,
                k = 6,
                diss = TRUE)
set.seed(123) # replicate
k_med_r7 <- pam(x = gower_dis_r,
                k = 7,
                diss = TRUE)
set.seed(123) # replicate
k_med_r8 <- pam(x = gower_dis_r,
                k = 8,
                diss = TRUE)
set.seed(123) # replicate
k_med_r9 <- pam(x = gower_dis_r,
                k = 9,
                diss = TRUE)
set.seed(123) # replicate
k_med_r10 <- pam(x = gower_dis_r,
                 k = 10,
                 diss = TRUE)

# store data in a dataframe - demographic
silho_r <- data.frame(clusterr = 1:10)
silho_r$avgs <- k_med_r2$silinfo$avg.width
silho_r$avgs[3] <- k_med_r3$silinfo$avg.width
silho_r$avgs[4] <- k_med_r4$silinfo$avg.width
silho_r$avgs[5] <- k_med_r5$silinfo$avg.width
silho_r$avgs[6] <- k_med_r6$silinfo$avg.width
silho_r$avgs[7] <- k_med_r7$silinfo$avg.width
silho_r$avgs[8] <- k_med_r8$silinfo$avg.width
silho_r$avgs[9] <- k_med_r9$silinfo$avg.width
silho_r$avgs[10] <- k_med_r10$silinfo$avg.width
silho_r$avgs[1] <- 0

# Calinhara

set.seed(123) # replicate 
ch_r2 <- calinhara(gower_dis_r,k_med_r2$clustering)

set.seed(123) # replicate 
ch_r3 <- calinhara(gower_dis_r,k_med_r3$clustering)

set.seed(123) # replicate 
ch_r4 <- calinhara(gower_dis_r,k_med_r4$clustering)

set.seed(123) # replicate 
ch_r5 <- calinhara(gower_dis_r,k_med_r5$clustering)

set.seed(123) # replicate 
ch_r6 <- calinhara(gower_dis_r,k_med_r6$clustering)

set.seed(123) # replicate 
ch_r7 <- calinhara(gower_dis_r,k_med_r7$clustering)

set.seed(123) # replicate 
ch_r8 <- calinhara(gower_dis_r,k_med_r8$clustering)

set.seed(123) # replicate 
ch_r9 <- calinhara(gower_dis_r,k_med_r9$clustering)

set.seed(123) # replicate 
ch_r10 <- calinhara(gower_dis_r,k_med_r10$clustering)

# store data in a dataframe - demographic
ch_r <- data.frame(clusterr = 1:10)
ch_r$score <- ch_r2
ch_r$score[3] <- ch_r3
ch_r$score[4] <- ch_r4
ch_r$score[5] <- ch_r5
ch_r$score[6] <- ch_r6
ch_r$score[7] <- ch_r7
ch_r$score[8] <- ch_r8
ch_r$score[9] <- ch_r9
ch_r$score[10] <- ch_r10
ch_r$score[1] <- 0

# Plot avg silhouette and cluster plot
r <- ggplot(silho_r, aes(x = clusterr, y = avgs)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 7, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters k",
    y = "Average silhouette width"
  ) # avg silhouette

r2 <- ggplot(ch_r, aes(x = clusterr, y = score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Calinski-Harabasz Index"
  ) # ch

patchwork_rr <- r/r2
  plot_annotation(tag_levels = 'a')

ggsave("patchwork_redo_rr.png", patchwork_rr,
       width = 10,
       height = 10,
       dpi = 300)

sil_r <- silhouette(k_med_r2$clustering, dist = gower_dis_r)
fviz_silhouette(sil_r) # silhouette cluster plot 2

# store cluster in dataset
survey_r$CLUSTER2 <- k_med_r2$clustering
survey_r$CLUSTER2 <- as.factor(survey_r$CLUSTER2)

# visualize clusters  - demographic

# bar charts
d1_r <- ggplot(survey_r, aes(x = D1, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Age Category",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("18-34", "35-44", "45-54", "55+")) +
  theme(legend.position = "bottom") # age

d2_r <- ggplot(survey_r, aes(x = D2, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Education Level",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Some high school or less", "Graduated High school", "Some post secondary", "Graduated university / college")) +
  theme(axis.text.x = element_text(size = 6, hjust = 0.5)) +
  theme(legend.position = "bottom") # education

Q19_r <- ggplot(survey_r, aes(x = Q19, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  labs(
    x = "Frequency of Information-Seeking Behaviour",
    y = "Proportion"
  ) +
  geom_text(stat = "prop", position = position_fill(.5)) +
  scale_x_discrete(labels = c("Don't Know", "Never", "Rarely", "Sometimes", "Most of the Time", "Always")) # information-seeking behaviour

swim_r <- ggplot(survey_r, aes(x = Q18_swimming, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Swimming",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # swimming

fish_r <- ggplot(survey_r, aes(x = Q18_fishing, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Fishing",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # fishing

board_r <- ggplot(survey_r, aes(x = Q18_boarding, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Water Sports",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # boarding

boat_r <- ggplot(survey_r, aes(x = Q18_boating, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Boating",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # boating

#box plots

lat_r <- ggplot(survey_r, aes(x = CLUSTER2, y = Latitude)) +
  geom_boxplot() # latitude

long_r <- ggplot(survey_r, aes(x = CLUSTER2, y = Longitude)) +
  geom_boxplot() # longitude

patchwork_r <- (d1_r + d2_r) / Q19_d / (swim_r + fish_r) / (board_r + boat_r) / (lat_r + long_r) + 
  plot_annotation(tag_levels = 'a')

ggsave("patchwork_redo_r.png", patchwork_r,
       width = 10,
       height = 10,
       dpi = 300)

# save file for maps and join-count
write.csv(survey_r, file="Chp2redo_Data_kr.csv")

## k-nn models start here ##

library(caret) #[7]

cluster_d <- read_csv("Chp2_Data_kd.csv")
cluster_d <- cluster_d[,c(5,6,7)]
cluster_d$CLUSTER2 <- as.factor(cluster_d$CLUSTER2)

cluster_d[,-3] <- scale(cluster_d[,-3])

validationIndex <- createDataPartition(cluster_d$CLUSTER2,
                                       p = 0.70, list = FALSE)

train_d <- cluster_d[validationIndex,] # 70% of data
test_d <- cluster_d[-validationIndex,] # 30% of data

trainControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3) # 10-fold cross validation, 3 times
metric <- "Accuracy"

# train for optimal k
grid <- expand.grid(.k=seq(1,50,by=1))
set.seed(7) #repeatable
fit.knn_d <- train(CLUSTER2~.,
                   data = train_d,
                   method = "knn",
                   metric =metric,
                   tuneGrid = grid,
                   trControl = trainControl)

print(fit.knn_d)

## repeat for recreation
cluster_r <- read_csv("Chp2_Data_kr.csv")

cluster_r <- cluster_r[,c(9,10,11)]
cluster_r$CLUSTER2 <- as.factor(cluster_r$CLUSTER2)

cluster_r[,-3] <- scale(cluster_r[,-3])

validationIndex_2 <- createDataPartition(cluster_r$CLUSTER2,
                                         p = 0.70, list = FALSE)

train_r <- cluster_r[validationIndex_2,] # 70% of data
test_r <- cluster_r[-validationIndex_2,] # 30% of data

trainControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3) # 10-fold cross validation, 3 times
metric <- "Accuracy"

# train for optimal k

grid <- expand.grid(.k=seq(1,50,by=1))

set.seed(7) #repeatable
fit.knn_r <- train(CLUSTER2~.,
                   data = train_r,
                   method = "knn",
                   metric =metric,
                   tuneGrid = grid,
                   trControl = trainControl)
print(fit.knn_r)

## kruskal-wallis starts here ##

library(stats) #[8]
library(rstatix) #[9]

## k=2 demographic/geography

cluster_d <- read_csv("Chp2redo_Data_kd.csv")
cluster_d$Q19 <- as.ordered(survey_d$Q19)
cluster_d$D1 <- as.ordered(survey_d$D1)
cluster_d$D2 <- as.ordered(survey_d$D2)

kruskal_test(D1 ~ CLUSTER2, data = cluster_d)
kruskal_test(D2 ~ CLUSTER2, data = cluster_d)
kruskal_test(Q19 ~ CLUSTER2, data = cluster_d)

kruskal_effsize(D1 ~ CLUSTER2, data = cluster_d)
kruskal_effsize(D2 ~ CLUSTER2, data = cluster_d)
kruskal_effsize(Q19 ~ CLUSTER2, data = cluster_d)

# kruskal-wallis - k=2 recreation

cluster_r <- read_csv("Chp2_Data_kr.csv")
cluster_r$Q19 <- as.ordered(cluster_r$Q19)
cluster_r$D1 <- as.ordered(cluster_r$D1)
cluster_r$D2 <- as.ordered(cluster_r$D2)
cluster_r$Q18_swimming <- as.factor(cluster_r$Q18_swimming)
cluster_r$Q18_fishing <- as.factor(cluster_r$Q18_fishing)
cluster_r$Q18_boarding <- as.factor(cluster_r$Q18_boarding)
cluster_r$Q18_boating <- as.factor(cluster_r$Q18_boating)

kruskal_test(D1 ~ CLUSTER2, data = cluster_r)
kruskal_test(D2 ~ CLUSTER2, data = cluster_r)
kruskal_test(Q19 ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_swimming ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_fishing ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_boarding ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_boating ~ CLUSTER2, data = cluster_r)

kruskal_effsize(D1 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(D2 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q19 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_swimming ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_fishing ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_boarding ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_boating ~ CLUSTER2, data = cluster_r)


















library(tidyverse) #[1]

# import 2024 Great Lakes Regional Poll data
survey <- read_csv("Cleaned_numbers_2024_ICJ_Telephone_Poll.csv")

# subset data to only include post/zip code, information-seeking behaviours
surveyUS <- survey[,c(1,3,4,11,12,13,17,18,19,20)]
survey <- survey[,c(1,3,4,11,12,13,17,18,19,20)]
glimpse(survey) # see datatypes

# Filter by Canadian/US Data
survey <- survey[complete.cases(survey),]
survey <- survey[survey$JURISDICTION == 7,]

surveyUS <- surveyUS[complete.cases(surveyUS),]
surveyUS <- surveyUS[surveyUS$JURISDICTION != 7, ]

# Save a csv file 
write.csv(survey, file="Chp2_Data_CAD.csv")
write.csv(surveyUS, file="Chp2_Data_US.csv")

# import postal/zip code data
fsa <- read_csv("CanadianPostalCodes202403.csv")
zipc <- read_csv("USZIPCodes202506.csv")

fsa <- distinct(fsa, POSTAL_CODE,.keep_all = TRUE)
zipc <- distinct(zipc, Zip_Code,.keep_all = TRUE)
zipc$Zip_Code <- as.numeric(zipc$Zip_Code)

# merge postal/zip code data for survey with longitude and latitude data

survey_combined <- merge(survey, fsa,
                         by.x = "POSTAL_CODE",
                         by.y = "POSTAL_CODE",
                         all.x = TRUE)

names(surveyUS)[names(surveyUS) == 'POSTAL_CODE'] <- 'Zip_Code'
survey_combined_US <- merge(surveyUS, zipc,
                            by.x = "Zip_Code",
                            by.y = "Zip_Code",
                            all.x = TRUE)

write.csv(survey_combined, file="Chp2_Data_CAD2.csv")
write.csv(survey_combined_US, file="Chp2_Data_US2.csv")

# re-combine the datasets
survey_combined_US <- survey_combined_US[,c(1,4,5,6,7,8,9,10,11,13,18,19)]
survey_combined <- survey_combined[,c(1,4,5,6,7,8,9,10,11,12,14,15)]

col_names <- c("POSTAL_ZIP", "Q19", "D1", "D2", 
               "Q18_swimming", "Q18_fishing", "Q18_boarding", "Q18_boating", 
               "City", "PROV_STATE","Latitude","Longitude") # Rename columns
colnames(survey_combined_US) <- col_names
colnames(survey_combined) <- col_names

survey2 <- rbind(survey_combined_US, survey_combined)

## k-medoids clustering starts here ##

library(cluster) #[2]
library(factoextra) #[3]
library(ggstats) #[4]
library(fpc) #[5]
library(patchwork) #[6]

# subset data to only include demographics(age, education), ISB, lat/long
survey_d <- survey2[,c(2,3,4,11,12)]

# remove NAs - demographic
survey_d <- na.omit(survey_d)

# convert data types
survey_d$Q19 <- as.ordered(survey_d$Q19)
survey_d$D1 <- as.ordered(survey_d$D1)
survey_d$D2 <- as.ordered(survey_d$D2)
glimpse(survey_d)

# standardize and distance metric - demographics
gower_dis_d <- daisy(survey_d, metric = "gower",
                     type = list(ordered = 1:3,
                                 numeric = 4:5))

# pamk - silhouette and Calinski Harabasz index 
set.seed(123) # replicate 
k_med_d_asw <- pamk(data = gower_dis_d,
                    krange = 2:10,
                    criterion = "asw",
                    usepam = TRUE,
                    diss = TRUE)

set.seed(123) # replicate 
k_med_d_ch <- pamk(data = gower_dis_d,
                   krange = 2:10,
                   criterion = "ch",
                   usepam = TRUE,
                   diss = TRUE)

# Run Partitioning Around Medoids (PAM) - 2 to 10 - demographics D1, D2, Q19, Lat/Long
set.seed(123) # replicate 
k_med_d2 <- pam(x = gower_dis_d,
                k = 2,
                diss = TRUE)
set.seed(123)
k_med_d3 <- pam(x = gower_dis_d,
                k = 3,
                diss = TRUE)
set.seed(123)
k_med_d4 <- pam(x = gower_dis_d,
                k = 4,
                diss = TRUE)
set.seed(123)
k_med_d5 <- pam(x = gower_dis_d,
                k = 5,
                diss = TRUE)
set.seed(123)
k_med_d6 <- pam(x = gower_dis_d,
                k = 6,
                diss = TRUE)
set.seed(123)
k_med_d7 <- pam(x = gower_dis_d,
                k = 7,
                diss = TRUE)
set.seed(123)
k_med_d8 <- pam(x = gower_dis_d,
                k = 8,
                diss = TRUE)
set.seed(123)
k_med_d9 <- pam(x = gower_dis_d,
                k = 9,
                diss = TRUE)
set.seed(123)
k_med_d10 <- pam(x = gower_dis_d,
                 k = 10,
                 diss = TRUE)

# store data in a dataframe
silho_d <- data.frame(clusterr = 1:10)
silho_d$avgs <- k_med_d2$silinfo$avg.width
silho_d$avgs[3] <- k_med_d3$silinfo$avg.width
silho_d$avgs[4] <- k_med_d4$silinfo$avg.width
silho_d$avgs[5] <- k_med_d5$silinfo$avg.width
silho_d$avgs[6] <- k_med_d6$silinfo$avg.width
silho_d$avgs[7] <- k_med_d7$silinfo$avg.width
silho_d$avgs[8] <- k_med_d8$silinfo$avg.width
silho_d$avgs[9] <- k_med_d9$silinfo$avg.width
silho_d$avgs[10] <- k_med_d10$silinfo$avg.width
silho_d$avgs[1] <- 0

# Calinhara
set.seed(123) # replicate 
ch_d2 <- calinhara(gower_dis_d,k_med_d2$clustering)

set.seed(123) # replicate 
ch_d3 <- calinhara(gower_dis_d,k_med_d3$clustering)

set.seed(123) # replicate 
ch_d4 <- calinhara(gower_dis_d,k_med_d4$clustering)

set.seed(123) # replicate 
ch_d5 <- calinhara(gower_dis_d,k_med_d5$clustering)

set.seed(123) # replicate 
ch_d6 <- calinhara(gower_dis_d,k_med_d6$clustering)

set.seed(123) # replicate 
ch_d7 <- calinhara(gower_dis_d,k_med_d7$clustering)

set.seed(123) # replicate 
ch_d8 <- calinhara(gower_dis_d,k_med_d8$clustering)

set.seed(123) # replicate 
ch_d9 <- calinhara(gower_dis_d,k_med_d9$clustering)

set.seed(123) # replicate 
ch_d10 <- calinhara(gower_dis_d,k_med_d10$clustering)

# store data in a dataframe - demographic
ch_d <- data.frame(clusterr = 1:10)
ch_d$score <- ch_d2
ch_d$score[3] <- ch_d3
ch_d$score[4] <- ch_d4
ch_d$score[5] <- ch_d5
ch_d$score[6] <- ch_d6
ch_d$score[7] <- ch_d7
ch_d$score[8] <- ch_d8
ch_d$score[9] <- ch_d9
ch_d$score[10] <- ch_d10
ch_d$score[1] <- 0

# Plot avg silhouette, ch and cluster plot
ggplot(silho_d, aes(x = clusterr, y = avgs)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) # avg silhouette

ggplot(ch_d, aes(x = clusterr, y = score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Calinski-Harabasz Index"
  ) # ch

sil_d2 <- silhouette(k_med_d2$clustering, dist = gower_dis_d)
fviz_silhouette(sil_d2) # silhouette cluster plot

# store cluster in data set
survey_d$CLUSTER2 <- k_med_d2$clustering
survey_d$CLUSTER2 <- as.factor(survey_d$CLUSTER2)

# visualize clusters  - demographic

# bar charts
d1_d <- ggplot(survey_d, aes(x = D1, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Age Category",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("18-34", "35-44", "45-54", "55+")) +
  theme(legend.position = "bottom") # age

d2_d <- ggplot(survey_d, aes(x = D2, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Education Level",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Some high school or less", "Graduated High school", "Some post secondary", "Graduated university / college")) +
  theme(axis.text.x = element_text(size = 6, hjust = 0.5)) +
  theme(legend.position = "bottom") # education

Q19_d <- ggplot(survey_d, aes(x = Q19, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Frequency of Information-Seeking Behaviour",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always")) +
  theme(legend.position = "bottom") # information-seeking behaviour

#box plots
lat_d <- ggplot(survey_d, aes(x = CLUSTER2, y = Latitude)) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Latitude"
  ) # latitude

long_d <- ggplot(survey_d, aes(x = CLUSTER2, y = Longitude)) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Longitude"
  ) # longitude

patchwork_d <- (d1_d + d2_d) / Q19_d / (lat_d + long_d) + 
  plot_annotation(tag_levels = 'a')

ggsave("patchwork_d.png", patchwork_d,
       width = 10,
       height = 10,
       dpi = 300)

# save file for maps and join-count
write.csv(survey_d, file="Chp2_Data_kd.csv")

## repeat for recreation ##

# subset data to only include demographic D1, D2, jurisdiction, lat/long, recreation
survey_r <- survey2[,c(2,3,4,5,6,7,8,11,12)]

# remove NAs - demographic
survey_r <- na.omit(survey_r)
# convert data types
survey_r$Q19 <- as.ordered(survey_r$Q19)
survey_r$D1 <- as.ordered(survey_r$D1)
survey_r$D2 <- as.ordered(survey_r$D2)
survey_r$Q18_swimming <- as.factor(survey_r$Q18_swimming)
survey_r$Q18_fishing <- as.factor(survey_r$Q18_fishing)
survey_r$Q18_boarding <- as.factor(survey_r$Q18_boarding)
survey_r$Q18_boating <- as.factor(survey_r$Q18_boating)
glimpse(survey_r)

# standardize and distance metric - demographics
gower_dis_r <- daisy(survey_r, metric = "gower",
                     type = list(ordered = 1:3,
                                 symm = 4:7,
                                 numeric = 8:9))

# pamk - silhouette and Calinski Harabasz index 
set.seed(123) # replicate 
k_med_r_asw <- pamk(data = gower_dis_r,
                    krange = 2:10,
                    criterion = "asw",
                    usepam = TRUE,
                    diss = TRUE)

set.seed(123) # replicate 
k_med_r_ch <- pamk(data = gower_dis_r,
                   krange = 2:10,
                   criterion = "ch",
                   usepam = TRUE,
                   diss = TRUE)

# Run Partitioning Around Medoids (PAM) - 2 to 10 - demographics D1, D2, Lat/Long

set.seed(123) # replicate 
k_med_r2 <- pam(x = gower_dis_r,
                k = 2,
                diss = TRUE)
set.seed(123) # replicate
k_med_r3 <- pam(x = gower_dis_r,
                k = 3,
                diss = TRUE)
set.seed(123) # replicate
k_med_r4 <- pam(x = gower_dis_r,
                k = 4,
                diss = TRUE)
set.seed(123) # replicate
k_med_r5 <- pam(x = gower_dis_r,
                k = 5,
                diss = TRUE)
set.seed(123) # replicate
k_med_r6 <- pam(x = gower_dis_r,
                k = 6,
                diss = TRUE)
set.seed(123) # replicate
k_med_r7 <- pam(x = gower_dis_r,
                k = 7,
                diss = TRUE)
set.seed(123) # replicate
k_med_r8 <- pam(x = gower_dis_r,
                k = 8,
                diss = TRUE)
set.seed(123) # replicate
k_med_r9 <- pam(x = gower_dis_r,
                k = 9,
                diss = TRUE)
set.seed(123) # replicate
k_med_r10 <- pam(x = gower_dis_r,
                 k = 10,
                 diss = TRUE)

# store data in a dataframe - demographic
silho_r <- data.frame(clusterr = 1:10)
silho_r$avgs <- k_med_r2$silinfo$avg.width
silho_r$avgs[3] <- k_med_r3$silinfo$avg.width
silho_r$avgs[4] <- k_med_r4$silinfo$avg.width
silho_r$avgs[5] <- k_med_r5$silinfo$avg.width
silho_r$avgs[6] <- k_med_r6$silinfo$avg.width
silho_r$avgs[7] <- k_med_r7$silinfo$avg.width
silho_r$avgs[8] <- k_med_r8$silinfo$avg.width
silho_r$avgs[9] <- k_med_r9$silinfo$avg.width
silho_r$avgs[10] <- k_med_r10$silinfo$avg.width
silho_r$avgs[1] <- 0

# Calinhara

set.seed(123) # replicate 
ch_r2 <- calinhara(gower_dis_r,k_med_r2$clustering)

set.seed(123) # replicate 
ch_r3 <- calinhara(gower_dis_r,k_med_r3$clustering)

set.seed(123) # replicate 
ch_r4 <- calinhara(gower_dis_r,k_med_r4$clustering)

set.seed(123) # replicate 
ch_r5 <- calinhara(gower_dis_r,k_med_r5$clustering)

set.seed(123) # replicate 
ch_r6 <- calinhara(gower_dis_r,k_med_r6$clustering)

set.seed(123) # replicate 
ch_r7 <- calinhara(gower_dis_r,k_med_r7$clustering)

set.seed(123) # replicate 
ch_r8 <- calinhara(gower_dis_r,k_med_r8$clustering)

set.seed(123) # replicate 
ch_r9 <- calinhara(gower_dis_r,k_med_r9$clustering)

set.seed(123) # replicate 
ch_r10 <- calinhara(gower_dis_r,k_med_r10$clustering)

# store data in a dataframe - demographic
ch_r <- data.frame(clusterr = 1:10)
ch_r$score <- ch_r2
ch_r$score[3] <- ch_r3
ch_r$score[4] <- ch_r4
ch_r$score[5] <- ch_r5
ch_r$score[6] <- ch_r6
ch_r$score[7] <- ch_r7
ch_r$score[8] <- ch_r8
ch_r$score[9] <- ch_r9
ch_r$score[10] <- ch_r10
ch_r$score[1] <- 0

# Plot avg silhouette and cluster plot
ggplot(silho_r, aes(x = clusterr, y = avgs)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 7, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters k",
    y = "Average silhouette width"
  ) # avg silhouette

ggplot(ch_r, aes(x = clusterr, y = score)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  geom_vline(xintercept = 2, color = "blue", linetype = "dotted") +
  labs(
    x = "Number of clusters (k)",
    y = "Calinski-Harabasz Index"
  ) # ch

sil_r <- silhouette(k_med_r2$clustering, dist = gower_dis_r)
fviz_silhouette(sil_r) # silhouette cluster plot 2

# store cluster in dataset
survey_r$CLUSTER2 <- k_med_r2$clustering
survey_r$CLUSTER2 <- as.factor(survey_r$CLUSTER2)

# visualize clusters  - demographic

# bar charts
d1_r <- ggplot(survey_r, aes(x = D1, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Age Category",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("18-34", "35-44", "45-54", "55+")) +
  theme(legend.position = "bottom") # age

d2_r <- ggplot(survey_r, aes(x = D2, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Education Level",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("Some high school or less", "Graduated High school", "Some post secondary", "Graduated university / college")) +
  theme(axis.text.x = element_text(size = 6, hjust = 0.5)) +
  theme(legend.position = "bottom") # education

Q19_r <- ggplot(survey_r, aes(x = Q19, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  labs(
    x = "Frequency of Information-Seeking Behaviour",
    y = "Proportion"
  ) +
  geom_text(stat = "prop", position = position_fill(.5)) +
  scale_x_discrete(labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always")) # information-seeking behaviour

swim_r <- ggplot(survey_r, aes(x = Q18_swimming, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Swimming",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # swimming

fish_r <- ggplot(survey_r, aes(x = Q18_fishing, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Fishing",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # fishing

board_r <- ggplot(survey_r, aes(x = Q18_boarding, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Water Sports",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # boarding

boat_r <- ggplot(survey_r, aes(x = Q18_boating, fill = CLUSTER2)) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
  labs(
    x = "Boating",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(legend.position = "bottom") # boating

#box plots

lat_r <- ggplot(survey_r, aes(x = CLUSTER2, y = Latitude)) +
  geom_boxplot() # latitude

long_r <- ggplot(survey_r, aes(x = CLUSTER2, y = Longitude)) +
  geom_boxplot() # longitude

patchwork_r <- (d1_r + d2_r) / Q19_d / (swim_r + fish_r) / (board_r + boat_r) / (lat_r + long_r) + 
  plot_annotation(tag_levels = 'a')

ggsave("patchwork_r.png", patchwork_r,
       width = 10,
       height = 15,
       dpi = 300)

# save file for maps and join-count
write.csv(survey_r, file="Chp2_Data_kr.csv")

## k-nn models start here ##

library(caret) #[7]

cluster_d <- read_csv("Chp2_Data_kd.csv")
cluster_d <- cluster_d[,c(5,6,7)]
cluster_d$CLUSTER2 <- as.factor(cluster_d$CLUSTER2)

cluster_d[,-3] <- scale(cluster_d[,-3])

validationIndex <- createDataPartition(cluster_d$CLUSTER2,
                                       p = 0.70, list = FALSE)

train_d <- cluster_d[validationIndex,] # 70% of data
test_d <- cluster_d[-validationIndex,] # 30% of data

trainControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3) # 10-fold cross validation, 3 times
metric <- "Accuracy"

# train for optimal k
grid <- expand.grid(.k=seq(1,50,by=1))
set.seed(7) #repeatable
fit.knn_d <- train(CLUSTER2~.,
                   data = train_d,
                   method = "knn",
                   metric =metric,
                   tuneGrid = grid,
                   trControl = trainControl)

print(fit.knn_d)

## repeat for recreation
cluster_r <- read_csv("Chp2_Data_kr.csv")

cluster_r <- cluster_r[,c(9,10,11)]
cluster_r$CLUSTER2 <- as.factor(cluster_r$CLUSTER2)

cluster_r[,-3] <- scale(cluster_r[,-3])

validationIndex_2 <- createDataPartition(cluster_r$CLUSTER2,
                                         p = 0.70, list = FALSE)

train_r <- cluster_r[validationIndex_2,] # 70% of data
test_r <- cluster_r[-validationIndex_2,] # 30% of data

trainControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 3) # 10-fold cross validation, 3 times
metric <- "Accuracy"

# train for optimal k

grid <- expand.grid(.k=seq(1,50,by=1))

set.seed(7) #repeatable
fit.knn_r <- train(CLUSTER2~.,
                    data = train_r,
                    method = "knn",
                    metric =metric,
                    tuneGrid = grid,
                    trControl = trainControl)
print(fit.knn_r)

## kruskal-wallis starts here ##

library(stats) #[8]
library(rstatix) #[9]

## k=2 demographic/geography

cluster_d <- read_csv("Chp2_Data_kd.csv")
cluster_d$Q19 <- as.ordered(survey_d$Q19)
cluster_d$D1 <- as.ordered(survey_d$D1)
cluster_d$D2 <- as.ordered(survey_d$D2)

kruskal_test(D1 ~ CLUSTER2, data = cluster_d)
kruskal_test(D2 ~ CLUSTER2, data = cluster_d)
kruskal_test(Q19 ~ CLUSTER2, data = cluster_d)

kruskal_effsize(D1 ~ CLUSTER2, data = cluster_d)
kruskal_effsize(D2 ~ CLUSTER2, data = cluster_d)
kruskal_effsize(Q19 ~ CLUSTER2, data = cluster_d)

# kruskal-wallis - k=2 recreation

cluster_r <- read_csv("Chp2_Data_kr.csv")
cluster_r$Q19 <- as.ordered(cluster_r$Q19)
cluster_r$D1 <- as.ordered(cluster_r$D1)
cluster_r$D2 <- as.ordered(cluster_r$D2)
cluster_r$Q18_swimming <- as.factor(cluster_r$Q18_swimming)
cluster_r$Q18_fishing <- as.factor(cluster_r$Q18_fishing)
cluster_r$Q18_boarding <- as.factor(cluster_r$Q18_boarding)
cluster_r$Q18_boating <- as.factor(cluster_r$Q18_boating)

kruskal_test(D1 ~ CLUSTER2, data = cluster_r)
kruskal_test(D1 ~ CLUSTER2, data = cluster_r)
kruskal_test(D2 ~ CLUSTER2, data = cluster_r)
kruskal_test(Q19 ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_swimming ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_fishing ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_boarding ~ CLUSTER2, data = cluster_r)
kruskal_test(Q18_boating ~ CLUSTER2, data = cluster_r)

kruskal_effsize(D1 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(D2 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q19 ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_swimming ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_fishing ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_boarding ~ CLUSTER2, data = cluster_r)
kruskal_effsize(Q18_boating ~ CLUSTER2, data = cluster_r)

#---------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stats)
library(rstatix)

# import distance data
distance_data <- read_csv("distance_redo.csv")

# Kruskal-Wallis Test
kruskal.test(Q19 ~ distance,
             data = distance_data)

kruskal_effsize(Q19 ~ distance, data = distance_data)

# sqrt the distance
distance_data <- distance_data %>% 
  mutate(distance_sq = sqrt(distance))

# plot distance x ISB
v_plot <- ggplot(distance_data, aes(factor(Q19, order = TRUE,
                                           levels = c("1","2","3","4","5","0")),distance_sq))
v_plot <- v_plot + geom_violin() + geom_boxplot(width=0.1)+
  labs(
    x = "Information Seeking Frequency",
    y = "Distance (sqrt)"
  )+
  scale_x_discrete(labels = c("Never", "Rarely", "Sometimes", "Most of the Time", "Always", "Don't Know"))

ggsave("violin.png", v_plot,
       width = 6,
       height = 6.5,
       dpi = 300)
