#First, we need to install the required R packages. 
#If you already installed these packages, you can skip this step. Otherwise, run the following commands.

install.packages('readxl')
install.packages("dplyr")
install.packages("tidyr")
install.packages("openxlsx")
install.packages("readr")
install.packages("stats")
install.packages("janitor")

#Now let's load all required libraries into our R session.
# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readr)
library(janitor)
library(stats)
library(reshape2)
library(writexl)


# T-Test-results of P value, FDR, log2Fold changes KD over CTRL

# Load dataset that I showed as example.
result <- read.csv('Normalization_Log2_Example_Meta.csv', row.names = 1, check.names = FALSE)

View(result)

# Separate the data and group information
data_matrix <- as.matrix(result[, -ncol(result)])
group <- result$Group

View(data_matrix)

# Calculate log2 fold changes
log2FC <- apply(data_matrix, 2, function(x) {
  (mean(x[group == "KD"]) - mean(x[group == "CTRL"]))
})

# Calculate linear fold changes

linearFC <- 2^log2FC

# Perform T-tests and fin dp values
p_values <- apply(data_matrix, 2, function(x) {
  t.test(x[group == "CTRL"], x[group == "KD"])$p.value
})

# Calculate FDR
fdr <- p.adjust(p_values, method = "fdr")

# Calculate mean expression for each feature (optional)
mean_expression <- colMeans(data_matrix)

# Combine results into a data frame
results <- data.frame(
  mean_expression = mean_expression,
  p_value = p_values,
  FDR = fdr,
  linearFC = linearFC,
  log2FC = log2FC)


View(results)

# Save the results to an Excel file
write.xlsx(results, file = "Report_KD over CTRL.xlsx", rowNames = TRUE)


##############################ANOVA############################

# ANOVA_results of P value, FDR_ANOVA_CTRL vs KD vs sh_CTRL_10 vs KD_10


# Perform ANOVA
anova_p_values <- apply(data_matrix, 2, function(x) {
  # Define the groups explicitly in the formula
  group_factor <- factor(result$Group)
    # Perform ANOVA
  aov_result <- aov(x ~ group_factor)
  summary(aov_result)[[1]]["Pr(>F)"][1]  # Extracting the p-value from the summary
})



# Calculate FDR
fdr <- p.adjust(p_values, method = "fdr")

# Combine results into a data frame
results <- data.frame(
  p_value = p_values,
  FDR = fdr)


View(results)

# Save the results to an Excel file
write.xlsx(results, file = "Report_ANOVA_ANOVA_CTRL vs KD vs sh_CTRL_10 vs KD_10.xlsx", rowNames = TRUE)

