#average elements for each stream group

library(dplyr)


laicpms_all <- read_excel("LAICPMS Data/Geology LAICPMS.xlsx")

laicpms_optimal <- read.csv("LAICPMS Data/subset_data_optimal_features.csv")

#subset just streams 
streams_all <- subset(laicpms_all, !(Group %in% c("23G-W", "23G-X", "23G-AA", "23G-BA", "23G-F", 
                                                  "La Milpa Quarry", "23G-E", "23G-I", "23G-V", "23G-Y", 
                                                  "23G-Z", "24G-J", "23G-VA","La Milpa N", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))

streams_optimal <- subset(laicpms_optimal, !(Group %in% c("23G-W", "23G-X", "23G-AA", "23G-BA", "23G-F", 
                                                  "La Milpa Quarry", "23G-E", "23G-I", "23G-V", "23G-Y", 
                                                  "23G-Z", "24G-J", "23G-VA","La Milpa N", "24G-G", "24G-H", "24G-I", "24G-J", "24G-N", "24G-T")))

######
#ALL ELEMENTS IN ORIGINAL DATASET 
#Print all variables in dataset
print(colnames(streams_all))

# Calculate averages for each group
averages <- streams_all %>%
  group_by(Group) %>%
  summarize(across(c(
    "P", "K2O", "CaCO3", "Sc", "Ti", "V", "Cr", "Mn", "Fe2O3", "Ni", 
    "Co", "Cu", "Zn", "Ga", "Ge", "As", "Rb", "Sr", "Y", "Zr", "Nb", 
    "Mo", "Ag", "Cd", "In", "Sn", "Sb", "Cs", "Ba", "La", "Ce", "Pr", 
    "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu", 
    "Hf", "Ta", "W", "Au", "Tl", "Pb", "Bi", "Th", "U"), mean, na.rm = TRUE))

# Print the result
print(averages)

View(averages)

##ANOVA

# List of elements you want to test
variables <- c("P", "K2O", "CaCO3", "Sc", "Ti", "V", "Cr", "Mn", "Fe2O3", 
               "Ni", "Co", "Cu", "Zn", "Ga", "Ge", "As", "Rb", "Sr", "Y", 
               "Zr", "Nb", "Mo", "Ag", "Cd", "In", "Sn", "Sb", "Cs", "Ba", 
               "La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", 
               "Er", "Tm", "Yb", "Lu", "Hf", "Ta", "W", "Au", "Tl", "Pb", 
               "Bi", "Th", "U")

# Initialize an empty list to store significant variables
significant_elements <- list()

# Loop through each variable and perform ANOVA
for (var in variables) {
  # Perform ANOVA for the variable
  formula <- as.formula(paste(var, "~ Group"))
  anova_result <- aov(formula, data = streams)
  
  # Extract the p-value from ANOVA result
  anova_summary <- summary(anova_result)
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]  # Get p-value of the first factor
  
  # Check if the p-value is significant (less than 0.05)
  if (p_value < 0.05) {
    # Store the variable name in the list if significant
    significant_elements[[var]] <- p_value
  }
}

# Print the list of significant elements
print(significant_elements)


#####
#OPTIMAL FEATURES SELECTED BY RFECV

#Print all variables in dataset
print(colnames(streams_optimal))

# Calculate averages for each group
averages_optimal <- streams_optimal %>%
  group_by(Group) %>%
  summarize(across(c(
    "Sr", "Co", "B", "U", "Li", "Ni", "MgO", "Na2O", "Ti", "Al2O3", "K2O", "Sc", "Zn", "CaCO3", "Mo",
    "SiO2", "Rb", "Cd", "Mn", "Ba", "Be", "La", "Cr", "Nb", "Y", "P", "Cs", "Eu", "Sn", "V", "Bi",
    "Pb", "As", "Sb" ), mean, na.rm = TRUE))

# Print the result
print(averages_optimal)

View(averages_optimal)

##ANOVA

# List of elements you want to test
variables_optimal <- c("Sr", "Co", "B", "U", "Li", "Ni", "MgO", "Na2O", "Ti", "Al2O3", "K2O", "Sc", "Zn", "CaCO3", "Mo",
                       "SiO2", "Rb", "Cd", "Mn", "Ba", "Be", "La", "Cr", "Nb", "Y", "P", "Cs", "Eu", "Sn", "V", "Bi",
                       "Pb", "As", "Sb")

# Initialize an empty list to store significant variables
significant_elements_optimal <- list()

# Loop through each variable and perform ANOVA
for (var in variables_optimal) {
  # Perform ANOVA for the variable
  formula_optimal <- as.formula(paste(var, "~ Group"))
  anova_result_optimal <- aov(formula_optimal, data = streams_optimal)
  
  # Extract the p-value from ANOVA result
  anova_summary_optimal <- summary(anova_result_optimal)
  p_value_optimal <- anova_summary_optimal[[1]]$`Pr(>F)`[1]  # Get p-value of the first factor
  
  # Check if the p-value is significant (less than 0.05)
  if (p_value_optimal < 0.05) {
    # Store the variable name in the list if significant
    significant_elements_optimal[[var]] <- p_value_optimal
  }
}

# Print the list of significant elements
print(significant_elements_optimal)

