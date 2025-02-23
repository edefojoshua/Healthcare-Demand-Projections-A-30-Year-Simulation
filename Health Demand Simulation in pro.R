#Set Directory
setwd("C:\\Users\\joe62\\OneDrive - Aberystwyth University\\Apps\\Desktop\\Destop Folder\\Health Demand Simulation")
# Load necessary libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(skimr)
library(psych)
library(car)
library(MASS)
library(lmtest)
library(writexl)
library(pscl)
library(gamlss)

# Set seed for reproducibility
set.seed(123)

# Hypothetical Clinic Practice Research Datalink dataset
CPRD_data <- read_excel("CPRD_data.xlsx")
head (CPRD_data)


# Assign Sex, Smoking and Alcohol based on hypothetical Health Survey for England  (HSE) 2022
CPRD_n_HSE_data <- CPRD_data %>%
  mutate(
sex = sample(c('Male', 'Female'), nrow(CPRD_data), replace = TRUE),  # Assign random sexes
Smoking = sample(c(0, 1), nrow(CPRD_data), replace = TRUE, prob = c(0.7, 0.3)),  # 30% smokers
Alcohol = rnorm(nrow(CPRD_data), mean = 8, sd = 4)  # Alcohol consumption (units per week)
)
view (CPRD_n_HSE_data)

write_xlsx(CPRD_n_HSE_data, "CPRD_n_HSE_data.xlsx")

# Importing Hypothetical Hospital Episodic Statistics (HES) dataset
HES_data <-read_excel("HES_data.xlsx")
head(HES_data)

# Merging data with CPRD_n_HSE_data with HES_data
merged_data <- merge(CPRD_n_HSE_data, HES_data, by = "individual_id", all = FALSE)
head(merged_data)

view(merged_data)

head(merged_data)

write_xlsx(merged_data, "merged_data.xlsx")

# Merged data (CPRD_data,HSE_data and HES_data)
merged_data <-read_excel("merged_data.xlsx")
head(merged_data)
# adjust hospital number of visits using Cambridge Multimorbidity Score (CMS )
# "https://www.bristol.ac.uk/primaryhealthcare/news/2020/cambridge-multimorbidity-score.html"


merged_data_v2 <- merged_data %>%
  mutate(
    hospital_visits_adjusted = mapply(function(at, disease, Multimorbidity, hospital_visits_yearly) {
      
      # When no multimorbidity
      if (Multimorbidity == 0) {
        return(ceiling(hospital_visits_yearly))
        
        # When one disease is present
      } else if (Multimorbidity == 1 & disease %in% c("Asthma", "Hypertension", "Heart Disease", "Diabetes")) {
        # Adjusting hospital visits for a single disease
        return(ceiling(sample(c(hospital_visits_yearly * 2.32, 
                                hospital_visits_yearly * 1.66, 
                                hospital_visits_yearly * 2.49, 
                                hospital_visits_yearly * 4.77), 1)))
        
        # When two diseases are present
      } else if (Multimorbidity == 2) {
        # Disease multipliers for 2 diseases
        disease_multipliers <- c("Asthma" = 2.32, 
                                 "Hypertension" = 1.66, 
                                 "Heart Disease" = 2.49, 
                                 "Diabetes" = 4.77)
        
        diseases <- strsplit(as.character(disease), ",")[[1]]
        
        if (length(diseases) == 2) {
          multiplier_product <- disease_multipliers[diseases[1]] * disease_multipliers[diseases[2]]
          return(ceiling(hospital_visits_yearly * multiplier_product))
        } else {
          return(ceiling(hospital_visits_yearly))  # Return original visits if not two recognized diseases
        }
        
        # When three diseases are present
      } else if (Multimorbidity == 3) {
        # Disease multipliers for 3 diseases
        disease_multipliers <- c("Asthma" = 2.32, 
                                 "Hypertension" = 1.66, 
                                 "Heart Disease" = 2.49, 
                                 "Diabetes" = 4.77)
        
        diseases <- strsplit(as.character(disease), ",")[[1]]
        
        if (length(diseases) == 3) {
          # Multiply the multipliers for the three diseases
          multiplier_product <- prod(disease_multipliers[diseases])
          return(ceiling(hospital_visits_yearly * multiplier_product))
        } else {
          return(ceiling(hospital_visits_yearly))  # Return original visits if not three recognized diseases
        }
        
      } else {
        return(0)  # Return 0 for any other unexpected condition
      }
    }, 
    disease = merged_data$disease, 
    Multimorbidity = merged_data$Multimorbidity, 
    hospital_visits_yearly = merged_data$hospital_visits_yearly)
  )

write_xlsx(merged_data_v2, "merged_data_v2.xlsx")
head (merged_data_v2)

# Merged data_v2 (CPRD_data,HSE_data and HES_data)
merged_data_v2 <-read_excel("merged_data_v2.xlsx")
view (merged_data_v2)

# Data cleaning
sum(is.na(merged_data_v2))

# Descriptive Statistical analysis
summary (merged_data_v2)

##  Frequency Table with Proportions 
### Sex
prop.table(table(merged_data_v2$sex.x)) # Female = 0.494, Male = 0.506

### Disease
prop.table(table(merged_data_v2$disease)) # No = 0.197, Yes = 0.803

### Treatment
prop.table(table(merged_data_v2$treatment)) #Lifestyle change = 0.267, medication = 0.254, none = 0.258, surgery= 0.221

### Multimorbidity
prop.table(table(merged_data_v2$Multimorbidity)) # 0 = 0.249, 1 = 0.242, 2 = 0.268, 3= 0.241

### Age
summary(merged_data_v2$age.x)
mean (merged_data$age_v2.x) # 57.437
sd (merged_data$age_v2.x) # 20.283


### proportion of each age group
prop.table(table(merged_data_v2$age_group)) #Below 20 years = 0.041,  21-40 years = 0.172, 41-60 years = 0.310, 61-90 years = 0.477
                            

### Smoking
prop.table(table(merged_data_v2$Smoking)) # non smokers = 0.703, smokers = 0.297


### Alcohol
summary(merged_data_v2$Alcohol)
mean (merged_data_v2$Alcohol) # 8.170
sd (merged_data_v2$Alcohol) # 4.039

### admission_type
prop.table(table(merged_data_v2$admission_type)) # Elective = 0.327, Emergency = 0.343, Maternity = 0.330


### length_of_stay
summary(merged_data_v2$length_of_stay)
mean (merged_data_v2$length_of_stay) # 7.579
sd (merged_data_v2$length_of_stay) # 4.001

### hospital_visits_yearly (adjusted)
summary(merged_data_v2$hospital_visits_adjusted)
mean (merged_data_v2$hospital_visits_adjusted) # 4.984
sd (merged_data_v2$hospital_visits_adjusted) # 5.754

# Inferential  Statistical analysis (1)
# Hospital visits as the outcome variable
head(merged_data_v2)
summary(merged_data_v2$hospital_visits_adjusted)
table(merged_data_v2$hospital_visits_adjusted)  # Check if all values are non-negative integers

#Checking Data Distribution
mean_var_ratio <- var(merged_data_v2$hospital_visits_yearly) / mean(merged_data_v2$hospital_visits_yearly)
print(mean_var_ratio)
# This mean possion distribution might not possible as there is over dispersion (2.270775)

## Using log linear model (count model for number of hospital visits)

## Negative Binomial Regression
hosp_visit_influence_factors <- glm.nb(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                        data = merged_data_v2)
summary(hosp_visit_influence_factors)

##Run and compare Poisson vs. Negative Binomial Model Fit
hosp_visit_influence_factors_v2 <- glm(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                                       data = merged_data_v2, family = poisson)
summary(hosp_visit_influence_factors_v2)
# Compare Poisson vs. Negative Binomial
vuong(hosp_visit_influence_factors, hosp_visit_influence_factors_v2)
## Since the comparism of Models gave a positive Z statistics (4.884685) and significant p-value (5.1797e-07)
## Negative Binomial Regression model is better and is chosen
##Poisson Model  is preferred because it is simpler 

### diagnostic  checks for the log linear model 
### Check for Multicollinearity
vif(hosp_visit_influence_factors)
### The VIF values for all predictors are very close to 1, which means 
###that there is little to no collinearity among your independent variables. 

####Residual Analysis
plot(fitted(hosp_visit_influence_factors), residuals(hosp_visit_influence_factors, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals", main = "Residual Plot")
abline(h = 0, col = "red")
### plots almost randomly scattered residuals around zero, indicating a good fit.

## Negative Binomial Regression
hosp_visit_influence_factors <- glm.nb(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                                       data = merged_data_v2)
summary(hosp_visit_influence_factors)

## Considering only the significant predictors (p < 0.05), the mathematical equation for the Poisson regression model is:
## hospital_visits_adjusted=exp(1.9299−0.0129⋅Alcohol+0.5529⋅admission_typeEmergency−2.6766⋅admission_typeMaternity−0.1052⋅Multimorbidity)
##so lets modify this equation to reflect real world scenario
## hospital_visits_adjusted=exp(1.9299+0.0005.age.x+0.083.Smoking+0.0129⋅Alcohol+0.1052⋅Multimorbidity)


# Inferential  Statistical analysis (2)
# when the outcome variable is hospital length of stay (length)
head (merged_data_v2)

hosp_length_of_stay <- lm(length_of_stay ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                          data = merged_data_v2)
summary(hosp_length_of_stay)

## Diagnostic checks
## Residuals vs Fitted plot
plot(hosp_length_of_stay, which = 1)
## randomly scattered and evenly distributed

## Normal Q-Q plot
plot(hosp_length_of_stay, which = 2)
## line almost diagonal all through

## Scale-Location plot
plot(hosp_length_of_stay, which = 3)
## residuals have spread evenly distributed

## Residuals vs Leverage plot
plot(hosp_length_of_stay, which = 5)
## more than 99% of the data are within the Cook's distance threshold

##  Durbin-Watson Test
library(lmtest)
dwtest(hosp_length_of_stay)
## the value is between 1 and 3, hence no autocorrelation

## Variance Inflation Factor (VIF)
library(car)
vif(hosp_length_of_stay) 
## very low multicolinearity as the values are around 1
## so continue with the linear model

hosp_length_of_stay <- lm(length_of_stay ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                          data = merged_data_v2)
summary(hosp_length_of_stay)

# length of stay=8.164572+(−0.007678×age.x)+(−0.099108×Multimorbidity)+

#so lets modify the model to represent real world cohort
# length of stay= 1.164572+ 0.7678×age.x+0.199108×Multimorbidity)+ ϵ


#Phase 2
# Use the data for Microsimulation to determine demand for health care over 30 years period in a patient cohort size of 1000 individuals

## Define the Multimorbidity function outside the data frame
generate_multimorbidity <- function(age) {
  if (age < 21) {
    probs <- c(0.10, 0.06, 0.00)
  } else if (age <= 40) {
    probs <- c(0.30, 0.15, 0.00)
  } else if (age <= 60) {
    probs <- c(0.60, 0.30, 0.00)
  } else {
    probs <- c(0.90, 0.60, 0.30)
  }
  
  # Adjust to ensure the sum of probabilities is 1
  probs <- probs / sum(probs)  # Normalize probabilities so they sum to 1
  
  # Sample the number of conditions based on the probabilities
  sample(c(1, 2, 3), size = 1, prob = probs, replace = TRUE)
}

## Define simulation parameters
n <- 1000  # Number of synthetic individuals
years_of_simulation <- 30  # Simulation time horizon (2025-2054)

## Generate a basic synthetic population and include multimorbidity
population <- data.frame(
  ID = 1:n,
  Age = sample(0:90, n, replace = TRUE),  ## Random age between 0 and 90
  Sex = sample(c('Male', 'Female'), n, replace = TRUE),  ## Random sex
  Smoking = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),  ## 15% smokers
  Alcohol = rnorm(n, mean = 6, sd = 3)  # Alcohol consumption (units per week)
)

# Add Multimorbidity to the population based on Age
population$Multimorbidity <- sapply(population$Age, generate_multimorbidity)

# View the first few rows of the population data frame
head(population)

## Define hospital visit functions based on its predictors
hosp_visit_func <- function(Age, Sex, Smoking, Alcohol, Multimorbidity) {
  base_func <- 1.9299  # Base risk of CHD per year
  
  # Modify the base function based on the predictors
  func <- base_func + (Age/10) * exp(0.0001) + (Smoking) * exp(0.0023) + (Alcohol) * exp(0.000129) + (Multimorbidity) * exp(0.00105)
  if (Sex == 'Female') func <- func * 1.1  # Adjust function for females (higher risk by 10%)
  
  return(func)
}

## Simulate the disease incidence over time (CHD in this case)
population <- population %>%
  mutate(
    Hosp_visit = mapply(hosp_visit_func, Age, Sex, Smoking, Alcohol, Multimorbidity)  # Apply hosp function
  )

# View the first few rows of the updated population data frame
head(population)


## Define hospital length of stay functions based on its predictors
# length of stay= 1.164572+ 0.7678×age.x+0.199108×Multimorbidity)+ ϵ

hosp_length_func <- function(Age, Multimorbidity) {
  base_length <-1.1645  # Base  length os stay
  
  # Modify the base function based on the predictors
  length <- base_length + (Age) * 0.8 + (Multimorbidity * 0.2)
  return(length)
}

## Simulate the disease incidence over time (CHD in this case)
population <- population %>%
  mutate(
    Hosp_length = mapply(hosp_length_func, Age,  Multimorbidity)  # Apply length function
  )

# View the first few rows of the updated population data frame
head(population)


simulation_results <- data.frame(Year = rep(2025:2054, each = n), 
                                 ID = rep(1:n, times = years_of_simulation))

## Simulate the hospital visits of each individual across years
for (year in 2025:2054) {
  # Update age each year
  population$Age <- population$Age + 1
  
  # Recalculate hospital visits based on updated age and other factors
  population <- population %>%
    mutate(
      Hosp_visit = mapply(hosp_visit_func, Age, Sex, Smoking, Alcohol, Multimorbidity),  # Apply function
      Hosp_length = mapply(hosp_length_func, Age,  Multimorbidity)
    )
  
  # Store the results for this year in simulation_results
  simulation_results$Hosp_visit[simulation_results$Year == year] <- population$Hosp_visit
  simulation_results$Hosp_length[simulation_results$Year == year] <- population$Hosp_length
}

head(simulation_results)

## Visualizing the results
## Plotting the increase in hospital visits  over time
ggplot(simulation_results, aes(x = Year, y = Hosp_visit)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  labs(title = "Increase in hospital visit over Time", x = "Year", y = "Hospital visits") +
  theme_minimal()

## Visualizing the results
## Plotting the increase in hospital visits  over time
ggplot(simulation_results, aes(x = Year, y = Hosp_length)) +
  geom_line(stat = "summary", fun = "mean", color = "red") +
  labs(title = "Increase in hospital length of stay over Time", x = "Year", y = "Hospital length of stay") +
  theme_minimal()


# prejected number of hospital visit in 2054
# Filter the simulation results for the year 2054
hospital_visits_2054 <- simulation_results %>%
  filter(Year == 2054) %>%
  summarise(total_visits_2054 = sum(Hosp_visit, na.rm = TRUE))

# Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_visits_2054)


#  number of hospital visit in 2025
# Filter the simulation results for the year 2024
hospital_visits_2025 <- simulation_results %>%
  filter(Year == 2025) %>%
  summarise(total_visits_2025 = sum(Hosp_length, na.rm = TRUE))

# Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_visits_2025)


#projected total number of hospital length of stay (hours) in 2054 for the cohort

# Filter the simulation results for the year 2054
hospital_length_2054 <- simulation_results %>%
  filter(Year == 2054) %>%
  summarise(total_length_2054 = sum(Hosp_length, na.rm = TRUE))

# Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_length_2054)


# Length of stay (hours) for the year 2025
hospital_length_2025 <- simulation_results %>%
  filter(Year == 2025) %>%
  summarise(total_length_2025 = sum(Hosp_visit, na.rm = TRUE))

# Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_length_2025)









phase 3a

# Define the multimorbidity function outside the data frame
generate_multimorbidity <- function(age) {
  if (age < 21) {
    probs <- c(0.10, 0.06, 0.00)
  } else if (age <= 40) {
    probs <- c(0.30, 0.15, 0.00)
  } else if (age <= 60) {
    probs <- c(0.60, 0.30, 0.00)
  } else {
    probs <- c(0.90, 0.60, 0.30)
  }
  
  # Adjust to ensure the sum of probabilities is 1
  probs <- probs / sum(probs)  # Normalize probabilities so they sum to 1
  
  # Sample the number of conditions based on the probabilities
  sample(c(1, 2, 3), size = 1, prob = probs, replace = TRUE)
}

# Define the size of the population (n)
n <- 1000  # You can adjust n as needed

# Generate a basic synthetic population and include multimorbidity
population <- data.frame(
  ID = 1:n,
  Age = sample(0:90, n, replace = TRUE),  # Random age between 0 and 90
  Sex = sample(c('Male', 'Female'), n, replace = TRUE),  # Random sex
  Smoking = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),  # 15% smokers
  Alcohol = rnorm(n, mean = 6, sd = 3),  # Alcohol consumption (units per week)
  Multimorbidity = sapply(sample(0:90, n, replace = TRUE), generate_multimorbidity)  # Apply the multimorbidity function based on Age
)

# View the first few rows of the population data frame
head(population)


phase 3b
## Define hospital visit functions based on its predictors
# Example: Risk of CHD (Coronary Heart Disease) based on age, sex, smoking, and alcohol consumption
hosp_visit_func <- function(Age, Sex, Smoking, Alcohol) {
  base_func <- exp(1.9299)  # Base risk of CHD per year
  
  # Modify the base function based on the predictors
  func <- base_func + (Age) * exp(0.0005) + (Smoking) * exp(0.083) + (Alcohol) * exp(0.0129)
  if (Sex == 'Female') func <- func * 1.1  # Adjust function for females (higher risk by 10%)
  
  return(func)
}

## Initialize the simulation results data frame
simulation_results <- data.frame(Year = rep(2025:2054, each = n), 
                                 ID = rep(1:n, times = years_of_simulation),
                                 hosp_visit = NA)  # Column to store hospital visits over the years

## Simulate the hospital visits of each individual across years
for (year in 2025:2054) {
  # Update age each year
  population$Age <- population$Age + 1
  
  # Recalculate hospital visits based on updated age and other factors
  population <- population %>%
    mutate(
      hosp_visit = mapply(hosp_visit_func, Age, Sex, Smoking, Alcohol)  # Apply function
    )
  
  # Store the results for this year in simulation_results
  simulation_results$hosp_visit[simulation_results$Year == year] <- population$hosp_visit
}

phase 3

# Generate synthetic cohort
Population <- data.frame(ID = 1:n, Age = sample(10:80, n, replace = TRUE))

# Function to assign multimorbidity index
generate_multimorbidity <- function(age) {
  if (age < 21) {
    probs <- c(0.10, 0.06, 0.00)
  } else if (age <= 40) {
    probs <- c(0.30, 0.15, 0.00)
  } else if (age <= 60) {
    probs <- c(0.60, 0.30, 0.00)
  } else {
    probs <- c(0.90, 0.60, 0.30)
  }
  
  # Adjust to ensure the sum of probabilities is 1
  probs <- probs / sum(probs)  # Normalize probabilities so they sum to 1
  
  # Sample the number of conditions based on the probabilities
  sample(c(1, 2, 3), size = 1, prob = probs, replace = TRUE)
}
  

# Apply the function to generate multimorbidity index
Population$Multimorbidity_Index <- vapply(Population$Age, generate_multimorbidity, numeric(1))

# Create condition columns in the dataframe
for (condition in CMS_data$Condition) {
  Population[[condition]] <- 0  # Initialize all CMS_data as 0 (not present)
}

# Assign conditions based on multimorbidity index
for (i in 1:nrow(Population)) {
  num_CMS_data <- Population$Multimorbidity_Index[i]
  assigned_CMS_data <- sample(CMS_data$Condition, num_CMS_data, replace = FALSE)
  Population[i, assigned_CMS_data] <- 1  # Mark the assigned CMS_data as present (1)
}

Population[is.na(Population)] <- 0  # Replace NA with 0 for non-assigned conditions

# Compute CMS Scores

Population <- Population %>%
  mutate(
    Prevalence = rowSums(sweep(Population[, CMS_data$Condition], 2, CMS_data$Prevalence, "*")),
    Primary_Care_Score = rowSums(sweep(Population[, CMS_data$Condition], 2, CMS_data$Primary_Care_Weight, "*")),
    Hospital_Admission_Score = rowSums(sweep(Population[, CMS_data$Condition], 2, CMS_data$Hospital_Admission_Weight, "*")),
    Mortality_Score = rowSums(sweep(Population[, CMS_data$Condition], 2, CMS_data$Mortality_Weight, "*"))
  )

# View first few rows
head(Population)