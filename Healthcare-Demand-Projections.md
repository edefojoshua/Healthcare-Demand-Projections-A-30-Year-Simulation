Healthcare Demand Projections: A 30-Year Simulation
================
Joshua Edefo
2025-02-23

The study estimated healthcare demand over a 30-year period using UK
hypothetical data for conditions such as hypertension and diabetes in a
cohort of 1,000 individuals. In the first phase, data from sources
namely CPRD, HES, HSE and CMS were merged, cleaned, and analysed to
model hospital visits and lengths of stay. Negative Binomial and linear
regression models were employed to identify the factors influencing
hospital visits. In the second phase, a microsimulation was conducted to
generate a synthetic population, simulating healthcare demand over 30
years while considering estimates of variables such as age, sex, and
multimorbidity from the previous phase. The results projected hospital
visits and stays for 2025 and 2054 in phase 3. Finally, healthcare costs
were estimated by multiplying the length of hospital stays by an assumed
rate of £250 per hour, applying a discount rate for future costs. These
projections provided insights into long-term healthcare demand and
associated costs.

Libraries

``` r
# Load necessary libraries
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

``` r
library(readxl)
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.3

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.3.3

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.3.3

``` r
library(car)
```

    ## Warning: package 'car' was built under R version 4.3.3

    ## Warning: package 'carData' was built under R version 4.3.3

``` r
library(MASS)
library(lmtest)
library(writexl)
```

    ## Warning: package 'writexl' was built under R version 4.3.3

``` r
library(pscl)
```

    ## Warning: package 'pscl' was built under R version 4.3.3

``` r
library(gamlss)
```

    ## Warning: package 'gamlss' was built under R version 4.3.3

    ## Warning: package 'gamlss.data' was built under R version 4.3.3

    ## Warning: package 'gamlss.dist' was built under R version 4.3.3

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

Working environment

``` r
# Set working Directory
setwd("C:\\Users\\joe62\\OneDrive - Aberystwyth University\\Apps\\Desktop\\Destop Folder\\Health Demand Simulation")

# Set seed for reproducibility
set.seed(123)
```

Phase 1 - Data collection, and statistical analysis to generate model
parameters

``` r
# Data collection and integration

# Hypothetical Clinic Practice Research Datalink dataset
CPRD_data <- read_excel("CPRD_data.xlsx")
head (CPRD_data)
```

    ## # A tibble: 6 × 6
    ##   individual_id disease       treatment  Multimorbidity prescription_count   age
    ##           <dbl> <chr>         <chr>               <dbl>              <dbl> <dbl>
    ## 1             1 Asthma        Lifestyle…              0                 25    46
    ## 2             2 Hypertension  Lifestyle…              0                 44    48
    ## 3             3 Hypertension  Medication              1                 38    79
    ## 4             4 Heart Disease Lifestyle…              3                 40    89
    ## 5             5 Diabetes      Medication              2                 40    41
    ## 6             6 Heart Disease Lifestyle…              2                 49    52

``` r
# Assign Sex, Smoking and Alcohol based on hypothetical Health Survey for England  (HSE) 2022
CPRD_n_HSE_data <- CPRD_data %>%
  mutate(
    sex = sample(c('Male', 'Female'), nrow(CPRD_data), replace = TRUE),  # Assign random sexes
    Smoking = sample(c(0, 1), nrow(CPRD_data), replace = TRUE, prob = c(0.7, 0.3)),  # 30% smokers
    Alcohol = rnorm(nrow(CPRD_data), mean = 8, sd = 4)  # Alcohol consumption (units per week)
  )

## view (CPRD_n_HSE_data)

## write_xlsx(CPRD_n_HSE_data, "CPRD_n_HSE_data.xlsx")

# Importing hypothetical Hospital Episodic Statistics (HES) dataset
HES_data <-read_excel("HES_data.xlsx")
head(HES_data)
```

    ## # A tibble: 6 × 8
    ##   individual_id admission_type procedure_type length_of_stay discharge_status
    ##           <dbl> <chr>          <chr>                   <dbl> <chr>           
    ## 1             1 Elective       Radiology                  12 Died            
    ## 2             2 Elective       Surgery                    14 Died            
    ## 3             3 Emergency      Cardiology                 14 Transferred     
    ## 4             4 Maternity      Surgery                     7 Transferred     
    ## 5             5 Elective       Surgery                    14 Died            
    ## 6             6 Emergency      None                        9 Transferred     
    ## # ℹ 3 more variables: hospital_visits_yearly <dbl>, sex <chr>, age <dbl>

``` r
# Merging data with CPRD_n_HSE_data with HES_data
merged_data <- merge(CPRD_n_HSE_data, HES_data, by = "individual_id", all = FALSE)
head(merged_data)
```

    ##   individual_id       disease        treatment Multimorbidity
    ## 1             1        Asthma Lifestyle Change              0
    ## 2             2  Hypertension Lifestyle Change              0
    ## 3             3  Hypertension       Medication              1
    ## 4             4 Heart Disease Lifestyle Change              3
    ## 5             5      Diabetes       Medication              2
    ## 6             6 Heart Disease Lifestyle Change              2
    ##   prescription_count age.x  sex.x Smoking   Alcohol admission_type
    ## 1                 25    46   Male       0  4.016805       Elective
    ## 2                 44    48   Male       0  3.840180       Elective
    ## 3                 38    79   Male       0  7.928079      Emergency
    ## 4                 40    89 Female       1  7.471299      Maternity
    ## 5                 40    41   Male       1 -2.197371       Elective
    ## 6                 49    52 Female       0 12.162294      Emergency
    ##   procedure_type length_of_stay discharge_status hospital_visits_yearly  sex.y
    ## 1      Radiology             12             Died                      3   Male
    ## 2        Surgery             14             Died                      5   Male
    ## 3     Cardiology             14      Transferred                      6   Male
    ## 4        Surgery              7      Transferred                      0 Female
    ## 5        Surgery             14             Died                      4   Male
    ## 6           None              9      Transferred                      6 Female
    ##   age.y
    ## 1    84
    ## 2    58
    ## 3    76
    ## 4    22
    ## 5    23
    ## 6    75

``` r
## view(merged_data)

head(merged_data)
```

    ##   individual_id       disease        treatment Multimorbidity
    ## 1             1        Asthma Lifestyle Change              0
    ## 2             2  Hypertension Lifestyle Change              0
    ## 3             3  Hypertension       Medication              1
    ## 4             4 Heart Disease Lifestyle Change              3
    ## 5             5      Diabetes       Medication              2
    ## 6             6 Heart Disease Lifestyle Change              2
    ##   prescription_count age.x  sex.x Smoking   Alcohol admission_type
    ## 1                 25    46   Male       0  4.016805       Elective
    ## 2                 44    48   Male       0  3.840180       Elective
    ## 3                 38    79   Male       0  7.928079      Emergency
    ## 4                 40    89 Female       1  7.471299      Maternity
    ## 5                 40    41   Male       1 -2.197371       Elective
    ## 6                 49    52 Female       0 12.162294      Emergency
    ##   procedure_type length_of_stay discharge_status hospital_visits_yearly  sex.y
    ## 1      Radiology             12             Died                      3   Male
    ## 2        Surgery             14             Died                      5   Male
    ## 3     Cardiology             14      Transferred                      6   Male
    ## 4        Surgery              7      Transferred                      0 Female
    ## 5        Surgery             14             Died                      4   Male
    ## 6           None              9      Transferred                      6 Female
    ##   age.y
    ## 1    84
    ## 2    58
    ## 3    76
    ## 4    22
    ## 5    23
    ## 6    75

``` r
## write_xlsx(merged_data, "merged_data.xlsx")

# Merged data (CPRD_data,HSE_data and HES_data)
merged_data <-read_excel("merged_data.xlsx")
head(merged_data)
```

    ## # A tibble: 6 × 16
    ##   individual_id disease  treatment Multimorbidity prescription_count age.x sex.x
    ##           <dbl> <chr>    <chr>              <dbl>              <dbl> <dbl> <chr>
    ## 1             1 Asthma   Lifestyl…              0                 25    46 Male 
    ## 2             2 Hyperte… Lifestyl…              0                 44    48 Male 
    ## 3             3 Hyperte… Medicati…              1                 38    79 Male 
    ## 4             4 Heart D… Lifestyl…              3                 40    89 Fema…
    ## 5             5 Diabetes Medicati…              2                 40    41 Male 
    ## 6             6 Heart D… Lifestyl…              2                 49    52 Fema…
    ## # ℹ 9 more variables: Smoking <dbl>, Alcohol <dbl>, admission_type <chr>,
    ## #   procedure_type <chr>, length_of_stay <dbl>, discharge_status <chr>,
    ## #   hospital_visits_yearly <dbl>, sex.y <chr>, age.y <dbl>

``` r
# adjust hospital number of visits using the information from Cambridge Multimorbidity Score (CMS )
# "https://www.bristol.ac.uk/primaryhealthcare/news/2020/cambridge-multimorbidity-score.html"

merged_data_v2 <- merged_data %>%
  mutate(
    hospital_visits_adjusted = mapply(function(at, disease, Multimorbidity, hospital_visits_yearly) {
      
      ## When no multimorbidity
      if (Multimorbidity == 0) {
        return(ceiling(hospital_visits_yearly))
        
        ## When one disease is present
      } else if (Multimorbidity == 1 & disease %in% c("Asthma", "Hypertension", "Heart Disease", "Diabetes")) {
        ## Adjusting hospital visits for a single disease
        return(ceiling(sample(c(hospital_visits_yearly * 2.32, 
                                hospital_visits_yearly * 1.66, 
                                hospital_visits_yearly * 2.49, 
                                hospital_visits_yearly * 4.77), 1)))
        
        ## When two diseases are present
      } else if (Multimorbidity == 2) {
        ## Disease multipliers for 2 diseases
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
        
        ## When three diseases are present
      } else if (Multimorbidity == 3) {
        ## Disease multipliers for 3 diseases
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

## write_xlsx(merged_data_v2, "merged_data_v2.xlsx")
head (merged_data_v2)
```

    ## # A tibble: 6 × 17
    ##   individual_id disease  treatment Multimorbidity prescription_count age.x sex.x
    ##           <dbl> <chr>    <chr>              <dbl>              <dbl> <dbl> <chr>
    ## 1             1 Asthma   Lifestyl…              0                 25    46 Male 
    ## 2             2 Hyperte… Lifestyl…              0                 44    48 Male 
    ## 3             3 Hyperte… Medicati…              1                 38    79 Male 
    ## 4             4 Heart D… Lifestyl…              3                 40    89 Fema…
    ## 5             5 Diabetes Medicati…              2                 40    41 Male 
    ## 6             6 Heart D… Lifestyl…              2                 49    52 Fema…
    ## # ℹ 10 more variables: Smoking <dbl>, Alcohol <dbl>, admission_type <chr>,
    ## #   procedure_type <chr>, length_of_stay <dbl>, discharge_status <chr>,
    ## #   hospital_visits_yearly <dbl>, sex.y <chr>, age.y <dbl>,
    ## #   hospital_visits_adjusted <dbl>

``` r
# Merged data_v2 (CPRD_data,HSE_data and HES_data)
merged_data_v2 <-read_excel("merged_data_v2.xlsx")
## view (merged_data_v2)

# Data cleaning
sum(is.na(merged_data_v2))
```

    ## [1] 0

``` r
# Descriptive Statistical analysis
summary (merged_data_v2)
```

    ##  individual_id      disease           treatment         Multimorbidity 
    ##  Min.   :   1.0   Length:1000        Length:1000        Min.   :0.000  
    ##  1st Qu.: 250.8   Class :character   Class :character   1st Qu.:1.000  
    ##  Median : 500.5   Mode  :character   Mode  :character   Median :2.000  
    ##  Mean   : 500.5                                         Mean   :1.501  
    ##  3rd Qu.: 750.2                                         3rd Qu.:2.000  
    ##  Max.   :1000.0                                         Max.   :3.000  
    ##  prescription_count     age.x          sex.x              Smoking     
    ##  Min.   : 0.00      Min.   :10.00   Length:1000        Min.   :0.000  
    ##  1st Qu.:11.00      1st Qu.:43.00   Class :character   1st Qu.:0.000  
    ##  Median :24.00      Median :59.50   Mode  :character   Median :0.000  
    ##  Mean   :24.24      Mean   :57.44                      Mean   :0.297  
    ##  3rd Qu.:37.00      3rd Qu.:74.00                      3rd Qu.:1.000  
    ##  Max.   :50.00      Max.   :90.00                      Max.   :1.000  
    ##     Alcohol       admission_type     procedure_type     length_of_stay  
    ##  Min.   :-4.191   Length:1000        Length:1000        Min.   : 1.000  
    ##  1st Qu.: 5.387   Class :character   Class :character   1st Qu.: 4.000  
    ##  Median : 8.219   Mode  :character   Mode  :character   Median : 8.000  
    ##  Mean   : 8.170                                         Mean   : 7.579  
    ##  3rd Qu.:11.014                                         3rd Qu.:11.000  
    ##  Max.   :21.561                                         Max.   :14.000  
    ##  discharge_status   hospital_visits_yearly    sex.y               age.y      
    ##  Length:1000        Min.   :0.000          Length:1000        Min.   : 1.00  
    ##  Class :character   1st Qu.:0.000          Class :character   1st Qu.:45.00  
    ##  Mode  :character   Median :4.000          Mode  :character   Median :64.00  
    ##                     Mean   :3.795                             Mean   :58.91  
    ##                     3rd Qu.:6.000                             3rd Qu.:78.00  
    ##                     Max.   :9.000                             Max.   :90.00  
    ##  hospital_visits_adjusted
    ##  Min.   : 0.000          
    ##  1st Qu.: 0.000          
    ##  Median : 4.000          
    ##  Mean   : 4.984          
    ##  3rd Qu.: 7.000          
    ##  Max.   :39.000

``` r
##  Frequency Table with Proportions 
## Sex
prop.table(table(merged_data_v2$sex.x)) ## Female = 0.494, Male = 0.506
```

    ## 
    ## Female   Male 
    ##  0.494  0.506

``` r
##Disease
prop.table(table(merged_data_v2$disease)) ## No = 0.197, Yes = 0.803
```

    ## 
    ##        Asthma      Diabetes Heart Disease  Hypertension          None 
    ##         0.204         0.202         0.201         0.196         0.197

``` r
##Treatment
prop.table(table(merged_data_v2$treatment)) ## Lifestyle change = 0.267, medication = 0.254, none = 0.258, surgery= 0.221
```

    ## 
    ## Lifestyle Change       Medication             None          Surgery 
    ##            0.267            0.254            0.258            0.221

``` r
## Multimorbidity
prop.table(table(merged_data_v2$Multimorbidity)) ## 0 = 0.249, 1 = 0.242, 2 = 0.268, 3= 0.241
```

    ## 
    ##     0     1     2     3 
    ## 0.249 0.242 0.268 0.241

``` r
## Age
summary(merged_data_v2$age.x)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10.00   43.00   59.50   57.44   74.00   90.00

``` r
mean (merged_data$age_v2.x) # 57.437
```

    ## Warning: Unknown or uninitialised column: `age_v2.x`.

    ## Warning in mean.default(merged_data$age_v2.x): argument is not numeric or
    ## logical: returning NA

    ## [1] NA

``` r
sd (merged_data$age_v2.x) # 20.283
```

    ## Warning: Unknown or uninitialised column: `age_v2.x`.

    ## [1] NA

``` r
## proportion of each age group
prop.table(table(merged_data_v2$age_group)) #Below 20 years = 0.041,  21-40 years = 0.172, 41-60 years = 0.310, 61-90 years = 0.477
```

    ## Warning: Unknown or uninitialised column: `age_group`.

    ## numeric(0)

``` r
## Smoking
prop.table(table(merged_data_v2$Smoking)) # non smokers = 0.703, smokers = 0.297
```

    ## 
    ##     0     1 
    ## 0.703 0.297

``` r
## Alcohol
summary(merged_data_v2$Alcohol)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.191   5.387   8.219   8.170  11.014  21.561

``` r
mean (merged_data_v2$Alcohol) # 8.170
```

    ## [1] 8.169861

``` r
sd (merged_data_v2$Alcohol) # 4.039
```

    ## [1] 4.038697

``` r
##Admission_type
prop.table(table(merged_data_v2$admission_type)) # Elective = 0.327, Emergency = 0.343, Maternity = 0.330
```

    ## 
    ##  Elective Emergency Maternity 
    ##     0.327     0.343     0.330

``` r
## Length_of_stay
summary(merged_data_v2$length_of_stay)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   4.000   8.000   7.579  11.000  14.000

``` r
mean (merged_data_v2$length_of_stay) # 7.579
```

    ## [1] 7.579

``` r
sd (merged_data_v2$length_of_stay) # 4.001
```

    ## [1] 4.000971

``` r
## Hospital_visits_yearly (adjusted)
summary(merged_data_v2$hospital_visits_adjusted)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   4.000   4.984   7.000  39.000

``` r
mean (merged_data_v2$hospital_visits_adjusted) # 4.984
```

    ## [1] 4.984

``` r
sd (merged_data_v2$hospital_visits_adjusted) # 5.754
```

    ## [1] 5.753681

``` r
# Inferential  Statistical analysis (1)
# Hospital visits as the outcome variable
head(merged_data_v2)
```

    ## # A tibble: 6 × 17
    ##   individual_id disease  treatment Multimorbidity prescription_count age.x sex.x
    ##           <dbl> <chr>    <chr>              <dbl>              <dbl> <dbl> <chr>
    ## 1             1 Asthma   Lifestyl…              0                 25    46 Male 
    ## 2             2 Hyperte… Lifestyl…              0                 44    48 Male 
    ## 3             3 Hyperte… Medicati…              1                 38    79 Male 
    ## 4             4 Heart D… Lifestyl…              3                 40    89 Fema…
    ## 5             5 Diabetes Medicati…              2                 40    41 Male 
    ## 6             6 Heart D… Lifestyl…              2                 49    52 Fema…
    ## # ℹ 10 more variables: Smoking <dbl>, Alcohol <dbl>, admission_type <chr>,
    ## #   procedure_type <chr>, length_of_stay <dbl>, discharge_status <chr>,
    ## #   hospital_visits_yearly <dbl>, sex.y <chr>, age.y <dbl>,
    ## #   hospital_visits_adjusted <dbl>

``` r
summary(merged_data_v2$hospital_visits_adjusted)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   4.000   4.984   7.000  39.000

``` r
table(merged_data_v2$hospital_visits_adjusted)  # Check if all values are non-negative integers
```

    ## 
    ##   0   3   4   5   6   7   8   9  10  12  13  14  15  17  18  19  20  21  24  29 
    ## 345  75  93  89 104  86  87   9  22  11   4  13  10   6   3   9  13   1   8   6 
    ##  34  39 
    ##   1   5

``` r
## Checking Data Distribution
mean_var_ratio <- var(merged_data_v2$hospital_visits_yearly) / mean(merged_data_v2$hospital_visits_yearly)
print(mean_var_ratio)
```

    ## [1] 2.270775

``` r
## This mean possion distribution might not possible as there is over dispersion (2.270775)

## Using log linear model (count model for number of hospital visits)

## Negative Binomial Regression
hosp_visit_influence_factors <- glm.nb(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                                       data = merged_data_v2)
summary(hosp_visit_influence_factors)
```

    ## 
    ## Call:
    ## glm.nb(formula = hospital_visits_adjusted ~ sex.x + age.x + Smoking + 
    ##     Alcohol + admission_type + procedure_type + Multimorbidity, 
    ##     data = merged_data_v2, init.theta = 3.038170929, link = log)
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              1.9298997  0.1086962  17.755  < 2e-16 ***
    ## sex.xMale               -0.0506388  0.0518928  -0.976   0.3291    
    ## age.x                   -0.0004698  0.0012721  -0.369   0.7119    
    ## Smoking                  0.0822880  0.0560777   1.467   0.1423    
    ## Alcohol                 -0.0128891  0.0063298  -2.036   0.0417 *  
    ## admission_typeEmergency  0.5529303  0.0537908  10.279  < 2e-16 ***
    ## admission_typeMaternity -2.6765988  0.1053923 -25.397  < 2e-16 ***
    ## procedure_typeNone      -0.0051325  0.0707108  -0.073   0.9421    
    ## procedure_typeRadiology  0.0384473  0.0719977   0.534   0.5933    
    ## procedure_typeSurgery    0.0203406  0.0726788   0.280   0.7796    
    ## Multimorbidity          -0.1051938  0.0234804  -4.480 7.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(3.0382) family taken to be 1)
    ## 
    ##     Null deviance: 2781.9  on 999  degrees of freedom
    ## Residual deviance: 1134.1  on 989  degrees of freedom
    ## AIC: 4389.3
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  3.038 
    ##           Std. Err.:  0.260 
    ## 
    ##  2 x log-likelihood:  -4365.314

``` r
## Run and compare Poisson vs. Negative Binomial Model Fit
hosp_visit_influence_factors_v2 <- glm(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                                       data = merged_data_v2, family = poisson)
summary(hosp_visit_influence_factors_v2)
```

    ## 
    ## Call:
    ## glm(formula = hospital_visits_adjusted ~ sex.x + age.x + Smoking + 
    ##     Alcohol + admission_type + procedure_type + Multimorbidity, 
    ##     family = poisson, data = merged_data_v2)
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              1.8804849  0.0596932  31.502  < 2e-16 ***
    ## sex.xMale                0.0630050  0.0286436   2.200  0.02783 *  
    ## age.x                   -0.0016154  0.0006948  -2.325  0.02008 *  
    ## Smoking                  0.0060875  0.0309867   0.196  0.84425    
    ## Alcohol                 -0.0108045  0.0034850  -3.100  0.00193 ** 
    ## admission_typeEmergency  0.5631094  0.0300847  18.717  < 2e-16 ***
    ## admission_typeMaternity -2.6594747  0.0948477 -28.039  < 2e-16 ***
    ## procedure_typeNone       0.0496370  0.0391883   1.267  0.20529    
    ## procedure_typeRadiology  0.0524914  0.0397590   1.320  0.18676    
    ## procedure_typeSurgery    0.0512135  0.0401057   1.277  0.20162    
    ## Multimorbidity          -0.0841985  0.0130113  -6.471 9.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 6129.3  on 999  degrees of freedom
    ## Residual deviance: 2663.5  on 989  degrees of freedom
    ## AIC: 5128.7
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
##Compare Poisson vs. Negative Binomial
vuong(hosp_visit_influence_factors, hosp_visit_influence_factors_v2)
```

    ## Vuong Non-Nested Hypothesis Test-Statistic: 
    ## (test-statistic is asymptotically distributed N(0,1) under the
    ##  null that the models are indistinguishible)
    ## -------------------------------------------------------------
    ##               Vuong z-statistic             H_A    p-value
    ## Raw                    4.884685 model1 > model2 5.1797e-07
    ## AIC-corrected          4.884685 model1 > model2 5.1797e-07
    ## BIC-corrected          4.884685 model1 > model2 5.1797e-07

``` r
## Since the comparison of Models gave a positive Z statistics (4.884685) and significant p-value (5.1797e-07)
## Negative Binomial Regression model is better and is chosen
##Poisson Model  is preferred because it is simpler 

## Diagnostic  checks for the log linear model 
## Check for Multicollinearity
vif(hosp_visit_influence_factors)
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## sex.x          1.019302  1        1.009605
    ## age.x          1.030858  1        1.015312
    ## Smoking        1.009190  1        1.004585
    ## Alcohol        1.010659  1        1.005315
    ## admission_type 1.009092  2        1.002265
    ## procedure_type 1.025272  3        1.004168
    ## Multimorbidity 1.026976  1        1.013398

``` r
## The VIF values for all predictors are very close to 1, which means 
##that there is little to no collinearity among your independent variables. 

###Residual Analysis
plot(fitted(hosp_visit_influence_factors), residuals(hosp_visit_influence_factors, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals", main = "Residual Plot")
abline(h = 0, col = "red")
```

![](Healthcare-Demand-Projections_files/figure-gfm/b-1.png)<!-- -->

``` r
## plots almost randomly scattered residuals around zero, indicating a good fit.

## Negative Binomial Regression
hosp_visit_influence_factors <- glm.nb(hospital_visits_adjusted ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                                       data = merged_data_v2)
summary(hosp_visit_influence_factors)
```

    ## 
    ## Call:
    ## glm.nb(formula = hospital_visits_adjusted ~ sex.x + age.x + Smoking + 
    ##     Alcohol + admission_type + procedure_type + Multimorbidity, 
    ##     data = merged_data_v2, init.theta = 3.038170929, link = log)
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)              1.9298997  0.1086962  17.755  < 2e-16 ***
    ## sex.xMale               -0.0506388  0.0518928  -0.976   0.3291    
    ## age.x                   -0.0004698  0.0012721  -0.369   0.7119    
    ## Smoking                  0.0822880  0.0560777   1.467   0.1423    
    ## Alcohol                 -0.0128891  0.0063298  -2.036   0.0417 *  
    ## admission_typeEmergency  0.5529303  0.0537908  10.279  < 2e-16 ***
    ## admission_typeMaternity -2.6765988  0.1053923 -25.397  < 2e-16 ***
    ## procedure_typeNone      -0.0051325  0.0707108  -0.073   0.9421    
    ## procedure_typeRadiology  0.0384473  0.0719977   0.534   0.5933    
    ## procedure_typeSurgery    0.0203406  0.0726788   0.280   0.7796    
    ## Multimorbidity          -0.1051938  0.0234804  -4.480 7.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Negative Binomial(3.0382) family taken to be 1)
    ## 
    ##     Null deviance: 2781.9  on 999  degrees of freedom
    ## Residual deviance: 1134.1  on 989  degrees of freedom
    ## AIC: 4389.3
    ## 
    ## Number of Fisher Scoring iterations: 1
    ## 
    ## 
    ##               Theta:  3.038 
    ##           Std. Err.:  0.260 
    ## 
    ##  2 x log-likelihood:  -4365.314

``` r
## Considering only the significant predictors (p < 0.05), the mathematical equation for the Poisson regression model is:
## hospital_visits_adjusted=exp(1.9299−0.0129⋅Alcohol+0.5529⋅admission_typeEmergency−2.6766⋅admission_typeMaternity−0.1052⋅Multimorbidity)
##so lets modify this equation to reflect real world scenario
## hospital_visits_adjusted = 1.9299+0.05.age.x+0.083.Smoking+0.0129⋅Alcohol+0.1052⋅Multimorbidity)


# Inferential  Statistical analysis (2)
# when the outcome variable is hospital length of stay (length)
head (merged_data_v2)
```

    ## # A tibble: 6 × 17
    ##   individual_id disease  treatment Multimorbidity prescription_count age.x sex.x
    ##           <dbl> <chr>    <chr>              <dbl>              <dbl> <dbl> <chr>
    ## 1             1 Asthma   Lifestyl…              0                 25    46 Male 
    ## 2             2 Hyperte… Lifestyl…              0                 44    48 Male 
    ## 3             3 Hyperte… Medicati…              1                 38    79 Male 
    ## 4             4 Heart D… Lifestyl…              3                 40    89 Fema…
    ## 5             5 Diabetes Medicati…              2                 40    41 Male 
    ## 6             6 Heart D… Lifestyl…              2                 49    52 Fema…
    ## # ℹ 10 more variables: Smoking <dbl>, Alcohol <dbl>, admission_type <chr>,
    ## #   procedure_type <chr>, length_of_stay <dbl>, discharge_status <chr>,
    ## #   hospital_visits_yearly <dbl>, sex.y <chr>, age.y <dbl>,
    ## #   hospital_visits_adjusted <dbl>

``` r
hosp_length_of_stay <- lm(length_of_stay ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                          data = merged_data_v2)
summary(hosp_length_of_stay)
```

    ## 
    ## Call:
    ## lm(formula = length_of_stay ~ sex.x + age.x + Smoking + Alcohol + 
    ##     admission_type + procedure_type + Multimorbidity, data = merged_data_v2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.3413 -3.3130  0.1077  3.3890  7.0021 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              8.164572   0.558505  14.619   <2e-16 ***
    ## sex.xMale                0.089088   0.255755   0.348    0.728    
    ## age.x                   -0.007678   0.006325  -1.214    0.225    
    ## Smoking                  0.174621   0.278857   0.626    0.531    
    ## Alcohol                  0.012992   0.031644   0.411    0.681    
    ## admission_typeEmergency -0.196070   0.310994  -0.630    0.529    
    ## admission_typeMaternity -0.210915   0.314647  -0.670    0.503    
    ## procedure_typeNone      -0.063326   0.351437  -0.180    0.857    
    ## procedure_typeRadiology -0.183005   0.357457  -0.512    0.609    
    ## procedure_typeSurgery   -0.010309   0.357212  -0.029    0.977    
    ## Multimorbidity          -0.099108   0.115775  -0.856    0.392    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.013 on 989 degrees of freedom
    ## Multiple R-squared:  0.003972,   Adjusted R-squared:  -0.006099 
    ## F-statistic: 0.3944 on 10 and 989 DF,  p-value: 0.9495

``` r
## Diagnostic checks
## Residuals vs Fitted plot
plot(hosp_length_of_stay, which = 1)
```

![](Healthcare-Demand-Projections_files/figure-gfm/b-2.png)<!-- -->

``` r
## randomly scattered and evenly distributed

## Normal Q-Q plot
plot(hosp_length_of_stay, which = 2)
```

![](Healthcare-Demand-Projections_files/figure-gfm/b-3.png)<!-- -->

``` r
## line almost diagonal all through

## Scale-Location plot
plot(hosp_length_of_stay, which = 3)
```

![](Healthcare-Demand-Projections_files/figure-gfm/b-4.png)<!-- -->

``` r
## residuals have spread evenly distributed

## Residuals vs Leverage plot
plot(hosp_length_of_stay, which = 5)
```

![](Healthcare-Demand-Projections_files/figure-gfm/b-5.png)<!-- -->

``` r
## more than 99% of the data are within the Cook's distance threshold

##  Durbin-Watson Test
library(lmtest)
dwtest(hosp_length_of_stay)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  hosp_length_of_stay
    ## DW = 2.0027, p-value = 0.5174
    ## alternative hypothesis: true autocorrelation is greater than 0

``` r
## the value is between 1 and 3, hence no autocorrelation

## Variance Inflation Factor (VIF)
library(car)
vif(hosp_length_of_stay) 
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## sex.x          1.015203  1        1.007573
    ## age.x          1.020792  1        1.010342
    ## Smoking        1.008098  1        1.004041
    ## Alcohol        1.013112  1        1.006535
    ## admission_type 1.013414  2        1.003337
    ## procedure_type 1.020186  3        1.003336
    ## Multimorbidity 1.023671  1        1.011766

``` r
## very low multicolinearity as the values are around 1
## so continue with the linear model

hosp_length_of_stay <- lm(length_of_stay ~ sex.x + age.x + Smoking + Alcohol + admission_type + procedure_type + Multimorbidity, 
                          data = merged_data_v2)
summary(hosp_length_of_stay)
```

    ## 
    ## Call:
    ## lm(formula = length_of_stay ~ sex.x + age.x + Smoking + Alcohol + 
    ##     admission_type + procedure_type + Multimorbidity, data = merged_data_v2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.3413 -3.3130  0.1077  3.3890  7.0021 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              8.164572   0.558505  14.619   <2e-16 ***
    ## sex.xMale                0.089088   0.255755   0.348    0.728    
    ## age.x                   -0.007678   0.006325  -1.214    0.225    
    ## Smoking                  0.174621   0.278857   0.626    0.531    
    ## Alcohol                  0.012992   0.031644   0.411    0.681    
    ## admission_typeEmergency -0.196070   0.310994  -0.630    0.529    
    ## admission_typeMaternity -0.210915   0.314647  -0.670    0.503    
    ## procedure_typeNone      -0.063326   0.351437  -0.180    0.857    
    ## procedure_typeRadiology -0.183005   0.357457  -0.512    0.609    
    ## procedure_typeSurgery   -0.010309   0.357212  -0.029    0.977    
    ## Multimorbidity          -0.099108   0.115775  -0.856    0.392    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.013 on 989 degrees of freedom
    ## Multiple R-squared:  0.003972,   Adjusted R-squared:  -0.006099 
    ## F-statistic: 0.3944 on 10 and 989 DF,  p-value: 0.9495

``` r
## length of stay=8.164572+(−0.007678×age.x)+(−0.099108×Multimorbidity)+

##so lets modify the model to represent real world cohort
## length of stay= 1.1645+ 0.08×age.x+0.199×Multimorbidity)+ ϵ
```

Phase 2 - Microsimulation

``` r
# Use the estimates from phase 1 for imulation to determine demand for health care over 30 years period in a patient cohort size of 1000 individuals

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
  
  ## Adjust to ensure the sum of probabilities is 1
  probs <- probs / sum(probs)  # Normalize probabilities so they sum to 1
  
  # #Sample the number of conditions based on the probabilities
  sample(c(1, 2, 3), size = 1, prob = probs, replace = TRUE)
}

## Define simulation parameters
n <- 1000  # Number of synthetic individuals
years_of_simulation <- 30  ## Simulation time horizon (2025-2054)

## Generate a basic synthetic population and include multimorbidity
population <- data.frame(
  ID = 1:n,
  Age = sample(0:90, n, replace = TRUE),  ## Random age between 0 and 90
  Sex = sample(c('Male', 'Female'), n, replace = TRUE),  ## Random sex
  Smoking = sample(c(0, 1), n, replace = TRUE, prob = c(0.85, 0.15)),  ## 15% smokers
  Alcohol = rnorm(n, mean = 6, sd = 3)  ## Alcohol consumption (units per week)
)

## Add Multimorbidity to the population based on Age
population$Multimorbidity <- sapply(population$Age, generate_multimorbidity)

## Assume no smoking and alcohol intake for those below 18 years of age
population$Smoking[population$Age < 18] <- 0  ## No smoking for individuals under 18
population$Alcohol[population$Age < 18] <- 0  ## No alcohol consumption for individuals under 18
## View the first few rows of the population data frame
head(population)
```

    ##   ID Age    Sex Smoking  Alcohol Multimorbidity
    ## 1  1  54   Male       0 3.888998              2
    ## 2  2  77   Male       0 6.605159              3
    ## 3  3   3 Female       0 0.000000              1
    ## 4  4  25   Male       0 7.008545              1
    ## 5  5  49   Male       0 8.249477              1
    ## 6  6  35 Female       0 3.597353              2

``` r
## Define hospital visit functions based on its predictors
hosp_visit_func <- function(Age, Sex, Smoking, Alcohol, Multimorbidity) {
  base_func <- 1.93  ## Base risk of CHD per year
  
  # Modify the base function based on the predictors
  func <- base_func + (Age) * 0.05 + (Smoking) * 0.083 + (Alcohol) * 0.013 + (Multimorbidity) * 0.105
  if (Sex == 'Female') func <- func * 1.1  # Adjust function for females (higher risk by 10%)
  
  return(func)
}

## Simulate the disease incidence over time (CHD in this case)
population <- population %>%
  mutate(
    Hosp_visit = mapply(hosp_visit_func, Age, Sex, Smoking, Alcohol, Multimorbidity)  # Apply hosp function
  )

## View the first few rows of the updated population data frame
head(population)
```

    ##   ID Age    Sex Smoking  Alcohol Multimorbidity Hosp_visit
    ## 1  1  54   Male       0 3.888998              2   4.890557
    ## 2  2  77   Male       0 6.605159              3   6.180867
    ## 3  3   3 Female       0 0.000000              1   2.403500
    ## 4  4  25   Male       0 7.008545              1   3.376111
    ## 5  5  49   Male       0 8.249477              1   4.592243
    ## 6  6  35 Female       0 3.597353              2   4.330442

``` r
## Define hospital length of stay functions based on its predictors
## Length of stay= 1.164572+ 0.7678×age.x+0.199108×Multimorbidity)+ ϵ

hosp_length_func <- function(Age, Multimorbidity) {
  base_length <-1.1645  ##Base  length os stay
  
  # Modify the base function based on the predictors
  length <- base_length + (Age) * 0.08 + (Multimorbidity * 0.2)
  return(length)
}

## Simulate the disease incidence over time (CHD in this case)
population <- population %>%
  mutate(
    Hosp_length = mapply(hosp_length_func, Age,  Multimorbidity)  ## Apply length function
  )

## View the first few rows of the updated population data frame
head(population)
```

    ##   ID Age    Sex Smoking  Alcohol Multimorbidity Hosp_visit Hosp_length
    ## 1  1  54   Male       0 3.888998              2   4.890557      5.8845
    ## 2  2  77   Male       0 6.605159              3   6.180867      7.9245
    ## 3  3   3 Female       0 0.000000              1   2.403500      1.6045
    ## 4  4  25   Male       0 7.008545              1   3.376111      3.3645
    ## 5  5  49   Male       0 8.249477              1   4.592243      5.2845
    ## 6  6  35 Female       0 3.597353              2   4.330442      4.3645

``` r
simulation_results <- data.frame(Year = rep(2025:2054, each = n), 
                                 ID = rep(1:n, times = years_of_simulation))

## Simulate the hospital visits of each individual across years
for (year in 2025:2054) {
  ## Update age each year
  population$Age <- population$Age + 1
  
  ## Recalculate hospital visits based on updated age and other factors
  population <- population %>%
    mutate(
      Hosp_visit = mapply(hosp_visit_func, Age, Sex, Smoking, Alcohol, Multimorbidity),  # Apply function
      Hosp_length = mapply(hosp_length_func, Age,  Multimorbidity)
    )
  
  ## Store the results for this year in simulation_results
  simulation_results$Hosp_visit[simulation_results$Year == year] <- population$Hosp_visit
  simulation_results$Hosp_length[simulation_results$Year == year] <- population$Hosp_length
}

head(simulation_results)
```

    ##   Year ID Hosp_visit Hosp_length
    ## 1 2025  1   4.940557      5.9645
    ## 2 2025  2   6.230867      8.0045
    ## 3 2025  3   2.458500      1.6845
    ## 4 2025  4   3.426111      3.4445
    ## 5 2025  5   4.642243      5.3645
    ## 6 2025  6   4.385442      4.4445

``` r
## Visualizing the results
## Plotting the increase in hospital visits  over time
ggplot(simulation_results, aes(x = Year, y = Hosp_visit)) +
  geom_line(stat = "summary", fun = "mean", color = "blue") +
  labs(title = "Increase in hospital visit over Time", x = "Year", y = "Hospital visits") +
  theme_minimal()
```

![](Healthcare-Demand-Projections_files/figure-gfm/d-1.png)<!-- -->

``` r
## Visualizing the results
## Plotting the increase in hospital visits  over time
ggplot(simulation_results, aes(x = Year, y = Hosp_length)) +
  geom_line(stat = "summary", fun = "mean", color = "red") +
  labs(title = "Increase in hospital length of stay over Time", x = "Year", y = "Hospital length of stay") +
  theme_minimal()
```

![](Healthcare-Demand-Projections_files/figure-gfm/d-2.png)<!-- -->

Phase 3 - Projections and Evaluations

``` r
##  Number of hospital visits in 2025
## Filter the simulation results for the year 2024
hospital_visits_2025 <- simulation_results %>%
  filter(Year == 2025) %>%
  summarise(total_visits_2025 = sum(Hosp_visit, na.rm = TRUE))
## Print the total number of hospital visits in 2054 for the cohort
print(hospital_visits_2025)
```

    ##   total_visits_2025
    ## 1          4644.808

``` r
## the number of hospital visit in the start year (2025) of the simulation is 4676

## Projected number of hospital visits in 2054 ( 30 years from the start date of simulation)
## Filter the simulation results for the year 2054
hospital_visits_2054 <- simulation_results %>%
  filter(Year == 2054) %>%
  summarise(total_visits_2054 = sum(Hosp_visit, na.rm = TRUE))
## Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_visits_2054)
```

    ##   total_visits_2054
    ## 1          6167.743

``` r
## Length of stay (hours) for the year 2025 (start year of the simulation)
hospital_length_2025 <- simulation_results %>%
  filter(Year == 2025) %>%
  summarise(total_length_2025 = sum(Hosp_length, na.rm = TRUE))
## Print the total number of hospital visits in 2025 for the cohort
print(hospital_length_2025)
```

    ##   total_length_2025
    ## 1           5089.74

``` r
#projected total number of hospital length of stay (hours) in 2054 for the cohort
## Filter the simulation results for the year 2054
hospital_length_2054 <- simulation_results %>%
  filter(Year == 2054) %>%
  summarise(total_length_2054 = sum(Hosp_length, na.rm = TRUE))
## Print the projected total number of hospital visits in 2054 for the cohort
print(hospital_length_2054)
```

    ##   total_length_2054
    ## 1           7409.74

``` r
# Cost of Healthcare demand

# Assuming an hour spent in the hospital will cost the NHS an average of £250

## The cost of healthare demand in 2025 
## cost_of_health_demand_2025 <- simulation_results$Hosp_length * 250

cost_of_health_demand_2025 <- simulation_results %>%
  filter(Year == 2025) %>%
  summarise(cost_of_health_demand_2025 = sum(Hosp_length* 250, na.rm = TRUE))
print (cost_of_health_demand_2025)
```

    ##   cost_of_health_demand_2025
    ## 1                    1272435

``` r
cost_of_health_demand_2054 <- simulation_results %>%
  filter(Year == 2054) %>%
  summarise(cost_of_health_demand_2054 = sum(Hosp_length * 250, na.rm = TRUE))

# Print the original cost of health demand for 2054
print(cost_of_health_demand_2054)
```

    ##   cost_of_health_demand_2054
    ## 1                    1852435

``` r
## Applying yearly discounting rate of 3.5%
r <- 0.035        ## Discount rate (3.5%)
n <- 30           ## Number of years

# Calculate the discounted cost for 2054
cost_of_health_demand_2054_dis <- cost_of_health_demand_2054$cost_of_health_demand_2054 / (1 + r) ^ n

# Print the discounted cost of health demand for 2054
print(cost_of_health_demand_2054_dis)
```

    ## [1] 659982.6

``` r
## Applying yearly discounting rate of (3.5%)
cost_of_health_demand_2054 <- cost_of_health_demand_2054      
r <- 0.035        ## Discount rate (3.5%)
n <- 30           ## Number of years

## Calculate the present value for each year
cost_of_health_demand_2054_dis <- cost_of_health_demand_2054 / (1 + r) ^ n

## #Print the result
print(cost_of_health_demand_2054_dis)
```

    ##   cost_of_health_demand_2054
    ## 1                   659982.6

Session information

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] parallel  splines   stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] usethis_2.2.2     gamlss_5.4-22     nlme_3.1-162      gamlss.dist_6.1-1
    ##  [5] gamlss.data_6.0-6 pscl_1.5.9        writexl_1.5.1     lmtest_0.9-40    
    ##  [9] zoo_1.8-12        MASS_7.3-60       car_3.1-3         carData_3.0-5    
    ## [13] psych_2.4.12      skimr_2.1.5       lubridate_1.9.3   forcats_1.0.0    
    ## [17] stringr_1.5.0     purrr_1.0.2       readr_2.1.4       tidyr_1.3.0      
    ## [21] tibble_3.2.1      ggplot2_3.5.1     tidyverse_2.0.0   readxl_1.4.3     
    ## [25] dplyr_1.1.4      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.4      xfun_0.40         lattice_0.21-8    tzdb_0.4.0       
    ##  [5] vctrs_0.6.5       tools_4.3.1       generics_0.1.3    fansi_1.0.4      
    ##  [9] pkgconfig_2.0.3   Matrix_1.6-1.1    lifecycle_1.0.3   farver_2.1.1     
    ## [13] compiler_4.3.1    munsell_0.5.0     mnormt_2.1.1      repr_1.1.7       
    ## [17] htmltools_0.5.8.1 yaml_2.3.7        Formula_1.2-5     pillar_1.9.0     
    ## [21] abind_1.4-5       tidyselect_1.2.0  digest_0.6.33     stringi_1.7.12   
    ## [25] labeling_0.4.3    fastmap_1.2.0     grid_4.3.1        colorspace_2.1-0 
    ## [29] cli_3.6.1         magrittr_2.0.3    base64enc_0.1-3   survival_3.7-0   
    ## [33] utf8_1.2.3        withr_2.5.0       scales_1.3.0      timechange_0.2.0 
    ## [37] rmarkdown_2.25    cellranger_1.1.0  hms_1.1.3         evaluate_0.21    
    ## [41] knitr_1.44        rlang_1.1.1       glue_1.6.2        rstudioapi_0.15.0
    ## [45] jsonlite_1.8.7    R6_2.5.1          fs_1.6.3
