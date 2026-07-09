library(dplyr)


Final_Educ <- read_csv("Final_Educ.csv")

data <- Final_Educ %>% 
  filter(!(between(R6B_KD, 1, 100) | between(R6B_KD, 279, 294))) %>%
  filter(!is.na(R6B_KD)) %>% 
  mutate(
    job_self_employed = Job_Status == 1,
    job_self_employed_informal = Job_Status == 2,
    job_self_employed_paid = Job_Status == 3,
    job_employee = Job_Status == 4,
    job_freelancer_agriculture = Job_Status == 5,
    job_freelancer_non_agriculture = Job_Status == 6, 
    log_Wage = log(Wage
    ),
    STEM = R6B_KD %in% c(
      106,107,108,109,110,111,113,114,120,121,122,130,131,139,
      141:176,177:232,236,238,239,240,241,242,243,244,247,248,
      249,250,251,252,253,254,255,256,257,258,260,261,265:267,
      269,271:278
    ),
    NonSTEM = R6B_KD %in% c(
      101,102,103,104,105,112,115,116,117,118,119,123,124,125,
      126,127,128,129,132,133,134,135,136,137,138,140,233,235,
      237,245,246,259,262,263,264,268,270
    ),
    men_STEM = (Gender == 1 & STEM == TRUE),
    women_STEM = (Gender == 2 & STEM == TRUE),
    men_non_STEM = (Gender == 1 & STEM == FALSE),
    women_non_STEM = (Gender == 2 & STEM == FALSE)) %>%
  select(-all_of(c("R8B","K3","K5_TH","R17B","R17A1","R17A2","R17B","R6E")))


write.csv(data, "Final_Educ.csv", row.names = FALSE)

######load data
df <- read.csv("C:/Users/LENOVO/Downloads/Final_Educ.csv")
library(tidyr)
library(dplyr)
##dropping NA data
df_clean_all <- df %>%
  drop_na()
##creating highest degree achieved variables
df_final <- df_clean_all %>%
# Use mutate() to create new columns
  mutate(
    # 1. Bachelors Degree: Education_Level equal to 9
    Bachelors_Degree = if_else(Education_Level == 9, 1, 0),
    
    # 2. Masters Degree: Education_Level equal to 10
    Masters_Degree = if_else(Education_Level == 10, 1, 0),
    
    # 3. Doctoral Degree: Education_Level equal to 12
    Doctoral_Degree = if_else(Education_Level == 12, 1, 0)
  )
##cleaning for binary variables
df_final <- df_final %>%
  mutate(
    across(
      # Columns to transform from 1/2 to 1/0
      c(Urban_Rural, Gender, Marital_Status, Certifications, ComputerForWork, InternetForWork),
      
      # Transformation logic: if 2, change to 0, otherwise keep the original value (1)
      ~ if_else(.x == 2, 0, as.numeric(.x))
    )
  )
#cleaning phone for work
df_final <- df_final %>%
  mutate(
    SmartphoneForWork_New = as.numeric(SmartphoneForWork == 3)
  )
df_final <- df_final %>%
  select(
    -SmartphoneForWork, 
    -SmartphoneForWork_New
  )
#cleaning marital status, dropping marital status if divorced and a widow
df_final <- df_final %>%
  filter(Marital_Status != 3 & Marital_Status != 4)


##creating hourly wage
df_final <- df_final %>%
  mutate(
    Hourly_Wage = Wage / Hours_Worked,
    log_Hourly_Wage = log(Hourly_Wage)
  )
##creating groups based on hours work, 
df_final <- df_final %>%
  mutate(
    Employment_Status = case_when(
      Hours_Worked <= 24               ~ "Minijob",
      Hours_Worked >= 42               ~ "Full-Time",
      Hours_Worked > 24 & Hours_Worked < 42 ~ "Part-Time",
      TRUE ~ as.character(NA) # Assign NA if Hours_Worked is NA or 0 (if 0 wasn't excluded previously)
    ),
    Employment_Status = factor(Employment_Status, 
                               levels = c("Minijob", "Part-Time", "Full-Time"))
  )
df_final <- df_final %>%
  mutate(
    # Full-Time (1 if Full-Time, 0 otherwise)
    Fulltime_Work = as.integer(Employment_Status == "Full-Time"),
    
    # Part-Time (1 if Part-Time, 0 otherwise)
    Parttime_Work = as.integer(Employment_Status == "Part-Time"),
    
    # Minijob (1 if Minijob, 0 otherwise)
    Minijob = as.integer(Employment_Status == "Minijob")
  )

##creating euro value on wage
#conversion and create the new log variable
library(dplyr)
EXCHANGE_RATE_IDR_PER_EURO <- 16925.57
df_final <- df_final %>%
  mutate(
    # Convert monthly wage from IDR to EUR
    Wage_EUR = Wage / EXCHANGE_RATE_IDR_PER_EURO,
    
    # Create the logarithm of the new Euro wage variable
    log_Wage_EUR = log(Wage_EUR)
  )

##OLS with monthly wage (not logged) in euros 
model_formula_wage_euros <- Wage_EUR ~ STEM * Gender +
  Marital_Status + Certifications + 
  ComputerForWork + InternetForWork + 
  Urban_Rural + Age + Hours_Worked +
  job_self_employed + job_self_employed_informal + 
  job_employee + job_self_employed_paid +
  Fulltime_Work + Parttime_Work

ols_wage_euros_model <- lm(model_formula_wage_euros, data = df_final)
summary(ols_wage_euros_model)

library(emmeans)

gender_gap_focus_wage_euros <- emmeans(
  ols_wage_euros_model,
  specs = ~ Gender | STEM,
  data = df_final
)

print(gender_gap_focus_wage_euros)


plot(gender_gap_focus_wage_euros) +
  labs(
    title = "Adjusted Monthly Wage in Euros by Gender and STEM Status",
    x = "Estimated Monthly Wage in Euros",
    y = "Gender (0 = Female, 1 = Male)"
  )

##OLS STEM vs Non-STEM
model_formula_stem <- log_Wage ~ STEM * Gender +
  Marital_Status + Certifications + 
  ComputerForWork + InternetForWork + 
  Urban_Rural + Age + Fulltime_Work + Parttime_Work +
  job_self_employed + job_self_employed_informal + 
  job_employee + job_self_employed_paid
#Run the OLS regression
ols_stem_model <- lm(model_formula_stem, data = df_final)
#View the regression results
summary(ols_stem_model)

##calculate Estimated Marginal Means (EMMs) for each group ---
library(emmeans)
group_means <- emmeans(
  ols_stem_model,          
  specs = ~ STEM | Gender, 
  data = df_final          
)

print(group_means)
#calculate Pairwise Differences (The Comparison Table) ---
pairwise_diffs <- pairs(group_means)
print(pairwise_diffs)

library(emmeans)

#Create a NEW emmeans object with the specification flipped ---
# We specify specs = ~ Gender | STEM to tell R to structure the results
# with STEM as the outer grouping variable and Gender as the inner variable for comparison.

gender_gap_focus <- emmeans(
  ols_stem_model,
  specs = ~ Gender | STEM,
  data = df_final
)

print(gender_gap_focus)

#pairwise Differences on Gender (gender gaps)

gender_gap_table <- pairs(gender_gap_focus)
print(gender_gap_table)

# Plot gender wage gap using your emmeans result:
plot(gender_gap_focus) +
  labs(
    title = "Adjusted Wage Levels by Gender and STEM Status",
    x = "Gender (0 = Female, 1 = Male)",
    y = "Estimated Log Hourly Wage"
  )

##OLS with monthly wage (not logged)
model_formula_wage_monthly <- Wage ~ STEM * Gender +
  Marital_Status + Certifications + 
  ComputerForWork + InternetForWork + 
  Urban_Rural + Age + Hours_Worked +
  job_self_employed + job_self_employed_informal + 
  job_employee + job_self_employed_paid +
  Fulltime_Work + Parttime_Work

ols_wage_model <- lm(model_formula_wage_monthly, data = df_final)
summary(ols_wage_model)

library(emmeans)

gender_gap_focus_wage <- emmeans(
  ols_wage_model,
  specs = ~ Gender | STEM,
  data = df_final
)

print(gender_gap_focus_wage)


plot(gender_gap_focus_wage) +
  labs(
    title = "Adjusted Monthly Wage by Gender and STEM Status",
    x = "Gender (0 = Female, 1 = Male)",
    y = "Estimated Monthly Wage"
  )

################################
##OLS with hourly wage
model_formula_final <- log_Hourly_Wage ~ STEM * Gender +
  Marital_Status + Certifications + 
  ComputerForWork + InternetForWork + 
  Urban_Rural + Age +
  job_self_employed + job_self_employed_informal + 
  job_employee + job_self_employed_paid +
  Fulltime_Work + Parttime_Work
#OLS Regression
ols_final_model <- lm(model_formula_final, data = df_final)
summary(ols_final_model)

##pairwise gender gaps with hourly wage
gender_gap_hourly_final_focus <- emmeans(
  ols_final_model,
  specs = ~ Gender | STEM,  
  data = df_final,
  rg.limit = 20000
)
gender_gap_final_table <- pairs(gender_gap_hourly_final_focus)
print(gender_gap_final_table)

############################################################
## BLINDER-OAXACA DECOMPOSITION
############################################################

# Install package if necessary
# install.packages("oaxaca")

library(oaxaca)
library(dplyr)

############################################################
## 1. STEM SAMPLE
############################################################

df_stem <- df_final %>%
  filter(STEM == TRUE)

oaxaca_stem <- oaxaca(

  log_Hourly_Wage ~
    Marital_Status +
    Certifications +
    ComputerForWork +
    InternetForWork +
    Urban_Rural +
    Age +
    job_self_employed +
    job_self_employed_informal +
    job_employee +
    job_self_employed_paid +
    Fulltime_Work +
    Parttime_Work
  | Gender,

  data = df_stem,

  R = 1000

)

############################################################
## STEM RESULTS
############################################################

summary(oaxaca_stem)

print(oaxaca_stem)

plot(oaxaca_stem)



############################################################
## 2. NON-STEM SAMPLE
############################################################

df_nonstem <- df_final %>%
  filter(STEM == FALSE)

oaxaca_nonstem <- oaxaca(

  log_Hourly_Wage ~
    Marital_Status +
    Certifications +
    ComputerForWork +
    InternetForWork +
    Urban_Rural +
    Age +
    job_self_employed +
    job_self_employed_informal +
    job_employee +
    job_self_employed_paid +
    Fulltime_Work +
    Parttime_Work
  | Gender,

  data = df_nonstem,

  R = 1000

)

############################################################
## NON-STEM RESULTS
############################################################

summary(oaxaca_nonstem)

print(oaxaca_nonstem)

plot(oaxaca_nonstem)



############################################################
## 3. ENTIRE SAMPLE
############################################################

oaxaca_all <- oaxaca(

  log_Hourly_Wage ~
    STEM +
    Marital_Status +
    Certifications +
    ComputerForWork +
    InternetForWork +
    Urban_Rural +
    Age +
    job_self_employed +
    job_self_employed_informal +
    job_employee +
    job_self_employed_paid +
    Fulltime_Work +
    Parttime_Work
  | Gender,

  data = df_final,

  R = 1000

)

############################################################
## ENTIRE SAMPLE RESULTS
############################################################

summary(oaxaca_all)

print(oaxaca_all)

plot(oaxaca_all)



############################################################
## OPTIONAL: VARIABLE-BY-VARIABLE CONTRIBUTIONS
############################################################

summary(oaxaca_stem, decomposition = "detailed")

summary(oaxaca_nonstem, decomposition = "detailed")

summary(oaxaca_all, decomposition = "detailed")
