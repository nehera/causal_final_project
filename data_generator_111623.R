# 2018-2020 NHANES proportion (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0255583)
# 2011-2018 NHANES proportion (https://jamanetwork.com/journals/jama/fullarticle/2784659)

library(tidyverse)

set.seed(343)

# Parameters for sample sizes
N = 200
N_Trial = 100
N_Target = N - N_Trial

# Parameters for differential specification
k <- 0.05
l <- 0.5
p <- 0.5

##### Gender ##### 
p_male1 = 0.396 # p_male = 0.396 from NHANES
Male_trial <- rbinom(N_Trial, 1, p_male1)
p_male2 = 0.492
Male_target <- rbinom(N_Target, 1, p_male2)

Male = c(Male_trial, Male_target)

##### Age ##### 
# Trial
alpha <- 7.5
beta <- 5.4
Sim_age_Trial <- rbeta(N_Trial, alpha, beta)

# Adjust the simulated age data to your desired age range
min_age <- 15 
max_age <- 80   
Sim_age_Trial <- min_age + Sim_age_Trial * (max_age - min_age)

# Target
alpha <- 6
beta <- 5
Sim_age_Target <- rbeta(N_Target, alpha, beta)

# Adjust the simulated age data to your desired age range
min_age <- 18   
max_age <- 75   
Sim_age_Target <- min_age + Sim_age_Target * (max_age - min_age)

Age = c(Sim_age_Trial, Sim_age_Target)

#Race
Race_Trial <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian", "Other"), 
                    N_Trial, replace=TRUE, prob=c(0.508, 0.234, 0.205, 0.032, 0.021)) # From Obesity study
Race_Target <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian", "Other"), 
                     N_Target, replace=TRUE, prob=c(0.381, 0.235, 0.245, 0.131, 0.008)) # From CVD study 
Race <- as.factor(c(Race_Trial, Race_Target))

# Use model.matrix to create one-hot encoded vectors
# The "-1" removes the intercept term
Race_one_hot_encoded <- model.matrix(~ Race - 1, data.frame(Race)) %>% as.data.frame()

# Rename the columns for clarity (optional)
colnames(Race_one_hot_encoded) <- levels(Race)

#BMI
slope_age <- 0.6 # From NHANES
slope_sex <- -0.425 # From NHANES
BMI <- slope_age * Age + slope_sex * Male  + rnorm(N, mean = 0, sd = 6.5) # Males on average have lower BMI

# Generate Latent Variable
UTrial <- rnorm(N_Trial)
UTarget <- rnorm(N_Target) 

# Cut Latent Variable
Trial_cut_less <- rbinom(1, 1, prob = 0.5) # Binary indicator for Trial cut point less than Target cut point
a <- l - k/2
b <- l + k/2
a_thresh <- qnorm(a)
b_thresh <- qnorm(b)
if (Trial_cut_less==1) {
  VTrial <- ifelse(UTrial < a_thresh, "Low", "High")
  VTarget <- ifelse(UTarget < b_thresh, "Low", "High")
} else {
  VTrial <- ifelse(UTrial < b_thresh, "Low", "High")
  VTarget <- ifelse(UTarget < a_thresh, "Low", "High")
}

V = factor(c(VTrial, VTarget), levels = c("Low", "High"))

# Create treatment and study indicator variables
ATrial <- rbinom(N_Trial, 1, .5)
A <- c(ATrial, rep(NA, N_Target)) # 1 if treat, 0 if control (only for those in trial!)
S <- c(rep(1, N_Trial), rep(0, N_Target)) # indicator variable for trial

# Define potential outcomes
ATE <- 30
beta_0 <- 100
epsilon <- 50
beta_race <- matrix(5:1, ncol = 1)
beta_male <- 3
beta_age <- -.2
beta_bmi <- 1
beta_V <- 3

effect_race <- as.matrix(Race_one_hot_encoded) %*% beta_race %>% 
  as.vector()

Y0 <- beta_male * Male + beta_age * Age + beta_bmi * BMI + beta_V * as.numeric(V=="High") + 
  effect_race + rnorm(N, beta_0, sd = epsilon) 
Y1 <- beta_male * Male + beta_age * Age + beta_bmi * BMI + beta_V * as.numeric(V=="High") + 
  effect_race + rnorm(N, beta_0, sd = epsilon) + ATE
Y <- ifelse(A==1, Y1, Y0) # Y is NA for subjects in the target population

mean(Y0)
mean(Y1)
mean(Y, na.rm = TRUE)

data <- data.frame(Male, Age, Race, BMI, V, A, S, Y0, Y1, Y)