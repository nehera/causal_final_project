# 2018-2020 NHANES proportion (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0255583)
# 2011-2018 NHANES proportion (https://jamanetwork.com/journals/jama/fullarticle/2784659)

library(tidyverse)
library(dplyr)

#### ---- Define Custom Functions
simulate_data <- function(k) {
  
  # Parameters for sample sizes
  N = 5000
  N_Trial = floor(0.5*N)
  N_Target = N - N_Trial
  
  # Parameters for differential specification
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
  
  #Race # fix Asian+other
  Race_Trial <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"), 
                      N_Trial, replace=TRUE, prob=c(0.518, 0.239, 0.209, 0.034)) # From Obesity NHANES
  Race_Target <-sample(c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"), 
                       N_Target, replace=TRUE, prob=c(0.382, 0.237, 0.247, 0.134)) # From CVD NHANES 
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
  U = c(UTrial, UTarget)
  
  # Cut Latent Variable
  Trial_cut_less <- rbinom(1, 1, prob = 0.5) # Binary indicator for Trial cut point less than Target cut point
  a <- l - k/2 # l=0.5
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
  ATE_param = 5
  ATE <- 30 + U*ATE_param
  beta_0 <- 100
  epsilon <- 50
  beta_race <- matrix(4:1, ncol = 1)
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
  data <- data.frame(Male, Age, Race, BMI, V, A, S, Y0, Y1, Y)
  
  ## -- Quality Assure Data
  # Check to ensure that after grouping by S and A, each Race level includes at least one observation
  N_Race <- levels(Race) %>% length()
  N_Race_by_group <- data %>% group_by(S, A) %>% count(Race) %>% group_split() %>% sapply(nrow)
  if (any(N_Race_by_group != N_Race)) { stop("After grouping by S and A, at least one level of Race has no observations.") }
  
  return(data)
}

get_tate_true <- function(data) {
  targetdata <- subset(data, S==0)
  tateTrue <- mean(targetdata$Y1-targetdata$Y0)
  return(tateTrue)
}

estimate_tates <- function(data) {
  S<-data$S
  A<-data$A
  #Analyze data
  #outcome regression version
  
  #Step one: create necessary datasets
  studydata<- subset(data, S==1)
  studydataT<- subset(studydata, A==1)
  studydataC<- subset(studydata, A==0)
  targetdata <- subset(data, S==0)
  
  #Step two: make models for E[Y|X, S=1, A=a]
  treatlm <- lm(Y ~ Male+Age+Race+BMI+V, data=studydataT)
  contlm <- lm(Y ~ Male+Age+Race+BMI+V, data=studydataC)
  
  #Step 3: get predicted values for set st S=0 for both assignments
  #set A=1
  targetdata$A <- 1
  gt<-data$gt <- predict(treatlm,newdata =data)
  #set A=0
  targetdata$A <- 0
  gc<-data$gc <- predict(contlm, newdata = data)
  
  #Step 4: get E[Y^a|S=0]'s
  mut <- (1/sum((1-S)))*sum((1-S)*gt)
  muc <- (1/sum((1-S)))*sum((1-S)*gc)
  #Step 5: subtract them to get the estimated TATE
  tateOR <- mut - muc
  
  
  #IOW1
  data$p <- 1/(1+exp(-1* predict(glm(S ~ Male+Age+Race+BMI+V, data=data, family="binomial"),newdata = data)))#P(S=1|X)
  data$e1 <- 1/(1+exp(-1* predict(glm(A ~ Male+Age+Race+BMI+V, data=studydata, family="binomial"),newdata = data)))# P(A=1|X, S=1)
  data$e0 <-  1/(1+exp(-1*predict(glm((1-A) ~ Male+Age+Race+BMI+V, data=studydata, family="binomial"),newdata = data)))# P(A=0|X, S=1)
  
  data$w1<-ifelse(S==1 & A==1, (1-data$p)/(data$p*data$e1), 0)
  data$w0<-ifelse(S==1 & A==0, (1-data$p)/(data$p*data$e0), 0)
  muIOW1t <- (1/sum((1-S)))*sum(data$w1*data$Y,na.rm =T)
  muIOW1c <- (1/sum((1-S)))*sum(data$w0*data$Y,na.rm =T)
  tateIOW1 <- muIOW1t-muIOW1c
  
  #IOW2
  muIOW2t <- (1/sum(data$w1,na.rm =T))*sum(data$w1*data$Y,na.rm =T)
  muIOW2c <- (1/sum(data$w0,na.rm =T))*sum(data$w0*data$Y,na.rm =T)
  tateIOW2 <- muIOW2t-muIOW2c
  
  #DR1
  muDR1t <-  (1/sum((1-S)))*(sum(data$w1*(data$Y-gt),na.rm =T)+sum((1-S)*gt,na.rm =T))
  muDR1c <- (1/sum((1-S)))*(sum(data$w0*(data$Y-gc),na.rm =T) +sum((1-S)*gc,na.rm =T))
  tateDR1 <- muDR1t - muDR1c
  
  #DR2
  muDR2t <-  (1/sum(data$w1,na.rm =T))*sum(data$w1*(data$Y-gt),na.rm =T) +(1/sum((1-S)))*sum((1-S)*gt,na.rm =T)
  muDR2c <-  (1/sum(data$w0,na.rm =T))*sum(data$w0*(data$Y-gc),na.rm =T) +(1/sum((1-S)))*sum((1-S)*gc,na.rm =T)
  tateDR2 <- muDR2t - muDR2c
  
  tates <- c(tateOR, tateIOW1, tateIOW2, tateDR1, tateDR2)
  return(tates)
}

# Function to perform bootstrap resampling and estimate the five quantities
bootstrap_tates <- function(data, B=100) {
  
  # Number of observations
  n_obs <- nrow(data)
  
  # Number of estimates
  num_estimates <- 5
  
  # Matrix to store bootstrap estimates
  bootstrap_matrix <- matrix(NA, nrow = B, ncol = num_estimates)
  
  # Perform bootstrap resampling
  for (b in 1:B) {
    
    # Sample with replacement
    bootstrap_ids <- sample(1:n_obs, n_obs, replace = TRUE)
    bootstrap_sample <- data[bootstrap_ids, ]
    
    # Calculate the five estimates
    bootstrap_estimates <- estimate_tates(bootstrap_sample)
    
    # Store the estimates in the matrix
    bootstrap_matrix[b, ] <- bootstrap_estimates
  }
  
  return(bootstrap_matrix)
}

# Determine for checking if the truth is in the confidence intervals
check_inclusion <- function(ci, truth) {
  truth_included <- as.numeric(ci[1] < truth & truth < ci[2])
  return (truth_included)
}

#### ---- Analysis Start here

## Base case: k=0
k <- 0

# Matrix for coverage indicators
M <- 50

seed = floor(20*k*M+343)

set.seed(seed)

# Number of estimates
num_estimates <- 5

true_tates <- rep(NA, M)
tate_matrix <- coverage_matrix <- matrix(NA, nrow = M, ncol = num_estimates) 

for (m in 1:M) {
  
  # Simulate 
  data_m <- simulate_data(k)
  # Get the truth
  tate_true_m <- get_tate_true(data_m)
  # Point estimate tates
  tates_m <- estimate_tates(data_m)
  # Bootstrap variability
  bootstrap_matrix <- bootstrap_tates(data_m)
  # Get Confidence Intervals
  confidence_intervals <- apply(bootstrap_matrix, MARGIN = 2, 
                                FUN = quantile, probs = c(0.025, 0.975)) 
  # Determine if the truth is in the confidence intervals
  coverage_indicators <- apply(confidence_intervals, MARGIN = 2, 
                               check_inclusion, tate_true_m)
  
  # Store outputs
  true_tates[m] <- tate_true_m
  tate_matrix[m, ] <- tates_m
  coverage_matrix[m, ] <- coverage_indicators
}

result_k0 <- list(true_tates, tate_matrix, coverage_matrix)
names(result_k0) <- c("true_tates", "tate_matrix", "coverage_matrix")

## Case 2: k=0.05
k <- 0.05

# Matrix for coverage indicators
M <- 50

seed = floor(20*k*M+343)

set.seed(seed)

# Number of estimates
num_estimates <- 5

true_tates <- rep(NA, M)
tate_matrix <- coverage_matrix <- matrix(NA, nrow = M, ncol = num_estimates) 

for (m in 1:M) {
  
  # Simulate 
  data_m <- simulate_data(k)
  # Get the truth
  tate_true_m <- get_tate_true(data_m)
  # Point estimate tates
  tates_m <- estimate_tates(data_m)
  # Bootstrap variability
  bootstrap_matrix <- bootstrap_tates(data_m)
  # Get Confidence Intervals
  confidence_intervals <- apply(bootstrap_matrix, MARGIN = 2, 
                                FUN = quantile, probs = c(0.025, 0.975)) 
  # Determine if the truth is in the confidence intervals
  coverage_indicators <- apply(confidence_intervals, MARGIN = 2, 
                               check_inclusion, tate_true_m)
  
  # Store outputs
  true_tates[m] <- tate_true_m
  tate_matrix[m, ] <- tates_m
  coverage_matrix[m, ] <- coverage_indicators
}

result_k0.05 <- list(true_tates, tate_matrix, coverage_matrix)
names(result_k0.05) <- c("true_tates", "tate_matrix", "coverage_matrix")

## Case 3: k=0.2
k <- 0.2

# Matrix for coverage indicators
M <- 50

seed = floor(20*k*M+343)

set.seed(seed)

true_tates <- rep(NA, M)
tate_matrix <- coverage_matrix <- matrix(NA, nrow = M, ncol = num_estimates) 

for (m in 1:M) {
  
  # Simulate 
  data_m <- simulate_data(k)
  # Get the truth
  tate_true_m <- get_tate_true(data_m)
  # Point estimate tates
  tates_m <- estimate_tates(data_m)
  # Bootstrap variability
  bootstrap_matrix <- bootstrap_tates(data_m)
  # Get Confidence Intervals
  confidence_intervals <- apply(bootstrap_matrix, MARGIN = 2, 
                                FUN = quantile, probs = c(0.025, 0.975)) 
  # Determine if the truth is in the confidence intervals
  coverage_indicators <- apply(confidence_intervals, MARGIN = 2, 
                               check_inclusion, tate_true_m)
  
  # Store outputs
  true_tates[m] <- tate_true_m
  tate_matrix[m, ] <- tates_m
  coverage_matrix[m, ] <- coverage_indicators
}

result_k0.2 <- list(true_tates, tate_matrix, coverage_matrix)
names(result_k0.2) <- c("true_tates", "tate_matrix", "coverage_matrix")

## Case 4: k=0.5
k <- 0.5

# Matrix for coverage indicators
M <- 50

seed = floor(20*k*M+343)

set.seed(seed)

true_tates <- rep(NA, M)
tate_matrix <- coverage_matrix <- matrix(NA, nrow = M, ncol = num_estimates) 

for (m in 1:M) {
  
  # Simulate 
  data_m <- simulate_data(k)
  # Get the truth
  tate_true_m <- get_tate_true(data_m)
  # Point estimate tates
  tates_m <- estimate_tates(data_m)
  # Bootstrap variability
  bootstrap_matrix <- bootstrap_tates(data_m)
  # Get Confidence Intervals
  confidence_intervals <- apply(bootstrap_matrix, MARGIN = 2, 
                                FUN = quantile, probs = c(0.025, 0.975)) 
  # Determine if the truth is in the confidence intervals
  coverage_indicators <- apply(confidence_intervals, MARGIN = 2, 
                               check_inclusion, tate_true_m)
  
  # Store outputs
  true_tates[m] <- tate_true_m
  tate_matrix[m, ] <- tates_m
  coverage_matrix[m, ] <- coverage_indicators
}

result_k0.5 <- list(true_tates, tate_matrix, coverage_matrix)
names(result_k0.5) <- c("true_tates", "tate_matrix", "coverage_matrix")

## Cass 5: k=0.8
k <- 0.8

# Matrix for coverage indicators
M <- 50

seed = floor(20*k*M+343)

set.seed(seed)

# Number of estimates
num_estimates <- 5

true_tates <- rep(NA, M)
tate_matrix <- coverage_matrix <- matrix(NA, nrow = M, ncol = num_estimates)

for (m in 1:M) {
  
  # Simulate 
  data_m <- simulate_data(k)
  # Get the truth
  tate_true_m <- get_tate_true(data_m)
  # Point estimate tates
  tates_m <- estimate_tates(data_m)
  # Bootstrap variability
  bootstrap_matrix <- bootstrap_tates(data_m)
  # Get Confidence Intervals
  confidence_intervals <- apply(bootstrap_matrix, MARGIN = 2, 
                                FUN = quantile, probs = c(0.025, 0.975)) 
  # Determine if the truth is in the confidence intervals
  coverage_indicators <- apply(confidence_intervals, MARGIN = 2, 
                               check_inclusion, tate_true_m)
  
  # Store outputs
  true_tates[m] <- tate_true_m
  tate_matrix[m, ] <- tates_m
  coverage_matrix[m, ] <- coverage_indicators
}

result_k0.8 <- list(true_tates, tate_matrix, coverage_matrix)
names(result_k0.8) <- c("true_tates", "tate_matrix", "coverage_matrix")