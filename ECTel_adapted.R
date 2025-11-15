
library(stats)
library(dplyr)
library(tidyr)
library(tidyverse)
library(purrr)
library(lme4)
library(lmerTest)
library(report)
library(emmeans)
library(MASS)
library(ggplot2)
library(psych)
library(gridExtra)
library(ggsignif)
library(ggpubr)
library(RColorBrewer)
library(lmtest)
library(stringr)
library(car)
library(forcats)

#set the relevant working directory
setwd('/Users/zzy/PhD/24:25/AI Trust/Experiment/Response_Yr 1 and 2 stats')

data <- read.csv('results_processed_11Mar.csv')

# First filter the data for AI_years <= 4 and remove NA/blank values in P2_selected_feedback_co
filtered_data <- data %>%
  mutate(AI_ed_years = P4_student_AI_ed_years) %>%
  mutate(AI_years = P4_student_AI_years) %>%  
  filter(AI_ed_years <= 4) %>%
  filter(AI_years <= 4) %>%
  filter(!is.na(P2_selected_feedback_co) & P2_selected_feedback_co != "")

write.csv(filtered_data, "filtered_data_23Mar.csv", row.names = FALSE)

############################################################################
# Data Preparation & Descriptives

# Check for duplicates
data %>%
  dplyr::group_by(course_id, student_id) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)

# Get counts by course
data %>%
  dplyr::group_by(course_id) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# Get demographic information
data %>%
  dplyr::group_by(course_id, P4_student_gender) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# Age distribution
age_distribution <- data %>%
  dplyr::group_by(course_id, P4_student_age) %>%
  dplyr::summarise(n = n(), .groups = "drop")
print(age_distribution)

# Selected AI feedback distribution
data %>%
  dplyr::group_by(P2_selected_feedback) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# Co-produced feedback distribution
data %>%
  dplyr::group_by(P2_selected_feedback_co) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# Define courses
courses <- c('PALS0045', 'PALS0046')

############################################################################
# Process data function remains the same
process_likert_columns <- function(filtered_data, prefix) {
  # First check if the prefix columns exist
  prefix_cols <- names(filtered_data)[grep(prefix, names(filtered_data))]
  
  if(length(prefix_cols) == 0) {
    warning(paste("No columns found with prefix", prefix))
    return(NULL)
  }
  
  # Process the data
  result <- filtered_data %>%
    # Select only the necessary columns
    dplyr::select(student_id, course_id, all_of(prefix_cols)) %>%
    # Pivot to long format
    pivot_longer(
      cols = all_of(prefix_cols),
      names_to = "column_name",
      values_to = "scores"
    ) %>%
    # Extract feedback type from column name
    mutate(feedback_type = str_extract(column_name, "[^_]+$")) %>%
    # Separate scores into individual columns
    separate_wider_delim(
      scores,
      delim = ",",
      names_sep = "",
      too_few = "align_start"
    ) %>%
    # Pivot the scores columns to long format
    pivot_longer(
      cols = starts_with("scores"),
      names_to = "item",
      values_to = "score",
      values_transform = list(score = as.numeric)
    ) %>%
    # Clean up item names
    mutate(item = paste0("ans", as.numeric(str_extract(item, "\\d+")))) %>%
    # Select final columns
    dplyr::select(student_id, course_id, feedback_type, item, score)
  
  return(result)
}

# Processing the data
try({
  # Process P1 data
  P1_data <- process_likert_columns(filtered_data, "P1_likert")
  
  # Process P3 data  
  P3_data <- process_likert_columns(filtered_data, "P3_likert")
  
  # Combine the data
  if(!is.null(P1_data) && !is.null(P3_data)) {
    data_likert <- bind_rows(
      mutate(P1_data, timing = "P1"),
      mutate(P3_data, timing = "P3")
    ) %>% distinct()
    
    print(paste("Successfully processed", nrow(data_likert), "rows of Likert data"))
  } else {
    stop("Failed to process one or both Likert data sources")
  }
})

# Create a wide format dataframe for reliability analysis
data_wide <- data_likert %>%
  pivot_wider(
    id_cols = c(student_id, course_id, feedback_type, timing),
    names_from = item,
    values_from = score
  )

# Create an empty dataframe to store results
reliability_results <- data.frame(
  scale = character(),
  alpha = numeric(),
  stringsAsFactors = FALSE
)

# ---------- CALCULATE BY SCALE (ACROSS ALL FEEDBACK TYPES) ----------

# 1. Objectivity (items ans1-ans3) - Overall
if(all(c("ans1", "ans2", "ans3") %in% colnames(data_wide))) {
  alpha_obj <- psych::alpha(data_wide[c("ans1", "ans2", "ans3")], check.keys = TRUE)
  reliability_results <- rbind(
    reliability_results,
    data.frame(
      scale = "Objectivity (Overall)",
      alpha = round(alpha_obj$total$raw_alpha, 3)
    )
  )
}

# 2. Usefulness (items ans4-ans6) - Overall
if(all(c("ans4", "ans5", "ans6") %in% colnames(data_wide))) {
  alpha_use <- psych::alpha(data_wide[c("ans4", "ans5", "ans6")], check.keys = TRUE)
  reliability_results <- rbind(
    reliability_results,
    data.frame(
      scale = "Usefulness (Overall)",
      alpha = round(alpha_use$total$raw_alpha, 3)
    )
  )
}

# 3. Genuineness (items ans7-ans8) - Overall
if(all(c("ans7", "ans8") %in% colnames(data_wide))) {
  alpha_gen <- psych::alpha(data_wide[c("ans7", "ans8")], check.keys = TRUE)
  reliability_results <- rbind(
    reliability_results,
    data.frame(
      scale = "Genuineness (Overall)",
      alpha = round(alpha_gen$total$raw_alpha, 3)
    )
  )
}

# ---------- CALCULATE BY FEEDBACK TYPE (ACROSS ALL SCALES) ----------
feedback_types <- c("AI", "human", "humanplusai")

for (ft in feedback_types) {
  subset_data <- data_wide %>% filter(feedback_type == ft)
  
  # Skip if no data for this feedback type
  if(nrow(subset_data) == 0) {
    warning(paste("No data found for feedback type:", ft))
    reliability_results <- rbind(
      reliability_results,
      data.frame(
        scale = paste0(ft, " (Overall)"),
        alpha = NA
      )
    )
    next
  }
  
  # Calculate reliability for all items in this feedback type
  all_items <- paste0("ans", 1:8)
  available_items <- all_items[all_items %in% colnames(subset_data)]
  
  if(length(available_items) > 1) {  # Need at least 2 items for reliability
    alpha_all <- psych::alpha(subset_data[available_items], check.keys = TRUE)
    reliability_results <- rbind(
      reliability_results,
      data.frame(
        scale = paste0(ft, " (Overall)"),
        alpha = round(alpha_all$total$raw_alpha, 3)
      )
    )
  }
}

# Print the final results table
cat("\n----- RELIABILITY RESULTS -----\n\n")
print(reliability_results)

############################################################################
# RQ1    Name    Values    Type
#Dependent variable 1    TT_AI_passed    0 (incorrect), 1 (correct)    Binary
#Dependent variable 2    TT_co_passed    0 (incorrect), 1 (correct)    Binary
#Dependent variable 3    TT_combination    Both Passed, AI Only Passed, Co-Prod Only Passed, Both Failed    Factor

#Covariate 1    course_id    PALS0045, PALS0046    Factor
#Covariate 2    age    Numeric
#Covariate 3    gender    female (includes nonbinary), male    Factor
#Covariate 4    AI_years    Years of AI education experience (≤ 4)    Numeric
#Covariate 5    Perceived_Usefulness    Trust dimension score (items 1-6)    Numeric
#Covariate 6    Perceived_Obstacles    Trust dimension score (items 7-13, reverse-coded)    Numeric
#Covariate 7    Perceived_Readiness    Trust dimension score (items 14-15)    Numeric
#Covariate 8    Perceived_Trust    Trust dimension score (items 16-21)    Numeric

# Main formulas:
# TT_AI_passed ~ age + gender + course_id + AI_years + Perceived_Usefulness + 
#                Perceived_Obstacles + Perceived_Readiness + Perceived_Trust
# TT_co_passed ~ age + gender + course_id + AI_years + Perceived_Usefulness + 
#                Perceived_Obstacles + Perceived_Readiness + Perceived_Trust

# Then create data_rq1 with the rest of the transformations
data_rq1 <- filtered_data %>%
  mutate(
    # Existing variables
    TT_AI_passed = if_else(P2_selected_feedback == "feedback_AI", 1, 0),
    TT_co_passed = if_else(P2_selected_feedback_co == "feedback_humanplusai", 1, 0),
    TT_combination = factor(
      case_when(
        TT_AI_passed == 1 & TT_co_passed == 1 ~ "Both Passed",
        TT_AI_passed == 1 & TT_co_passed == 0 ~ "AI Only Passed",
        TT_AI_passed == 0 & TT_co_passed == 1 ~ "Co-Prod Only Passed",
        TRUE ~ "Both Failed"
      ),
      levels = c("Both Passed", "AI Only Passed", "Co-Prod Only Passed", "Both Failed")
    ),
    age = P4_student_age,
    gender_modified = replace(P4_student_gender, P4_student_gender == 'nonbinary', 'female'),
    gender = factor(gender_modified),
    course_id = factor(course_id)
  )

# Parse the comma-separated trust survey responses
data_rq1 <- data_rq1 %>%
  mutate(
    # Clean and split the trust survey responses
    trust_scores = str_trim(P1.5_likertTrustSurvey),
    trust_scores = str_replace_all(trust_scores, "\\s+", "")  # Remove any whitespace
  ) %>%
  separate(
    trust_scores,
    into = paste0("trust_item_", 1:21),  # Create 21 separate columns
    sep = ",",
    convert = TRUE,  # Convert to numeric
    fill = "right"   # Handle missing values
  )

usefulness_items <- paste0("trust_item_", 1:6)  # Items 1-6 measure usefulness
obstacles_items <- paste0("trust_item_", 7:13)  # Items 7-13 measure obstacles
readiness_items <- paste0("trust_item_", 14:15) # Items 14-15 measure readiness
trust_items <- paste0("trust_item_", 16:21)     # Items 16-21 measure trust

max_likert_value <- 5

data_rq1 <- data_rq1 %>%
  mutate(
    # Reverse code Perceived Obstacles items (items 7-13)
    across(all_of(obstacles_items), ~(max_likert_value + 1 - .), .names = "rev_{.col}")
  )

# Now collect columns for each dimension
usefulness_cols <- data_rq1[, usefulness_items]
obstacles_rev_cols <- data_rq1[, paste0("rev_", obstacles_items)]
readiness_cols <- data_rq1[, readiness_items]
trust_cols <- data_rq1[, trust_items]

# Calculate means for each dimension
data_rq1$Perceived_Usefulness <- rowMeans(usefulness_cols, na.rm = TRUE)
data_rq1$Perceived_Obstacles <- rowMeans(obstacles_rev_cols, na.rm = TRUE)
data_rq1$Perceived_Readiness <- rowMeans(readiness_cols, na.rm = TRUE)
data_rq1$Perceived_Trust <- rowMeans(trust_cols, na.rm = TRUE)

write.csv(data_rq1, "data_rq1.csv", row.names = FALSE)

# Check reliability (Cronbach's alpha) for each dimension
cat("\n--- Trust Survey Reliability Analysis ---\n")

# For Perceived Usefulness (items 1-6)
alpha_usefulness <- psych::alpha(data_rq1[, usefulness_items], check.keys = TRUE)
cat("\nPerceived Usefulness (alpha =", round(alpha_usefulness$total$raw_alpha, 3), ")\n")

# For Perceived Obstacles (original items, not reverse-coded)
alpha_obstacles <- psych::alpha(data_rq1[, obstacles_items], check.keys = TRUE)
cat("Perceived Obstacles (alpha =", round(alpha_obstacles$total$raw_alpha, 3), ")\n")

# For Perceived Readiness (items 14-15)
alpha_readiness <- psych::alpha(data_rq1[, readiness_items], check.keys = TRUE)
cat("Perceived Readiness (alpha =", round(alpha_readiness$total$raw_alpha, 3), ")\n")

# For Perceived Trust (items 16-21)
alpha_trust <- psych::alpha(data_rq1[, trust_items], check.keys = TRUE)
cat("Perceived Trust (alpha =", round(alpha_trust$total$raw_alpha, 3), ")\n")

# Updated logistic regression models with trust dimensions and AI years
cat("\n--- AI Feedback Recognition Predictors with Trust Dimensions and AI Experience ---\n")
model_ai_updated <- glm(TT_AI_passed ~ age + gender + course_id + 
                          AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                          Perceived_Readiness + Perceived_Trust, 
                        data = data_rq1, family = "binomial")
vif(model_ai_updated)
print(summary(model_ai_updated))

z_value <- 0.80
chi_square_ai_ed_years <- z_value^2
chi_square_ai_ed_years

cat("\n--- Co-Produced Feedback Recognition Predictors with Trust Dimensions and AI Experience ---\n")
model_co_updated <- glm(TT_co_passed ~ age + gender + course_id + 
                          AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                          Perceived_Readiness + Perceived_Trust, 
                        data = data_rq1, family = "binomial")
vif(model_co_updated)
print(summary(model_co_updated))

z_value <- 0.80
chi_square_ai_ed_years <- z_value^2
chi_square_ai_ed_years


# None of the trust survey dimensions (Perceived Usefulness, Perceived Obstacles, Perceived Readiness, or Perceived Trust) 
# significantly predict whether participants could identify AI-generated feedback (p > .17).
# Years of AI usage in education  (AI_ed_years) significantly predicted participants' ability to 
# correctly identify AI-generated feedback (chi-sqaure = 4.08, p = .028). 
# This experience factor did not significantly influence participants' accuracy in identifying co-produced feedback. 
# Years of AI usage in general does not significantly predict the ability to identify feedback source. 
# Demographic variables (age, gender) and course ID remain non-significant predictors, consistent with earlier findings.

############################################################################
#RQ2    Name    Values    Type

#Condition A = independent variable 1    feedback_provider    AI, human, humanplusai    Factor
#Condition B = independent variable 2    timing    P1 (Blind), P3 (Informed)    Factor

#Dimension 1 = dependent variable    Objectivity    mean(ans0, ans1, ans2)    Number
#Dimension 2 = dependent variable    Usefulness    mean(ans3, ans4, ans5)    Number
#Dimension 3 = dependent variable    Genuineness    mean(ans6, ans7)    Number

#Covariate 1    course_id    PALS0045, PALS0046    Factor
#Covariate 2    age    Numeric
#Covariate 3    TT_combination    Both Passed, AI Only Passed, Co-Prod Only Passed, Both Failed    Factor
#Covariate 4    gender    female (includes nonbinary), male    Factor
#Covariate 5    AI_years    Years of AI experience    Numeric
#Covariate 6    AI_ed_years    Years of AI experience in education    Numeric
#Covariate 7    Perceived_Usefulness    Trust dimension score    Numeric
#Covariate 8    Perceived_Obstacles    Trust dimension score    Numeric
#Covariate 9    Perceived_Readiness    Trust dimension score    Numeric
#Covariate 10    Perceived_Trust    Trust dimension score    Numeric
#Participant id    student_id    Numeric

# Main formula:
# Score ~ feedback_provider * timing + TT_combination + age + gender + course_id + 
#         AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + Perceived_Readiness + 
#         Perceived_Trust + (1|student_id)

data_rq2 <- data_rq1 %>%
  # Process Likert responses
  pivot_longer(
    cols = matches("P[13]_likert_(AI|human|humanplusai)"),
    names_to = c("timing", "scale", "feedback_provider"),
    names_sep = "_",
    values_to = "raw_scores",
    values_drop_na = TRUE
  ) %>%
  # Process scores
  mutate(raw_scores = str_trim(raw_scores)) %>%
  filter(str_detect(raw_scores, "^\\d+(,\\d+)*$")) %>%
  separate_wider_delim(
    raw_scores,
    delim = ",", 
    names = paste0("ans", 0:7),
    too_many = "debug",
    too_few = "align_start"
  ) %>%
  dplyr::select(-raw_scores) %>%
  pivot_longer(
    cols = starts_with("ans"),
    names_to = "item",
    values_to = "score",
    values_transform = list(score = as.numeric),
    values_drop_na = TRUE
  ) %>%
  # Map to dimensions
  mutate(
    Dimension = case_when(
      item %in% c("ans0", "ans1", "ans2") ~ "Objectivity",
      item %in% c("ans3", "ans4", "ans5") ~ "Usefulness",
      item %in% c("ans6", "ans7") ~ "Genuineness",
      TRUE ~ "UNMAPPED"
    )
  ) %>%
  filter(Dimension != "UNMAPPED") %>%
  # Calculate dimension scores
  group_by(course_id, student_id, timing, feedback_provider, Dimension, TT_combination, age, gender, 
           AI_years, AI_ed_years, Perceived_Usefulness, Perceived_Obstacles, Perceived_Readiness, Perceived_Trust) %>%  
  summarise(
    Score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Score))

# Modified run_analysis function with trust dimensions and AI years
run_analysis <- function(dimension) {
  # Define formula with new covariates
  formula <- Score ~ feedback_provider * timing + TT_combination + 
    age + gender + course_id + AI_years + AI_ed_years + 
    Perceived_Usefulness + Perceived_Obstacles + Perceived_Readiness + Perceived_Trust + 
    (1|student_id)
  
  # Run model
  model <- lmer(
    formula,
    data = data_rq2 %>% 
      filter(Dimension == dimension) %>%
      # Ensure factor levels are properly set
      mutate(
        TT_combination = factor(TT_combination),
        feedback_provider = factor(feedback_provider),
        timing = factor(timing)
      )
  )
  
  print(summary(model))
  print(emmeans(model, specs = pairwise ~ feedback_provider|timing))
  
  return(model)
}

# Run for each dimension
objectivity_model <- run_analysis("Objectivity")
usefulness_model <- run_analysis("Usefulness")
genuineness_model <- run_analysis("Genuineness")

# Define the comparisons we want to make
comparisons <- list(
  # Compare AI in P1 vs P3
  AI_P1_vs_P3 = c(1, 0, 0, -1, 0, 0),
  # Compare human in P1 vs P3
  human_P1_vs_P3 = c(0, 1, 0, 0, -1, 0),
  # Compare humanplusai in P1 vs P3
  humanplusai_P1_vs_P3 = c(0, 0, 1, 0, 0, -1)
)

# Use a different approach with emmeans
fb_timing_grid <- expand.grid(
  feedback_provider = c("AI", "human", "humanplusai"),
  timing = c("P1", "P3")
)

# Get emmeans for all combinations
fb_timing_means <- emmeans(genuineness_model, 
                           ~ feedback_provider:timing)

# Use custom contrasts
specific_contrasts <- pairs(fb_timing_means, by = "feedback_provider")
print(specific_contrasts)
# Calculate main effect of timing
timing_means <- emmeans(genuineness_model, ~ timing)
print(timing_means)
print(pairs(timing_means))

data_rq2 <- data_rq1 %>%
  # Process Likert responses
  pivot_longer(
    cols = matches("P[13]_likert_(AI|human|humanplusai)"),
    names_to = c("timing", "scale", "feedback_provider"),
    names_sep = "_",
    values_to = "raw_scores",
    values_drop_na = TRUE
  ) %>%
  # Process scores
  mutate(raw_scores = str_trim(raw_scores)) %>%
  filter(str_detect(raw_scores, "^\\d+(,\\d+)*$")) %>%
  separate_wider_delim(
    raw_scores,
    delim = ",", 
    names = paste0("ans", 0:7),
    too_many = "debug",
    too_few = "align_start"
  ) %>%
  dplyr::select(-raw_scores) %>%
  pivot_longer(
    cols = starts_with("ans"),
    names_to = "item",
    values_to = "score",
    values_transform = list(score = as.numeric),
    values_drop_na = TRUE
  ) %>%
  # Map to dimensions
  mutate(
    Dimension = case_when(
      item %in% c("ans0", "ans1", "ans2") ~ "Objectivity",
      item %in% c("ans3", "ans4", "ans5") ~ "Usefulness",
      item %in% c("ans6", "ans7") ~ "Genuineness",
      TRUE ~ "UNMAPPED"
    )
  ) %>%
  filter(Dimension != "UNMAPPED") %>%
  # Calculate dimension scores
  group_by(course_id, student_id, timing, feedback_provider, Dimension, TT_combination, age, gender, 
           # Add new covariates
           AI_years, Perceived_Usefulness, Perceived_Obstacles, Perceived_Readiness, Perceived_Trust) %>%  
  summarise(
    Score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Score))

# Modified run_analysis function with trust dimensions and AI years
run_analysis <- function(dimension) {
  # Define formula with new covariates
  formula <- Score ~ feedback_provider * timing + TT_combination + 
    age + gender + course_id + AI_years + 
    Perceived_Usefulness + Perceived_Obstacles + Perceived_Readiness + Perceived_Trust + 
    (1|student_id)
  
  # Run model
  model <- lmer(
    formula,
    data = data_rq2 %>% 
      filter(Dimension == dimension) %>%
      # Ensure factor levels are properly set
      mutate(
        TT_combination = factor(TT_combination),
        feedback_provider = factor(feedback_provider),
        timing = factor(timing)
      )
  )
  
  print(summary(model))
  print(emmeans(model, specs = pairwise ~ feedback_provider|timing))
  
  return(model)
}

# Run for each dimension
objectivity_model <- run_analysis("Objectivity")
usefulness_model <- run_analysis("Usefulness")
genuineness_model <- run_analysis("Genuineness")

# Define the comparisons we want to make
comparisons <- list(
  # Compare AI in P1 vs P3
  AI_P1_vs_P3 = c(1, 0, 0, -1, 0, 0),
  # Compare human in P1 vs P3
  human_P1_vs_P3 = c(0, 1, 0, 0, -1, 0),
  # Compare humanplusai in P1 vs P3
  humanplusai_P1_vs_P3 = c(0, 0, 1, 0, 0, -1)
)

# Use a different approach with emmeans
fb_timing_grid <- expand.grid(
  feedback_provider = c("AI", "human", "humanplusai"),
  timing = c("P1", "P3")
)

# Get emmeans for all combinations
fb_timing_means <- emmeans(genuineness_model, 
                           ~ feedback_provider:timing)

# Use custom contrasts
specific_contrasts <- pairs(fb_timing_means, by = "feedback_provider")
print(specific_contrasts)
# Calculate main effect of timing
timing_means <- emmeans(genuineness_model, ~ timing)
print(timing_means)
print(pairs(timing_means))

# Run a combined model with interaction to test if slopes differ by feedback type
combined_model <- lmer(
  Score ~ feedback_provider*AI_years + TT_combination + age + gender + course_id + 
    AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
    Perceived_Readiness + Perceived_Trust + (1|student_id),
  data = data_rq2 %>% filter(Dimension == "Usefulness")
)

print("Test of interaction between feedback provider and AI years:")
print(summary(combined_model)$coefficients[grep("feedback_provider.*AI_years", rownames(summary(combined_model)$coefficients)),])
# no interaction between AI_years and feedback provider



# Main Effects of Feedback Provider:
# AI feedback was rated significantly higher than Human feedback across all dimensions (all p < .024), 
# Co-produced feedback was rated significantly higher than Human feedback across all dimensions (all p < .001)

# Objectivity: AI (M=4.11-4.01) and Coproduced feedback (M=4.07-4.08) were rated similarly (p > .78), 
# both significantly higher than Human feedback (M=3.31-3.39, p < .001)
# Usefulness: Similar pattern with AI (M=4.12-4.03) and Coproduced feedback (M=4.04-3.99) rated similarly (p > .766), 
# both significantly higher than Human feedback (M=3.17-3.25, p < .001).
# Genuineness: AI feedback (M=4.11-3.67) was rated similarly as Co-produced (M=3.82-3.84), all p > .077
# both significantly higher than Human feedback (M=3.19-3.32, p < .024).

# Main Effect of Timing:
# Genuineness: Significant main effect of timing (p < .001). 
# Objectivity and Usefulness: No significant main effects of timing (p > .39)

# Interaction Effects:
# Genuineness: Significant feedback provider × timing interaction -
# AI feedback was rated more genuine in the Blind condition (M=4.11) than the Informed condition (M=3.67, p < .001)
# Co-produced feedback and human feedback remained stable across timing conditions. 

# Demographic Effects
# Gender: Male participants rated feedback significantly lower across Usefulness: p =.051, 
# but not for Genuineness and Objectivity
# Age and Course: No significant effects on any perception dimension (all p > .23)

# AI Experience Effects
# AI years in general: more years of AI usage in General, lower Usefulness rating: p = .024
# but not for Genuineness and Objectivity
# AI years in education does not significantly predict ratings

# AI Trust Effects
# AI Trust did not significantly predict ratings (all p > .07)

# Turing Test Effects
# TT combination did not significantly predict ratings on any dimension:
# All p values > .44 across all dimensions

# Interaction analysis for rq2:
# Function to extract all relevant comparisons for a dimension
get_dimension_comparisons <- function(model, dimension_name) {
  # Get all means combinations
  fb_timing_means <- emmeans(model, ~ feedback_provider:timing)
  
  # Within-feedback provider comparisons (Blind vs Informed)
  within_contrasts <- contrast(
    fb_timing_means,
    method = "pairwise",
    by = "feedback_provider",
    adjust = "none"
  ) %>% 
    summary(infer = TRUE) %>% 
    as.data.frame() %>% 
    mutate(
      comparison_type = "within",
      contrast = paste0(feedback_provider, "_P1_vs_P3")
    )
  
  # Between-feedback provider comparisons (within timing)
  between_contrasts <- contrast(
    fb_timing_means,
    method = "pairwise",
    by = "timing",
    adjust = "tukey"
  ) %>% 
    summary(infer = TRUE) %>% 
    as.data.frame() %>% 
    mutate(
      comparison_type = "between",
      contrast = paste0(timing, "_", sub(" - ", "_vs_", contrast))
    )
  
  # Combine and format results
  bind_rows(within_contrasts, between_contrasts) %>% 
    transmute(
      Dimension = dimension_name,
      comparison = contrast,
      p_value = p.value,
      estimate = estimate,
      comparison_type = comparison_type
    )
}

# Calculate comparisons for all dimensions
all_comparisons <- bind_rows(
  get_dimension_comparisons(objectivity_model, "Objectivity"),
  get_dimension_comparisons(usefulness_model, "Usefulness"),
  get_dimension_comparisons(genuineness_model, "Genuineness")
)

# Filter only significant comparisons if needed
sig_comparisons <- all_comparisons %>% 
  filter(p_value <= 0.05) %>% 
  mutate(
    label = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*"
    )
  )

# Create position mapping
position_mapping <- tribble(
  ~comparison,               ~xmin, ~xmax, ~y_level,
  "AI_P1_vs_P3",             0.6,   1.4,   1,
  "human_P1_vs_P3",          1.6,   2.4,   1,
  "humanplusai_P1_vs_P3",    2.6,   3.4,   1,
  "P1_AI_vs_human",          0.6,   1.6,   2,
  "P1_AI_vs_humanplusai",    0.6,   2.6,   3,
  "P1_human_vs_humanplusai", 1.6,   2.6,   2,
  "P3_AI_vs_human",          1.4,   2.4,   2,
  "P3_AI_vs_humanplusai",    1.4,   3.4,   3,
  "P3_human_vs_humanplusai", 2.4,   3.4,   2
)

# Merge with significant comparisons
signif_data <- sig_comparisons %>% 
  inner_join(position_mapping, by = "comparison") %>% 
  mutate(
    y_pos = max(summary_data$mean_Score) + 0.2 * y_level
  )

# Create a model with the interaction between gender and feedback provider
gender_interaction_model <- lmer(
  Score ~ feedback_provider * gender + timing + TT_combination + 
    age + course_id + AI_years + AI_ed_years + 
    Perceived_Usefulness + Perceived_Obstacles + Perceived_Readiness + Perceived_Trust + 
    (1|student_id),
  data = data_rq2 %>% 
    filter(Dimension == "Usefulness") %>%
    mutate(
      feedback_provider = factor(feedback_provider),
      gender = factor(gender)
    )
)

# compare genders within each feedback provider
emm_feedback_gender <- emmeans(gender_interaction_model, ~ gender | feedback_provider)
print("Comparison of genders within each feedback provider:")
print(emm_feedback_gender)
print(pairs(emm_feedback_gender, by = "feedback_provider", adjust = "tukey"))


#################################################################################
#RQ3    Name    Values    Type
#Condition A = independent variable 1    feedback_provider    AI, human, humanplusai    Factor
#Dimension 1 = dependent variable    Credibility    mean(ans0, ans1, ans2)    Number

#Covariate 1    course_id    PALS0045, PALS0046    Factor
#Covariate 2    age    Numeric
#Covariate 3    TT_combination    Both Passed, AI Only Passed, Co-Prod Only Passed, Both Failed    Factor
#Covariate 4    gender    female (includes nonbinary), male    Factor
#Covariate 5    AI_years    Years of AI experience    Numeric
#Covariate 6    AI_ed_years    Years of AI experience in education    Numeric
#Covariate 7    Perceived_Usefulness    Trust dimension score    Numeric
#Covariate 8    Perceived_Obstacles    Trust dimension score    Numeric
#Covariate 9    Perceived_Readiness    Trust dimension score    Numeric
#Covariate 10    Perceived_Trust    Trust dimension score    Numeric

# Credibility ~ feedback_provider + TT_combination + age + gender + course_id + 
#         AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + Perceived_Readiness + 
#         Perceived_Trust + (1|student_id)

data_rq3 <- data_rq1 %>% 
  # Extract credibility scores from the likert data
  pivot_longer(
    cols = matches("P3_extra_likert_(AI|human|humanplusai)"),
    names_to = c("timing", "extra", "likert", "feedback_provider"),
    names_sep = "_",
    values_to = "raw_scores",
    values_drop_na = TRUE
  ) %>%
  mutate(raw_scores = str_trim(raw_scores)) %>%
  filter(str_detect(raw_scores, "^\\d+(,\\d+)*$")) %>%
  separate_wider_delim(
    raw_scores,
    delim = ",", 
    names = paste0("ans", 0:2),  
    too_many = "drop",
    too_few = "align_start"
  ) %>%
  # Convert answers to numeric
  mutate(across(starts_with("ans"), as.numeric)) %>%
  # Calculate the Credibility score
  mutate(Credibility = rowMeans(dplyr::select(., ans0, ans1, ans2), na.rm = TRUE)) %>%
  # Select necessary columns from data_rq1 that already contain our covariates
  dplyr::select(course_id, student_id, feedback_provider, ans0, ans1, ans2, Credibility,
                TT_combination, gender, age, AI_years, AI_ed_years, 
                Perceived_Usefulness, Perceived_Obstacles, Perceived_Readiness, Perceived_Trust)

# Create the full model with all covariates as specified in your updated model
model_full <- lmer(Credibility ~ feedback_provider + TT_combination + age + gender + course_id + 
                     AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                     Perceived_Readiness + Perceived_Trust + (1|student_id), 
                   data = data_rq3)

# Print model summary
print(summary(model_full))
print(report(model_full))

# Calculate estimated marginal means for feedback_provider
emmeans_result <- emmeans(model_full, pairwise ~ feedback_provider)
print(emmeans_result)

# Check normality of residuals from the model
model_residuals <- residuals(model_full)

# Create QQ plot of residuals
qqnorm(model_residuals)
qqline(model_residuals)

# Histogram of residuals
hist(model_residuals, breaks = 20, main = "Histogram of Model Residuals")

# Formal tests of normality
shapiro_test <- shapiro.test(model_residuals)
print(shapiro_test)

# Calculate summary statistics by feedback provider
feedback_summary <- data_rq3 %>%
  group_by(feedback_provider) %>%
  summarise(
    mean = mean(Credibility, na.rm = TRUE), 
    median = median(Credibility, na.rm = TRUE), 
    sd = sd(Credibility, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(feedback_summary)

# Calculate effect sizes (Cohen's d) for each feedback provider comparison
# Extract model summary
model_summary <- summary(model_full)

# Get coefficients for feedback provider comparisons
# Note: The reference level is typically the first alphabetically (AI in this case)
coef_human <- model_summary$coefficients["feedback_providerhuman", "Estimate"]
coef_humanplusai <- model_summary$coefficients["feedback_providerhumanplusai", "Estimate"]

# Extract the residual standard deviation
residual_sd <- model_summary$sigma

# Calculate Cohen's d for each comparison
d_human_vs_ai <- coef_human / residual_sd
d_humanplusai_vs_ai <- coef_humanplusai / residual_sd
d_human_vs_humanplusai <- (coef_human - coef_humanplusai) / residual_sd

# Print the effect sizes
print(paste("Cohen's d for human vs AI feedback:", d_human_vs_ai))
print(paste("Cohen's d for co-produced vs AI feedback:", d_humanplusai_vs_ai))
print(paste("Cohen's d for human vs co-produced feedback:", d_human_vs_humanplusai))

# Interpret effect sizes
interpret_d <- function(d) {
  d_abs <- abs(d)
  if (d_abs < 0.2) return("negligible")
  if (d_abs < 0.5) return("small")
  if (d_abs < 0.8) return("medium")
  return("large")
}

print(paste("Effect size for human vs AI is", interpret_d(d_human_vs_ai)))
print(paste("Effect size for co-produced vs AI is", interpret_d(d_humanplusai_vs_ai)))
print(paste("Effect size for human vs co-produced is", interpret_d(d_human_vs_humanplusai)))

# AI_years                           -0.27460    0.12004  69.05303  -2.288 0.025229 *  
# AI_ed_years                         0.27180    0.12060  69.05303   2.254 0.027386 *  
# Perceived_Usefulness                0.17696    0.08052  69.05303   2.198 0.031326 *
# Effect size for human (4.18) vs AI (3.28) on the rate credibility differnce is large (p < .001, Cohen’s d = 1.21)
# Effect size for co-produced (4.04) vs AI (3.28) on the rate credibility differnce is  large (p < .001, Cohen’s d = 1.02)
# Effect size for human vs co-produced on the rate credibility differnce is negligible (Cohen’s d = 0.18)


# Interaction Analysis for RQ3
# Examining interactions between feedback provider and significant covariates

# Create models with interaction terms
# 1. Interaction with AI_years
model_interact_ai_years <- lmer(Credibility ~ feedback_provider * AI_years + 
                                  TT_combination + age + gender + course_id + 
                                  AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                                  Perceived_Readiness + Perceived_Trust + (1|student_id), 
                                data = data_rq3)

# 2. Interaction with AI_ed_years
model_interact_ai_ed_years <- lmer(Credibility ~ feedback_provider * AI_ed_years + 
                                     TT_combination + age + gender + course_id + 
                                     AI_years + Perceived_Usefulness + Perceived_Obstacles + 
                                     Perceived_Readiness + Perceived_Trust + (1|student_id), 
                                   data = data_rq3)

# 3. Interaction with Perceived_Usefulness
model_interact_usefulness <- lmer(Credibility ~ feedback_provider * Perceived_Usefulness + 
                                    TT_combination + age + gender + course_id + 
                                    AI_years + AI_ed_years + Perceived_Obstacles + 
                                    Perceived_Readiness + Perceived_Trust + (1|student_id), 
                                  data = data_rq3)

# 4. Model with all three interactions (may be complex but can show combined effects)
model_all_interactions <- lmer(Credibility ~ feedback_provider * (AI_years + AI_ed_years + Perceived_Usefulness) + 
                                 TT_combination + age + gender + course_id + 
                                 Perceived_Obstacles + Perceived_Readiness + Perceived_Trust + (1|student_id), 
                               data = data_rq3)

# Print summaries of interaction models
print("Model with AI_years interaction:")
print(summary(model_interact_ai_years))

print("Model with AI_ed_years interaction:")
print(summary(model_interact_ai_ed_years))

print("Model with Perceived_Usefulness interaction:")
print(summary(model_interact_usefulness))
#Significant -> proceeds to comparison: 

# For Human feedback
human_data <- data_rq3 %>% filter(feedback_provider == "human")
human_model <- glm(Credibility ~ TT_combination + age + gender + course_id + 
                     AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                     Perceived_Readiness + Perceived_Trust, 
                   data = human_data)

human_summary <- summary(human_model)
human_coef <- coef(human_summary)
human_p <- human_coef["Perceived_Usefulness", "Pr(>|t|)"]
human_estimate <- human_coef["Perceived_Usefulness", "Estimate"]
cat("Human provider - Perceived_Usefulness coefficient:", human_estimate, 
    "p-value:", human_p, "\n")

# For AI feedback
ai_data <- data_rq3 %>% filter(feedback_provider == "AI")
ai_model <- glm(Credibility ~ TT_combination + age + gender + course_id + 
                  AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                  Perceived_Readiness + Perceived_Trust, 
                data = ai_data)

ai_summary <- summary(ai_model)
ai_coef <- coef(ai_summary)
ai_p <- ai_coef["Perceived_Usefulness", "Pr(>|t|)"]
ai_estimate <- ai_coef["Perceived_Usefulness", "Estimate"]
cat("AI provider - Perceived_Usefulness coefficient:", ai_estimate, 
    "p-value:", ai_p, "\n")

# For Co-produced feedback
coproduced_data <- data_rq3 %>% filter(feedback_provider == "humanplusai")
coproduced_model <- glm(Credibility ~ TT_combination + age + gender + course_id + 
                          AI_years + AI_ed_years + Perceived_Usefulness + Perceived_Obstacles + 
                          Perceived_Readiness + Perceived_Trust, 
                        data = coproduced_data)

coproduced_summary <- summary(coproduced_model)
coproduced_coef <- coef(coproduced_summary)
coproduced_p <- coproduced_coef["Perceived_Usefulness", "Pr(>|t|)"]
coproduced_estimate <- coproduced_coef["Perceived_Usefulness", "Estimate"]
cat("Co-produced provider - Perceived_Usefulness coefficient:", coproduced_estimate, 
    "p-value:", coproduced_p, "\n")

cat("\nUsing ANOVA to test interaction significance:\n")
# Fit model without interaction
no_interaction_model <- lmer(Credibility ~ feedback_provider + Perceived_Usefulness + 
                               TT_combination + age + gender + course_id + 
                               AI_years + AI_ed_years + Perceived_Obstacles + 
                               Perceived_Readiness + Perceived_Trust + (1|student_id), 
                             data = data_rq3)

# Compare models with and without interaction
anova_result <- anova(no_interaction_model, interaction_model)
print(anova_result)
# Significant AI provider - Perceived_Usefulness coefficient: 0.3027015 p-value: 0.01880825 
# Human provider - Perceived_Usefulness coefficient: 0.09075122 p-value: 0.5485264 
# Co-produced provider - Perceived_Usefulness coefficient: 0.1374216 p-value: 0.2364367 



############################################################################
########### PRESENTATION RQ1 ###############################################
############################################################################

# Create a summary dataset for AI experience and identification rates
identification_summary <- data_rq1 %>%
  group_by(AI_ed_years) %>%
  summarise(
    n = n(),
    AI_success_rate = mean(TT_AI_passed, na.rm = TRUE) * 100,
    Coproduced_success_rate = mean(TT_co_passed, na.rm = TRUE) * 100,
    
    # Calculate confidence intervals for AI feedback
    AI_se = sqrt((AI_success_rate/100 * (1-AI_success_rate/100)) / n) * 100,
    AI_lower = pmax(0, AI_success_rate - 1.96 * AI_se),
    AI_upper = pmin(100, AI_success_rate + 1.96 * AI_se),
    
    # Calculate confidence intervals for co-produced feedback
    Coproduced_se = sqrt((Coproduced_success_rate/100 * (1-Coproduced_success_rate/100)) / n) * 100,
    Coproduced_lower = pmax(0, Coproduced_success_rate - 1.96 * Coproduced_se),
    Coproduced_upper = pmin(100, Coproduced_success_rate + 1.96 * Coproduced_se),
    .groups = "drop"
  )

# View the summary data
print(identification_summary)

# Fit linear models for trend lines
ai_model <- lm(AI_success_rate ~ AI_ed_years, data = identification_summary, weights = n)
coproduced_model <- lm(Coproduced_success_rate ~ AI_ed_years, data = identification_summary, weights = n)

# Print model summaries
cat("\nAI feedback identification model:\n")
print(summary(ai_model))

cat("\nCo-produced feedback identification model:\n")
print(summary(coproduced_model))

# Create trend line data
trend_data <- data.frame(AI_ed_years = seq(0, 4, by = 0.1)) %>%
  mutate(
    AI_trend = predict(ai_model, newdata = .),
    Coproduced_trend = predict(coproduced_model, newdata = .)
  )

# Create the visualization
library(ggplot2)

p1 <- ggplot() +
  # Add confidence interval ribbons
  geom_ribbon(data = identification_summary, 
              aes(x = AI_ed_years, ymin = AI_lower, ymax = AI_upper), 
              fill = "#8DA0CB", alpha = 0.2) +
  geom_ribbon(data = identification_summary, 
              aes(x = AI_ed_years, ymin = Coproduced_lower, ymax = Coproduced_upper), 
              fill = "#66C2A5", alpha = 0.2) +
  
  # Add trend lines
  geom_line(data = trend_data, 
            aes(x = AI_ed_years, y = AI_trend, color = "AI Feedback"), 
            linewidth = 1) +
  geom_line(data = trend_data, 
            aes(x = AI_ed_years, y = Coproduced_trend, color = "Co-produced Feedback"), 
            linewidth = 1) +
  
  # Add data points
  geom_point(data = identification_summary, 
             aes(x = AI_ed_years, y = AI_success_rate, color = "AI Feedback", size = n), 
             shape = 16) +
  geom_point(data = identification_summary, 
             aes(x = AI_ed_years, y = Coproduced_success_rate, color = "Co-produced Feedback", size = n), 
             shape = 16) +
  
  # Set labels and titles
  labs(
    x = "Years of Educational AI Usage",
    y = "Success Rate (%)") +
  
  # Set scale and colors
  scale_color_manual(name = "Feedback Type", 
                     values = c("AI Feedback" = "#8DA0CB", "Co-produced Feedback" = "#66C2A5")) +
  scale_size_continuous(name = "Sample Size", range = c(2, 6)) +
  scale_x_continuous(breaks = 0:4) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  
  # Theme customization
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p1)

ggsave("feedback_identification_by_ai_experience.png", p1, width = 8, height = 6, dpi = 300)

############################################################################
########### PRESENTATION RQ2 ###############################################
############################################################################

summary_data <- data_rq2 %>%
  # Make sure we're including all relevant grouping variables
  group_by(Dimension, feedback_provider, timing) %>%
  # Calculate mean and SE
  summarise(
    mean_Score = mean(Score, na.rm = TRUE),
    SE_Score = sd(Score, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  ) %>%
  # Format factors
  mutate(
    feedback_provider = factor(feedback_provider, 
                               levels = c("AI", "human", "humanplusai"),
                               labels = c("AI", "Human", "Co-produced")),
    # Recode timing factor labels
    timing = factor(timing, 
                    levels = c("P1", "P3"),
                    labels = c("Blind", "Informed"))
  )

# Create the plot with feedback provider as fill and timing as x
p2 <- ggplot(summary_data, 
             aes(x = timing, y = mean_Score, 
                 fill = feedback_provider, group = feedback_provider)) +
  # Create grouped bar chart
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  # Add error bars
  geom_errorbar(aes(ymin = mean_Score-SE_Score, ymax = mean_Score+SE_Score),
                width = 0.2, position = position_dodge(0.8), size = 1) +
  # Create separate facets horizontally
  facet_wrap(~ Dimension, ncol = 3) +
  # Use the same color palette but now for feedback_provider
  scale_fill_manual(values = c("AI" = "#8DA0CB", "Human" = "#FC8D62", "Co-produced" = "#66C2A5"),
                    labels = c("AI", "Human", "Co-produced")) +
  # Updated labels
  labs(x = "Timing Condition", 
       y = "Rating Score", 
       fill = "Feedback Type",
       title = "Effects of Feedback Provider and Timing on Ratings") +
  # Set y-axis limit to 5
  ylim(0, 5) +
  # Theme customization
  theme_classic2() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5)
  )

# Print the plot
print(p2)
ggsave("feedback_rating_by_time_23Mar.png", p2, width = 12, height = 6, dpi = 300)


visualization_summary <- usefulness_summary %>%
  mutate(feedback_provider = case_when(
    feedback_provider == "AI" ~ "AI",
    feedback_provider == "human" ~ "Human",
    feedback_provider == "humanplusai" ~ "Co-produced",
    TRUE ~ feedback_provider  # Keep any other values as they are
  )) %>%
  pivot_wider(
    id_cols = c(AI_years, n),
    names_from = feedback_provider,
    values_from = c(mean_score, lower, upper),
    names_sep = "_"
  ) %>%
  rename(
    AI_success_rate = mean_score_AI,
    Human_success_rate = `mean_score_Human`,
    Coproduced_success_rate = `mean_score_Co-produced`,
    AI_lower = lower_AI,
    Human_lower = `lower_Human`,
    Coproduced_lower = `lower_Co-produced`,
    AI_upper = upper_AI,
    Human_upper = `upper_Human`,
    Coproduced_upper = `upper_Co-produced`
  )

# Create trend lines using linear models for all three types
AI_model <- lm(Score ~ AI_years, data = filter(data_rq2, Dimension == "Usefulness", feedback_provider == "AI"))
human_model <- lm(Score ~ AI_years, data = filter(data_rq2, Dimension == "Usefulness", feedback_provider == "human"))
humanplusai_model <- lm(Score ~ AI_years, data = filter(data_rq2, Dimension == "Usefulness", feedback_provider == "humanplusai"))

# Create a dataset for trend lines
trend_data <- tibble(
  AI_years = rep(seq(0, max(data_rq2$AI_years, na.rm = TRUE), by = 0.1), 3)
) %>%
  mutate(
    AI_trend = predict(AI_model, newdata = data.frame(AI_years = AI_years)),
    human_trend = predict(human_model, newdata = data.frame(AI_years = AI_years)),
    humanplusai_trend = predict(humanplusai_model, newdata = data.frame(AI_years = AI_years))
  )

p2_useful_year <- ggplot() +
  # Add confidence interval ribbons for all three types
  geom_ribbon(data = visualization_summary, 
              aes(x = AI_years, ymin = AI_lower, ymax = AI_upper), 
              fill = "#8DA0CB", alpha = 0.2) +
  geom_ribbon(data = visualization_summary, 
              aes(x = AI_years, ymin = Human_lower, ymax = Human_upper), 
              fill = "#FC8D62", alpha = 0.2) +
  geom_ribbon(data = visualization_summary, 
              aes(x = AI_years, ymin = Coproduced_lower, ymax = Coproduced_upper), 
              fill = "#66C2A5", alpha = 0.2) +
  
  # Add trend lines for all three types
  geom_line(data = trend_data, 
            aes(x = AI_years, y = AI_trend, color = "AI"), 
            linewidth = 1) +
  geom_line(data = trend_data, 
            aes(x = AI_years, y = human_trend, color = "Human"), 
            linewidth = 1) +
  geom_line(data = trend_data, 
            aes(x = AI_years, y = humanplusai_trend, color = "Co-produced"), 
            linewidth = 1) +
  
  # Add data points for all three types
  geom_point(data = visualization_summary, 
             aes(x = AI_years, y = AI_success_rate, color = "AI", size = n), 
             shape = 16) +
  geom_point(data = visualization_summary, 
             aes(x = AI_years, y = Human_success_rate, color = "Human", size = n), 
             shape = 16) +
  geom_point(data = visualization_summary, 
             aes(x = AI_years, y = Coproduced_success_rate, color = "Co-produced", size = n), 
             shape = 16) +
  
  # Set labels and titles
  labs(
    x = "Years of General AI Usage",
    y = "Usefulness Rating") +
  
  # Set scale and colors for all three types
  scale_color_manual(name = "Feedback Type", 
                     values = c("AI" = "#8DA0CB", 
                                "Human" = "#FC8D62", 
                                "Co-produced" = "#66C2A5")) +
  scale_size_continuous(name = "Sample Size", range = c(2, 6)) +
  scale_x_continuous(breaks = 0:max(data_rq2$AI_years, na.rm = TRUE)) +
  scale_y_continuous(limits = c(min(data_rq2$Score[data_rq2$Dimension == "Usefulness"], na.rm = TRUE) - 0.5, 
                                max(data_rq2$Score[data_rq2$Dimension == "Usefulness"], na.rm = TRUE) + 0.5)) +
  
  # Theme customization
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

print(p2_useful_year)

# Create a dataset for gender differences in Usefulness dimension by feedback provider
gender_feedback_summary <- data_rq2 %>%
  filter(Dimension == "Usefulness") %>%
  group_by(gender, feedback_provider) %>%
  # Calculate mean and SE
  summarise(
    mean_Score = mean(Score, na.rm = TRUE),
    SE_Score = sd(Score, na.rm = TRUE)/sqrt(n()),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  # Format factors
  mutate(
    # Combine female and non-binary if present in your data
    Gender = factor(gender, 
                    levels = c("male", "female"),
                    labels = c("Male", "Female")),
    # Format feedback provider labels
    Feedback_Type = factor(feedback_provider, 
                           levels = c("AI", "human", "humanplusai"),
                           labels = c("AI", "Human", "Co-produced"))
  )

# Create the bar chart showing gender differences by feedback type
p_gender_feedback <- ggplot(gender_feedback_summary, 
                            aes(x = Feedback_Type, y = mean_Score, fill = Gender)) +
  # Create grouped bar chart
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  # Add error bars
  geom_errorbar(aes(ymin = mean_Score-SE_Score, ymax = mean_Score+SE_Score),
                width = 0.2, position = position_dodge(0.8), size = 1) +
  # Use a color palette that differentiates genders well
  scale_fill_manual(values = c("Male" = "#8DA0CB", "Female" = "#FC8D62")) +
  # Add significance annotations for human feedback
  annotate("text", x = 2, y = max(gender_feedback_summary$mean_Score) + 0.5, 
           label = "**p = .001", size = 4) +
  # Add labels
  labs(x = "Feedback Type", 
       y = "Usefulness Rating", 
       fill = "Gender") +
  # Set y-axis limit to 5
  scale_y_continuous(limits = c(0, 5)) +
  # Theme customization
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12)
  )

# Print the plot
print(p_gender_feedback)
#title = "Gender Differences in Perceived Usefulness by Feedback Type",
# subtitle = "Significant gender difference for Human feedback, but not for AI or Co-produced feedback") +
  
# Save the plot
ggsave("usefulness_rating_by_gender_23Mar.png", p_gender_feedback, width = 8, height = 6, dpi = 300)

library(gridExtra)
library(ggplot2)

combined_plot <- grid.arrange(p2_useful_year, p_gender_feedback, ncol = 2)

# Save the combined plot with the specified dimensions (width = 12, height = 6)
ggsave("combined_usefulness_plots.png", combined_plot, width = 12, height = 6, units = "in", dpi = 300)

############################################################################
########### PRESENTATION RQ3 ###############################################
############################################################################

# Load required libraries
library(ggplot2)
library(gridExtra) # For combining plots
library(dplyr)
library(grid) # For adding text annotations

# Assuming data_rq3 already exists in your environment
# Below is the code to recreate the three plots, then combine them

# PLOT A: Credibility by feedback provider
# Reshape data for visualization
long_data_rq3 <- data_rq3 %>%
  mutate(Dimension = "Credibility", 
         Score = Credibility,
         timing = "P3")

# Calculate summary statistics for plotting
summary_data_3 <- long_data_rq3 %>% 
  group_by(Dimension, feedback_provider) %>%
  summarise(
    mean_Score = mean(Score, na.rm = TRUE), 
    SE_Score = sd(Score, na.rm = TRUE) / sqrt(n()), 
    .groups = "drop"
  ) %>%
  mutate(
    # Ensure all three feedback types are included and properly labeled
    feedback_provider = factor(feedback_provider, 
                               levels = c("human", "AI", "humanplusai"),
                               labels = c("Human", "AI", "Co-produced"))
  ) %>%
  ungroup() 

# Define color palette for the three feedback types
palette_colors <- c("Human" = "#FC8D62", "AI" = "#8DA0CB", "Co-produced" = "#66C2A5")

# Plot A: Bar chart of means by feedback provider - REMOVED FACET_GRID TO ELIMINATE GREY LABEL
a <- ggplot(summary_data_3, 
            aes(x = feedback_provider, y = mean_Score, fill = feedback_provider)) + 
  geom_col(position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_Score - SE_Score, ymax = mean_Score + SE_Score), 
    width = 0.2, position = position_dodge(0.9), size = 1
  ) +
  # Removed facet_grid(Dimension ~ .) which created the "Credibility" grey belt
  annotate("text", x = 1.5, y = max(summary_data_3$mean_Score) + 0.5, label = "***p < .001", size = 8) +
  annotate("text", x = 2.5, y = max(summary_data_3$mean_Score) + 0.5, label = "***p < .001", size = 8) +
  labs(
    x = "Feedback Provider", 
    y = "Credibility Rating", 
    fill = "Provider Type"
  ) +
  theme_classic() +
  scale_fill_manual(values = palette_colors) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.margin = margin(t = 25, r = 10, b = 10, l = 10)
  ) +
  coord_cartesian(ylim = c(1, 5))

a

ggsave("6a_Mean credibility ratings by feedback provider type.png", a, width = 12, height = 9, dpi = 300)

#Plot b
model <- lmer(Credibility ~ feedback_provider + TT_combination + age + gender + 
               course_id + AI_years + AI_ed_years + Perceived_Usefulness + 
               Perceived_Obstacles + Perceived_Readiness + Perceived_Trust + (1|student_id),
               data = data_rq3)

library(ggplot2)
library(dplyr)
library(ggeffects)

# Create the AI experience plot (plot b)
b <- ggplot() +
  # Add confidence interval ribbons
  geom_ribbon(data = pred_general_df, 
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "#8DA0CB", alpha = 0.2) +
  geom_ribbon(data = pred_ed_df, 
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "#66C2A5", alpha = 0.2) +
  
  # Add trend lines
  geom_line(data = pred_general_df, 
            aes(x = x, y = predicted, color = "General"), 
            linewidth = 1) +
  geom_line(data = pred_ed_df, 
            aes(x = x, y = predicted, color = "Educational"), 
            linewidth = 1) +
  
  # Add size-weighted points for sample sizes
  geom_point(data = credibility_summary, 
             aes(x = experience_years, y = General_credibility, 
                 color = "General", size = n_general), 
             shape = 16) +
  geom_point(data = credibility_summary_ed, 
             aes(x = experience_years, y = Educational_credibility, 
                 color = "Educational", size = n_educational), 
             shape = 16) +
  
  # Add significance annotations
  annotate("text", x = max(pred_general_df$x) - 0.5, 
           y = min(pred_general_df$predicted), 
           label = "*p = .025", size = 8) +
  annotate("text", x = max(pred_ed_df$x) - 0.5, 
           y = max(pred_ed_df$predicted), 
           label = "*p = .027", size = 8) +
  
  # Set labels and titles
  labs(x = "Years of AI Usage", 
       y = "Credibility Rating") +
  
  # Set scale and colors
  scale_color_manual(name = "Experience Type", 
                     values = c("General" = "#8DA0CB", 
                                "Educational" = "#66C2A5")) +
  scale_size_continuous(name = "Sample Size", range = c(2, 6), guide = guide_legend(title.position = "top")) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  
  # Theme customization
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color = "black", size = 20),
    axis.title = element_text(size = 20),                          
    axis.text = element_text(size = 20),
    plot.margin = margin(t = 25, r = 25, b = 10, l = 10),
    # Ensure consistent legend order
    legend.box.just = "top"
  ) +
  # This forces the order of the legends
  guides(
    color = guide_legend(order = 1, title.position = "top"),
    size = guide_legend(order = 2, title.position = "top")
  )

b

ggsave("6b_relationship between years of AI usage and credibility.png", b, width = 12, height = 9, dpi = 300)

#Plot c
library(ggplot2)
library(dplyr)
library(ggeffects)

# Create the Perceived Benefits plot (plot c)
c <- ggplot() +
  # Add confidence interval ribbons for each provider type from model predictions
  geom_ribbon(data = pred_df, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2) +
  
  # Add trend lines for each provider type from model predictions
  geom_line(data = pred_df, 
            aes(x = x, y = predicted, color = group), 
            linewidth = 1) +
  
  # Add sample size points from the summarized data
  geom_point(data = usefulness_by_provider, 
             aes(x = Usefulness_group, y = Provider_credibility, 
                 color = feedback_provider, size = n_usefulness), 
             alpha = 0.7) +
  
  # Add p-value annotation
  annotate("text", 
           x = min(pred_df$x) + 0.5, 
           y = min(pred_df$predicted) + 0.3, 
           label = "*p = .019", size = 8) +
  
  # Set labels and titles
  labs(x = "Perceived Usefulness", 
       y = "Credibility Rating") +
  
  # Set scale and colors using the same palette as original
  scale_color_manual(name = "Feedback Provider", values = palette_colors) +
  scale_fill_manual(name = "Feedback Provider", values = palette_colors, guide = "none") + 
  scale_size_continuous(name = "Sample Size", range = c(2, 6), guide = guide_legend(title.position = "top")) +
  scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
  
  # Theme customization
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color = "black", size = 20),
    axis.title = element_text(size = 20),                          
    axis.text = element_text(size = 20),
    plot.margin = margin(t = 25, r = 20, b = 10, l = 10),
    # Ensure consistent legend order
    legend.box.just = "top"
  ) +
  # This forces the order of the legends
  guides(
    color = guide_legend(order = 1, title.position = "top"),
    size = guide_legend(order = 2, title.position = "top")
  )

c

ggsave("6c_Influence of perceived usefulness on credibility ratings.png", c, width = 12, height = 9, dpi = 300)