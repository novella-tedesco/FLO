# ============================================
# TITLE: Analysis of disfluency in transcultural communication with Mixed-Effects Models
# AUTHOR: Novella Tedesco
# ADAPTED FROM: Van Hoey et al. 2025 (Project DOI: 10.17605/OSF.IO/5X9YW) 
# DESCRIPTION: This script  analyzes how translanguaging affect speech disfluency , while accounting for other variables.
# NOTE: This script is adapted from: Van Hoey, T., Szmrecsanyi, B., Ma, R., (2025). "Isomorphism-inspired theorising about optionality and variation: no empirical support from English grammar."
# ============================================

# =====================================================
# MODEL 1: Translanguaging (any) → General Disfluency
# =====================================================

# --- Load packages ---
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(readxl)
library(broom.mixed)
library(ggeffects)
library(ggplot2)


# --- Load dataset ---
data <- read_excel(
  "/Users/andreina/Library/CloudStorage/OneDrive-AlmaMaterStudiorumUniversitàdiBologna/cose da pc/dottorato/CChapters/dataset_Casestudy4_tedesco_2026.xlsx"
)

# --- Random-effect grouping variables ---
data$speaker_ID <- as.factor(data$speaker_ID)

# --- Fixed effects ---
data$translanguaging_any <- as.factor(data$translanguaging_any)
data$context <- as.factor(data$context)
data$speaker_nationality <- as.factor(data$speaker_nationality)

data$Plurilingualism <- as.numeric(data$`num_langs(Plurilingualism)`)
data$turn_length <- as.numeric(data$turn_length)
data$lexical_diversity <- as.numeric(data$`lexical_diversity(STTR)`)
data$content_complexity <- as.numeric(
  data$`content_complexity(mean_ortographic_word_length)`
)

# --- Dependent variable ---
data$disfluencies <- as.numeric(data$disfluencies)

# --- Fit linear mixed-effects model ---
model_1 <- lmer(
  disfluencies ~
    translanguaging_any +
    context +
    speaker_nationality +
    Plurilingualism +
    turn_length +
    lexical_diversity +
    content_complexity +
    (1 | speaker_ID),
  data = data,
  REML = FALSE
)

# --- Model summary ---
summary(model_1)

# --- Multicollinearity check ---
car::vif(model_1)

# --- Model fit ---
performance::r2(model_1)

# --- Visualise main effect of translanguaging ---
pred_tl <- ggpredict(model_1, terms = "translanguaging_any")

ggplot(pred_tl, aes(x = x, y = predicted)) +
  geom_point(size = 2) +
  geom_line(group = 1) +
  theme_minimal() +
  labs(
    title = "Effect of Translanguaging on Disfluency",
    x = "Translanguaging (any)",
    y = "Predicted Disfluency"
  )

# --- Export tidy results ---
model_1 %>%
  broom.mixed::tidy() %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  write.csv(
    "model1_translanguaging_any_disfluency_context_nationality_fixed.csv",
    row.names = FALSE
  )


# =====================================================
# MODEL 2: Translanguaging (any) → Disfluency TYPES
# =====================================================

# --- Disfluency-dependent variables (functional core set) ---
disfluency_vars <- c(
  "impediments",
  "repairs",
  "discourse_markers"
)

# --- Containers ---
models_model2 <- list()
results_model2 <- list()

# --- Loop over dependent variables ---
for (dv in disfluency_vars) {
  
  cat("\n=====================================\n")
  cat("Fitting Model 2 for DV:", dv, "\n")
  cat("=====================================\n")
  
  # Build formula
  formula_model2 <- as.formula(
    paste(
      dv,
      "~ translanguaging_any +",
      "context + speaker_nationality +",
      "Plurilingualism + turn_length +",
      "lexical_diversity + content_complexity +",
      "(1 | speaker_ID)"
    )
  )
  
  # Fit model
  model <- lmer(
    formula_model2,
    data = data,
    REML = FALSE
  )
  
  # Store model
  models_model2[[dv]] <- model
  
  # Extract fixed effects
  results_model2[[dv]] <- broom.mixed::tidy(
    model,
    effects = "fixed"
  ) %>%
    mutate(dependent_variable = dv)
  
  # Print translanguaging effect for monitoring
  cat("\nEffect of translanguaging_any:\n")
  print(
    summary(model)$coefficients["translanguaging_any1", ]
  )
}

# --- Combine results into one table ---
model2_results_table <- bind_rows(results_model2) %>%
  select(
    dependent_variable,
    term,
    estimate,
    std.error,
    statistic,
    p.value
  )

# --- Save results table ---
write.csv(
  model2_results_table,
  "model2_core_disfluency_dimensions_results.csv",
  row.names = FALSE
)

# --- Save models ---
for (dv in names(models_model2)) {
  saveRDS(
    models_model2[[dv]],
    paste0("model2_", dv, "_translanguaging_any.rds")
  )
}

# =====================================================
# MODEL 3: Translanguaging STRATEGIES → Impediments
# =====================================================

# --- Load packages ---
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(readxl)
library(broom.mixed)
library(ggeffects)
library(ggplot2)

# --- Load dataset ---
data <- read_excel(
  "/Users/andreina/Library/CloudStorage/OneDrive-AlmaMaterStudiorumUniversitàdiBologna/cose da pc/dottorato/CChapters/dataset_Casestudy4_tedesco_2026.xlsx"
)

# =====================================================
# Variable preparation
# =====================================================

# --- Random effect ---
data$speaker_ID <- as.factor(data$speaker_ID)

# --- Translanguaging strategies (binary predictors) ---
data$codeswitching <- as.numeric(data$codeswitching)
data$codemixing    <- as.numeric(data$codemixing)
data$translation   <- as.numeric(data$translation)

# --- Control variables ---
data$context <- as.factor(data$context)
data$speaker_nationality <- as.factor(data$speaker_nationality)

data$Plurilingualism <- as.numeric(data$`num_langs(Plurilingualism)`)
data$turn_length <- as.numeric(data$turn_length)
data$lexical_diversity <- as.numeric(data$`lexical_diversity(STTR)`)
data$content_complexity <- as.numeric(
  data$`content_complexity(mean_ortographic_word_length)`
)

# --- Dependent variable ---
data$impediments <- as.numeric(data$impediments)

# =====================================================
# Fit Model 3
# =====================================================

model_3 <- lmer(
  impediments ~
    codeswitching +
    codemixing +
    translation +
    context +
    speaker_nationality +
    Plurilingualism +
    turn_length +
    lexical_diversity +
    content_complexity +
    (1 | speaker_ID),
  data = data,
  REML = FALSE
)

# =====================================================
# Output and diagnostics
# =====================================================

# --- Model summary ---
summary(model_3)

# --- Multicollinearity ---
car::vif(model_3)

# --- Model fit ---
performance::r2(model_3)

# =====================================================
# Save results
# =====================================================

# --- Fixed effects table ---
model_3 %>%
  broom.mixed::tidy(effects = "fixed") %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  write.csv(
    "model3_impediments_translanguaging_strategies.csv",
    row.names = FALSE
  )

# --- Save model object ---
saveRDS(
  model_3,
  "model3_impediments_translanguaging_strategies.rds"
)

