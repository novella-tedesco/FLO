library(tidyverse)
library(readxl)

data <- read_excel(
  "/Users/andreina/Library/CloudStorage/OneDrive-AlmaMaterStudiorumUniversitàdiBologna/cose da pc/dottorato/CChapters/dataset_Casestudy4_tedesco_2026.xlsx"
)

data_clean <- data %>%
  filter(
    speaker_ID != "Researcher_IT",
    turn_length >= 4
  ) %>%
  arrange(session_ID, speaker_ID, turn_ID)

# =====================================================
# 3. IDENTIFY PRECEDING IMPEDIMENT
# (same speaker, same session, earlier turn)
# =====================================================

data_clean <- data_clean %>%
  group_by(session_ID, speaker_ID) %>%
  arrange(turn_ID, .by_group = TRUE) %>%
  mutate(
    preceding_impediment_same_speaker =
      lag(impediments > 0, order_by = turn_ID)
  ) %>%
  ungroup()

# Replace NA with FALSE
data_clean$preceding_impediment_same_speaker[
  is.na(data_clean$preceding_impediment_same_speaker)
] <- FALSE

# =====================================================
# 4. DEFINE SAMPLING POOLS
# =====================================================

# ---- CODESWITCHING ----
cs_same_turn <- data_clean %>%
  filter(
    codeswitching == 1,
    impediments > 0
  )

cs_preceding <- data_clean %>%
  filter(
    codeswitching == 1,
    preceding_impediment_same_speaker == TRUE
  )

# ---- CODEMIXING ----
cm_same_turn <- data_clean %>%
  filter(
    codemixing == 1,
    impediments > 0
  )

cm_preceding <- data_clean %>%
  filter(
    codemixing == 1,
    preceding_impediment_same_speaker == TRUE
  )

# ---- TRANSLATION (preceding only) ----
tr_preceding <- data_clean %>%
  filter(
    translation == 1,
    preceding_impediment_same_speaker == TRUE
  )

# =====================================================
# 5. SAMPLE CASES
# =====================================================
# (random but reproducible)

set.seed(2026)

samples <- bind_rows(
  cs_same_turn  %>% slice_sample(n = 2) %>% mutate(Macro_group = "Codeswitching – same turn"),
  cs_preceding  %>% slice_sample(n = 2) %>% mutate(Macro_group = "Codeswitching – preceding turn"),
  cm_same_turn  %>% slice_sample(n = 2) %>% mutate(Macro_group = "Codemixing – same turn"),
  cm_preceding  %>% slice_sample(n = 2) %>% mutate(Macro_group = "Codemixing – preceding turn"),
  tr_preceding  %>% slice_sample(n = 2) %>% mutate(Macro_group = "Translation – preceding turn")
)

# =====================================================
# 6. FINAL TABLE FOR QUALITATIVE ANALYSIS
# =====================================================

final_samples <- samples %>%
  mutate(
    Sample_ID = paste0("S", row_number())
  ) %>%
  select(
    Sample_ID,
    Macro_group,
    session_ID,
    speaker_ID,
    turn_ID,
    transcript,
    impediments,
    codeswitching,
    codemixing,
    translation
  )

# =====================================================
# 7. EXPORT
# =====================================================

write.csv(
  final_samples,
  "Qualitative_Samples_Impediments_Translanguaging.csv",
  row.names = FALSE
)