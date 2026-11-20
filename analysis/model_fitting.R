library(tidyverse)
library(lme4)

contr.dummy <- function (f, levelsToKeep) {
  f <- as.factor(f)
  mm <- model.matrix(~f)[, -1, drop = FALSE]    # model.matrix(~0 + f)
  colnames(mm) <- sub("^f", "", colnames(mm))
  missingLevels <- missing(levelsToKeep)
  if (missingLevels) 
    levelsToKeep <- colnames(mm)                    
  mm <- mm[, levelsToKeep, drop = FALSE]
  return(mm)
}

data_dir <- "../data"
fit_dir <- "fits"

model_name <- "exp2_recall_recognition_ns1"
print(model_name)

exp_num <- str_extract(model_name, "(?<=exp)\\d+")

models <- list(
  exp1 = old_chosen ~ old_value * memorability_bin + 
    (old_value * memorability_bin | participant_id) +
    (old_value | old_image) + (1 | new_image),
  # exp1_ns1 = old_chosen ~ old_value * memorability_bin + 
  #   diag(old_value * memorability_bin | participant_id) +
  #   diag(old_value | old_image) + (1 | new_image),
  
  exp2_recognition = correct_recognition ~ old_value * memorability_bin +
    (old_value * memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  # exp2_recognition_ns1 = correct_recognition ~ old_value * memorability_bin +
  #   diag(old_value * memorability_bin | participant_id) + 
  #   diag(old_value | old_image) + (1 | new_image),
  
  exp2_recall = correct_recall ~ old_value * memorability_bin +
    (old_value * memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  # converged
  exp2_recall_ns1 = correct_recall ~ old_value * memorability_bin +
    diag(old_value * memorability_bin | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  exp2_recall_ns2 = correct_recall ~ old_value * memorability_bin +
    (old_value * memorability_bin | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  # converged
  exp2_recall_ns3 = correct_recall ~ old_value * memorability_bin +
    diag(old_value * memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns4 = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) + (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns4a = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) + (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns4b = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) +
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")) / old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns4c = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) +
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")) + contr.dummy(memorability_bin, c(".L")):old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  # converged
  exp2_recall_ns5 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns6 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns7 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_ns8 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".Q")) | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  
  exp2_recall_cr = correct_recall ~ old_value * memorability_bin +
    (old_value * memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns1 = correct_recall ~ old_value * memorability_bin +
    diag(old_value * memorability_bin | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns2 = correct_recall ~ old_value * memorability_bin +
    (old_value * memorability_bin | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns3 = correct_recall ~ old_value * memorability_bin +
    diag(old_value * memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns4 = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) + (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns4a = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) + (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns4b = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) + (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns4c = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) +
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")) + contr.dummy(memorability_bin, c(".L", ".Q")):old_value | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns4d = correct_recall ~ old_value * memorability_bin +
    (old_value | participant_id) +
    diag(0 + contr.dummy(memorability_bin, c(".L", ".Q")) + contr.dummy(memorability_bin, c(".L")):old_value | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  # converged
  exp2_recall_cr_ns5 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns6 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".L", ".Q")) | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  # converged
  exp2_recall_cr_ns7 = correct_recall ~ old_value * memorability_bin +
    (old_value / memorability_bin | participant_id) + 
    (0 + contr.dummy(memorability_bin, c(".Q")) | participant_id) + 
    (old_value | old_image) + (1 | new_image),
  exp2_recall_cr_ns8 = correct_recall ~ old_value * memorability_bin +
    diag(old_value * contr.dummy(memorability_bin, c(".L", ".Q")) - contr.dummy(memorability_bin, c(".Q")):old_value | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  
  exp2_recall_recognition_ns1 = correct_recall ~ old_value * memorability_bin * correct_recognition_num +
    diag(old_value * memorability_bin * correct_recognition_num | participant_id) + 
    diag(old_value | old_image) + (1 | new_image),
  
  exp3 = optimal_choice ~ memorability_bin + 
    (memorability_bin | participant_id) + (1 | high_image) + (1 | low_image),
  # exp3_ns1 = optimal_choice ~ memorability_bin + 
  #   diag(memorability_bin | participant_id) + (1 | high_image) + (1 | low_image),
  
  exp4 = chose_high_mem ~ overall_value + 
    (overall_value | participant_id) + (overall_value | high_mem_image) + (overall_value | low_mem_image),
  exp4_ns1 = chose_high_mem ~ overall_value + 
    diag(overall_value | participant_id) + diag(overall_value | high_mem_image) + diag(overall_value | low_mem_image),
  
  # drop 2
  exp4_ns2 = chose_high_mem ~ overall_value + 
    (overall_value | participant_id) + diag(overall_value | high_mem_image) + diag(overall_value | low_mem_image),
  # this one converges
  exp4_ns3 = chose_high_mem ~ overall_value + 
    diag(overall_value | participant_id) + (overall_value | high_mem_image) + diag(overall_value | low_mem_image),
  exp4_ns4 = chose_high_mem ~ overall_value + 
    diag(overall_value | participant_id) + diag(overall_value | high_mem_image) + (overall_value | low_mem_image),
  
  # drop 1
  exp4_ns5 = chose_high_mem ~ overall_value + 
    (overall_value | participant_id) + (overall_value | high_mem_image) + diag(overall_value | low_mem_image),
  exp4_ns6 = chose_high_mem ~ overall_value + 
    diag(overall_value | participant_id) + (overall_value | high_mem_image) + (overall_value | low_mem_image),
  # also converges
  exp4_ns7 = chose_high_mem ~ overall_value + 
    (overall_value | participant_id) + diag(overall_value | high_mem_image) + (overall_value | low_mem_image)
)

fp <- switch(exp_num,
             "1" = "episodic_choice_data.csv",
             "2" = "episodic_choice_data-direct.csv",
             "3" = "episodic_choice_data-matched_memorability.csv",
             "4" = "episodic_choice_data-mixed_memorability.csv")
data <- read_csv(file.path(data_dir, fp))

# Preprocessing ------------------------------------------------------------

model_df <- switch(exp_num,
                   "1" = data |> 
                     filter(old_trial == 1) |>
                     mutate(
                       old_value = if_else(old_value == 1, 1, -1),
                       old_image = if_else(old_side == "left", left_image_name, right_image_name),
                       new_image = if_else(old_side == "left", right_image_name, left_image_name),
                       memorability_bin = ordered(memorability_bin, levels = c("low", "mid", "high")),
                    ),
                   "2" = data |> 
                     mutate(
                       old_value = if_else(old_value == 1, 1, -1),
                       old_image = if_else(old_side == "left", left_image_name, right_image_name),
                       new_image = if_else(old_side == "left", right_image_name, left_image_name),
                       memorability_bin = ordered(memorability_bin, levels = c("low", "mid", "high")),
                       correct_recall = replace_when(value_test_correct, response == "null" ~ NA),
                       correct_recognition = replace_when(recognition_correct, response == "null" ~ NA),
                       correct_recognition_num = if_else(correct_recognition == 1, 1, -1)
                     ) |> group_by(participant_id, trial_number) |>
                     mutate(
                       correct_recall = lead(correct_recall)
                     ) |> filter(is_recognition_trial),
                   "3" = data |>
                     filter(old_trial == 1) |>
                     mutate(
                       high_image = if_else(left_value == 1, left_image_name, right_image_name),
                       low_image = if_else(left_value == 0, left_image_name, right_image_name),
                       memorability_bin = ordered(memorability_bin, levels = c("low", "high")),
                     ),
                   "4" = data |>
                     filter(old_trial == 1) |>
                     mutate(
                       overall_value = (left_value + right_value) - 1, # Centered
                       high_mem_image = if_else(left_is_high, left_image_name, right_image_name),
                       low_mem_image = if_else(left_is_high, right_image_name, left_image_name),
                       chose_high_mem = if_else((chosen_side == "left") == left_is_high, TRUE, FALSE)
                     ))

if(str_detect(model_name, "recall_cr")) {
  print(nrow(model_df))
  model_df <- model_df |>
    filter(correct_recognition == 1)
  print(nrow(model_df))
}

# Model fitting ------------------------------------------------------------

print(models[[model_name]])
m <- glmer(models[[model_name]], family = binomial,
                  data = model_df)

saveRDS(m, file = file.path(fit_dir, paste0(model_name, ".rds")))

