library(tidyverse)
library(lme4)

strict_tol <- glmerControl(optCtrl=list(FtolAbs=1e-7, XtolRel = 1e-9, rhoend = 2e-9, maxfun = 1000000))
data_dir <- "../data"
fit_dir <- "fits"

model_name <- "m2_right_all"
use_strict_tol <- TRUE
update_fit <- TRUE
jitter <- 0.001
fit_n <- 3

cat(dataset, model_name)

food_choice_data <-
  read_csv(file.path(data_dir, paste0("food_choice_data_gap23.csv")))

source("food_choice_preprocessing.R")

models <- list(
  m1_right_similar = chose_right ~ delta.mem * scale(SumValue) + 
       (delta.mem * scale(SumValue) | participant_id) + 
       (1 | left_image_name) +
       (1 | right_image_name),
  m1_right_distant = chose_right ~ z.delta.value * scale(SumMem) + 
       (z.delta.value * scale(SumMem) | participant_id) + 
       (1 | left_image_name) +
       (1 | right_image_name),
  m2_right_all = chose_right ~ delta.mem * scale(SumValue) + z.delta.value * scale(SumMem) +
       (delta.mem * scale(SumValue) + z.delta.value * scale(SumMem) | participant_id) +
       (1 | left_image_name) +
       (1 | right_image_name),
  m1_right_similar_ns1 = chose_right ~ delta.mem * scale(SumValue) + 
    diag(delta.mem * scale(SumValue) | participant_id) + 
    (1 | left_image_name) +
    (1 | right_image_name),
  m1_right_similar_ns2 = chose_right ~ delta.mem * scale(SumValue) + 
    (delta.mem | participant_id) + 
    diag(0 + scale(SumValue) / delta.mem | participant_id) +
    (1 | left_image_name) +
    (1 | right_image_name),
  m1_right_distant_ns1 = chose_right ~ z.delta.value * scale(SumMem) + 
    diag(z.delta.value * scale(SumMem) | participant_id) + 
    (1 | left_image_name) +
    (1 | right_image_name),
  m1_right_distant_ns2 = chose_right ~ z.delta.value * scale(SumMem) + 
    diag(z.delta.value + scale(SumMem) | participant_id) + 
    (1 | left_image_name) +
    (1 | right_image_name),
  m1_right_distant_ns3 = chose_right ~ z.delta.value * scale(SumMem) + 
    (z.delta.value + scale(SumMem) | participant_id) + 
    (1 | left_image_name) +
    (1 | right_image_name),
  m1_right_distant_ns4 = chose_right ~ z.delta.value * scale(SumMem) + 
    (z.delta.value + scale(SumMem) | participant_id) + 
    (0 + z.delta.value:scale(SumMem) | participant_id) +
    (1 | left_image_name) +
    (1 | right_image_name)
  )

model_class <- str_remove(model_name, "_ns\\d+$")

model_df <- switch(model_class,
                   "m1_right_similar" = choice.similar.v,
                   "m1_right_distant" = choice.distant.v,
                   "m2_right_all" = choice.z)

if(update_fit) {
  prev_fit <- readRDS(file.path(fit_dir, paste(paste0(model_name, ".restart", fit_n - 1), 
                                               paste0(dataset, ".rds"), sep = "_")))
  # print(paste(paste0(model_name, ".restart", fit_n - 1), 
  #             paste0(dataset, ".rds"), sep = "_"))
  # print(summary(prev_fit))
  pars <- getME(prev_fit, c("theta", "beta"))
  pars_x <- pars
  pars_x$theta <- pmax(pars$theta, getME(prev_fit, "lower"))
  pars_x$theta <- pars_x$theta + runif(length(pars_x$theta), -jitter, jitter) * abs(pars_x$theta)
  m <- update(prev_fit, start = pars_x, control = strict_tol)
  saveRDS(m, file = file.path(fit_dir, paste(paste0(paste0(model_name, ".restart", fit_n)), 
                                             paste0(dataset, ".rds"), sep = "_")))
  # saveRDS(m, file = file.path(fit_dir, paste(paste0(paste0(model_name, ".restart", fit_n)), 
  #                                            jitter,
  #                                            paste0(dataset, ".rds"), sep = "_")))
} else {
  if((model_name == "m2_right_all") | use_strict_tol) {
    m <- glmer(models[[dataset]][[model_name]], family = binomial,
               data = model_df, control = strict_tol)
  } else {
    m <- glmer(models[[dataset]][[model_name]], family = binomial,
               data = model_df)
  }
  
  if(use_strict_tol) {
    saveRDS(m, file = file.path(fit_dir, paste(paste0(model_name, ".restart1"), 
                                               paste0(dataset, ".rds"), sep = "_")))
  } else {
    saveRDS(m, file = file.path(fit_dir, paste(model_name, paste0(dataset, ".rds"), sep = "_")))
  }
}

