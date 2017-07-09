###############################################################################
# Code to hold simple band model for ZP elements of PHM Data Challange 2017
# Author: Marcin Borucki
# Email: Marcin.Borucki@gmail.com or Marcin.Borucki@ge.com
# Creation date: 07/09/2017
# Modification:
# Version: 0.1
###############################################################################

train_data <- readRDS("./input/train_data.RDS")

test_data  <- readRDS("./input/test_data.RDS")

#functions :
source("./R/Data_challenge_functions.R")


#linear models:
source("./R Scripts/models_eq3_azp.R")

res3_zp_train <- run_fits(model_df = models, data_df = data_train)

zp_train_set_models <- res3_zp_train %>%
    group_by( Track, modeled_element, freq) %>%
    dplyr::summarise(max_c2 = max(c2), min_c2 = min(c2), max_c2 = mean(c2), sd_c2 = sd(c2) )


res3_zp_test <- run_fits(model_df = models, data_df = test_data)
names(res3_zp_test_clasify)

res3_zp_test_clasify <- left_join(res3_zp_test,zp_train_set_models) %>%

    mutate(c2_above_m = ifelse(c2 > max_c2, 1, 0),
           c2_below_m = ifelse(c2 < min_c2, 1, 0)) %>%
    # mutate(c2_ = paste(modeled_element, freq, sep="_"),
    #        c2_above_m_ = paste(modeled_element, freq, sep="_"),
    #        c2_below_m_ = paste(modeled_element, freq, sep="_"),
    #        max_c2_ = paste(modeled_element, freq, sep="_"),
    #        min_c2_ = paste(modeled_element, freq, sep="_"),
    #        mean_c2_ = paste(modeled_element, freq, sep="_"),
    #        sd_c2_ = paste(modeled_element, freq, sep="_")) %>%
    # leave only freq
    mutate(c2_ = paste( freq, sep="_"),
           c2_above_m_ = paste( freq, sep="_"),
           c2_below_m_ = paste( freq, sep="_"),
           max_c2_ = paste( freq, sep="_"),
           min_c2_ = paste( freq, sep="_"),
           mean_c2_ = paste( freq, sep="_"),
           sd_c2_ = paste( freq, sep="_")) %>%
    select(-x_name1, -x_name2, -lm_cmd, -model, -freq, -c1) %>%
    tidyr::spread(c2_, c2, sep="") %>%
    tidyr::spread(c2_above_m_, c2_above_m, sep="") %>%
    tidyr::spread(c2_below_m_, c2_below_m, sep="") %>%
    tidyr::spread(max_c2_, max_c2, sep="") %>%
    tidyr::spread(min_c2_, min_c2, sep="") %>%
    tidyr::spread(mean_c2_, mean_c2, sep="") %>%
    tidyr::spread(sd_c2_, sd_c2, sep="") %>%
    group_by(modeled_element, ExperimentID) %>% 
    summarise_each(funs(max(., na.rm = TRUE)))
    
    

