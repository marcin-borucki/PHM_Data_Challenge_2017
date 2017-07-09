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
    mutate(c2_above_m = ifelse(c2 > max_c2, TRUE, FALSE),
           c2_below_m = ifelse(c2 < min_c2, TRUE, FALSE)) %>%
    unite(temp, modeled_element, freq,ExperimentID) %>%
    select(-c1, -x_name1, -x_name2, -max_c2 ,-max_c2) %>%
    tidyr::spread(temp,c2)
