###############################################################################
# Code to hold simple band model for ZP elements of PHM Data Challange 2017
# Fit all apmplitude coef on Test data 
# Fit aplitude coef for Offseted and sub setted Train data 
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




# Leverage offset --------------------------------------------------------------
# from Track_offset_train_test.R code
Test_amplitude_coef <- run_fits(model_df = models, data_df = test_data)

head(Test_amplitude_coef)

test_offsets <- Test_amplitude_coef %>% select(ExperimentID, Track) %>% distinct() %>%
    left_join(test_offsets_az[,1:2], by = c("ExperimentID" = "Test_ExpID"))

# Slecting distincts offsets and trakc combinations
dist_test_offsets <- test_offsets[,2:3] %>% distinct()

train_data <- train_data %>% group_by(ExperimentID) %>% mutate(timeID = row_number()) 

# This coefficients dont seem to be moving us forward...
do
Train_ampl_coef_offset <- ddply(
    dist_test_offsets,
    .(Track, offset),
    .fun = function(df) {
        run_fits(
            model_df = models,
            data_df = train_data %>%
                filter(Track == df$Track) %>%
                slice(df$offset:(df$offset + 49))
        )
    },
    .progress = progress_text(style = 3)
)


save