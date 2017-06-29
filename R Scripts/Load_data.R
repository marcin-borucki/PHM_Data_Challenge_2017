###############################################################################
# Data loading & manipulation script for PHM Data Challange 2017
# Author: Marcin Borucki
# Email: Marcin.Borucki@gmail.com & Marcin.Borucki@ge.com
# Creation date: 06/25/2017
# Modification:
# Version: 0.1
###############################################################################


# Sepcifying required libraries & order ----------------------------------------
require(dplyr)
require(plyr)
require(tidyr)
require(ggplot2)
require(plotly)
# install.packages("plotly")

source("./R/Data_challenge_functions.R")
# Loading sample data ----------------------------------------------------------

# unzip("./input/data_challenge_0.zip", overwrite = FALSE , exdir = "./input")

smpl_pred <- read.csv("./input/Sample-Predictions.csv")

# Training data with all positive or "healty" data experiments
training <- read.csv("./input/training.csv",
                    stringsAsFactors = FALSE)

# Test data with data experiments to be labeled by the algorithm
testing <- read.csv("./input/testing.csv",
                    stringsAsFactors = FALSE)

# Max to fault components are possible per train car
fault_codes <- read.csv2("./input/fault_components.csv", 
                         stringsAsFactors = FALSE)


# Stacking training data into one data frame -----------------------------------

# train_data <- training %>%  mutate(k = 1) %>% 
#     adply(1, function(x)  (inner_join(x, load_expetiment(x$ExperimentID,
#                                      dir = "./input/training/",
#                                      names = TRUE) %>%  mutate(k = 1)))
#           ) %>%
#     select(-k)
    
# Stacking training data into one data frame -----------------------------------

# test_data <- testing %>%  mutate(k = 1) %>% 
#     adply(1, function(x)  (inner_join(x, load_expetiment(x$ExperimentID,
#                                                          dir = "./input/testing/",
#                                                          names = TRUE) %>%  mutate(k = 1)))
#     ) %>%
#     select(-k)

# Saving data to RSD files -----------------------------------------------------

# saveRDS(train_data, "./input/train_data.RDS")
# saveRDS(test_data, "./input/test_data.RDS")

# Cleanig... -------------------------------------------------------------------

# unlink("./input/training/*")
# unlink("./input/testing/*")

# Loading previously saved data ------------------------------------------------

readRDS("./input/train_data.RDS")
readRDS("./input/test_data.RDS")

# Data review & testing --------------------------------------------------------

# example read in to review the data
# exp_tr1 <- load_expetiment(101, dir = "./input/training/", names = TRUE)
# exp_tr1_r <- load_expetiment(1, dir = "./input/training/", names = FALSE)

# exp_tst1 <- load_expetiment(1, dir = "./input/testing/")

# check if names the same 
# sum(names(exp_tr1$az_1l_1) != names(exp_tst1))
 
# first_plot <- ggplot(exp_tr1) +
    # geom_line(aes(x = seq_along(az_1l_1), y=az_1l_1, color = 1 )) +
    # geom_line(aes(x = seq_along(az_1l_1), y=az_2l_1, color = 2 )) +
    # geom_line(aes(x = seq_along(az_1l_1), y=az_3l_1, color = 3 )) +
    # geom_line(aes(x = seq_along(az_1l_1), y=az_4l_1, color = 4 )) 

# exp_tr1 %>% mutate(row = row_number()) %>% gather(key = "az_l", "n", az_1l_2,  az_2l_2, az_3l_2, az_4l_2)


# az <- ggplot(exp_tr1 %>% 
#                  mutate(row = row_number())  %>%
#                  gather( "az_l", "n",
#                          az_1l_2,  az_2l_2, az_3l_2, az_4l_2 )) +
#     geom_line(aes(x = row , y=n, colour = az_l , group = az_l)) 

# ggplotly(az)

# az <- ggplot(exp_tr1 %>% 
#                  mutate(row = row_number())  %>%
#                  gather( "az_l", "n",
#                          az_1l_1,  az_1l_2, az_1l_3, az_1l_4, az_1l_5 )) +
#     geom_line(aes(x = row , y=n, colour = az_l , group = az_l)) 
# 
# ggplotly(az)


# exp_tst1 %>%
#     gather() %>% 
#     ggplot(aes(value)) +
#     facet_wrap(~ key, scales = "free") +
#     geom_histogram()