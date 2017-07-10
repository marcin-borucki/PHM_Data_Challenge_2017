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

# Filtering 85th percentile per track. -----------------------------------------
train_data_filter <- train_data %>% select( Track, matches("az_.")) %>% 
    group_by(Track) %>% 
    summarise_each_( funs(quantile(., c(.85))), matches("az_*")) %>%
    # summarise_each_( funs(median), matches("az_*")) %>%
    tidyr::gather(filter_var, cut_off, matches("az_.") )

train_data_f_low <-  train_data %>%  tidyr::gather(filter_var, value, matches("az_.") ) %>% #
    left_join(train_data_filter ) %>% filter( value <= cut_off)  %>%  
    select(-cut_off) %>% tidyr::spread(filter_var, value)

# Running fits on training data and calculating stats --------------------------
res3_zp_train <- run_fits(model_df = models, data_df = train_data_f_low)

zp_train_set_models <- res3_zp_train %>%
    group_by( Track, modeled_element, freq) %>%
    dplyr::summarise(max_c2 = max(c2), min_c2 = min(c2), mean_c2 = mean(c2), sd_c2 = sd(c2) )
# Applying same filtering to test set (filtering based on trainig) -------------

# Critical comment: Median (50th percentile) sucks! Gives us some empty sets!
# 85th percentile seems to work ok.
#
test_data_f_low <-  test_data %>%  tidyr::gather(filter_var, value, matches("az_.") ) %>% #
    left_join(train_data_filter ) %>% filter( value <= cut_off)  %>%  
    select(-cut_off) %>% tidyr::spread(filter_var, value)



# Running fits on test filtered data -------------------------------------------
res3_zp_test <- run_fits(model_df = models, data_df = test_data_f_low)

res3_zp_test_clasify <- left_join(res3_zp_test,zp_train_set_models) %>%

    mutate(c2_above_m = ifelse(c2 > (max_c2 + sd_c2), 1, 0),
           c2_below_m = ifelse(c2 < (min_c2 - sd_c2/3), 1, 0),
           residual_min =  c2 - min_c2, 
           residual_max =  max_c2 - c2 ) %>%
    mutate(c2_ = paste( freq, sep="_"),
           c2_above_m_ = paste( freq, sep="_"),
           c2_below_m_ = paste( freq, sep="_"),
           max_c2_ = paste( freq, sep="_"),
           min_c2_ = paste( freq, sep="_"),
           mean_c2_ = paste( freq, sep="_"),
           sd_c2_ = paste( freq, sep="_"),
           residual_min_ = paste( freq, sep="_"),
           residual_max_ = paste( freq, sep="_")) %>%
    select(-x_name1, -x_name2, -lm_cmd, -model, -freq, -c1) %>%
    tidyr::spread(c2_, c2, sep="") %>%
    tidyr::spread(c2_above_m_, c2_above_m, sep="") %>%
    tidyr::spread(c2_below_m_, c2_below_m, sep="") %>%
    tidyr::spread(max_c2_, max_c2, sep="") %>%
    tidyr::spread(min_c2_, min_c2, sep="") %>%
    tidyr::spread(mean_c2_, mean_c2, sep="") %>%
    tidyr::spread(sd_c2_, sd_c2, sep="") %>%
    tidyr::spread(residual_min_, residual_min, sep="") %>%
    tidyr::spread(residual_max_, residual_max, sep="") %>%
    group_by(modeled_element, ExperimentID) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    ungroup() 

res3_zp_test_clasify$c2_above_m <-
    res3_zp_test_clasify %>% select(starts_with("c2_above_m_")) %>% rowSums()
res3_zp_test_clasify$c2_below_m <-
    res3_zp_test_clasify %>% select(starts_with("c2_below_m_")) %>% rowSums()
res3_zp_test_clasify$residual_min <-
    res3_zp_test_clasify %>% select(starts_with("residual_min_")) %>% rowSums()
res3_zp_test_clasify$residual_max <-
    res3_zp_test_clasify %>% select(starts_with("residual_max_")) %>% rowSums()

res3_zp_test_clasify <-
res3_zp_test_clasify  %>% select(-starts_with("c2_above_m_")) %>%
    select(-starts_with("c2_below_m_")) %>% 
    select(-starts_with("residual_min_")) %>% 
    select(-starts_with("residual_max_")) %>% mutate(failure_zp = ifelse(c2_above_m >2, TRUE, ifelse(c2_below_m > 4, TRUE, FALSE))) %>% ungroup()


# Checking for to much marks
res3_zp_test_clasify %>%
    filter(failure_zp == TRUE) %>% select(ExperimentID,residual_min, residual_max ) %>% ungroup() %>% 
    group_by(ExperimentID) %>% summarise(count = n()) %>% ungroup() %>% filter(count > 2)


# NEED TO SELECT the ones with most negative residules!
res3_zp_test_clasify_selection <- res3_zp_test_clasify %>%
    filter(failure_zp == TRUE) %>%
    mutate(residual = ifelse(c2_above_m < c2_below_m ,residual_min, residual_max)) %>%
    group_by(ExperimentID ) %>% top_n(- 2 ) %>% ungroup()


# Formating output -------------------------------------------------------------
primary_sus_clasification <-
res3_zp_test_clasify_selection %>% select(ExperimentID, c2_above_m,
                                          c2_below_m, modeled_element ) %>%
    mutate(State = ifelse(c2_above_m < c2_below_m,
                          sub("^.", "c" , modeled_element),
                          sub("^.", "d" , modeled_element))) %>%
    select(ExperimentID,State) %>%
    ddply(.(ExperimentID), summarize, State=paste(State, collapse="+")) %>%
    
    right_join(data.frame(ExperimentID = 1:200)) %>%
    mutate(State = ifelse(is.na(State), "healthy",State)) 


table(primary_sus_clasification$State)
write.csv(primary_sus_clasification,
          "./output/primary_sus_clasification_20170711_MB.csv",
            row.names = FALSE , quote = FALSE)

# checking for oveall count ----------------------------------------------------
res3_zp_test_clasify %>% 
    filter(failure_zp == TRUE)%>% ungroup() %>% summarise(count = n())

res3_zp_test_clasify %>%
    filter(failure_zp == TRUE) %>% select(modeled_element,  
                                          ExperimentID) %>% table()

# Overall some example plots ---------------------------------------------------
t <- table(res3_zp_test_clasify$modeled_element, res3_zp_test_clasify$c2_below_m)
sum(t[,6])


tt <-   res3_zp_train %>%
    group_by(Track, modeled_element, freq) %>%
    dplyr::summarise(mean_c2 = mean(c2), sd_c2 = sd(c2) )


p <- bind_rows(mutate(res3_zp_train, origin = "train"), 
               mutate(res3_zp_test %>% filter(ExperimentID == 177),
                      origin = "test") ) %>%
    filter(modeled_element %in% c("azp_1l", "azp_2l", "azp_2r", "azp_1r"),
           Track == 2) %>%
    ggplot(., aes(x = freq + as.numeric(as.factor(interaction(modeled_element)))/10,
                  y = c2,
                  color = interaction(origin,modeled_element),
                  group = interaction(origin,modeled_element)
    )) + 
    geom_line(alpha = 0.5) 
show(p)

mutate(res3_zp_test %>% filter(ExperimentID == 3),
       origin = "res3_zp_test")  %>%
    filter(modeled_element %in% c("azp_1l", "azp_1r") )
