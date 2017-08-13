# ###############################################################################
# Script for determining ALL anomalous behavior - primary & secondary
#       for PHM Data Challange 2017
# Author: Marcin Borucki
# Email: Marcin.Borucki@gmail.com & Marcin.Borucki@ge.com
# Creation date: 08/12/2017
# Modification:
# Version: 0.1
###############################################################################

# Priamry suspesnion
az_pairs <-    cbind(
    test_fit %>% slice(grep("^az_", acc, invert = FALSE)) %>% select(acc) %>% distinct(),
    test_fit %>% slice(grep("^azp_", acc, invert = FALSE)) %>% select(acc) %>% distinct()
)
names(az_pairs) <- c("p_l", "p_h")

gains_test_fit <- ddply(.data = az_pairs , .(p_l) , .fun = function(df){
    gain_calc(par_l = paste(df$p_l) , par_h = paste(df$p_h))})

gains_test <- ddply(.data = az_pairs , .(p_l) , .fun = function(df){
    gain_calc(df = test, par_l = paste(df$p_l) , par_h = paste(df$p_h),
              val_var = "reading")})

gains_test_all <- left_join(gains_test_fit, 
                            gains_test %>% select(one_of(names(gains_test_fit)[-1])),
                            by = c("ExperimentID" = "ExperimentID",
                                   "lp_off" = "lp_off",
                                   "gain_func" = "gain_func"))

gains_test_all <- gains_test_all %>% mutate(residuals_gain = gain.y - gain.x )

gains_test_sum <- gains_test_all %>%
    select(1:2, one_of("gain_func", "residuals_gain")) %>%
                  group_by(ExperimentID, p_l, gain_func) %>%
                  summarize(sum_res_gain = sum(residuals_gain),
                            res_gain_sd = sd(residuals_gain),
                            q25 = quantile(residuals_gain, probs=0.25),
                            q50 = quantile(residuals_gain, probs=0.5),
                            q75 = quantile(residuals_gain, probs=0.75)              
                  ) %>% ungroup() %>% mutate(part = substr(p_l,1,5))


# Secondary : sligtly different approach - instead of gains pure residuals from inital fit
# 

azs_par <- test_fit %>% slice(grep("^azs_", acc, invert = FALSE)) %>% select(acc) %>% distinct()

sec_res <- ddply(.data = azs_par , .(acc) , .fun = function(df){
    sensor_res(par = df$acc)})


sec_res_summ <- sec_res %>%
    select(1:2, one_of("Payload", "Speed", "Track", "res_t_tf")) %>%
    group_by(ExperimentID, acc, Payload, Speed, Track) %>%
    summarize(sum_res_gain = sum(res_t_tf),
              res_gain_sd = sd(res_t_tf),
              q25 = quantile(res_t_tf, probs=0.25),
              q50 = quantile(res_t_tf, probs=0.5),
              q75 = quantile(res_t_tf, probs=0.75)
    ) %>% ungroup() %>% mutate(part = substr(acc,1,5))

outlier_values_azs_1 <- boxplot.stats(sec_res_summ[sec_res_summ$part == "azs_1", ]$q75)$out
outlier_values_azs_2 <- boxplot.stats(sec_res_summ[sec_res_summ$part == "azs_2", ]$q75)$out
outlier_values_azs_1 <- data_frame(q75 = outlier_values_azs_1)
outlier_values_azs_2 <- data_frame(q75 = outlier_values_azs_2)
outlier_values_azs_1$part <- "azs_1"
outlier_values_azs_2$part <- "azs_2"

outlier_values_azs <- rbind(outlier_values_azs_1, outlier_values_azs_2)
outlier_values_azs$outlier_ <- TRUE
sec_res_summ <- left_join(sec_res_summ, outlier_values_azs)


anomalies_trains_mod_sec <- sec_res_summ %>% filter(outlier_75) %>%
    select(ExperimentID, acc, Payload, Speed, Track, part) %>% 
    group_by(ExperimentID, part,Payload, Speed, Track) %>% 
    summarise(issue = paste(acc , collapse="+"),
                                               iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+"),
                                         iCounts_n = sum(iCount),
                                         row = n()) %>%
    select(ExperimentID,parts,issues, iCounts, iCounts_n, row) %>% ungroup()

anomalies_trains_mod_sec

anomalies_trains_mod_sec <- left_join(data_frame(ExperimentID = 1:200), anomalies_trains_mod_sec)
# Cool script 
# sec_res_summ  %>%  filter(part == "azs_1" ) %>%
#     outlierKD(res_gain_sd)

# sec_res_summ  %>%  filter(part == "azs_1" ) %>%
#     outlierKD(q75)


# Identifying anomalies 
# For now primary
primary_anomalies <- gains_test_sum  %>%
    filter( q50 > 0.15 | q50 < -0.15 | q75 > 0.4 | q75 < -0.4) 


# Modified summary 
anomalies_trains_mod <- primary_anomalies %>% select(ExperimentID, part, p_l, gain_func) %>% 
    mutate(State = substr(gain_func,2,6)) %>% select(-gain_func) %>%
    group_by(ExperimentID, part) %>% summarise(issue = paste(p_l , collapse="+"),
                                               iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+"),
                                         iCounts_n = sum(iCount),
                                         row = n()) %>%
    select(ExperimentID,parts,issues, iCounts, iCounts_n, row) %>% ungroup()

# anomalies_trains_mod_det <- primary_anomalies %>% select(ExperimentID, part, p_l, gain_func) %>% 
#     mutate(State = substr(gain_func,2,6)) %>% select(-gain_func) %>%
#     group_by(ExperimentID, part) %>% summarise(issue = paste(p_l , collapse="+"),
#                                                iCount = n())

anomalies_trains_mod <- left_join(data_frame(ExperimentID = 1:200),
                                      anomalies_trains_mod)
# write.csv(anomalies_trains_mod_det, file = "./input/anomalies_trains_20170811.csv")


anomalies_trains_all <- left_join(anomalies_trains_mod, 
                                 anomalies_trains_mod_sec, 
                                 by = c("ExperimentID" = "ExperimentID"),
                                 suffix = c(".x", ".y"))

write.csv(anomalies_trains_all, file = "./input/anomalies_trains_20170813.csv")

