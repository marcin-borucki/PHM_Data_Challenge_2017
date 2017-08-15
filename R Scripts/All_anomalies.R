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

names(gains_test_all)

gains_test_sum <- gains_test_all %>%
    select(1:2, one_of("gain_func", "residuals_gain", "gain.y", "gain.x")) %>%
                  group_by(ExperimentID, p_l, gain_func) %>%
                  summarize(sum_res_gain = sum(residuals_gain),
                            res_gain_mean = mean(residuals_gain),
                            res_gain_sd = sd(residuals_gain),
                            gain_mean_exp = mean(gain.x),
                            q25 = quantile(residuals_gain, probs=0.25),
                            q50 = quantile(residuals_gain, probs=0.5),
                            q75 = quantile(residuals_gain, probs=0.75),
                            eq25 = quantile(gain.x, probs=0.25),
                            eq50 = quantile(gain.x, probs=0.5),
                            eq75 = quantile(gain.x, probs=0.75),
                            gain_sd = sd(gain.y),
                            gain_mean = mean(gain.y),
                            IQR_gp = IQR(gain.y),
                            gq25 = quantile(gain.y, probs=0.25),
                            gq50 = quantile(gain.y, probs=0.5),
                            gq75 = quantile(gain.y, probs=0.75)              
                  ) %>% ungroup() %>% mutate(part = substr(p_l,1,5))


# Secondary : direct residuls approach ---------------------------------------- 
# - instead of gains pure residuals from inital fit
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

# This part is wrong

# outlier_values_azs_1 <- boxplot.stats(sec_res_summ[sec_res_summ$part == "azs_1", ]$q75)$out
# outlier_values_azs_2 <- boxplot.stats(sec_res_summ[sec_res_summ$part == "azs_2", ]$q75)$out
# outlier_values_azs_1 <- data_frame(q75 = outlier_values_azs_1)
# outlier_values_azs_2 <- data_frame(q75 = outlier_values_azs_2)
# outlier_values_azs_1$part <- "azs_1"
# outlier_values_azs_2$part <- "azs_2"
# 
# outlier_values_azs <- rbind(outlier_values_azs_1, outlier_values_azs_2)
# outlier_values_azs$outlier_ <- TRUE
# sec_res_summ <- left_join(sec_res_summ, outlier_values_azs)

# redoing...
head(sec_res_summ)
parts_res_s <- sec_res_summ %>% select(acc) %>% distinct()

box_outl_pq75 <- ddply(parts_res_s, .(acc), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(sec_res_summ[sec_res_summ$acc == df$acc, ]$q75)$out)
})

box_outl_pq25 <- ddply(parts_res_s, .(acc), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(sec_res_summ[sec_res_summ$acc == df$acc, ]$q25)$out)
})

box_outl_pq75$is_outlier_pq75 <- TRUE
box_outl_pq25$is_outlier_pq25 <- TRUE

sec_res_summ_pq75 <- left_join(sec_res_summ, box_outl_pq75,
                          by = c("acc" = "acc", "q75" = "outlier_val"),
                          suffix = c("", ".q75"))

sec_res_summ_pq25 <- left_join(sec_res_summ, box_outl_pq25,
                             by = c("acc" = "acc", "q25" = "outlier_val"),
                             suffix = c("", ".q25"))

sec_res_summ_o <- left_join(sec_res_summ_pq75, sec_res_summ_pq25)
                               
# names(sec_res_summ_o)

anomalies_trains_mod_sec_pq75 <- sec_res_summ_o %>% filter(is_outlier_pq75) %>%
    select(ExperimentID, acc, Payload, Speed, Track, part) %>% 
    group_by(ExperimentID, part,Payload, Speed, Track) %>% 
    summarise(issue = paste(acc , collapse="+"),
                                               iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
                                         ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()

anomalies_trains_mod_sec_pq25 <- sec_res_summ_o %>% filter(is_outlier_pq25) %>%
    select(ExperimentID, acc, Payload, Speed, Track, part) %>% 
    group_by(ExperimentID, part,Payload, Speed, Track) %>% 
    summarise(issue = paste(acc , collapse="+"),
              iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
    ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()


anomalies_trains_mod_sec_pq75 <- left_join(data_frame(ExperimentID = 1:200),
                                        anomalies_trains_mod_sec_pq75)

anomalies_trains_mod_sec_p <- left_join(anomalies_trains_mod_sec_pq75,
                                        anomalies_trains_mod_sec_pq25,
                               by = c("ExperimentID" = "ExperimentID"),
                               suffix = c("_q75_rs", "_q25_rs"))


# anomalies_trains_mod_sec

anomalies_trains_mod_sec_p <- left_join(data_frame(ExperimentID = 1:200), anomalies_trains_mod_sec_p)
# Cool script 
# sec_res_summ  %>%  filter(part == "azs_1" ) %>%
#     outlierKD(res_gain_sd)

# sec_res_summ  %>%  filter(part == "azs_1" ) %>%
#     outlierKD(q75)

# Scondary lerf & right gain estimation ----------------------------------------


azps_pairs <-    rbind(
    cbind(
        test_fit %>%
            slice(grep("^azp_1l", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        test_fit %>%
            slice(grep("^azp_2l", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        var = "azps_1l"
    ),
    cbind(
        test_fit %>%
            slice(grep("^azp_3l", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        test_fit %>%
            slice(grep("^azp_4l", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        var = "azps_2l"
    ),
    cbind(
        test_fit %>%
            slice(grep("^azp_1r", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        test_fit %>%
            slice(grep("^azp_2r", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        var ="azps_1r"
    ),
    cbind(
        test_fit %>%
            slice(grep("^azp_3r", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        test_fit %>%
            slice(grep("^azp_4r", acc, invert = FALSE)) %>%
            select(acc) %>% distinct(),
        var = "azps_2r"
    )
)

names(azps_pairs) <-c("val1", "val2", "var")
require(stringr)
azps_pairs$var <- 
    paste(azps_pairs$var , str_sub(azps_pairs$val1,-1,-1), sep = "_")

azps_pairs_comg <- ddply(azps_pairs, .(var), .fun = function(df){
    sensor_combine(df_tf = test,
                   # df_t = azps_pairs,
                   par_1 = df$val1,
                   par_2 = df$val2,
                   par_o = df$var,
                   par_var_tf = "acc",
                   val_var_tf = "reading")
}) %>% select(one_of(#"var",
                     "ExperimentID", "Payload", "Speed", "Track",         
               "State", "lp", "off", #"par_val_v.x"    "val_var_v.x"   
               "lp_off", #         "par_val_v.y"    "val_var_v.y"    "sum_side_b"
              "avg_side_bogie" , "par_o")) #

azps_pairs_comg_fit <- ddply(azps_pairs, .(var), .fun = function(df){
    sensor_combine(df_tf = test_fit,
                   # df_t = azps_pairs,
                   par_1 = df$val1,
                   par_2 = df$val2,
                   par_o = df$var,
                   par_var_tf = "acc",
                   val_var_tf = "exptd_reading",
                match_by = c("ExperimentID","lp_off"))
}) %>% select(one_of(#"var",
    "ExperimentID",  #"par_val_v.x"    "val_var_v.x"   
    "lp_off", #         "par_val_v.y"    "val_var_v.y"    "sum_side_b"
    "avg_side_bogie" , "par_o")) 

# names(test_fit)
# names(test) 
names(azps_pairs_comg)[9:10] <- c("reading", "acc")

names(azps_pairs_comg_fit)[3:4] <- c("exptd_reading", "acc")

test_ext <- bind_rows(test, azps_pairs_comg)

test_fit_ext <- bind_rows(test_fit, azps_pairs_comg_fit)



# Priamry to secondary suspesnion gain ---------------------------------------

azs_pairs <-    
    test_ext %>% slice(grep("^azps_", acc, invert = FALSE)) %>% select(acc) %>% distinct()

azs_pairs$sec <- paste0("azs_",
                        str_sub(azs_pairs$acc, 6,6), "_",
                        str_sub(azs_pairs$acc, -1,-1)) 

names(azs_pairs) <- c("p_l", "p_h")


gains_test_azsp_ext <- ddply(.data = azs_pairs , .(p_l) , .fun = function(df){
    gain_calc(par_l = paste(df$p_l),
              par_h = paste(df$p_h),
              df = test_ext,
              val_var = "reading",
              match_by = c("ExperimentID", 
                           "Payload", 
                           "Speed", 
                           "Track", 
                           "State", 
                           "lp", "off",
                           "lp_off")
              )})
# names(test_ext)
# 
# names(gains_test_azsp_ext)

gains_test_fit_azsp_ext <- ddply(.data = azs_pairs , .(p_l) , .fun = function(df){
    gain_calc(par_l = paste(df$p_l),
              par_h = paste(df$p_h),
              df = test_fit_ext)})

# head(gains_test_fit_azsp_ext)
# head(gains_test_azsp_ext)

gains_test_all_sec <- left_join(gains_test_fit_azsp_ext, 
                                gains_test_azsp_ext %>% select(one_of(names(gains_test_azsp_ext)[-1])),
                                by = c("ExperimentID" = "ExperimentID",
                                       "lp_off" = "lp_off",
                                       "gain_func" = "gain_func"))

# head(gains_test_all_sec)
# names(gains_test_all_sec)


gains_test_all_sec <- gains_test_all_sec %>% mutate(residuals_gain = gain.y - gain.x )

gains_test_sum_sec <- gains_test_all_sec %>%
    select(1:2, one_of("gain_func", "residuals_gain")) %>%
    group_by(ExperimentID, p_l, gain_func) %>%
    summarize(sum_res_gain = sum(residuals_gain),
              res_gain_sd = sd(residuals_gain),
              q25 = quantile(residuals_gain, probs=0.25),
              q50 = quantile(residuals_gain, probs=0.5),
              q75 = quantile(residuals_gain, probs=0.75)              
    ) %>% ungroup() %>% mutate(part = substr(p_l,1,7))

# head(gains_test_sum_sec)
parts <- gains_test_sum_sec %>% select(p_l) %>% distinct()

box_outl_grq75 <- ddply(parts, .(p_l), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(gains_test_sum_sec[gains_test_sum_sec$p_l == df$p_l, ]$q75)$out)
})

box_outl_grq25 <- ddply(parts, .(p_l), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(gains_test_sum_sec[gains_test_sum_sec$p_l == df$p_l, ]$q25)$out)
})

box_outl_grq75$is_outlier_grq75 <- TRUE

box_outl_grq25$is_outlier_grq25 <- TRUE

# sec_gain_res_summ_o <- left_join(gains_test_sum_sec, box_outl)

gains_test_sum_sec_ogrq75 <- left_join(gains_test_sum_sec, box_outl_grq75,
                            by = c("p_l" = "p_l", "q75" = "outlier_val"))

gains_test_sum_sec_ogrq25 <- left_join(gains_test_sum_sec, box_outl_grq25,
                                       by = c("p_l" = "p_l", "q25" = "outlier_val"))

# gains_test_sum_sec_og <- left_join(gains_test_sum_sec_ogrq75, gains_test_sum_sec_ogrq25)

anomalies_trains_mod_gain_sec_q75 <- gains_test_sum_sec_ogrq75 %>% filter(is_outlier_grq75) %>%
    select(ExperimentID, p_l, part) %>% 
    group_by(ExperimentID, part) %>% 
    summarise(issue = paste(p_l , collapse="+"),
              iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
                                         ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()


anomalies_trains_mod_gain_sec_q25 <- gains_test_sum_sec_ogrq25 %>% filter(is_outlier_grq25) %>%
    select(ExperimentID, p_l, part) %>% 
    group_by(ExperimentID, part) %>% 
    summarise(issue = paste(p_l , collapse="+"),
              iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
    ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()


anomalies_trains_mod_gain_sec_q75 <- left_join(data_frame(ExperimentID = 1:200), 
                                             anomalies_trains_mod_gain_sec_q75)

anomalies_trains_mod_gain_sec_q <- left_join(anomalies_trains_mod_gain_sec_q75, 
                                             anomalies_trains_mod_gain_sec_q25,
                                        by = c("ExperimentID" = "ExperimentID"),
                                        suffix = c("_q75_gs", "_q25_gs"))


# anomalies_trains_mod_sec

anomalies_trains_mod_gain_sec_q <- left_join(data_frame(ExperimentID = 1:200), 
                                             anomalies_trains_mod_gain_sec_q)



# Identifying anomalies ------------------------------------------------------
# For now primary ------------------------------------------------------------
primary_anomalies <- gains_test_sum  %>%
    filter( q50 > 0.15 | q50 < -0.15 | q75 > 0.4 | q75 < -0.4) 


# Modified summary Primary and naive secondary --------------------------------
# 13/08/2017
anomalies_trains_mod <- primary_anomalies %>% select(ExperimentID, part, p_l, gain_func) %>% 
    mutate(State = substr(gain_func,2,6)) %>% select(-gain_func) %>%
    group_by(ExperimentID, part) %>% summarise(issue = paste(p_l , collapse="+"),
                                               iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
                                         ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()


anomalies_trains_mod <- left_join(data_frame(ExperimentID = 1:200),
                                      anomalies_trains_mod)

# Outliers in primary -------------------------------------------------------


parts_p <- gains_test_sum %>% select(p_l) %>% distinct()

box_outl_pgq75 <- ddply(parts_p, .(p_l), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(gains_test_sum[gains_test_sum$p_l == df$p_l, ]$q75)$out)
})

box_outl_pgq25 <- ddply(parts_p, .(p_l), .fun = function(df){
    data_frame(outlier_val = boxplot.stats(gains_test_sum[gains_test_sum$p_l == df$p_l, ]$q25)$out)
})

box_outl_pgq75$is_outlier_pgq75 <- TRUE

box_outl_pgq25$is_outlier_pgq25 <- TRUE

# sec_gain_res_summ_o <- left_join(gains_test_sum_sec, box_outl)

gains_test_sum_pri_pgq75 <- left_join(gains_test_sum, box_outl_pgq75,
                                       by = c("p_l" = "p_l", "q75" = "outlier_val"))

gains_test_sum_pri_pgq25 <- left_join(gains_test_sum, box_outl_pgq25,
                                       by = c("p_l" = "p_l", "q25" = "outlier_val"))

# gains_test_sum_sec_og <- left_join(gains_test_sum_sec_ogrq75, gains_test_sum_sec_ogrq25)

anomalies_gain_pri_q75 <- gains_test_sum_pri_pgq75 %>% filter(is_outlier_pgq75) %>%
    select(ExperimentID, p_l, part) %>% 
    group_by(ExperimentID, part) %>% 
    summarise(issue = paste(p_l , collapse="+"),
              iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
    ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()


anomalies_gain_pri_q25 <- gains_test_sum_pri_pgq25 %>% filter(is_outlier_pgq25) %>%
    select(ExperimentID, p_l, part) %>% 
    group_by(ExperimentID, part) %>% 
    summarise(issue = paste(p_l , collapse="+"),
              iCount = n())%>%
    ungroup() %>%
    group_by(ExperimentID) %>% summarise(parts = paste(part , collapse="+"),
                                         issues = paste(issue , collapse="+"),
                                         iCounts = paste(iCount , collapse="+")
                                         # iCounts_n = sum(iCount),
                                         # row = n()
    ) %>%
    select(ExperimentID,parts,issues, iCounts) %>% ungroup()

names(anomalies_gain_pri_q75)

anomalies_gain_pri_q75 <- left_join(data_frame(ExperimentID = 1:200),
                                  anomalies_gain_pri_q75)

anomalies_gain_pri_q <- left_join(anomalies_gain_pri_q75, anomalies_gain_pri_q25,
                                             by = c("ExperimentID" = "ExperimentID"),
                                             suffix = c("_pq75", "_pq25"))


# collect all anomalies
names(anomalies_trains_all)
names(anomalies_trains_mod)
names(anomalies_trains_mod_sec_p)

names(anomalies_trains_all_pq)

names(anomalies_gain_pri_q)



anomalies_trains_all_sr <- left_join(anomalies_trains_mod, 
                                  anomalies_trains_mod_sec_p, 
                                 by = c("ExperimentID" = "ExperimentID"),
                                 suffix = c("_p", "_sr")) 

anomalies_trains_all_pq <- left_join(anomalies_trains_mod, 
                                     anomalies_gain_pri_q, 
                                     by = c("ExperimentID" = "ExperimentID"),
                                     suffix = c("_p", "_pq")) 

anomalies_trains_all_g <- left_join(anomalies_trains_mod , 
                                  anomalies_trains_mod_gain_sec_q, 
                                  by = c("ExperimentID" = "ExperimentID"),
                                  suffix = c("_p", "_sg"))



names(anomalies_trains_all)
# job_20170814_3
anomalies_all_apq <-  left_join(anomalies_trains_all_pq ,anomalies_trains_all_sr , 
                                
                                by = c("ExperimentID" = "ExperimentID",
                                       "parts" = "parts",
                                       "issues" = "issues",
                                       "iCounts" = "iCounts"  ),
                                suffix = c("_sr", "_pq")) %>% ungroup()



anomalies_all_ag <-  left_join(anomalies_all_apq , 
                            anomalies_trains_all_g, 
          by = c("ExperimentID" = "ExperimentID",
                 "parts" = "parts",
                 "issues" = "issues",
                 "iCounts" = "iCounts"  ),
          suffix = c("_sr", "_sg"))


names(anomalies_all_ag)
# write.csv(anomalies_trains_all, file = "./input/anomalies_trains_20170813.csv")

 # write.csv(anomalies_trains_all, file = "./input/anomalies_trains_20170814.csv")
write.csv(anomalies_all_ag, file = "./input/anomalies_trains_20170815_all.csv")
