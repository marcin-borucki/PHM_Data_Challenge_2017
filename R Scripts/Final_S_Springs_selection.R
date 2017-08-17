head(gains_test_all_sec)
#  Secondary spring detection -------------------------------------------------
failed_czs <-
    gains_test_all_sec %>% #mutate(part_p = substr(p_l, 1, 17)) %>%
    # filter(( residuals_gain > 0.25 & substr(p_l, 6, 6) == 1) |
    #            (residuals_gain > 0.19 & substr(p_l, 6, 6) == 2)) %>%
    # filter(( residuals_gain > 0.5 & substr(p_l, 6, 6) == 1) |
    #                       (residuals_gain > 0.4 & substr(p_l, 6, 6) == 2) ) %>%
    filter(( residuals_gain**2 > 0.19 & substr(p_l, 6, 6) == 1) |
               (residuals_gain**2 > 0.14 & substr(p_l, 6, 6) == 2) ) %>%
   filter(str_sub(p_l,-1,-1) < 3 )%>%
    group_by(ExperimentID, p_l) %>%
    summarise(count_over = n()) %>%
    # filter(count_over > 20 )%>% ungroup() %>%
    filter(count_over > 10 )%>% ungroup() %>%
    mutate(part_pp = substr(p_l, 1, 6))%>%
    select(-count_over, -p_l) %>% distinct() %>%
    mutate(status_p = paste0("czs_", str_sub(part_pp,-1,-1)))%>%
    select( -part_pp) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))

# Alternatywna (WYDAJE SIĘ DECYDOWANIE LEPSZA OD TEGO CO POWYŻEJ)
View(failed_czs_m)
failed_czs_m <-
    gains_test_sum_sec_m %>% 
    filter((gain_mean - gain_mean_exp) > 0.18 ) %>%
    filter((gq50 - eq50) > 0.18  ) %>%
    # filter((gq50 > 0.9 & substr(p_l, 6, 6) == 1) |
    #            (gq50 > 0.9 & substr(p_l, 6, 6) == 2)) %>%
    filter(str_sub(p_l,-1,-1) < 2 )%>%
    mutate(part_pp = substr(p_l, 1, 6)) %>%
    # mutate(part_pp = s ubstr(p_l, 1, 7)) %>%
    group_by(ExperimentID, part_pp) %>%
    # summarise(count_over = n(), diff_med = sum(gq50-eq50)-min(gq50-eq50)) %>%
    
    summarise(count_over = n(), 
              diff_mean = sum((gain_mean - gain_mean_exp))-min((gain_mean - gain_mean_exp)),
              diff_med = sum(gq50-eq50)-min(gq50-eq50)
              )%>%
    # filter(count_over > 20 )%>% ungroup() %>%
    filter(count_over > 1 )%>% ungroup() %>%
    # filter(count_over > 1 &( diff_med < 0.4 | diff_mean < 0.4 ))%>%
    # ungroup()

    # mutate(part_pp = substr(p_l, 1, 6))%>%
    # select(-count_over, -p_l) %>% distinct() %>%
    select(-count_over, -diff_med, -diff_mean) %>% distinct() %>%
    mutate(status_p = paste0("czs_", str_sub(part_pp,-1,-1)))%>%
    select( -part_pp) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))


failed_czs_a <- left_join(data_frame(ExperimentID = 1:200), 
                                             failed_czs)
View(failed_czs_1)
# write.csv(failed_czs, file = "./input/czs_det20170814.csv")

# write.csv(failed_czs_a, file = "./input/czs_det20170814_3.csv")
# wersja 3 wydaje się rządzić
# porównania v2 i v3 są owocne
# look next: 2, 3, 117 , 198
# porównania v1 i v3 są owocne
# look next: 10, 82, 84, 88, 125, 134, 
# 142, 151 153 192

# Other interesting: 22

names(gains_test_all)
#  Secondary dumpner? detection -------------------------------------------------
# failed_dzs_det <-

View(failed_dzs_mp)
failed_dzs_m <-
    gains_test_sum_sec_m %>% 
    # filter((gain_mean - gain_mean_exp) < -0.1 ) %>%
     filter((gq50 - eq50) < -0.1 ) %>%
    # filter(str_sub(p_l,-1,-1) < 2 )%>%
    mutate(part_pp = substr(p_l, 1, 7)) %>%
    group_by(ExperimentID, part_pp) %>%
    # summarise(count_over = n(), diff_med = sum(gq50-eq50)-min(gq50-eq50)) %>%
    
    summarise(count_over = n(), 
              diff_med = sum((gain_mean - gain_mean_exp))-min((gain_mean - gain_mean_exp))) %>%
    # filter(count_over > 20 )%>% ungroup() %>%
    filter(count_over > 0)%>% ungroup()


failed_dzs_mp <-
    gains_test_sum %>%
    filter(
        # (gain_mean - gain_mean_exp)  > 0.06,
        #    (gain_mean - gain_mean_exp)  < 0.75,
           IQR_gp > 0.35
           # ,
           # (gq50 - eq50)  > 0.05
    ) %>%
    # filter(str_sub(p_l,-1,-1) > 1 )%>%
    group_by(ExperimentID, part) %>%
    summarise(count_over = n(), sum_res_mean = sum((gain_mean - gain_mean_exp)) )%>%
    filter(count_over > 2) %>%
    ungroup() %>% 
    mutate(status_p = paste0("dzp_", str_sub(part,-2,-1)))%>%
    select( -part) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))


View(health_res)
health_res <- read.csv(file = "./output/Most likely healthy restrictive.csv", 
                       stringsAsFactors = FALSE)

health_general <- read.csv(file = "./output/Most likely healthy_general.csv", 
                           stringsAsFactors = FALSE)

View(failed_dzs_det)

failed_dzs_det <-
    gains_test_sum %>%
    filter(
        # (gain_mean - gain_mean_exp)  > 0.075,
           # (gq50 - eq50)  < -0.1
           (gain_mean - gain_mean_exp)  < -0.05
           # IQR_gp > 0.35
    ) %>%
    filter(str_sub(p_l,-1,-1) < 4 )%>%
    group_by(ExperimentID, part) %>%
    summarise(count_over = n())%>%
    filter(count_over > 2) %>%
    ungroup() %>%
    mutate(status_p = paste0("dzs_", str_sub(part,-2,-1)))%>%
    select( -part) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))


failed_dzs <-
    gains_test_all_sec %>% #mutate(part_p = substr(p_l, 1, 17)) %>%
     filter(between( residuals_gain**2 , 0.015 , 0.95)  ) %>%
    # filter(residuals_gain > 0.15 ) %>%
    filter(str_sub(p_l,-1,-1) < 3 )%>%    
    group_by(ExperimentID, p_l, Track) %>%
    summarise(count_over = n())
%>%
    # filter(count_over > 15) %>%
    # filter(count_over > 10) %>% #det
    ungroup() %>%
    mutate(part_pp = substr(p_l, 1, 7), pll = paste0("f", str_sub(p_l, -2, -1)))%>%
    select(-p_l)%>% 
    spread( pll, count_over)%>%
    filter(is.na(f_2)) %>% 
    mutate(status_p = paste0("dzs_", str_sub(part_pp,-2,-1)))%>%
    select( -part_pp) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))


#  Primary detection -------------------------------------------------
 View(failed_dzp)
# View(gains_test_sum)
# failed_dzp_det <-
failed_dzp <-
    gains_test_sum %>%
    filter((gain_mean - gain_mean_exp)  > 0.06,
           (gain_mean - gain_mean_exp)  < 0.75,
           IQR_gp < 0.3,
           (gq50 - eq50)  > 0.05
           ) %>%
    filter(str_sub(p_l,-1,-1) > 1 )%>%
    group_by(ExperimentID, part) %>%
    summarise(count_over = n(), sum_res_mean = sum((gain_mean - gain_mean_exp)) )%>%
    filter(count_over > 2) %>%
      ungroup() %>% 
    mutate(status_p = paste0("dzp_", str_sub(part,-2,-1)))%>%
    select( -part) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))

    # spread( part, count_over)

# Primary Spring attempt....

View(failed_czp)
failed_czp <-
    gains_test_sum %>%
    filter(
             (gain_mean - gain_mean_exp)  > 0.06,
             
           # (gq50 - eq50)  > 0.05 
           # (gain_mean - gain_mean_exp)  < 0.75,
            IQR_gp > 0.3
    ) %>%
    filter(str_sub(p_l,-1,-1) < 5 )%>%
    group_by(ExperimentID, part) %>%
    summarise(count_over = n())%>%
    filter(count_over > 1) %>%
    ungroup() %>%
    mutate(status_p = paste0("czp_", str_sub(part,-2,-1)))%>%
    select( -part) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))

View(failed_dzp_f_1)
failed_dzp_f_1 <- left_join(failed_dzp, failed_czs,
                          by = c("ExperimentID"))

# 
#  Mergin various files!!
# 

# I Might have lost this config!!!

# failed_dzp_a <- left_join(data_frame(ExperimentID = 1:200), 
#                           failed_dzp)

failed_dzp_cza_a <- left_join(failed_dzp_a, failed_czs_a,
                            by = c("ExperimentID"))

failed_dzp_cza_m_a <- left_join(failed_dzp_a, failed_czs_m,
                              by = c("ExperimentID"))


# failed_dzp_cza_a_all <- left_join(data_frame(ExperimentID = 1:200), 
#                           failed_dzp_cza_a)


health_res_all <- left_join(data_frame(ExperimentID = 1:200), 
                                 health_res)

health_res_general_all <- left_join(data_frame(ExperimentID = 1:200), 
                            health_general)
View(merge_test)
merge_test <- left_join(failed_dzp_cza_a_all,health_res_all , by = c("ExperimentID"))

merge_test_geneal <- left_join(failed_dzp_cza_a_all,health_res_general_all , by = c("ExperimentID"))


merge_test_m <- left_join(failed_dzp_cza_m_a,health_res_all , by = c("ExperimentID"))

merge_test_geneal_m <- left_join(failed_dzp_cza_m_a,health_res_general_all , by = c("ExperimentID"))


merge_test %>% filter(
    is.na(status.x),
    is.na(status.y), 
    is.na(status_sc))

View(merge_test_geneal)

merge_test_geneal_m %>% filter(
    !is.na(status.x) |
    !is.na(status.y))

merge_test_m %>% filter(
    is.na(status.x),
    is.na(status.y), 
    is.na(status_sc))


merge_test_geneal_m %>% filter(
    is.na(status.x),
    is.na(status.y), 
    is.na(status))
# write.csv(failed_dzp_cza_a, file = "./input/failed_dzp_cza_a20170815_a.csv")

merge_test_geneal_m$status_rest <- "" 
write.csv(merge_test_geneal_m, file = "./input/failed_dzp_cza_a20170815_am.csv")

merge_test_geneal_m <-merge_test_geneal_m %>% filter(
    is.na(status.x),
    is.na(status.y), 
    is.na(status))


merge_test_geneal_m_M <- 
    merge_test_geneal_m %>% select(1:4) %>% group_by(ExperimentID) %>%
    gather(stat, val, -ExperimentID) %>% ungroup() %>% select(-stat) %>% 
    filter(!is.na(val))%>%
     group_by(ExperimentID) %>%
    mutate(status = paste(val, collapse= "+")) %>% ungroup()   %>% select(-val) 

    View(merge_test_geneal_m)

    merge_test_geneal_m_M
    
    merge_test_geneal_m_M_anti <- anti_join(data_frame(ExperimentID = 1:200), 
                                        merge_test_geneal_m_M)
    
    merge_test_geneal_m_M_anti$status <- "dzs_1r"
    merge_test_geneal_m_M_all <- union(merge_test_geneal_m_M_anti, merge_test_geneal_m_M) %>% arrange(ExperimentID)
write.csv(merge_test_geneal_m_M_all, file = "./input/failed_dzp_cza_a20170816_all.csv",
           row.names = FALSE, quote = FALSE)
