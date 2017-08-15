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
 View(failed_dzp_det)
# View(gains_test_sum)
failed_dzp_det <-
    gains_test_sum %>%
    filter((gain_mean - gain_mean_exp)  > 0.0751,
           (gq50 - eq50)  > 0.05 ) %>%
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
    mutate(status_p = paste0("dzp_", str_sub(part,-2,-1)))%>%
    select( -part) %>% group_by(ExperimentID) %>%
    summarise(status = paste(status_p, collapse= "+"))

View(failed_dzp_f_1)
failed_dzp_f_1 <- left_join(failed_dzp, failed_czs,
                          by = c("ExperimentID"))


failed_dzp_a <- left_join(data_frame(ExperimentID = 1:200), 
                          failed_dzp)
View(failed_dzp_cza_a)

failed_dzp_cza_a <- left_join(failed_dzp_a, failed_czs_a,
                            by = c("ExperimentID"))



failed_dzp_cza_a_all <- left_join(data_frame(ExperimentID = 1:200), 
                          failed_dzp_cza_a)

health_res_all <- left_join(data_frame(ExperimentID = 1:200), 
                                 health_res)

health_res_general_all <- left_join(data_frame(ExperimentID = 1:200), 
                            health_general)
View(merge_test)
merge_test <- left_join(failed_dzp_cza_a_all,health_res_all , by = c("ExperimentID"))

merge_test_geneal <- left_join(failed_dzp_cza_a_all,health_res_general_all , by = c("ExperimentID"))

merge_test %>% filter(
    is.na(status.x),
    is.na(status.y), 
    is.na(status_sc))

merge_test_geneal %>% filter(
    is.na(status.x),
    is.na(status.y),
    is.na(status))


write.csv(failed_dzp_cza_a, file = "./input/failed_dzp_cza_a20170815_a.csv")


EID = 6

ppppp <- gains_test_all  %>% filter(ExperimentID == EID) %>%
    ggplot(., aes(x = lp_off,
                  y = gain.y - gain.x 
                  
                  
    )) +
    geom_line(aes(color =  gain_func) ) 

show(ppppp)
