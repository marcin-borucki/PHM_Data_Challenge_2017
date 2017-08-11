gains_test_sum  %>%  filter(ExperimentID <= 200, q50 > 0.15 ) %>% 
    select(ExperimentID, part)%>% distinct()


gains_test_sum  %>%  filter(ExperimentID <= 200, q75 > 0.5 ) %>% 
    select(ExperimentID, part)%>% distinct() 

dumpners_primary <- gains_test_sum  %>%  filter(ExperimentID <= 200, q50 > 0.15 ) %>% 
    select(ExperimentID, part)%>% distinct() %>% group_by(ExperimentID) %>% 
    summarise(count = n()) %>% filter(count < 4 )

dumpners_primary_det <-
left_join(dumpners_primary, gains_test_sum) %>%  filter( q50 > 0.15 ) %>%
    select(c(1:4,10)) %>% group_by(ExperimentID, count, part) %>% 
    summarise(frec_count = n()) %>% ungroup() %>%
    left_join(gains_test_sum %>%  filter( q50 > 0.15 )) %>%
     filter( !(count > 2 & frec_count == 1) )

tt <- dumpners_primary_det %>% select(ExperimentID, gain_func) %>% 
    mutate(State = paste0("d",substr(gain_func,2,6))) %>% 
        select(1,3) %>%
    distinct() %>% group_by(ExperimentID) %>% mutate(row = row_number()) %>%
      ungroup() %>%  spread(row, State)

    tt<- left_join(data_frame(ExperimentID = 1:200), tt)
 write.csv(tt, file = "./input/dumpners_primary_det.csv")

 
 # positive gain anomalies
 gains_test_sum  %>%  filter( q50 > 0.15 ) %>% 
     select(ExperimentID, part)%>% distinct() %>%
      summarise(count = n())

anom_primary <- gains_test_sum  %>%  filter(q50 > 0.15 ) %>% 
    select(ExperimentID, part)%>% distinct() %>% group_by(ExperimentID) %>% 
    summarise(count = n())

 primary_anomalies <-
     left_join(anom_primary, gains_test_sum) %>%  filter( q50 > 0.15 ) %>%
     select(c(1:4,10)) %>% group_by(ExperimentID, count, part) %>% 
     summarise(frec_count = n()) %>% ungroup() %>%
     left_join(gains_test_sum %>%  filter( q50 > 0.15 )) 
 
 primary_anomalies %>% select(ExperimentID, gain_func) %>% 
     mutate(State = paste0("d",substr(gain_func,2,6))) %>% 
     select(1,3) %>%
     distinct() %>% group_by(ExperimentID) %>% mutate(row = row_number()) %>%
     ungroup() %>%  spread(row, State) 
# All Anomalies
 anom_primary <- gains_test_sum  %>%  
     filter(q50 > 0.15 | q50 < -0.15 | q75 > 0.5 | q75 < -0.5) %>% 
     select(ExperimentID, part)%>% distinct() %>% group_by(ExperimentID) %>% 
     summarise(count = n())
 
 
 primary_anomalies <-
     left_join(anom_primary, gains_test_sum) %>%  
     filter(q50 > 0.15 | q50 < -0.15 | q75 > 0.5 | q75 < -0.5 ) %>%
     select(c(1:4,10)) %>% group_by(ExperimentID, count, part) %>% 
     summarise(frec_count = n()) %>% ungroup() %>%
     left_join(gains_test_sum %>%
                   filter( q50 > 0.15 | q50 < -0.15 | q75 > 0.5 | q75 < -0.5 )) 
 
 
 anomalies_trains <- primary_anomalies %>% select(ExperimentID, gain_func) %>% 
     mutate(State = paste0("d",substr(gain_func,2,6))) %>% 
     select(1,3) %>%
     distinct() %>% group_by(ExperimentID) %>% mutate(row = row_number()) %>%
     ungroup() %>%  spread(row, State) 
     
 
 
 # All Anomalies low
 anom_primary <- gains_test_sum  %>%  
     filter( q50 < -0.15 ) %>% 
     select(ExperimentID, part)%>% distinct() %>% group_by(ExperimentID) %>% 
     summarise(count = n())
 
 
 primary_anomalies <-
     left_join(anom_primary, gains_test_sum) %>%  
     filter( q50 < -0.15 ) %>%
     select(c(1:4,10)) %>% group_by(ExperimentID, count, part) %>% 
     summarise(frec_count = n()) %>% ungroup() %>%
     left_join(gains_test_sum %>%
                   filter( q50 < -0.15  )) 
 
 
 anomalies_trains <- primary_anomalies %>% select(ExperimentID, gain_func) %>% 
     mutate(State = paste0("d",substr(gain_func,2,6))) %>% 
     select(1,3) %>%
     distinct() %>% group_by(ExperimentID) %>% mutate(row = row_number()) %>%
     ungroup() %>%  spread(row, State) 
 