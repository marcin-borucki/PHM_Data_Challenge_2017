###############################################################################
# Functions usefull for PHM Data Challange 2017
# Author: Marcin Borucki
# Email: Marcin.Borucki@gmail.com or Marcin.Borucki@ge.com
# Creation date: 06/25/2017
# Modification:
# Version: 0.1
###############################################################################


#' friendly_names
#'
#' @return Vector of strings containing more descriptive names of variables from
#' experimnet data frame
#' @export
#'
#' @examples
friendly_names <- function() {
    c(paste("azs_1",1:5,sep="_"),
      paste("azp_1r",1:5,sep="_"),
      paste("azp_1l",1:5,sep="_"),
      paste("azp_2r",1:5,sep="_"),
      paste("azp_2l",1:5,sep="_"),
      paste("az_1r",1:5,sep="_"),
      paste("az_1l",1:5,sep="_"),
      paste("az_2r",1:5,sep="_"),
      paste("az_2l",1:5,sep="_"),
      paste("azs_2",1:5,sep="_"),
      paste("azp_3r",1:5,sep="_"),
      paste("azp_3l",1:5,sep="_"),
      paste("azp_4r",1:5,sep="_"),
      paste("azp_4l",1:5,sep="_"),
      paste("az_3r",1:5,sep="_"),
      paste("az_3l",1:5,sep="_"),
      paste("az_4r",1:5,sep="_"),
      paste("az_4l",1:5,sep="_"))
}




#' load_expetiment
#' Loading experimental reading datafiles
#'
#' @param file_ID Numeric ID of the experimnet
#' @param dir Directory holding experimental data
#' @param names Boolean telling wheater to rename the vars 
#' with the names equivalent to the esperimental setup
#'
#' @return Returing dataframe
#' @export
#'
#' @examples
load_expetiment <- function(file_ID, dir = "./", names = TRUE) {
    experiment<- read.csv(paste0(dir, "Experiment-", file_ID, ".csv"))
    if(names)  names(experiment) <- friendly_names()
    experiment
}

#' plot_groups
#' Plot in plotly same sensors a cross the  train car
#'
#' @param data data frame with tidy names 
#' @param var prefix of var to plot
#' @param par_range define the ranego of sensors to plot usually 1:4,
#'  but for ASZ 1:2
#' @param lr to tell left and right, for ASZ leafe blank
#' @param freq_band usually digit in range 1:5 but no the range
#'
#' @return returnin plotly object
#' @export
#'
#' @examples
plot_groups <-
    function(df,
             var = "az",
             par_range = c(1:4),
             lr = "l" ,
             freq_band = 1) {
        az <- ggplot(df %>%
                         mutate(row = row_number())  %>%
                         gather("group", "y",
                                which(colnames(df)  %in% (
                                    paste0(var, "_", par_range, lr, "_", freq_band)
                                ))))  +
            geom_line(aes(
                x = row ,
                y = y,
                colour = group ,
                group = group
            ))
        
        ggplotly(az)
    }


#' calc_residules
#' function to calulate residules of different parameters based on mean 
#' and appends them to the data set
#' TO Consider: residuals based on median...?
#' @param df 
#' @param var 
#' @param par_range 
#' @param lr 
#' @param freq_band 
#'
#' @return
#' @export
#'
#' @examples
calc_residules <-
    function(df,
             var = "az",
             par_range = c(1:4),
             lr = "l" ,
             freq_band = 1) {
        vars <- paste0(var, "_", par_range, lr, "_", freq_band)
        # vars_res <- setNames(vars, paste0(vars, "_res")) 
        # df %>% select(one_of(vars)) %>% group_by(row_number()) %>% 
        #     mutate_each_(funs(res = mean(vars) - sum(.) ), vars_res)  %>% ungroup()
        vars
        df %>%  
            #dplyr::select(one_of(vars)) %>%
            mutate(mean_v = rowMeans(.,na.rm=TRUE) )%>%
            mutate_each_(funs(res = (mean_v - .) ), vars) %>%
            dplyr::select(-mean_v)
        
    }


#' find_d
#'
#' @param leading Name of leading parameter name like az_1l, 
#' suffix added by the code by deafuls. See  @param suffix
#' @param trailing Name of leading parameter name like az_1l, 
#' suffix added by the code by deafuls. See  @param suffix
#' @param Speed 
#' @param Payload 
#' @param suffix If TRUE then leading and trailing variables are already 
#' delivered fith band suffix and the code will only execute for one frec band
#'
#' @return
#' @export
#'
#' @examples
find_d <- function(leading, trailing, Speed, Payload, suffix = FALSE ){
    
}

