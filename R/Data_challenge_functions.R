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
        
        df %>%  
            dplyr::select(one_of(vars)) %>%
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

#' add_model_to_dict
#'
#' @param df dictionary data frame to be processe migh be null to start a new dict
#' @param element element name to be modeled. Used as Left hand side of the formula. 
#' Don't provide freq band suffix. Obligaroty.
#' If already present will overide the previous instance of the model
#' @param model_no not obligatory number denoting the model name, 
#' if supplemented will remove the previous instance to avoid conflicts. 
#' Function will not check for potential missmatch with element name. 
#' @param model_RHS_var model right hand side variables. This might be refined. For reference
#'  please look review i.e. http://faculty.chicagobooth.edu/richard.hahn/teaching/formulanotation.pdf
#'  A potential extended formula package: https://cran.r-project.org/web/packages/Formula/vignettes/Formula.pdf
#'
#' @return
#' @export
#'
#' @examples
add_model_to_dict <- function(df = NULL, element, model_no = NULL, model_RHS_var){
    require(dplyr)
    # creatating sceleton data frame if not provided
    if (is.null(df)) {
        df <- data.frame(
            model = numeric(),
            modeled_element = character(),
            lm_cmd = character(),
            freq = numeric(),
            stringsAsFactors = FALSE
        )
        model_no = 1
    }
    
    # removing potential model id conflicts
    if (is.null(model_no)) {
        if ( nrow(df) == 0) {
            model_no = 1
        }
        else{
            model_no = max(df$model, na.rm = TRUE) + 1
        }
    } 
    
    if (!is.null(nrow(df))){
        df <- df %>% filter(  model != model_no & modeled_element != element) #comment out? - this forbids using two models for the same element...
    }
    
    # Overriding modeled element
    
    # loop over frequency bands
    for (i in 1:5) {
        # model_formula <- as.formula()
        lhs <- paste(element, i, sep="_" )
        rhs <- paste( paste(model_RHS_var, i, sep="_" ) , collapse=" + ")
        model_formula <- paste0( lhs ,  "~" , rhs )
        
        df <- add_row(df, model = model_no,
                      modeled_element = element, 
                      lm_cmd = model_formula,
                      freq = i)
    }
    df
}


#' run_fits
#'
#' @param model_df data frame with model definitions
#' @param data_df data frame with actual esperiment data
#'
#' @return
#' @export
#'
#' @examples
run_fits <- function(model_df = models, data_df = data_train){
    inner_join(model_df %>% mutate(k = 1), data_df %>% mutate(k = 1), by = "k") %>%
        select(-k)  %>% ddply(.(modeled_element, freq , ExperimentID),
                              function(df) {
                                  model_f <-
                                      update(as.formula(df$lm_cmd), . ~ . )
                                  fit <- lm(model_f, df)
                                  # print(summary(fit)) 
                                  x <- coef(fit)
                                  x <- t(x)
                                  x <- as.data.frame(x)
                                  k <- names(x)
                                  k <- t(k)
                                  k <- as.data.frame(k)
                                  names(x) <- paste0("c", 1:ncol(x))
                                  names(k) <-
                                      paste0("x_name", 1:ncol(k))
                                  h <- df[1, ] %>%
                                      select(model, lm_cmd)
                                  # h <- as.data.frame(h)
                                  bind_cols(h, df[1,] %>% select(Payload, Speed, Track, State), x, k)
                              }  #commented out .progress="progress_text(char = ".")" 
                              # .parallel = TRUE)
        )
}


#' plot_lines
#' plot coefficients c2 as lines by freq band grouped by speed
#' @param rev_dat 
#' @param plot_tilte 
#'
#' @return
#' @export
#'
#' @examples
plot_lines <- function(rev_dat, plot_tilte = NULL){
    p <- rev_dat %>%
        ggplot(., aes(x = freq,
                      y = c2,
                      color = as.factor(grepl('*r',modeled_element)),
                      alpha = 0.5,
                      group = interaction(modeled_element,ExperimentID)
        )) +
        geom_line() +
        facet_grid(Speed ~ .) +
        labs(title = paste(plot_tilte)) 
    show(p)
}
