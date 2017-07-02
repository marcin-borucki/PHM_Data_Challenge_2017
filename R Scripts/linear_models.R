###############################################################################
# Linear models to provide elemnt characteristic
# Author: Antoni Tyburcy
# Colaborator: Marcin Borucki
# Author's e-mail:  Marcin.Borucki@gmail.com
# Colaborator's e-mail:
# Creation date: 07/01/2017
# Modification:
# Version: 0.1
###############################################################################

# 
# if(require("formula.tools")) {
#     install.packages("formula.tools")
#     require("formula.tools")
# }

library("ggplot2")
library("dplyr")

data_train <- readRDS("./input/train_data.RDS")

data_test  <- readRDS("./input/test_data.RDS")

################################################################################
#
# Creating linear models to reflect car geometry and constraints ---------------
#
################################################################################

# Creating Model Dictionary ----------------------------------------------------

models <- data.frame(model = numeric(),
                     modeled_element = character(),
                     lm_cmd = character(), 
                     freq = numeric(), 
                     stringsAsFactors = FALSE) 
    
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
        df <- df %>% filter(  model != model_no & modeled_element != element )
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

models <- add_model_to_dict(
        element = "azp_1r",
        model_RHS_var = c("az_1r",
                          "azp_1l",
                          "azp_2r",
                          "azp_2l",
                          "azs_1")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_2r",
        model_RHS_var = c("az_2r",
                          "azp_2l",
                          "azp_1r",
                          "azp_1l",
                          "azs_1")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_1l",
        model_RHS_var = c("az_1l",
                          "azp_1r",
                          "azp_2r",
                          "azp_2l",
                          "azs_1")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_2l",
        model_RHS_var = c("az_2l",
                          "azp_2r",
                          "azp_1r",
                          "azp_1l",
                          "azs_1")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_3r",
        model_RHS_var = c("az_3r",
                          "azp_3l",
                          "azp_4r",
                          "azp_4l",
                          "azs_2")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4r",
        model_RHS_var = c("az_4r",
                          "azp_4l",
                          "azp_3r",
                          "azp_3l",
                          "azs_2")
    )

models <-
    models %>% add_model_to_dict(
        element = "azp_3l",
        model_RHS_var = c("az_3l",
                          "azp_3r",
                          "azp_4r",
                          "azp_4l",
                          "azs_2")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4l",
        model_RHS_var = c("az_4l",
                          "azp_4r",
                          "azp_3r",
                          "azp_3l",
                          "azs_2")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_1",
        model_RHS_var = c("azp_1r",
                          "azp_1l",
                          "azp_2r",
                          "azp_2l",
                          "azs_2")
    )



models <-
    models %>% add_model_to_dict(
        element = "azs_2",
        model_RHS_var = c("azp_3r",
                          "azp_3l",
                          "azp_4r",
                          "azp_4l",
                          "azs_1")
    )

# 
# results_A <-
#     data.frame(
#         "model" = numeric(),
#         cmd = character(),
#         elemetn = character(),
#         freq = numeric(),
#         ID = numeric(),
#         c1 = numeric(),
#         c2 = numeric(),
#         c3 = numeric(),
#         c4 = numeric(),
#         c5 = numeric(),
#         c6 = numeric()
#     )


require(plyr)
system.time(
results <- inner_join(models %>% mutate(k = 1), data_train %>% mutate(k = 1), by = "k") %>%
    select(-k)  %>% ddply(.(modeled_element, freq , ExperimentID),
                          function(df) {
                              x <- coef(lm(df$lm_cmd, df))
                              x <- t(x)
                              x <- as.data.frame(x)
                              k <- names(x)
                              k <- t(k)
                              k <- as.data.frame(k)
                              names(x) <- paste0("c", 1:ncol(x))
                              names(k) <- paste0("x_name", 1:ncol(k))
                              h <- df[1,] %>%
                                  select(model, lm_cmd)
                              # h <- as.data.frame(h)
                              bind_cols(h, x, k)
                          }, .progress= progress_text(char = ".")
                          # .parallel = TRUE
                          )
)

system.time(
    results <- inner_join(models %>% mutate(k = 1), data_test %>% mutate(k = 1), by = "k") %>%
        select(-k)  %>% ddply(.(modeled_element, freq , ExperimentID),
                              function(df) {
                                  x <- coef(lm(df$lm_cmd, df))
                                  x <- t(x)
                                  x <- as.data.frame(x)
                                  k <- names(x)
                                  k <- t(k)
                                  k <- as.data.frame(k)
                                  names(x) <- paste0("c", 1:ncol(x))
                                  names(k) <- paste0("x_name", 1:ncol(k))
                                  h <- df[1,] %>%
                                      select(model, lm_cmd)
                                  # h <- as.data.frame(h)
                                  bind_cols(h, x, k)
                              }, .progress= progress_text(char = ".")
                              # .parallel = TRUE
        )
)


results %>% filter(model == 6) %>%
    ggplot(., aes(x = freq, y = c2)) + geom_boxplot(aes(fill = as.factor(freq)))

ggplot(results[results$model == 6, ], aes(x = freq, y = c2)) + geom_boxplot(aes(fill =
                                                                                    as.factor(freq)))

#ggsave("plot1.png")

results %>% filter(model == 2) %>%
    ggplot(., aes(x = freq, y = c2)) + geom_boxplot(aes(fill = as.factor(freq)))

results %>% filter(model == 3) %>%
    ggplot(., aes(x = freq, y = c2)) + geom_boxplot(aes(fill = as.factor(freq)))

results %>% filter(model == 4) %>%
    ggplot(., aes(x = freq, y = c2)) + geom_boxplot(aes(fill = as.factor(freq)))

results %>% filter(model == 10) %>%
    ggplot(., aes(x = freq, y = c)) + geom_boxplot(aes(fill = as.factor(freq)))



ggplot(results[results$model == 2, ], aes(x = freq, y = c2)) +
    geom_boxplot(aes(fill = as.factor(freq))) +
    geom_boxplot(data = results_test[results_test$model == 2, ], aes(
        x = freq + 0.5,
        y = c2,
        color = ExperimentID,
        fill = as.factor(freq)
    ))
