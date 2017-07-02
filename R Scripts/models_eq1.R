###############################################################################
# Linear models to provide elemnt characteristic
# Author: Antoni Tyburcy
# Colaborator: Marcin Borucki
# Author's e-mail:  antoni.tyburcy@gmail.com
# Colaborator's e-mail:
# Creation date: 07/02/2017
# Modification:
# Version: 0.1
###############################################################################


#
# this file contains linear model definitions that are suppose to describe
# car geometry a relations between acc meters
# 
# this file should be used with "linear_models.R"
#
#

require(dplyr)
if(exists("models")){
    rm(models)

}

models <- data.frame(model = numeric(),
                     modeled_element = character(),
                     lm_cmd = character(), 
                     freq = numeric(), 
                     stringsAsFactors = FALSE) 

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
