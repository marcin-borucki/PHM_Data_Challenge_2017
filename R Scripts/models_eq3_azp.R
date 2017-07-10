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
# car geometry a relations between acc meters - 
# for boggies only 
# wheel excitation only
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


# bogie models - just wheels

models <-add_model_to_dict(
    element = "azp_1r",
    model_RHS_var = c("az_1r")
)



models <-
    models %>% add_model_to_dict(
        element = "azp_2r",
        model_RHS_var = c("az_2r")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_1l",
        model_RHS_var = c("az_1l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_2l",
        model_RHS_var = c("az_2l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_3r",
        model_RHS_var = c("az_3r")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4r",
        model_RHS_var = c("az_4r")
    )

models <-
    models %>% add_model_to_dict(
        element = "azp_3l",
        model_RHS_var = c("az_3l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4l",
        model_RHS_var = c("az_4l")
    )

