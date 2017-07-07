###############################################################################
# Linear models to provide elemnt characteristic
# Author: Antoni Tyburcy
# Colaborator: Marcin Borucki
# Author's e-mail:  antoni.tyburcy@gmail.com
# Colaborator's e-mail:
# Creation date: 07/0207/2017
# Modification:
# Version: 0.1
###############################################################################


#
# this file contains linear model definitions that are suppose to describe
# car geometry a relations between acc meters - 
# for boggies only 
# wheel excitation only
#
# for platform - it will use four separate models per platform acc
# each mdoel will be azs_y ~ azp_ij i =1,2,3,4, j= "r" or "l"
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
        model_no = 1,
        model_RHS_var = c("az_1r")
    )



models <-
    models %>% add_model_to_dict(
        element = "azp_2r",
        model_no = 2,
        model_RHS_var = c("az_2r")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_1l",
        model_no = 3,
        model_RHS_var = c("az_1l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_2l",
        model_no = 4,
        model_RHS_var = c("az_2l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_3r",
        model_no = 5,
        model_RHS_var = c("az_3r")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4r",
        model_no = 6,
        model_RHS_var = c("az_4r")
    )

models <-
    models %>% add_model_to_dict(
        element = "azp_3l",
        model_no = 7,
        model_RHS_var = c("az_3l")
    )


models <-
    models %>% add_model_to_dict(
        element = "azp_4l",
        model_no = 8,
        model_RHS_var = c("az_4l")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_1",
        model_no = 9,
        model_RHS_var = c("azp_1l")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_1",
        model_no = 10,
        model_RHS_var = c("azp_1r")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_1",
        model_no = 11,
        model_RHS_var = c("azp_2l")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_1",
        model_no = 12,
        model_RHS_var = c("azp_2r")
    )
models <-
    models %>% add_model_to_dict(
        element = "azs_2",
        model_no = 13,
        model_RHS_var = c("azp_3l")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_2",
        model_no = 14,
        model_RHS_var = c("azp_3r")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_2",
        model_no = 15,
        model_RHS_var = c("azp_4l")
    )

models <-
    models %>% add_model_to_dict(
        element = "azs_2",
        model_no = 16,
        model_RHS_var = c("azp_4r")
    )