# ###############################################################################
# Script for determining offset (and direction?) of the test sample and training set track
# To prototype a universal function
#       for PHM Data Challange 2017
# Author: Marcin Borucki
# Email: Marcin.Borucki@gmail.com & Marcin.Borucki@ge.com
# Creation date: 07/23/2017
# Modification:
# Version: 0.1
###############################################################################

require(ggplot2)
require(plyr)
require(dplyr)
require(zoo)


#' Cross corelation from stack overflow leveraged to terermine offset
#' https://stats.stackexchange.com/questions/31666/how-can-i-align-synchronize-two-signals
#'
#' @param x0
#' @param y0
#' @param i
#'
#' @return
#' @export
#'
#' @examples
cor.cross <- function(x0, y0) {
    x <- as.vector(x0)
    y <- as.vector(y0)
    n <- length(x[[1]])
    cor_offset <- rollapply(
        y,
        n,
        align = "left",
        FUN = function(z)
            cor(
                z,
                x,
                use = "complete.obs"
            )
    )
    cor_offset <- bind_cols(data_frame(1:length(cor_offset)),data_frame(cor_offset[,1]))
    
    names(cor_offset) <- c("offset", "cor_val")
    cor_offset

    }


    
#' Slecting max corelated lag for the two time series - Finding the missmatch
#'
#' @param train_d
#' @param test_d
#' @param test_expID
#' @param train_expID
#' @param var Meaning full parameter number of column - not name - 6 is the first data column
#'
#' @return Returns LAG and ACF value maximizing abs(ACF)
#' @export
#'
#' @examples
max_acf_lag <-
    function(test_d = test_data,
             train_d = train_data,
             test_expID = NULL,
             train_expID = NULL,
             var = NULL) {
        a <-
            cor.cross(
                test_d %>% filter(ExperimentID == test_expID) %>% select(matches(var)),
                train_d %>% filter(ExperimentID == train_expID) %>% select(matches(var))
                
            )
        a %>% filter(cor_val == max(cor_val))
        
    }

#' Function determinig lag
#'
#' @param train_d
#' @param test_d
#' @param test_expID
#' @param type
#'
#' @return
#' @export
#'
#' @examples
lag_determ <- function(train_d = train_data,
                       test_d = test_data,
                       test_expID = NULL,
                       prefix = "az_",
                       sufix = "2",
                       cut_off = 0.1) {
    track_ID <- test_d %>% filter(ExperimentID == test_expID) %>%
        summarise(Track = first(Track))
    
    train_track_IDs <-
        train_d %>% filter(Track == as.numeric(track_ID)) %>%
        group_by(ExperimentID) %>% summarise(first(ExperimentID)) %>%
        select(ExperimentID)
    
    tr_var <-
        test_d %>%
        select(starts_with(prefix)) %>%
        select(ends_with(sufix)) %>%
        names() %>%
        data_frame()  %>%  mutate(k = 1) %>%
        inner_join(train_track_IDs %>%
                       mutate(k = 1), by = "k") %>% select(-k)
    
    names(tr_var) <- c("vars", "ExperimentID")
    
    stats_per_testExp <-
        ddply(tr_var,
              .(ExperimentID, vars),
              function(df)  {
                  max_acf_lag(
                      train_expID = df$ExperimentID,
                      test_d = test_d,
                      train_d = train_d,
                      test_expID = test_expID,
                      var = df$vars
                  )
              },
              .parallel = TRUE
              # .parports = list(.export = c(
              #     "lag_determ",
              #     "max_acf_lag",
              #     "cor.cross",
              #     "train_data",
              #     "test_data" ),
              #    .packages = c("plyr", "dplyr", "zoo")
                 # )
              # .progress = progress_text(char = ".")
              )
              oo <-
                  stats_per_testExp %>% mutate(Test_ExpID = test_expID)
                  #filter(cor_val > cut_off) %>% select(offset)
      oo        
}

clusterExport <- local({
    gets <- function(n, v) {
        assign(n, v, envir = .GlobalEnv)
        NULL
    }
    function(cl, list, envir = .GlobalEnv) {
        ## do this with only one clusterCall--loop on slaves?
        for (name in list) {
            clusterCall(cl, gets, name, get(name, envir = envir))
        }
    }
})


#' Create cluster function implementation from:
#' http://www.numbertheory.nl/2011/11/14/parallelization-using-plyr-loading-objects-and-packages-into-worker-nodes/
#'
#' @param noCores
#' @param logfile
#' @param export
#' @param lib
#'
#' @return
#' @export
#'
#' @examples
createCluster = function(noCores,
                         logfile = "/dev/null",
                         export = NULL,
                         lib = NULL) {
    require(doSNOW)
    cl <- makeCluster(noCores, type = "SOCK", outfile = logfile)
    if (!is.null(export))
        clusterExport(cl, export)
    if (!is.null(lib)) {
        l_ply(lib, function(dum) {
            clusterExport(cl, "dum", envir = environment())
            clusterEvalQ(cl, library(dum, character.only = TRUE))
        })
    }
    registerDoSNOW(cl)
    return(cl)
}

cl = createCluster(
    3,
    export = list(
        "lag_determ",
        "max_acf_lag",
        "cor.cross",
        "train_data",
        "test_data"
    ),
    lib = list("plyr", "dplyr", "zoo")
)


test_offset_correlations_1 <- ddply(data_frame(ExID = 1:200),
                                    .(ExID),
                                    function(df)  {
                                        lag_determ(test_expID = df$ExID,
                                                   cut_off = 0.0,
                                                   sufix = "1")
                                    },
                                    # .parallel = TRUE
                                    .progress = progress_text(char = "."))
test_offset_correlations_2 <- ddply(data_frame(ExID = 1:200),
                                    .(ExID),
                                    function(df)  {
                                        lag_determ(test_expID = df$ExID,
                                                   cut_off = 0.0,
                                                   sufix = "2")
                                    },
                                    # .parallel = TRUE
                                    .progress = progress_text(char = "."))
test_offset_correlations_3 <- ddply(data_frame(ExID = 1:200),
                                    .(ExID),
                                    function(df)  {
                                        lag_determ(test_expID = df$ExID,
                                                   cut_off = 0.0,
                                                   sufix = "3")
                                    },
                                    # .parallel = TRUE
                                    .progress = progress_text(char = "."))

test_offset_correlations_4 <- ddply(data_frame(ExID = 1:200),
                                    .(ExID),
                                    function(df)  {
                                        lag_determ(test_expID = df$ExID,
                                                   cut_off = 0.0,
                                                   sufix = "4")
                                    },
                                    # .parallel = TRUE
                                    .progress = progress_text(char = "."))
test_offset_correlations_5 <- ddply(data_frame(ExID = 1:200),
                                    .(ExID),
                                    function(df)  {
                                        lag_determ(test_expID = df$ExID,
                                                   cut_off = 0.0,
                                                   sufix = "5")
                                    },
                                    # .parallel = TRUE
                                    .progress = progress_text(char = "."))
test_offset_correlations_az <-
    bind_rows(
        test_offset_correlations_1,
        test_offset_correlations_2,
        test_offset_correlations_3,
        test_offset_correlations_4,
        test_offset_correlations_5
    )


stopCluster(cl)

system.time(
test_offset_correlations_azp <- ddply(inner_join(data_frame(ExID = 1:200, k=1),
                                                 data_frame(Band = 1:5, k=1)) %>% 
                                          select(-k),
                                    .(ExID, Band),
                                    function(df)  {
                                        lag_determ(test_expID = as.character((df$ExID)),
                                                   cut_off = 0.0,
                                                    prefix = "azp_",
                                                   sufix = as.character((df$Band)))
                                    },
                                     # .parallel = TRUE
                                    .progress = progress_text(char = ".")
                                    )
 )

system.time(
    test_offset_correlations_azs <- ddply(inner_join(data_frame(ExID = 1:200, k=1),
                                                 data_frame(Band = 1:5, k=1)) %>% 
                                          select(-k),
                                      .(ExID, Band),
                                      function(df)  {
                                          lag_determ(test_expID = as.character(df$ExID),
                                                     cut_off = 0.0,
                                                     prefix = "azs_",
                                                     sufix = as.character(df$Band))
                                      },
                                      # .parallel = TRUE
                                      .progress = progress_text(char = "."))
)



write.csv(test_offset_correlations_azs, file = "./input/test_offset_correlations_azs.csv")

saveRDS(test_offset_correlations_azs, file = "./input/test_offset_correlations_azs.RDS")

