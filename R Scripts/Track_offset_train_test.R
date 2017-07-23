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
    y_n <- length(y[[1]])
    cor_offset <- adply(ll <- 1:(y_n - n), 1, function(off_set) {
        cor(x[1:n, 1], y[(1 + off_set):(n + off_set), 1], use = "complete.obs")
    })
    
    names(cor_offset) <- c("offset", "cor_val")
    
    cor_offset[, 1] <- as.numeric((cor_offset[, 1]))
    cor_offset
}

t <-
    cor.cross(
        test_data %>% filter(ExperimentID == 1) %>% select(matches("az_1r_1")),
        train_data %>% filter(ExperimentID == 1) %>% select(matches("az_1r_1"))
    )


data.cor <- sapply(time.range, function(i)
    cor.cross(e, v, i))
i <- time.range[which.max(data.cor)]
print(paste("Expansion lags volume by", i / v.frequency, "seconds."))


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
        
        # b <-
        #     do.call(cbind, lapply(list(
        #         lag = a$lag, acf = a$acf
        #     ), data.frame, stringsAsFactors = FALSE))
        #
        # names(b) <- c("lag", "acf")
        #
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
                       cut_off = 0.4) {
    track_ID <- test_d %>% filter(ExperimentID == test_expID) %>%
        summarise(ExperimentID = first(ExperimentID))
    
    train_track_IDs <-
        train_d %>% filter(Track == as.numeric(track_ID)) %>%
        group_by(ExperimentID) %>% summarise(first(ExperimentID)) %>%
        select(ExperimentID)
    
    tr_var <-
        test_d %>%
        select(starts_with(prefix)) %>%
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
              # .progress = progress_text(char = ".")
              )
              oo <-
                  stats_per_testExp %>% filter(cor_val > cut_off) %>% select(offset)
              
              pp <- rbind(sort(table(oo), decreasing = TRUE)[1] , test_expID)
              pp <- t(pp)
              pp <- cbind(rownames(pp), pp)
              names(pp) <- c("Offset", "count", "ExperimentID")
              pp
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
    4,
    export = list(
        "lag_determ",
        "max_acf_lag",
        "cor.cross",
        "train_data",
        "test_data"
    ),
    lib = list("plyr", "dplyr")
)

system.time(t <- ddply(data_frame(ExID = 1:200),
                       .(ExID),
                       function(df)  {
                           lag_determ(test_expID = df$ExID)
                       },
                       .parallel = TRUE
                       )
)
                       
stopCluster(cl)
   
                    