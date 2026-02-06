# Utility functions

# range testing ----------

#' Test if numeric stays within a range.
#'
#' @description
#' Checks if a single numeric value stays within the user-provided range.
#'
#' @details
#' The range is open on the left and closed on the right side.
#'
#'
#' @param x a single numeric value.
#' @param lower the lower boundary of the range.
#' @param upper the upper boundary of the range.
#'
#' @export

  is_within_range <- function(x, lower, upper) {

    stopifnot(is.numeric(x))
    stopifnot(is.numeric(lower))
    stopifnot(is.numeric(upper))

    stopifnot(length(x) == 1)
    stopifnot(length(lower) == 1)
    stopifnot(length(upper) == 1)

    if(lower > upper) {

      stop("'upper' has to be larger than 'lower'", call. = FALSE)

    }

    return(x > lower & x <= upper)

  }

# stats --------

#' Compute Binary Model Stats.
#'
#' @description
#' Computes fit statistics for a simulated binary classifier.
#'
#' @details
#' Technically it employs the `caret` package functions
#' \code{\link[caret]{defaultSummary}} and \code{\link[caret]{twoClassSummary}}.
#'
#' @return a data frame with the following columns:
#'
#' * `n`: the total number of observations
#'
#' * `n_events`: the number of observed events coded in the
#' input data frame as 1
#'
#' * `n_predicted`: the number of predicted events
#'
#' * `prev_events`: the observed prevalence
#'
#' * `prev_predicted`: the predicted prevalence
#'
#' * `accuracy`: overall accuracy
#'
#' * `kappa`: Cohen's kappa
#'
#' * `Se`: sensitivity
#'
#' * `Sp`: specificity
#'
#' * `AUC`: are under the ROC curve
#'
#' @param data a data frame with the columns `outcome` and `predicted`.
#' The event or positivity is coded as 1.
#'
#' @export

  binary_stats <- function(data) {

    ## entry control -------

    x_err <- paste("'data' has to be a data frame with the columns",
                   "'outcome' and 'predicted'.")

    if(!is.data.frame(data)) stop(x_err, call. = FALSE)

    if(any(!c('outcome', 'predicted') %in% names(data))) stop(x_err, call. = FALSE)

    outcome <- NULL
    predicted <- NULL
    pred <- NULL

    ## class probabilities ---------

    data <- transmute(data,
                      obs = outcome,
                      pred = predicted)

    levs <- c('positive', 'negative')

    data <- map_dfc(data,
                    car::recode,
                    "1 = 'positive'; 0 = 'negative'")

    data <- map_dfc(data, factor, levs)

    data <- mutate(data,
                   `positive` = as.numeric(pred) - 1,
                   `negative` = 2 - as.numeric(pred))

    ## stats ---------

    def_summary <- defaultSummary(as.data.frame(data), lev = levs)

    two_summary <- twoClassSummary(as.data.frame(data), lev = levs)

    tibble(n = nrow(data),
           n_events = nrow(data) - sum(as.numeric(data$obs) - 1),
           n_predicted = nrow(data) - sum(as.numeric(data$pred) - 1),
           prev_events = (nrow(data) - sum(as.numeric(data$obs) - 1))/nrow(data),
           prev_predicted = (nrow(data) - sum(as.numeric(data$pred) - 1))/nrow(data),
           accuracy = def_summary['Accuracy'],
           kappa = def_summary['Kappa'],
           Se = two_summary['Sens'],
           Sp = two_summary['Spec'],
           AUC = 1 - two_summary['ROC'])

  }

# END ------
