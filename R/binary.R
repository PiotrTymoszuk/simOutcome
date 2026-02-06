# Tools for simulation of binary outcomes

# simulation of binary outcomes given specificity and sensitivity ------

#' Simulate A Binary Outcome
#'
#' @description
#' Simulates an outcome of a binary classifier (0/1) with the pre-defined
#' prevalence of the event (coded as 1), the sensitivity and specificity.
#'
#' @return an object of class `simOutcome`. Technically, data frame with
#' two columns `outcome` and `predicted` and a `summary()` method returning
#' statistics such as accuracy, sensitivity, specificity, ROC AUC
#' (are under the receiver-operator characteristic curve) and Cohen's kappa.
#'
#' @details
#' The total number of positive cases, true positives and true negatives
#' computed as defined by `n`, `prevalence`, `Se` and `Sp`
#' are each rounded to the nearest integer.
#'
#' @param prevalence observed prevalence of the event (coded as 1) in
#' the population.
#' @param n population size.
#' @param Se sensitivity of the classifier.
#' @param Sp specificity of the classifier.
#' @param reshuffle logical, should the output be randomly re-shuffled?
#' defaults to `TRUE`.
#' @param ... extra arguments, currently none.
#'
#' @export

  binary_SeSp <- function(prevalence, n, Se, Sp, reshuffle = TRUE, ...) {

    ## entry control --------

    stopifnot(is.numeric(prevalence))
    stopifnot(is.numeric(n))
    stopifnot(is.numeric(Se))
    stopifnot(is.numeric(Sp))

    lens <- purrr::map(list(prevalence, n, Se, Sp), length)

    if(any(lens > 1)) {

      stop(paste("The arguments 'prevalence', 'n', 'Se' and 'Sp'",
                 "must be single numeric values."), call. = FALSE)

    }

    if(!is_within_range(prevalence, 0, 1) |
       !is_within_range(Se, 0, 1) |
       !is_within_range(Sp, 0, 1)) {

      stop(paste("The arguments 'prevalence', 'Se' and 'Sp'",
                 "must stay within the (0, 1] range."), call. = FALSE)

    }

    ## computation of positives, true positives and true negatives -------

    n_pos <- round(prevalence * n)

    true_neg <- round(Sp  * (n - n_pos))

    true_pos <- round(Se * n_pos)

    false_neg <- n_pos - true_pos

    false_pos <- (n - n_pos) - true_neg

    ## generation of the data frame ---------

    pos_data <-
      tibble(outcome = rep(1, n_pos),
             predicted = c(rep(1, true_pos),
                           rep(0, false_neg)))

    neg_data <-
      tibble(outcome = rep(0, n - n_pos),
             predicted = c(rep(0, true_neg),
                           rep(1, false_pos)))

    out_data <- rbind(pos_data, neg_data)

    if(reshuffle) {

      out_data <- out_data[sample(1:nrow(out_data),
                                  size = nrow(out_data),
                                  replace = FALSE), ]

    }

    simOutcome(out_data, type = 'binary')

  }

# END ------
