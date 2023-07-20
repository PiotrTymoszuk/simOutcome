# S3 OOP interface for the 'simOutcome' class

# summary method -------

#' Model Statistic Summary for the simOutcome Objects.
#'
#' @description
#' Computes model fit stats for the `simOutcome` objects.
#'
#' @details
#' The fit stats are model type-specific.
#' See \code{\link{binary_stats}} for simulated binary classifiers.
#'
#' @param object an object of the `simOutcome` class.
#' @param ... extra arguments, currently none.
#'
#' @export summary.simOutcome
#' @export

  summary.simOutcome <- function(object, ...) {

    stopifnot(is_simOutcome(object))

    mod_type <- attr(object, 'type')

    switch(mod_type,
           regression = NULL,
           binary = binary_stats(object),
           multi_class = NULL)

  }

# END ------
