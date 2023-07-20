# Constructors and class identify checker for the simOutcome class

#' Create an Instance of the simOutcome Class.
#'
#' @description
#' Creates an object of the `simOutcome` class.
#'
#' @details
#' Technically a data frame with two columns, `outcome` and `predicted`, which
#' store the actual and model-predicted values of the outcome variable.
#' It has and additional attribute `type` which specifies the type of the
#' simulated model (`regression`, `binary` or `multi_class`).
#' The method `summary()` was defined for the `simOutcome` class which returns
#' a data frame with fit statistics appropriate for the type of the model.
#'
#' @param x a data frame with the columns `outcome` and `predicted`.
#' @param type type of the classifier: `regression` (default), `binary`
#' or `multi_class`.
#' @param ... extra arguments, currently none.
#'
#' @export

  simOutcome <- function(x,
                         type = c('regression', 'binary', 'multi_class'),
                         ...) {

    x_err <- paste("'x' has to be a data frame with the columns",
                   "'outcome' and 'predicted'.")

    if(!is.data.frame(x)) stop(x_err, call. = FALSE)

    if(any(!c('outcome', 'predicted') %in% names(x))) stop(x_err, call. = FALSE)

    type <- match.arg(type[1], c('regression', 'binary', 'multi_class'))

    structure(x,
              class = c('simOutcome', class(x)),
              type = type)

  }

#' @rdname simOutcome
#' @export

  is_simOutcome <- function(x) inherits(x, 'simOutcome')

# END ------

