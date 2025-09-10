#' Pipe operator
#'
#' Re-exports the pipe operator \code{\%>\%} from 'magrittr', allowing you to
#' chain commands together in a readable left-to-right style.
#'
#' @name %>%
#' @rdname pipe
#' @export
#' @importFrom magrittr %>%
#'
#' @param lhs A value or the result of a function call.
#' @param rhs A function call using the value from `lhs` as the first argument.
#'
#' @return The result of evaluating the right-hand side (`rhs`) expression,
#'   where the left-hand side (`lhs`) value is passed as the first argument.
NULL
