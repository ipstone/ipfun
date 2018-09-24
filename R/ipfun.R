#' Find name in dataframe
#'
#' Return the matched (ignore case) names in the data obj.
#' @param data - the data object to look into
#' @param name - the text string to search for
#' @keywords find name
#' @export
#' @examples
#' fdn(mtcars, 'p') # Return all the column names containing 'p'

fdn <- function(data, name) {print(grep(name, names(data), value=TRUE, ignore.case=TRUE))}

#' Find name in global env
#'
#' Return the matched (ignore case) names in the global env.
#' @param name - the text string to search for
#' @keywords find name
#' @export
#' @examples
#' ldn('d') # Return all the column names containing 'd'

ldn <- function (name) { ls(envir=.GlobalEnv, pattern=name) }