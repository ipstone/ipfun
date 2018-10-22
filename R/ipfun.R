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

#' Create folder similarly as mkdir -p
#'
#' @param folder - the path of the folder to be created
#' @export
#' @examples
#' mkdirp("./folder_to_create")
mkdirp <- function(folder) if (!dir.exists(folder)) dir.create(folder, recursive=T)

#' Return fixed name of input dataframe
#'
#' Fix the input dataframe names by removing special characters such as "_"
#' @param df - the input dataframe
#' @param rmlist - special chars to remove, default: " ", "(", ")", ":", "_", "-", "\\", "/"
#' @keywords fix name
#' @export
#' @examples
#' names(dataframe_tobe_fixed) = ipr_fixname(dataframe_tobe_fixed)
ipr_fixname <- function(df, 
                rmlist=c(" ", "(", ")", ":", "_", "-", "\\", "/")) {
  # Fix names by removing special chars from R dataframe names
  #     default rmlist is a set of common special sep chars
  dfn <- names(df)
  rm_char_list <- rmlist
  for (char in rm_char_list) {
    dfn <- gsub(as.character(char), "", dfn, fixed=TRUE)
  }
  return(dfn)
}

