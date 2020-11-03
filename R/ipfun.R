#' Find name in dataframe
#'
#' Return the matched (ignore case) names in the data obj.
#' @param data - the data object to look into
#' @param name - the text string to search for
#' @keywords find name
#' @export
#' @examples
#' fdn(mtcars, 'p') # Return all the column nam containing 'p'
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

#' Return the length of unique items
#'
#' Return the length of unique items for the given argument
#' @param obj  - the object/list to find the length of unique items
#' @keywords unique length
#' @export
#' @examples
#' lu(df$keyid) # Return the length of unique items in df$keyid
lu <- function (obj) { length(unique( obj )) }

#' List all available functions in a package
#'
#' Return all the functions in the input: package_name
#' @param name - package_name: input package to list its function
#' @keywords list pacakage function
#' @export
#' @examples
#' lsfun("dplyr") # Return all the functions in dplyr package (tree view)
#' lsfun('dplyr', output.list=T) # Return all the functions as list
lsfun <- function(package_name, output.list=F) { 
    pkg_search = paste0("package:", package_name)
    if (output.list) {
        return(ls(pkg_search))
    } else {
        return(lsf.str(pkg_search))
    }
}

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
                rmlist=c(" ", "(", ")", ":", "_", "-", "\\", "/", "'", '"')) {
  # Fix names by removing special chars from R dataframe names
  #     default rmlist is a set of common special sep chars
  dfn <- names(df)
  rm_char_list <- rmlist
  for (char in rm_char_list) {
    dfn <- gsub(as.character(char), "", dfn, fixed=TRUE)
  }
  return(dfn)
}

