# getMyIp

#' Get your public ip address as v4
#'
#' @param throw Logical. In case your ip address could not be resolved
#' should an error be thrown?
#' @return Character. Public ip address; if \code{!throw} \code{NULL}
#' on failure.
#'
#' @details Queryies ichanhazip.com through SSL.
#'
#' @export
publicV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  res <- curl::curl_fetch_memory('https://icanhazip.com', curl::new_handle())
  publicIp <- trimws(rawToChar(res$content))
  if (length(publicIp) == 1L && greplV4(publicIp)) {
    return(publicIp)
  } else if (throw && length(publicIp) != 1L || !greplV4(publicIp)) {
    stop('oops...', publicIp)
  } else {
    return(NULL)
  }
}

#' Get your private ip address v4
#'
#' @param throw Logical. In case your ip address could not be resolved
#' should an error be thrown?
#' @return Character. Private ip address; if \code{!throw} \code{NULL}
#' on failure.
#'
#' @details Calls \code{ipconfig} on Windows, \code{ifconfig} on OSX, in both
#' cases followed by regex parsing; \code{hostname -I} on Linux with out any
#' further parsing.
#'
#' @export
privateV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  if (grepl('win', .Platform$OS.type, TRUE)) {
    return(privateV4Win(throw=throw))
  } else if (grepl('(os\\s*x)|(darwin)', Sys.info()['sysname'], TRUE)) {
    return(privateV4OSX(throw=throw))
  } else if (grepl('(unix)|(linux)', .Platform$OS.type)) {
    return(privateV4Lin(throw=throw))
  }
}

#' Get a list of your private and public ipv4
#'
#' @param throw Logical. In case your ip addresses could not be resolved
#' should an error be thrown?
#' @return Named list; if \code{!throw} \code{NULL} on failure.
#'
#' @export
listV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  return(list(public=publicV4(throw=throw), private=privateV4(throw=throw)))
}
