# getMyIp

#' Get your public ip address
#'
#' @param version Integer. Ip version \code{4} or \code{6}. Caution: see below.
#' @param throw Logical. In case your ip address could not be resolved
#' should an error be thrown?
#' @return Character. Public ip address; if \code{!throw} \code{NULL}
#' on failure.
#'
#' @details CAUTION: forcing a specific version is unreliable - ipv4 and ipv6
#' seem to be mutually exclusively resolve... In case your desired version
#' could not be resolved, you can however get the version you did not demand.
#' Hehe. Queryies ichanhazip.com through SSL.
#'
#' @export
publicIp <- function(version=4L, throw=TRUE) {
  stopifnot(is.logical(throw), version %in% c(4L, 6L))
  h <- curl::new_handle()
  curl::handle_setopt(h, IPRESOLVE=ifelse(version == 4L, 1L, 2L))
  res <- curl::curl_fetch_memory('https://icanhazip.com', h)
  publicIp <- trimws(rawToChar(res$content))
  if (length(publicIp) == 1L &&
      ifelse(version == 4L, greplV4(publicIp), greplV6(publicIp))) {
    return(publicIp)
  } else if (throw && (length(publicIp) != 1L ||
               ifelse(version == 4L, !greplV4(publicIp), !greplV6(publicIp)))) {
    stop('oops...', publicIp)
  } else {
    return(NULL)
  }
}

#' Get your private ip address
#'
#' @param version Integer. Ip version \code{4} or \code{6}.
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
privateIp <- function(version=4L, throw=TRUE) {
  stopifnot(is.logical(throw), version %in% c(4L, 6L))
  if (grepl('win', .Platform$OS.type, TRUE)) {
    if (version == 4L) {
      return(privateV4Win(throw=throw))
    } else {
      return(privateV6Win(throw=throw))
    }
  } else if (grepl('(os\\s*x)|(darwin)', Sys.info()['sysname'], TRUE)) {
    if (version == 4L) {
      return(privateV4OSX(throw=throw))
    } else {
      return(privateV6OSX(throw=throw))
    }
  } else if (grepl('(unix)|(linux)', .Platform$OS.type)) {
    if (version == 4L) {
      return(privateV4Lin(throw=throw))
    } else {
      return(privateV6Lin(throw=throw))
    }
  }
}

#' Get a list of your private and public ip
#'
#' @param version Integer. Ip version \code{4} or \code{6}.
#' @param throw Logical. In case your ip addresses could not be resolved
#' should an error be thrown?
#' @return Named list; if \code{!throw} \code{NULL} on failure.
#'
#' @export
listIps <- function(version=4L, throw=TRUE) {
  stopifnot(is.logical(throw), version %in% c(4L, 6L))
  IPS <- list(public=publicIp(version=version, throw=throw),
              private=privateIp(version=version, throw=throw))
  return(IPS)
}
