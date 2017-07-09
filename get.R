# getMyIp

#' Get your public IP address as v4
#' 
#' @param throw Logical. In case your Ip address could not be resolved 
#' should an error be thrown?
#' @return Character. Public ip address; if \code{!throw} \code{NULL} 
#' on failure.
#' 
#' @details Queryies ichanhazip.com through SSL for your public Ip v4.
#' 
#' @export
publicV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  res <- curl::curl_fetch_memory('https://icanhazip.com', 
                                 curl::new_handle())
  publicIp <- trimws(rawToChar(res$content))
  if (length(publicIp) == 1L && 
      grepl('^(\\d+\\.?)+$', publicIp)) {
    return(publicIp)
  } else if (throw && 
             length(publicIp) != 1L || 
             !grepl('^(\\d+\\.?)+$', publicIp)) {
    stop('oops...')
  } else {
    return(NULL)
  }
}

#' Get your private IP address as v4
#' 
#' @param throw Logical. In case your Ip address could not be resolved 
#' should an error be thrown?
#' @return Character. Private ip address; if \code{!throw} \code{NULL} 
#' on failure.
#' 
#' @details Calls \code{ipconfig} on Windows and \code{ifconfig} on all
#' other OS.
#' 
#' @export
privateV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  if (grepl('win', .Platform$OS.type, TRUE)) {
    cmdout <- system2('cmd.exe', input='ipconfig | findstr /i ipv4', 
                      stdout=TRUE, stderr=TRUE)
    ipline <- grep('ipv4\\s+address', cmdout, ignore.case=TRUE, value=TRUE)
    privateIp <- regmatches(ipline, regexpr('(\\d+\\.?)+\\s*$', ipline))
    if (length(privateIp) == 1L && 
        grepl('^(\\d+\\.?)+$', privateIp)) {
      return(privateIp)
    } else if (throw && 
               length(privateIp) != 1L || 
               !grepl('^(\\d+\\.?)+$', privateIp)) {
      stop('oops...')
    } else {
      return(NULL)
    }
  } else {
    #..#
    NULL
  }
}

#' Get a list of your private and public IPv4
#' 
#' @param throw Logical. In case your Ip addresses could not be resolved 
#' should an error be thrown?
#' @return Named list; if \code{!throw} \code{NULL} on failure.
#' 
#' @export
listV4 <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  IPS <- list(public=publicV4(throw=throw), private=privateV4(throw=throw))
  if (length(IPS) == 2L && 
      all(grepl('^(\\d+\\.?)+$', IPS))) {
    return(IPS)
  } else if (throw) {
    stop('oops...')
  } else {
    return(NULL)
  }
}