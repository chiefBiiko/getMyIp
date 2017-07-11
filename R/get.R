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
      grepl('^(\\d+\\.)+\\d+$', publicIp)) {
    return(publicIp)
  } else if (throw && 
             length(publicIp) != 1L || 
             !grepl('^(\\d+\\.)+\\d+$', publicIp)) {
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
    cmdout <- system2(command='cmd.exe', input='ipconfig | findstr /i ipv4',
                      stdout=TRUE, stderr=TRUE)
    if (throw && length(cmdout) == 0L) {
      stop('error in cmd.exe')
    } else if (length(cmdout) == 0L) {
      return(NULL)
    }
    ipline <- grep('ipv4(?!\\s*$)', cmdout, TRUE, TRUE, TRUE)[1L]
    privateIp <- trimws(regmatches(ipline,
                                   regexpr('(\\d+\\.){3}\\d+\\s*$', ipline)))
    if (length(privateIp) == 1L && 
        grepl('^(\\d+\\.)+\\d+$', privateIp)) {
      return(privateIp)
    } else if (throw && 
               length(privateIp) != 1L ||  # aka > 1L
               !all(grepl('^(\\d+\\.)+\\d+$', privateIp))) {
      stop('oops...')
    } else {
      return(NULL)
    }
  } else {  # unix
    cli <- paste('ifconfig | grep "inet[^6]" | grep -v -F "127.0.0.1" |', 
                 'grep -o -E "([0-9]+\\.){3}[0-9]+" | head -1')
    privateIp <- system2(command='sh', input=cli, stdout=TRUE, stderr=TRUE)
    if (throw && length(privateIp) != 1L) stop('error in bash')
    return(if (length(privateIp) == 1L) privateIp else NULL)
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
      all(grepl('^(\\d+\\.)+\\d+$', IPS))) {
    return(IPS)
  } else if (throw) {
    stop('oops...')
  } else {
    return(NULL)
  }
}
