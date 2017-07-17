# getMyIp utils

#' @keywords internal
greplV4 <- function(x) {
  return(grepl(paste0('^((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}',
                      '(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])$'), x))
}

#' @keywords internal
greplLocalhost <- function(x) {
  return(grepl('127.0.0.1', x, fixed=TRUE))
}

#' @keywords internal
privateV4Win <- function(throw=TRUE) {
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
  if (length(privateIp) == 1L && greplV4(privateIp) &&
      !greplLocalhost(privateIp)) {
    return(privateIp)
  } else if (throw && length(privateIp) != 1L || !greplV4(privateIp) ||
             greplLocalhost(privateIp)) {
    stop('oops...something went wrong...: ', privateIp)
  } else {
    return(NULL)
  }
}

#' @keywords internal
privateV4OSX <- function(throw=TRUE) {
  cli <- paste('ifconfig | grep "inet[^6]" | grep -v -F "127.0.0.1" |',
               'grep -o -E "([0-9]+\\.){3}[0-9]+" | head -1')
  privateIp <- system2(command='sh', input=cli, stdout=TRUE, stderr=TRUE)
  if (length(privateIp) == 1L && greplV4(privateIp) &&
      !greplLocalhost(privateIp)) {
    return(privateIp)
  } else if (throw && length(privateIp) != 1L || !greplV4(privateIp) ||
            greplLocalhost(privateIp)) {
    stop('oops...something went wrong...: ', privateIp)
  } else {
    return(NULL)
  }
}

#' @keywords internal
privateV4Lin <- function(throw=TRUE) {
  privateIp <- trimws(system2(command='sh', input='hostname -I',
                              stdout=TRUE, stderr=TRUE))
  if (length(privateIp) == 1L && greplV4(privateIp) &&
      !greplLocalhost(privateIp)) {
    return(privateIp)
  } else if (throw && length(privateIp) != 1L || !greplV4(privateIp) ||
             greplLocalhost(privateIp)) {
    stop('oops...something went wrong...: ', privateIp)
  } else {
    return(NULL)
  }
}
