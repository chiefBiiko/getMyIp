# getMyIp utils

#' @keywords internal
greplV4 <- function(x) {
  return(grepl(paste0('^((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}', 
                      '(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])$'), x))
}

#' @keywords internal
greplV6 <- function(x) {
  return(grepl(paste0('^(([0-9a-fA-F]{1,4}\\:){7,7}[0-9a-fA-F]{1,4}|', 
                      '([0-9a-fA-F]{1,4}\\:){1,7}\\:|', 
                      '([0-9a-fA-F]{1,4}\\:){1,6}\\:[0-9a-fA-F]{1,4}|', 
                      '([0-9a-fA-F]{1,4}\\:){1,5}(\\:[0-9a-fA-F]{1,4}){1,2}|', 
                      '([0-9a-fA-F]{1,4}\\:){1,4}(\\:[0-9a-fA-F]{1,4}){1,3}|', 
                      '([0-9a-fA-F]{1,4}\\:){1,3}(\\:[0-9a-fA-F]{1,4}){1,4}|', 
                      '([0-9a-fA-F]{1,4}\\:){1,2}(\\:[0-9a-fA-F]{1,4}){1,5}|', 
                      '[0-9a-fA-F]{1,4}\\:((\\:[0-9a-fA-F]{1,4}){1,6})|', 
                      '\\:((\\:[0-9a-fA-F]{1,4}){1,7}|\\:)|', 
                      'fe80\\:(\\:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|', 
                      '\\:\\:(ffff(\\:0{1,4}){0,1}\\:){0,1}((25[0-5]|(2[0-4]|', 
                      '1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|', 
                      '(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|', 
                      '([0-9a-fA-F]{1,4}\\:){1,4}\\:((25[0-5]|(2[0-4]|', 
                      '1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|', 
                      '1{0,1}[0-9]){0,1}[0-9]))$'), x))
}

#' @keywords internal
greplLocalhost <- function(x) {
  return(grepl('(127.0.0.1)|(0\\:0\\:0\\:0\\:0\\:0\\:0\\:1)|(\\:\\:1)', x))
}

#' @keywords internal
grepV6 <- function(x) {
  V6regex <- paste0('(([0-9a-fA-F]{1,4}\\:){7,7}[0-9a-fA-F]{1,4}|', 
                    '([0-9a-fA-F]{1,4}\\:){1,7}\\:|', 
                    '([0-9a-fA-F]{1,4}\\:){1,6}\\:[0-9a-fA-F]{1,4}|', 
                    '([0-9a-fA-F]{1,4}\\:){1,5}(\\:[0-9a-fA-F]{1,4}){1,2}|', 
                    '([0-9a-fA-F]{1,4}\\:){1,4}(\\:[0-9a-fA-F]{1,4}){1,3}|', 
                    '([0-9a-fA-F]{1,4}\\:){1,3}(\\:[0-9a-fA-F]{1,4}){1,4}|', 
                    '([0-9a-fA-F]{1,4}\\:){1,2}(\\:[0-9a-fA-F]{1,4}){1,5}|', 
                    '[0-9a-fA-F]{1,4}\\:((\\:[0-9a-fA-F]{1,4}){1,6})|', 
                    '\\:((\\:[0-9a-fA-F]{1,4}){1,7}|\\:)|', 
                    'fe80\\:(\\:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|', 
                    '\\:\\:(ffff(\\:0{1,4}){0,1}\\:){0,1}((25[0-5]|(2[0-4]|', 
                    '1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|', 
                    '(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|', 
                    '([0-9a-fA-F]{1,4}\\:){1,4}\\:((25[0-5]|(2[0-4]|', 
                    '1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|', 
                    '1{0,1}[0-9]){0,1}[0-9]))')
  return(regmatches(x, regexpr(V6regex, x)))
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
    stop('oops...something went wrong...')
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
    stop('oops...something went wrong...')
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
    stop('oops...something went wrong...')
  } else {
    return(NULL)
  }
}

#' @keywords internal
privateV6Win <- function(throw=TRUE) {
  cmdout <- system2(command='cmd.exe', input='ipconfig | findstr /i ipv6',
                    stdout=TRUE, stderr=TRUE)
  if (throw && length(cmdout) == 0L) {
    stop('error in cmd.exe')
  } else if (length(cmdout) == 0L) {
    return(NULL)
  }
  ipline <- grep('ipv6(?!\\s*$)', cmdout, TRUE, TRUE, TRUE)[1L]
  privateIp <- grepV6(ipline)
  if (length(privateIp) == 1L && greplV6(privateIp) && 
      !greplLocalhost(privateIp)) {
    return(privateIp)
  } else if (throw && length(privateIp) != 1L || !greplV6(privateIp) ||
             greplLocalhost(privateIp)) {
    stop('oops...something went wrong...')
  } else {
    return(NULL)
  }
}

#' @keywords internal
privateV6OSX <- function(throw=TRUE) {
  cli <- 'ifconfig | grep "inet6" | grep -v "\\s::1.*"'
  cmdout <- system2(command='sh', input=cli, stdout=TRUE, stderr=TRUE)
  if (throw && length(cmdout) == 0L) stop('error in sh')
  privateIp <- grepV6(cmdout[1L])
  if (length(privateIp) == 1L && greplV6(privateIp) && 
      !greplLocalhost(privateIp)) {
    return(privateIp)
  } else if (throw && length(privateIp) != 1L || !greplV6(privateIp) ||
             greplLocalhost(privateIp)) {
    stop('oops...something went wrong...')
  } else {
    return(NULL)
  }
}

#' @keywords internal
privateV6Lin <- privateV6OSX
