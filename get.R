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
public <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  res <- curl::curl_fetch_memory('https://icanhazip.com', 
                                 curl::new_handle())
  publicIp <- sub('\n$', '', rawToChar(res$content))
  if (throw && length(publicIp) != 1L) stop('oops...')
  return(if (length(publicIp)) publicIp else NULL)
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
private <- function(throw=TRUE) {
  stopifnot(is.logical(throw))
  if(grepl('win', .Platform$OS.type, TRUE)) {
    cmdout <- system2('cmd.exe', input='ipconfig | findstr /i ipv4', 
                      stdout=TRUE, stderr=TRUE)
    ipline <- grep('ipv4\\s+address', cmdout, ignore.case=TRUE, value=TRUE)
    privateIp <- regmatches(ipline, regexpr('(\\d+\\.{0,1})+\\s*$', ipline))
    if (throw && length(privateIp) != 1L) stop('oops...')
    return(if (length(privateIp)) privateIp else NULL)
  } else {
    
  }
}