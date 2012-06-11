#' httpHeader
#'
#' Construct an HTTP request header
#'
#' @param ...		key-value pairs
#' @return			a list (or NULL if empty)
#' @export
httpHeader <- function(...) {
	if (missing(...)) return(NULL)
	else return(list(...))
}

#' httpGet
#'
#' Make an HTTP request
#'
#' @param url		character
#' @param header	see \link{httpHeader}
#' @return			text from the response
#' @export
httpGet <- function(url, header=httpHeader(), curl=getCurlHandle(), ...) {
	require(RCurl)
	if (missing(...)) {
		getURLContent(url, httpheader=header)	
	} else {
		args <- list(uri=url, ..., curl=curl, .opts=list(httpheader=header, verbose=TRUE))
		do.call('getForm', args)
	}
}