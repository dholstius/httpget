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
#' @param curl		RCurl handle (can reuse)
#' @param cache		whether to use the local cache
#' @return			text from the response
#' @export
httpGet <- function(url, header=httpHeader(), curl=getCurlHandle(), ..., cache=TRUE) {
	require(RCurl)
	key <- cacheKey(url)
	response <- cacheGet(key)
	if (is.null(response)) {
		tryCatch({
			if (missing(...)) {
				response <- getURLContent(url, httpheader=header)	
			} else {
				args <- list(uri=url, ..., curl=curl, .opts=list(httpheader=header, verbose=TRUE))
				response <- do.call('getForm', args)
			}
			cachePut(key, response)
			attr(response, 'cacheKey') <- key
		}, error=warning)
	}
	return(response)
}