\name{httpGet}
\alias{httpGet}
\title{httpGet}
\usage{
  httpGet(url, header = httpHeader(),
    curl = getCurlHandle(), ..., cache = TRUE)
}
\arguments{
  \item{url}{character}

  \item{header}{see \link{httpHeader}}

  \item{curl}{RCurl handle (can reuse)}

  \item{cache}{whether to use the local cache}
}
\value{
  text from the response
}
\description{
  Make an HTTP request
}

