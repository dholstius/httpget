\name{httpGet}
\alias{httpGet}
\title{httpGet}
\usage{
  httpGet(url, header = httpHeader(),
    curl = getCurlHandle(), ...)
}
\arguments{
  \item{url}{character}

  \item{header}{see \link{httpHeader}}
}
\value{
  text from the response
}
\description{
  Make an HTTP request
}

