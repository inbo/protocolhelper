% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_protocolsection.R
\name{get_data_media}
\alias{get_data_media}
\title{Internal function to get media and data files}
\usage{
get_data_media(rmd_content, code_subprotocol, tag)
}
\arguments{
\item{rmd_content}{Character vector containing lines read from an Rmarkdown
file}

\item{code_subprotocol}{Character string giving the protocol code from
which a section or sections will be extracted}

\item{tag}{A release tag to identify the version of the protocol in the git
repository}
}
\value{
Silent.
}
\description{
Extracts all paths to files in data and media from \code{rmd_content} and uses git
show to copy paste the files from the sub-protocol to the main protocol
}
\keyword{internal}
