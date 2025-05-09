% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_protocol_helpers.R
\name{get_protocolnumbers}
\alias{get_protocolnumbers}
\title{Function to list all occupied protocol numbers}
\usage{
get_protocolnumbers(
  protocol_type = c("sfp", "sip", "sap", "sop", "spp"),
  language = c("nl", "en")
)
}
\arguments{
\item{protocol_type}{A character string equal to \code{sfp} (default), \code{sip},
\code{sap}, \code{sop} or \code{spp}.}

\item{language}{Language of the protocol, either \code{"nl"} (Dutch),
the default, or \code{"en"} (English).}
}
\value{
A character vector with occupied protocol numbers for a specific
protocol type
}
\description{
This function will search for protocol numbers in filenames of
Rmarkdown files listed underneath the source folder.
The search will be restricted to files of a given protocol type and given
language.
}
\examples{
\dontrun{
get_protocolnumbers()
}
}
\seealso{
Other utility: 
\code{\link{add_label}()},
\code{\link{get_path_to_protocol}()},
\code{\link{get_protocol_type}()},
\code{\link{get_short_titles}()},
\code{\link{get_version_number}()},
\code{\link{increment_version_number}()}
}
\concept{utility}
