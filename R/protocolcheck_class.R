#' @title The protocolcheck R6 class
#'
#' @description A class that collects and shows all check results.
#'
#' @export
#' @importFrom R6 R6Class
#'
protocolcheck <- R6Class(
  "Protocolcheck",

  public = list(
    #' @field protocol_code Character string giving the protocol code.
    protocol_code = NULL,

    #' @field path Character string giving the relative path to the protocol.
    path = NULL,

    #' @field error Character vector containing all errors found in the protocol
    error = NULL,

    #' @description Create a new `Protocolcheck` object.
    #' @param protocol_code Character string giving the protocol code.
    #' @return A new `Protocolcheck` object
    initialize = function(protocol_code) {
      self$protocol_code <- protocol_code
      self$path <- get_path_to_protocol(
        protocol_code = protocol_code)
      invisible(self)
    },

    #' @description Add a new error to the Protocolcheck object.
    #' @param msg Error message to be added.
    add_error = function(msg) {
      self$error <- c(self$error, msg)
    }
  ),

    #' @description Give error report from Protocolcheck object.
    #' @param fail Should an error be dropped if the report contains errors?
    #' @return An error report (and if desired an error is dropped).
    check = function(fail) {
      if (length(self$error) > 0) {
        output <- c(
          paste0("Errors in protocol ", self$protocol_code, ":"),
          self$error
        )
        cat(output, sep = "\n- ")
        if (fail) {
          stop("Some problems occur in the protocol: see errors above.")
        }
      } else {
        cat("Well done! No problems found.")
      }
    }
  )
)
