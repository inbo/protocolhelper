#' @title The protocolcheck R6 class
#' @description A class which contains all check results.
#' @export
#' @importFrom R6 R6Class
protocolcheck <- R6Class(
  "Protocolcheck",

  public = list(
    protocol_code = NULL,
    path = NULL,
    error = NULL,
    initialize = function(protocol_code) {
      self$protocol_code <- protocol_code
      self$path <- get_path_to_protocol(
        protocol_code = protocol_code)
      invisible(self)  #werkt dat, of niet?  wat is de bedoeling?
    },
    add_error = function(msg) {
      self$error <- c(self$error, msg)
    }
  ),

  active = list(
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
