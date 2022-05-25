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
    # print = function() {
    #   output <- c(
    #     paste("protocol_code =", self$protocol_code)
    #   )
    #   cat()
    # },
    add_error = function(msg) {
      self$error <- c(self$error, msg)
    }
  )#,

  # private = list(
  #   path = character(0) #private of public?  en idem voor error, en wat is active?
  # )
)
