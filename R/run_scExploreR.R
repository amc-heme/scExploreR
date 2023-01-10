#' scExploreR app
#'
#' Initializes the main scExploreR app.
#' 
#' @param port specify a port for launching the browser. This is required to
#' run several instances of the browser at the same IP address. See "how to run scExploreR" for more information.
#'
#' @examples
#' run_scExploreR(port = 5320)
run_scExploreR <- 
  function(
    port = NULL
    ){
    runApp(
      # Defaults to the current directory of the repo
      appDir = ".",
      # Port: use default if not defined by user
      port = if (!is.null(port)) port else getOption("shiny.port")
    )
  }