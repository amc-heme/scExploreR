#' Reactive trigger function
#'
#' # Code adapted from thread by Joe Cheng, the Author of Shiny. From (https://community.rstudio.com/t/shiny-reactivetriggers-in-observeevent/42769). Creates an action button which is programmatically triggered instead of being triggered by the user
#'
#' @return a reactiveValues object with two "methods". Call trigger_name$depend() within a reactive expression or eventReactive/observeEvent to establish a dependency on the trigger, and use trigger_name$trigger() to invalidate the dependent expression(s) programmatically.
#'
#' @noRd
makeReactiveTrigger <- 
  function(){
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}