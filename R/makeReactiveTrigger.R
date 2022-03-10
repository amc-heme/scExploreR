# Reactive trigger function
# Code adapted from thread by Joe Cheng, the Author of Shiny.
# https://community.rstudio.com/t/shiny-reactivetriggers-in-observeevent/42769

# Creates an action button which is programmatically triggered instead of 
# triggered by the user
makeReactiveTrigger <- function(){
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