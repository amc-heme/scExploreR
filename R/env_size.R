#' env_size
#'
#' a function used to determine the total memory usage of all objects in the 
#' environment. This does not work well with Shiny however since it is currently 
#' unable to find reactive objects; due to this, it is currently unused.
#' 
#' @param env R environment object
#' @param units Units to use for size ("B", "kb", "MB", "GB")
#'
#' @noRd
env_size <- function(env, 
                     units = c("B", "kB", "MB", "GB")) {
  if (!units %in% c("B", "kB", "MB", "GB")){
    stop('Unrecognized specification for units. Please use one of the following: c("B", "kB", "MB", "GB")')
  }
  
  # Mem_df: builds dataframe of all objects in the current environment
  mem_df <- 
    data.frame(
      # ls(sys.frame()): gets all objects in the current environment
      object = ls(env),
      size = 
        unlist(
          # Get size of object for each object in the current environment
          lapply(
            ls(env), 
            function(x) {
              object.size(get(x))
            }
          )
        )
    )
  
  # After forming table of objects in current environment, take the sum of
  # the memory footprint across all objects
  if (nrow(mem_df) > 0){
    bytes <- 
      mem_df |> 
      select(size) |> 
      sum()
    
    # Output size is in bytes. Divide to get size in other 
    # units if specified
    if (units == "B"){
      divisor = 1
    } else if (units == "kB"){
      divisor = 1028
    } else if (units == "MB"){
      divisor = 1028^2 
    } else if (units == "GB"){
      divisor = 1028^3
    }
    
    bytes_hr <- bytes/divisor
    
    print(glue("Total RAM usage: {bytes_hr} {units}."))
  } else {
    print("No objects found in specified environment.")
  }
}