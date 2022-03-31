#remove_shiny_inputs: takes the namespace of a module and deletes all of the 
#input bindings associated with that module. Designed to be used with functions 
#that take a variable number of instances of a module.
#The code for this function has been copied from an example at
#https://www.r-bloggers.com/2020/02/shiny-add-removing-modules-dynamically/
#Arguments
#id: the namespace of the module to be removed
#.input: the shiny input. Enter the input without the period 
#(pass 'input' to this argument) 
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}