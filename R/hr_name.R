#hr_name: given the machine-readable name of an input feature, 
#return the human readable name. Used for modifying the titles of plots, which 
#use the machine-readable name by default.
hr_name <- function(machine_readable_name,assay_info){
  #Check each assay to see which assay's machine-readable prefix corresponds to 
  #the server value of the input
  for (assay in assay_info){
    if (grepl(assay$prefix_machine, machine_readable_name)){
      #When a match is found, remove the assay's machine-readable prefix with sub()
      HR <- sub(assay$prefix_machine,"",machine_readable_name)
      #Add the assay's human-readable suffix to the title
      HR <- paste0(HR,assay$suffix_human)
      break
    }
  }
  
  #Return the human-readable name 
  return(HR)
}