### Functions for constructing lists from the D0/D30 patient data ###
### Build patient list
#Taking a vector of valid patients as input, build_patient_list() compiles the 
#choices into a categorized list for display in the dropdown menu
#This is currently hard-coded for the d0/d30 object
build_patient_list <- function(valid_patients){
  #List of valid patients: based on group of patient in dropdown menu
  valid_patients_categories = list(
    `d0/d30` = list(),
    `Dx/Rl` = list(),
    `Normal Bone Marrow` = list()
  )
  
  #Sort valid patients into above framework
  for (patient in valid_patients) {
    #Iterate through valid_patients vector and place each choice in the relevant category
    if (patient %in% c("1325", "1650", "1510", "1526", "1378", "1724")) {
      #Append above patient ids to d0/d30 category
      valid_patients_categories$`d0/d30` <-
        append(valid_patients_categories$`d0/d30`, patient)
    } else if (patient %in% c("1261", "1467", "719")) {
      #Append above patient ids to Dx/Rl category
      valid_patients_categories$`Dx/Rl` <-
        append(valid_patients_categories$`Dx/Rl`, patient)
    } else if (patient %in% c("BMMC_1", "BMMC_2", "BMMC_3")) {
      #Append above patient (sample) ids to normal bone marrow category
      valid_patients_categories$`Normal Bone Marrow` <-
        append(valid_patients_categories$`Normal Bone Marrow`, patient)
    }
  }
  
  #Return the categorized list of valid patients
  valid_patients_categories
}

### Sort patient list
#For the list of patients that display in the dropdown for the d0/d30 object,
#Sort each sublist so the patients appear in order.
sort_patient_list <- function(patient_list){
  #Patients in d0/d30 and Dx/Rl are numeric values that can be sorted easily
  patient_list$`d0/d30` <- patient_list$`d0/d30` |> 
    as.numeric() |> 
    sort() |> 
    as.character() |> #Convert back to character values to avoid issues with further subsetting 
    as.list()
  patient_list$`Dx/Rl` <- patient_list$`Dx/Rl` |> 
    as.numeric() |> 
    sort() |> 
    as.character() |> 
    as.list()
  #Normal bone marrow column consists of character IDs that are sorted properly with sort()
  patient_list$`Normal Bone Marrow` <- patient_list$`Normal Bone Marrow` |> 
    as.character() |> 
    sort() |> 
    as.list()
  #Return patient list
  patient_list
}