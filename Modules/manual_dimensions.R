#Manual Plot Dimensions Module
#Creates a textbox and a slider to manually adjust the width and height

manual_dimensions_ui <- function(id){
  #Namespace function: prevents conflicts with IDs defined in other modules
  ns <- NS(id)

}

manual_dimensions_server <- function(id
                                     ){
  moduleServer(id,
               function(input,output,session){
             
             })
}