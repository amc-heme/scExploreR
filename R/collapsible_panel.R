### Collapsible panel UI function ###
#Will create a panel with a header that will toggle between hiding and showing 
#its contents when the user clicks the header. The header of the panel behaves 
#as an action button; therefore, an inputId is required.
#Must include the files "collapsible_panel.css" and "collapsible_panel.js" 
#in the UI function for this to work properly.
collapsible_panel <- function(inputId,label=NULL,active=FALSE,...){
  #Value: a required component of action button and included in the 
  #actionButton source code. I'm not sure why. 
  value <- restoreInput(id = inputId, default = NULL)
  #value is passed to `data-val` in the button tag (also don't know why)
  
  #Use taglist to return button tag for header and div tag for content
  tagList( 
    #Header of panel: built with button tag. The label the user enters will be header text 
    button_html <- tags$button(type="button",
                               #Set the id of the button to inputId
                               id=inputId,
                               #Required component for actionButton behavior
                               `data-val` = value,
                               #Collapsible: starts closed; collapsible active: 
                               #starts open
                               class=if (active==FALSE) "collapsible" else "collapsible active", 
                               #Pass the user-provided label to the header text
                               if (!is.null(label)) as.character(label)),
    
    #Pass all content to div tag
    #If active==TRUE, the style attribute display will be set to "block" to 
    #display the content upon loading
    if (active==TRUE){
      content_html <- div(...,
                          class="content",
                          style="display:block;")
    } else {
      #Otherwise, the default value of none will be used to hide content initially
      content_html <- div(...,
                          class="content")
    }
  )#End taglist
}
###