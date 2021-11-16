// Define the opening/closing behavior of the collapsible panel ////////////////
var coll = document.getElementsByClassName("collapsible");
var i;

for (i = 0; i < coll.length; i++) {
  // When clicked, toggle the display property to switch
  // betwen showing/hiding the panel content
  coll[i].addEventListener("click", function() {
    //Toggle the class of the object
    //When active, the header displays with a down arrow
    //When not active, the header displays with an arrow
    //pointing to the side.
    this.classList.toggle("active");
    //Find the content (sibling elements) and toggle the
    //display properties between 'none' (hidden) and
    //'block' (visible)
    var content = this.nextElementSibling;
    if (content.style.display === "block") {
      content.style.display = "none";
    } else {
      content.style.display = "block";
    }
  });
}

// Create a custom input container for the collapsible panel ///////////////////
// The panel header will function like an action button.
// This is used to ensure all uiOutput() elements render when
// the user opens the panel.

// Create instance of Shiny InputBinding
var panelActionButton = new Shiny.InputBinding();

// Add required methods to the InputBinding object
$.extend(panelActionButton, {
  // Find: returns the HTML element as a jQuery object to be read by JavaScript
  find: function(scope){
    // The header ("collapsible" class) will be the input container
    return $(scope).find(".collapsible")
  }, //Methods must be separated with commas

  //getId: returns the ID of the collapsible panel as defined in its HTML
  //The ID must be specified as an attribute in the R function creating the
  //panel, taking an inputId argument for consistency.
  getId: function(el){
    return el.id;
  },

  //Returns the value of the object as defined on the server
  getValue: function(el){
    //The value of the button, or zero if not yet defined
    return $(el).data("val") || 0;
  },

  //Sets the value on the server to the one provided in the argument
  setValue: function(el, value){
    $(el).data("val", value);
  },

  //Set up event listeners
  subscribe: function(el,callback){
    //Respond when the panel header is clicked, like an action button
    //$(el): the jQuery object for the panel
    //el: the DOM object for the panel
    $(el).on(
      "click.panelActionButton",
      function(){
        const $el = $(this);
        //Value of panel before it is clicked: same as actionButton
        //Equal to its current value on the server, or zero at startup
        const val = $el.data("val") || 0;
        //Increment value by one when clicked
        $el.data("val",val+1);
        //Callback: not needed for this event type
        callback(false);
      } //End event function
    ); //End el.on()
  }, //End subscribe

  //Code to remove event listeners
  unsubscribe: function(el){
    $(el).off(".panelActionButton");
  },

  //Describes the state of the element
  getState: function(el){
    //Use getValue method defined above to yield the value
    return { value: this.getValue(el) };
  },

  //getRatePolicy: not needed for this element due to event type
  getRatePolicy: function(){
    return {
      // This is defined but not used
      policy: 'debounce',
      delay: 200
    };
  }
  }); //End extend()

//Register the input binding defined above to make it useable by Shiny
Shiny.inputBindings.register(panelActionButton, "shiny.panelActionButton");
//Second argument is optional and used to change the priority if there is a
//clash with other input types, which is unlikely.
//See https://shiny.rstudio.com/articles/building-inputs.html
