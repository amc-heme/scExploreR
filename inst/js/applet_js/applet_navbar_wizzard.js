/* Applet Navbar Wizzard: adds the help button and the config file export button
   to the upper right hand corner of the navbar page */

/* The code in this file interfered with the button_wizzard code used in the
   main app. A Different means of implementation will be explored for the applet */

// Identify help button (help button will be added later)
// The tag with ID "help_state" contains the button and the associated content
let help_button = document.getElementById("help_state");
// Identify content (dropdown menu)
let help_content = document.getElementById("dropdown-menu-help");
/* Identify options button ("options_state" gets the full HTML content for the
   dropDownButton with inputId "options") */
let options_button = document.getElementById("options_state");

// Identify navbar header
let header = $('nav.navbar ul.nav');

// Create a new div in the header, aligned to the right of the screen
let right_panel = document.createElement("div");
right_panel.setAttribute("id", "menu_right");
right_panel.style.float = "right";
/* Set height of right_panel to 50 px (height of navbar panel) to allow elements
   to be vertically centered on the panel */
right_panel.style.height = "50px";
// Padding-top: 7px value centers buttons on navbar right_panel
right_panel.style["padding-top"] = "7px";
right_panel.style["padding-bottom"] = "7px";

// Create container to be placed inside right_panel, which will contain the help button
let help_div = document.createElement("div");
// Set ID to allow element to be referenced
help_div.setAttribute("id", "help_button_container");
// Set CSS style properties to align the container to the right
help_div.style.width = "37px";
help_div.style.float = "right";
help_div.style.display = "inline-block";
help_div.style["margin-left"] = "8px";

// Create new container for options button
let options_div = document.createElement("div");
// Set ID
options_div.setAttribute("id", "options_div");
// Add btn-container class (properly centers the button inside)
options_div.setAttribute("class", "btn-container");
// Do not specify float property (results in button appearing to 
// the left of the help button
options_div.style.display = "inline-block";
options_div.style["margin-right"] = "8px";

// Move elements defined above to the navbar header
// Append the new panel to the header
header.append(right_panel);
// Append new config export div to right_panel
right_panel.append(options_div);
// Append the new help button div to right_panel
right_panel.append(help_div);
// Move Help button to the new div
help_div.append(help_button);
// Move options export button to its new div
options_div.append(options_button);
