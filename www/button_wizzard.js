/* Button Wizzard: adds the help button to the upper right hand corner of the navbar page*/
/* Identify help button*/
/* The tag with ID "help_state" contains the button and the associated content */
let help_button = document.getElementById("help_state");
/* Identify content (dropdown menu)*/
let help_content = document.getElementById("dropdown-menu-help");
/* Identify navbar header*/
let header = $('.navbar > .container-fluid');

/*Create a new div in the header, aligned to the right of the screen*/
let new_box = document.createElement("div");
/* Set ID to allow element to be referenced*/
new_box.setAttribute("id","menu_right");
/* Set CSS style properties to align the container to the right */
new_box.style.width="37px";
new_box.style.float="right";

/*Append the new div to the header */
header.append(new_box);
/* Move Help button to the new div */
new_box.append(help_button);

