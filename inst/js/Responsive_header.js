// Dynamically changes the padding and height of "body" to match the size 
// of the header

function dynamicResize(){
  // Define value of spacing (difference in window heights plus 15)
  const dynamicSpacing = $("nav").position().top + $("nav").height() + 15;

  // Define height of main window: size of the window, minus the spacing computed above
  const body_height = window.innerHeight - dynamicSpacing;

  // Store the body height in a CSS variable, used to set the col-sm-* classes 
  // in ../css/fancy_scroll.css
  document.documentElement.style.setProperty('--body-height', `${body_height}px`);

  // Apply dynamic spacing value to <body>
  $("body").css("padding-top", dynamicSpacing);
}

// Run function upon loading page, and whenever the window is resized
$(document).ready(function(){
  dynamicResize()
});

$(window).resize(function(){
  dynamicResize()
});
