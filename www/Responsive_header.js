// Dynamically changes the padding of "body" to match the size of the header

function dynamicResize(){
  // Define value of spacing (difference in window heights plus 15)
  var dynamicSpacing = $("nav").position().top + $("nav").height() + 15;
  // Apply value to <body>
  $("body").css("padding-top", dynamicSpacing);
}

// Run function upon loading page, and whenever the window is resized
$(document).ready(function(){
  dynamicResize()
});

$(window).resize(function(){
  dynamicResize()
});
