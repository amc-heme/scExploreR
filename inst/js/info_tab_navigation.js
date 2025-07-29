function openTab(event, targetContentId){
  // Identify tabs, and info windows that are shown/hidden by tabs
  const tabLinks = document.getElementsByClassName("tab-button");
  const tabWindows = document.getElementsByClassName("tab-window");

  // Hide all tab-content elements (by removing the "active" class from thenm)
  for (let i = 0; i < tabWindows.length; i++) {
    tabWindows[i].classList.remove("active");
  }

  // Show content associated with selected tab 
  // The ID of target content is specified via targetContentId
  // Content is shown by adding the "active" class

  document.getElementById(targetContentId).classList.add("active");

  // Remove the 'active' class from all tab-link elements
  for (let i = 0; i < tabLinks.length; i++) {
    tabLinks[i].classList.remove("active");
  }

  // Add the 'active' class to the selected tab
  event.currentTarget.classList.add("active");
};