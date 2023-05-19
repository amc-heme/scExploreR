// Status of spinner and spinner message: set to true when shown
var plotsTabSpinnerShown = false;
var plotsTabSpinnerMessage = false;
var timerId = null;
// Tracker used to keep the "this might take a while message" from disappearing 
// if the spinner is hidden and then re-shown in a split second
var restoreMessage = false;

// Displays addional text beneath the main message of the spinner
function showSpinnerMessage(){
    let spinnerMessageElement = $("#plots_preparing_plots_spinner");

    // Message inserted after original spinner message
    spinnerMessageElement.after(
        "<div class = 'spinner_text' style = 'font-weight: normal;' \
        id = 'plots_preparing_plots_spinner_addon'>\
            (This might take a while for large datsets)\
            </div>"
        );

    // Set spinner message status to TRUE
    plotsTabSpinnerMessage = true;
};

// Function to show spinner: involved in multiple Shiny events below
function showPlotsTabSpinner(event){
    // Whenever "plot" or "plot_output_ui" in any module is 
    // invalidated, show a spinner over the main window of the plots tab.

    

    // Conditional event id matches, AND spinner does not already exist
    if ((event.target.id.endsWith("plot_output_ui") || 
        event.target.id.endsWith("plot")) &&
        plotsTabSpinnerShown == false){
        console.log("show spinner code")
        console.log(event.target.id)

        waiter.show({
            id: "object_plots-main_panel",
            html: '<div class="loaderz-02" style="color:#555588;"></div> \
                    <div class="spinner_text" id = "plots_preparing_plots_spinner"> \
                        Preparing plots, please wait... \
                    </div>', 
            color: '#FFFFFF', 
            hideOnRender: true, 
            hideOnError: true, 
            hideOnSilentError: true, 
            image: '',
            fadeOut: false
            });

        // Set spinner status to TRUE
        plotsTabSpinnerShown = true;

        // Spinner container takes up only one screen, and it is 
        // possible to scroll beneath it. To prevent this, plots 
        // are hidden while the spinner is displayed.
        let plots = $('[id$="plot_output_ui"]');
        plots.css('display', 'none');

        // If the spinner is still shown after 10 seconds, add text saying
        // the process might take a while on large datasets
        messageDuration = 10;
        // Display the message immediately if a previous spinner with the message was just hidden
        console.log("State of restoreMessage");
        console.log(restoreMessage);
        if (restoreMessage == true){
            showSpinnerMessage();
        } else {
            // Timer ID is stored: if the spinner is hidden before the indicated time has elapsed
            timerId = 
                setTimeout(
                    function() {
                        // Only create if there is still a spinner and no message has been added
                        if (plotsTabSpinnerShown == true && plotsTabSpinnerMessage == false){
                            showSpinnerMessage();
                        }
                    }, 
                    messageDuration * 1000
                    );
        }
    }
};

// Show spinner on response to the invalidation or computation of plots in the plots tab
$(document).on(
    'shiny:outputinvalidated', 
    function(event){
        showPlotsTabSpinner(event)
    });

$(document).on(
    'shiny:recalculating', 
    function(event){
        showPlotsTabSpinner(event)
    });

$(document).on('shiny:idle', function(){
    console.log("shiny idle")
    // When all computation is complete, hide spinner and show plots
    let plots = $('[id$="plot_output_ui"]');
    waiter.hide("object_plots-main_panel");
    plots.css('display', 'block');

    // Set status variables back to false
    plotsTabSpinnerShown = false;
    plotsTabSpinnerMessage = false;

    // If the timer for adding the message to the plot spinner has not yet gone off, cancel it
    clearTimeout(timerId);

    // If a new spinnner is drawn within 200 miliseconds, restore the message
    console.log("restoreMessage set to true");
    restoreMessage = true;
    setTimeout(() => {
        console.log("restoreMessage set to false");
        restoreMessage = false;
    }, 2000);

    // Delete the addon message from above function if it was created
    $('#plots_preparing_plots_spinner_addon').remove();
});