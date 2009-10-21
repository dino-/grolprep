function constructSelect (opts) {
   // Create the new select control
   var newSelect = document.createElement("select");
   newSelect.id = "questions";
   newSelect.name = "questions";
   newSelect.size = "12";

   // Add the specified options
   for (var i = 0; i < opts.length; i++) {
      newSelect[newSelect.length] = opts[i];
   }

   // Set the first list item selected
   newSelect[0].selected = true;

   // Return this shiny new select widget to the caller
   return newSelect;
}


function populateQuestionsList () {
   // Will use these to figure out what to do
   var radioStudy13 = document.getElementById("study13");
   var radioStudy8 = document.getElementById("study8");

   // Nodes we'll need to orient ourselves and remove things
   var div = document.getElementById("questions-parent");
   var oldControl = document.getElementById("questions");
   var newControl;

   // Build the new control based on current state
   if (radioStudy13.checked)
      newControl = constructSelect( questionOpts13 );
   else if (radioStudy8.checked)
      newControl = constructSelect( questionOpts8 );
   else {
      newControl = document.createElement("textarea");
      newControl.id = "questions";
      newControl.name = "questions";
      newControl.rows = "12";

      // These are good for development, showing off a sample
      // of various things that may occur in problems.
      // May need to remove this later
      newControl.value =
         "1-11B4 3-1A5 3-11B1 3-22C1 3-35E5 3-38E1 3-90O2 3-96P2 8-7A1";
   }

   // Replace old control with new one
   div.replaceChild(newControl, oldControl);

   // Set the focus on the newly-added control
   newControl.focus();
}
