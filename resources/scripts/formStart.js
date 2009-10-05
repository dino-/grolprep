function populateQuestionsList () {
   // Fish out the state of the radio button controls

   var elementSet = document.getElementById("study13");
   var opts;

   if (elementSet.checked) opts = questionOpts13;
   else opts = questionOpts8;


   // Then load the appropriate lists of questions into the list control

   var questionList = document.getElementById("questions");

   // Delete the options there now
   questionList.length = 0;

   // Add the new options
   for (var i = 0; i < opts.length; i++) {
      questionList[questionList.length] = opts[i];
   }

   // Set the first list item selected
   questionList[0].selected = true;
}
