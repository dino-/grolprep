function setInitialSelections () {
   var select13 = document.getElementById("select-study13");
   var select8 = document.getElementById("select-study8");

   select13.options[0].selected = true;
   select8.options[0].selected = true;
}


function setRadio (suffix) {
   var radio = document.getElementById("radio-" + suffix);

   radio.checked = true;
}
