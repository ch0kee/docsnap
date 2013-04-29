//var DocSnap.__sentContent = [];
//var DocSnap.__sentEdits = [];
DocSnap.__CANCOMMIT = true;


$(document).ready(function() {
  //legyen szerkeszthető
  $('#editor').attr('contenteditable',true);
  
  //formázó eszköztár
  var boldApplier = rangy.createCssClassApplier('ds_bold', { applyToEditableOnly: true, normalize: true });
  var italicApplier = rangy.createCssClassApplier('ds_italic', { applyToEditableOnly: true, normalize: true });

  $('#bold').mousedown(function(e) {
    boldApplier.toggleSelection();
    modified = true;
    e.preventDefault();
  });

  $('#italic').mousedown(function(e) {
    italicApplier.toggleSelection();
    modified = true;
    e.preventDefault();
  });
  
  //a TAB-ra szúrjunk be négy szóközt
  $('#editor').on({
    keydown: function(ev) {
      var code = ev.keyCode || ev.which;
      if (code == 9) {
        pasteHtmlAtSelection('&nbsp;&nbsp;&nbsp;&nbsp;');
        ev.preventDefault();
      }
    }
  });

  

});






