$(document).ready(function() {
  $('body').prepend('<div id="newdialog" title="DocSnap"></div>');
  $( "#newdialog" ).dialog({
    autoOpen: true
  , modal: true
  , buttons: [ {text: "Create New Document", click: function() {
      window.location.href = '/cmd/new';
    }}]
  , draggable: false
  , height: 100
  , width: 300
  , resizable: false
  , dialogClass: 'no-close'
  });

  $("#newdialog").focus();
  $("#newdialog").dialog("option", "position", "center");
  $(window).resize(function() {
    $("#newdialog").dialog("option", "position", "center");
  });
});





