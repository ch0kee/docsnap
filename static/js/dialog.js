$(document).ready(function() {
  $('body').prepend('<div id="dialog" style="white-space:pre" title="DocSnap">'+__dlgContent.replace('\\n','\n')+'</div>');
  $( "#dialog" ).dialog({
    autoOpen: true
  , modal: true
  , buttons: [ {text: __dlgButton, click: function() {
      window.location.href = __dlgTarget;
    }}]
  , draggable: false
  , height: 'auto'
  , width: 'auto'
  , resizable: false
  , dialogClass: 'no-close'
  , open: function (event, ui) {
    $('#dialog').css('overflow', 'hidden'); //scrollbar elrejtése
  }
  });

  $("#dialog").focus();
  $("#dialog").dialog("option", "position", "center");
  $(window).resize(function() {
    $("#dialog").dialog("option", "position", "center");
  });
});





