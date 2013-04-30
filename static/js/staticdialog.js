$(document).ready(function() {
  $('body').prepend('<div id="staticdialog" style="white-space:pre" title="DocSnap">'+__dlgContent.replace('\\n','\n')+'</div>');
  $( '#staticdialog' ).dialog({
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
    $('#staticdialog').css('overflow', 'hidden'); //scrollbar elrejtése
  }
  });

  $('#staticdialog').focus();
  $('#staticdialog').dialog('option', 'position', 'center');
  $(window).resize(function() {
    $('#staticdialog').dialog('option', 'position', 'center');
  });
});





