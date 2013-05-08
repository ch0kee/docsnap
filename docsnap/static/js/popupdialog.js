//@ Információs párbeszédablak
DocSnap.showInformationDialog = function(bodyText) {
  this.showDialog(bodyText, [ {text: "Ok", click: function(){} } ]);
};

//@ Eldöntendő kérdés párbeszédablak
DocSnap.showQuestionDialog = function(bodyText, onYes) {
  this.showDialog(bodyText,
    [ {text: "Yes", click: onYes }
    , {text: "No", click: function(){} } ]);
};

//@ Általános párbeszédablak
//@ bodyText: a dialog törzsében lévő szöveg
//@ actions: { text: <GombSzöveg>, click: <EseményKattintáskor> }
//@ szerkezetű objektumokból álló tömb
DocSnap.showDialog = function(bodyText, actions) {
  var buttonActions = [];
  $.each(actions, function(index, value) {
    buttonActions.push( { text: value.text, click: function() {
      $(this).dialog("close");
      $(this).remove();
      value.click();   
    }});
  });
  
 if (!$('#dialog').length) {
    $('body').prepend('<div id="dialog" style="white-space:pre" title="DocSnap">')  
  }
  $('#dialog').text(bodyText);
  
  $( '#dialog' ).dialog({
    autoOpen: true
  , modal: true
  , buttons: buttonActions
  , draggable: false
  , height: 'auto'
  , width: 'auto'
  , resizable: false
  , dialogClass: 'no-close'
  , open: function (event, ui) {
    $('#dialog').css('overflow', 'hidden'); //scrollbar elrejtése
  }
  });
  $('#dialog').focus();
  
  //automatikusan méretezzük
  $("#newdialog").dialog("option", "position", "center");
  $(window).resize(function() {
    $("#newdialog").dialog("option", "position", "center");
  });
};

