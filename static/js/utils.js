
//@ handleAjaxError
//@ Esetleges hibaüzenet kezelése
DocSnap.handleAjaxError = function(json) {
  if (json !== undefined && json.error !== undefined) {
    DocSnap.showInformationDialog(json.error);
    return true;
  }
  return false;
}


//@ sendAjaxCommand
//@ szerveroldali parancs hívása JSON visszatérési értékkel
DocSnap.sendAjaxCommand = function (command, arguments, onsuccess) {
  $.ajax({
    type    : 'POST',
    dataType: 'json',
    cache   : false,
    data    : {
      cmd:  command
    , args: JSON.stringify(arguments)
    },
    success : function(json) {
      if (!DocSnap.handleAjaxError(json)) {
        onsuccess(json);
      }
    },
    error : function( xhr, status ) {
      var text =
"The server stopped responding.\n\
\n\
If you want to save your work, close this dialog,\n\
and copy the content of the editor to somewhere else,\n\
using the clipboard.\n\
Do you want me to select the whole document now ?"


      DocSnap.showDialog(text,
    [ {text: "Yes, please select the whole document", click: function() {
var editor = document.getElementById("editor");
var range = rangy.createRange();
range.selectNodeContents(editor);
var sel = rangy.getSelection();
sel.setSingleRange(range);
    } }
    , {text: "No thanks, just close this popup", click: function(){
    
    } } ]);
    },
    complete : function( xhr, status ) {
      //alert("The request is complete!");
    }
  });
};

//@ downloadURL
//@ megadott hivatkozás letöltésének kényszerítése
DocSnap.downloadFile = function  (url) {

  if (!$('#__downloader').length) {
    $('<iframe id="__downloader">').hide().appendTo('body');
  }
  $('#__downloader').attr('src', url);
};


