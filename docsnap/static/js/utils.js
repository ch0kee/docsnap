
//@ getFirstRange
//@ Visszaadja az első, összefüggő kiválasztást
//@ (Pl. Firefox estén lehetséges több kijelölés)
DocSnap.getFirstRange = function () {
  var selection = rangy.getSelection();
  return selection.rangeCount ? selection.getRangeAt(0) : null;
};

//@ pasteHtmlAtSelection
//@ HTML kód beszúrása a kiválasztás helyére
DocSnap.pasteHtmlAtSelection = function (html) {
  var range = this.getFirstRange();
  if (range) {
    //létrehozatjuk a böngészővel a node objektumokat
    var div = document.createElement('div');
    div.innerHTML = html;

    //majd beszúrjuk őket  a kiválasztás helyére
    var fragment = document.createDocumentFragment()
      , lastNode;
    for(var node; (node = div.firstChild);
      lastNode = fragment.appendChild(node));
    range.insertNode(fragment);
    
    //villogjon a kurzor a beszúrt elem után
    if (lastNode) {
      range.setStartAfter(lastNode);
      range.collapse(true);
      rangy.getSelection().setSingleRange(range);
    }
  }
};

//@ elmentett kiválasztás
DocSnap.__savedSelection = null;

//@ elmentett fókuszban lévő elem
DocSnap.__savedActiveElement = null;

//@ saveSelection
//@ Aktuális kiválasztás megjelölése
DocSnap.saveSelection = function  () {
  if (this.__savedSelection) {
    rangy.removeMarkers(this.__savedSelection);
  }
  this.__savedSelection = rangy.saveSelection();
  this.__savedActiveElement = document.activeElement;
};

//@ restoreSelection
//@ Megjelölt kiválasztás visszaállítása
DocSnap.restoreSelection = function  () {
  if (this.__savedSelection) {
    rangy.restoreSelection(this.__savedSelection, true);
    this.__savedSelection = null;
    //fókusz visszaállítása
    window.setTimeout(function() {
      if (this.__savedActiveElement && typeof this.__savedActiveElement.focus != 'undefined') {
        this.__savedActiveElement.focus();
      }
    }, 1);
  }
};


//@ handleAjaxError
//@ Esetleges hibaüzenet kezelése
//@ visszatérési érték igaz, ha történt hiba
DocSnap.handleAjaxError = function(json) {
  if (json !== undefined && json.error !== undefined) {
    DocSnap.showInformationDialog(json.error);
    return true;
  }
  return false;
}
  
//@ showChatMessages
//@ chat üzenetek megjelenítése
DocSnap.showChatMessages = function (msgs) {
  for(var i = msgs.length-1; i >= 0; --i) {
    $('#chatlog').append('<div></div>');
    $("#chatlog div:last-child").text(msgs[i].sender+': '+msgs[i].message);
  }
  if (msgs.length > 0) {
    $("#chatlog").scrollTop($("#chatlog")[0].scrollHeight);
  }
}


//$("#mydiv").scrollTop($("#mydiv")[0].scrollHeight);
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
      console.log('** error during synchronization');
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

//@ getChatName
//@ csevegéshez használt név lekérdezése
DocSnap.getChatName = function () {
  var name = $('input[name="name"]').val();
  return (name ? name : 'unnamed');
};

//@ aktuális tartalom lekérdezése
DocSnap.getActualContent = function () {
  return $('#editor').html();
};

//@ aktuális tartalom beállítása
DocSnap.setActualContent = function (newContent) {
  return $('#editor').html(newContent);
};

