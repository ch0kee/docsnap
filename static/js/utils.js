
//@ getFirstRange
//@ Visszaadja az első, összefüggő kiválasztást
//@ (Pl. Firefox estén lehetséges több kijelölés)
DocSnap.getFirstRange = function () {
  var selection = rangy.getSelection();
  return selection.rangeCount ? selection.getRangeAt(0) : null;
};

/*
//@ pasteElementAtSelection
//@ HTML elem beszúrása a kiválasztás helyére
function DocSnap.pasteElementAtSelection(tagname) {
  var range = this.getFirstRange();
  if (range) {
    var element = document.createElement(tagname);
    range.insertNode(element);
    //move selection after insertion
    range.setStartAfter(element);
    range.collapse(true);
    rangy.getSelection().setSingleRange(range);
    //rangy.getSelection().collapseToEnd();
  }
}*/

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

DocSnap.showInformationDialog = function(dlgContent) {
  if (!$('#dialog').length) {
    $('body').prepend('<div id="dialog" style="white-space:pre" title="DocSnap">')  
  }
  $('#dialog').text(dlgContent);
  
//  $('body').prepend('<div id="dialog" style="white-space:pre" title="DocSnap">'+__dlgContent.replace('\\n','\n')+'</div>');
  $( '#dialog' ).dialog({
    autoOpen: true
  , modal: true
  , buttons: [ {text: "ok", click: function() {
      $(this).dialog("close");
      $(this).remove();
    }}]
  , draggable: true
  , height: 'auto'
  , width: 'auto'
  , resizable: false
  , dialogClass: 'no-close'
  , open: function (event, ui) {
    $('#dialog').css('overflow', 'hidden'); //scrollbar elrejtése
  }
  });
  $('#dialog').focus();
/*
  $('#dialog').dialog('option', 'position', 'center');
  $(window).resize(function() {
    $('#dialog').dialog('option', 'position', 'center');
  });*/
};

  
  
DocSnap.showChatMessages = function (msgs) {
  for(var i = msgs.length-1; i >= 0; --i) {
    $('#chatlog').append('<div>'+msgs[i].sender+': '+msgs[i].message+'</div>');
  }
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
    success : onsuccess,
    error : function( xhr, status ) {
      console.log('** error during synchronization');
    },
    complete : function( xhr, status ) {
      //alert("The request is complete!");
    }
  });
};

//@ downloadURL
//@ megadott hivatkozás letöltésre kényszerítése
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




//@ tokenize
//@ 'data' - string : feldarabolandó szöveg
//@ Tokenekre bontja a szöveget az összehasonításhoz.
//@ returns: Olyan tömböt ad vissza, amelynek minden eleme
//@ vagy egy karakter, vagy pedig egy html jelölőnyelvi elem.
DocSnap.tokenize = function (data) {
  var tokens = [];
  var len = data.length;
  var idx = 0;
  var tokenizerRegex = new RegExp('<[^>]+>|&[a-z]+;|<\/[^>]+>', 'g');
  var res = null;
  while ( (res = tokenizerRegex.exec(data)) ) {
    //megelozo karakterek
    for(var i = idx; i < res.index; ++i) {
      tokens.push(data[i]);
    }

    tokens.push(res[0]);

    idx = res.index + res[0].length;
  }
  for(var i = idx; i < len; ++i) {
    tokens.push(data[i]);
  }
  return tokens;
};
