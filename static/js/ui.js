
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

  
//@ showChatMessages
//@ chat üzenetek megjelenítése
DocSnap.showChatMessages = function (msgs) {
  for(var i = 0; i < msgs.length; ++i) {
    $('#chatlog').append('<div></div>');
    $("#chatlog div:last-child").text(msgs[i].sender+': '+msgs[i].message);
  }
  if (msgs.length > 0) {
    $("#chatlog").scrollTop($("#chatlog")[0].scrollHeight);
  }
}


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
