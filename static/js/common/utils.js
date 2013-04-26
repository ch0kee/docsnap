
//@ getFirstRange
//@ Visszaadja az első, összefüggő kiválasztást
//@ (Pl. Firefox estén lehetséges több kijelölés)
function getFirstRange() {
  var sel = rangy.getSelection();
  return sel.rangeCount ? sel.getRangeAt(0) : null;
}

//@ pasteElementAtSelection
//@ HTML elem beszúrása a kiválasztás helyére
function pasteElementAtSelection(tagname) {
  var range = getFirstRange();
  if (range) {
    var el = document.createElement(tagname);
    range.insertNode(el);
    //move selection after insertion
    range.setStartAfter(el);
    range.collapse(true);
    rangy.getSelection().setSingleRange(range);
    //rangy.getSelection().collapseToEnd();
  }
}

//@ pasteHtmlAtSelection
//@ HTML kód beszúrása a kiválasztás helyére
function pasteHtmlAtSelection(html) {
  var range = getFirstRange();
  if (range) {
    var el = document.createElement("div");
    el.innerHTML = html;
    var frag = document.createDocumentFragment(), node, lastNode;
    while ( (node = el.firstChild) ) {
        lastNode = frag.appendChild(node);
    }
    range.insertNode(frag);
    if (lastNode) {
      range.setStartAfter(lastNode);
      range.collapse(true);
      rangy.getSelection().setSingleRange(range);
    }
  }
}

var savedSel = null;
var savedSelActiveElement = null;

//@ saveSelection
//@ Aktuális kiválasztás megjelölése
function saveSelection() {
  if (savedSel) {
    rangy.removeMarkers(savedSel);
  }
  savedSel = rangy.saveSelection();
  savedSelActiveElement = document.activeElement;
}

//@ restoreSelection
//@ Megjelölt kiválasztás visszaállítása
function restoreSelection() {
  if (savedSel) {
    rangy.restoreSelection(savedSel, true);
    savedSel = null;
    window.setTimeout(function() {
      if (savedSelActiveElement && typeof savedSelActiveElement.focus != "undefined") {
        savedSelActiveElement.focus();
      }
    }, 1);
  }
}

//@ tokenize
//@ 'data' - string : feldarabolandó szöveg
//@ Tokenekre bontja a szöveget az összehasonításhoz.
//@ returns: Olyan tömböt ad vissza, amelynek minden eleme
//@ vagy egy karakter, vagy pedig egy html jelölőnyelvi elem.
function tokenize(data) {
  var tokens = [];
  var len = data.length;
  var idx = 0;
  var tokenizerRegex = new RegExp("<[^>]+>|&[a-z]+;|<\/[^>]+>", "g");
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
}
