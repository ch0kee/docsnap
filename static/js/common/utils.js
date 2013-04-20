
//http://rangy.googlecode.com/svn/trunk/demos/core.html
function getFirstRange() {
  var sel = rangy.getSelection();
  return sel.rangeCount ? sel.getRangeAt(0) : null;
}


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

//http://rangy.googlecode.com/svn/trunk/demos/saverestore.html
var savedSel = null;
var savedSelActiveElement = null;

function saveSelection() {
  // Remove markers for previously saved selection
  if (savedSel) {
    rangy.removeMarkers(savedSel);
  }
  savedSel = rangy.saveSelection();
  savedSelActiveElement = document.activeElement;
}
 
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
