
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
