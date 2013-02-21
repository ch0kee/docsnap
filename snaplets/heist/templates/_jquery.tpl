<script type="text/javascript">
$(document).ready(function() {
  var enableAJAXheartbeat = false;

  function caretPosition(pos) {
    var editor = document.getElementById("editor");
    $("#editor").focus(); //? kell ez ?
    var sel = window.getSelection();
    if (arguments.length == 0) {
      //get caret pos
      var cp = new Object();
      var range = sel.getRangeAt(0);
      var startContainer = range.startContainer;
      cp.index = 0;
      if (startContainer == editor) {
        cp.index = -1;
      }
      else {
        for(var child = editor.firstChild; child != startContainer; child = child.nextSibling)
          cp.index = cp.index + 1;
      }
      cp.offset = range.startOffset
      return cp;
    } else {
      //set caret pos
      var range = document.createRange();
      range.setStart(editor.childNodes[0], pos);
      range.collapse(true);
      sel.removeAllRanges();
      sel.addRange(range);
    }
  }


  function filteredEditorText() {
    var v = $("#editor").text();
    //csak ezek a karakterek maradhatnak
    var patt = /([^a-z0-9\s;&\+=-])/gi;
    var vn = v.replace(patt,"");
    return vn;
  }
  //on changed
  var suppressInput = false;
  $("#editor").on('input', function() {
    if (suppressInput)
      return;
    suppressInput = true;
    $("#editor").text( filteredEditorText() );
    //caretPosition(0);

    suppressInput = false;
  });

  //input handlers
  /* insert/change attól függően, hogy volt-e kijelölés
  - onpaste
  - onkeydown ? onkeypress ?
  - ondrop
  - oncut (remove)
  - ondrag (remove)
  - onselect kellhet
  */

  setInterval(function() {
      var cp = caretPosition();
    $("#caretchildindex").text( cp.index );
    $("#caretoffset").text( cp.offset );
  }, 1000);

  //ajax heartbeat
  if (enableAJAXheartbeat)
  setInterval(function() {
      var syncobj = new Object();
      syncobj.content = $("#editor").text();

      $.ajax({
          url     : "/cupdate",
          type    : "POST",
          dataType: "json",
          cache   : false,
          data    : {
              d: JSON.stringify(syncobj)
          },
          success : function(json) {

          },
          error : function( xhr, status ) {
              //alert("Sorry, there was a problem!");
          },
          // code to run regardless of success or failure
          complete : function( xhr, status ) {
              //alert("The request is complete!");
          }
      });
  }, 5000);
 });
</script>
