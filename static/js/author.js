//var DocSnap.__sentContent = [];
//var DocSnap.__sentEdits = [];
DocSnap.__CANCOMMIT = true;


$(document).ready(function() {
  //legyen szerkeszthető
  $('#editor').attr('contenteditable',true);

  $('td[id^="toolbar_item_ds_"] td.w2ui-tb-caption').each(function() {
    var target = "ds_" + $(this).text();
    w2ui['toolbar'].enable(target);
  });
  
  //a TAB beszúrása
  $('#editor').on({
    keydown: function(ev) {
      var code = ev.keyCode || ev.which;
      if (code == 9) {
//        DocSnap.pasteHtmlAtSelection('&nbsp;&nbsp;&nbsp;&nbsp;');
        DocSnap.pasteHtmlAtSelection('\t');
        ev.preventDefault();
      }
    }
  });

  

});






