//@ Lehet commitolni
DocSnap.__CANCOMMIT = true;

$(document).ready(function() {
  //legyen szerkeszthető a Szerkesztőpanel
  $('#editor').attr('contenteditable',true);

  //engedélyezzük a formázás eszköztárat
  $('.formatting').each(function() {
    $(this).button("enable");
  });

  //a TAB lenyomására szúrjunk be tabulátort
  $('#editor').on({
    keydown: function(ev) {
      var code = ev.keyCode || ev.which;
      if (code == 9) {
        DocSnap.pasteHtmlAtSelection('\t');
        ev.preventDefault();
      }
    }
  });

  

});






