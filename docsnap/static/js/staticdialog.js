$(document).ready(function() {
  DocSnap.showDialog(__dlgContent.replace('\\n','\n'),
                     [{text: __dlgButton , click: function() {window.location.href = __dlgTarget;} }]);
});





