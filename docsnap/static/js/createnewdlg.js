$(document).ready(function() {
  DocSnap.showDialog('Welcome to DocSnap',
    [{ text: "Create new document",
       click: function() { window.location.href = '/new'; }
    }]);
});





