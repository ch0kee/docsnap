
var layout = null;

//http://stackoverflow.com/questions/3749231/download-file-using-javascript-jquery
var downloadURL = function downloadURL(url) {
    var hiddenIFrameID = '__downloader',
        iframe = document.getElementById(hiddenIFrameID);
    if (iframe === null) {
        iframe = document.createElement('iframe');
        iframe.id = hiddenIFrameID;
        iframe.style.display = 'none';
        document.body.appendChild(iframe);
    }
    iframe.src = url;
};


//@ dokumentum előkészítése
$(document).ready(function() {
  $("#new").button();
  $("#share").button()
    .click(function() {
      $("#sharemenu").show('slow').position({
            my: "left top",
            at: "left bottom",
            of: this
          });
      //tüntessük el a menüt akárhova kattintunk
      $( document ).one( "click", function() {
        $("#sharemenu").hide();
      });
      return false;
    });
  $("#sharemenu").menu().hide();

  $("#export").button()
    .click(function() {
      $("#exportmenu").show('slow').position({
            my: "left top",
            at: "left bottom",
            of: this
          });
      //tüntessük el a menüt akárhova kattintunk
      $( document ).one( "click", function() {
        $("#exportmenu").hide();
      });
      return false;
    });
  $("#exportmenu").menu().hide();
  $("#exportmenu").children().button()
    .click(function() {
      var index = $(this).attr("data-index");
//      window.location.href = '/export';
      $.ajax(
      { type    : "POST"
      , dataType: "html"
      , cache   : false
      , data    :
        { cmd: "export"
        , args: index }
      , success : function(data) {
        //e.preventDefault();
       // alert(data);
        downloadURL(data);
      }
      , error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      }
      , complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
      });
    });
       
  
  $("#readershare").button();
  $("#readershare").click(function() {
      $.ajax({
      type    : "POST",
      dataType: "html",
      cache   : false,
      data    : {
        cmd: "share"
      , args: "reader"
      },
      success : function(revision) {
        alert(revision);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });    
  });
  $("#authorshare").button();
  $("#readershare").click(function() {
      $.ajax({
      type    : "POST",
      dataType: "html",
      cache   : false,
      data    : {
        cmd: "share"
      , args: "author"
      },
      success : function(revision) {
        alert(revision);
      },
      error : function( xhr, status ) {
        console.log("Sorry, there was a problem!");
      },
      complete : function( xhr, status ) {
        //alert("The request is complete!");
      }
    });    
  });

  
  $("#bold").button();
  $("#italic").button();
  
  //elrendezés beállítása
  layout = $('body').layout({
		north__resizable: false
		,	east__size:	200
    , south__minSize: 30
    , south__maxSize: 30
    , north__minSize: 70
    , north__maxSize: 70
    , north__spacing_open: 0
		, south__resizable: false
    , south__spacing_open: 0
		,	east__spacing_open: 10
		,	east__spacing_closed: 20
		,	east__resizable: false
    , east__closable: true
    , south__closable: false
		,	center__minWidth:	100
		,	east__fxSpeed_open: 300
		,	east__fxSpeed_close: 300
		,	stateManagement__enabled: true
		,	showDebugMessages: false
  });

  //szövegrészletek manupilálásához
  rangy.init(); 
});






