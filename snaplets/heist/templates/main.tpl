<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>DocSnap</title>
    <heiststyles />
    <link rel="stylesheet" type="text/css" href="/static/css/screen.css"/>
    <link rel="stylesheet" type="text/css" href="/static/css/styles.css"/>
    <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css" />
    <script src="/static/js/libs/jquery-1.9.1.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="/static/js/libs/jquery.layout-latest.js"> </script>
    
    <script src="/static/js/libs/rangy-1.2.3/rangy-core.js"> </script>
    <script src="/static/js/libs/rangy-1.2.3/rangy-selectionsaverestore.js"> </script>
    <script src="/static/js/libs/rangy-1.2.3/rangy-cssclassapplier.js"> </script>
   
    <script src="/static/js/environment.js"> </script>
    <script src="/static/js/utils.js"> </script>
    <script src="/static/js/diff.js"> </script>
    <script src="/static/js/docsnap.js"> </script>
    <heistscripts />
  </head>
  <body spellcheck="false">
    <ul id="sharemenu">
      <li data-type="author">with authors</li>
      <li data-type="reader">with readers</li>
    </ul>
    <exporters/>
    <div class='ui-layout-north'>
      <div id="toolbar" class="ui-widget-header ui-corner-all">
        <div id="newbtn">New</div>
        <div id="sharebtn">Share</div>
        <div id="exportbtn">Export</div>
        <div id="separator">|</div>
        <formatting/>
      </div>
    </div>
      
      <div class="ui-layout-center hidden">
        <div class="header"> Editor</div>
        <div id="editor" class="editor"> </div>
      </div>
      
      <div class='ui-layout-east'>
        <div class="header"> Chat</div>
        <input id="chatname" placeholder="Enter your name" type="text"  name="name" value="">
        <div id="chatlog" class="chatlog" contenteditable="false"></div>
        <div >
          <input id="chatfield" placeholder="Type your message" type="text"  name="message" value="">
        </div>
      </div>
      <div class="ui-layout-south sitefooter">
          DocSnap © ch0kee 2013
      </div>
  </body>
</html>
