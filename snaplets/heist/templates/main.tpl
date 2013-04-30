<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>DocSnap Multi-User Document Editor</title>
    <link rel="stylesheet" type="text/css" href="/static/css/screen.css"/>
    <link rel="stylesheet" type="text/css" href="/static/css/styles.css"/>
    <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css" />
    <link rel="stylesheet" type="text/css" href="/static/css/redmond/jquery-ui-1.10.2.custom.css" />
    <link rel="stylesheet" type="text/css" href="/static/css/bigbuttons/jquery-ui-1.10.2.custom.css" />
    <link type="text/css" rel="stylesheet" href="/static/css/layout-default-latest.css" />
    <script src="/static/js/libs/jquery-1.9.1.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="/static/js/libs/jquerypp/jquerypp.js"> </script>
    <script src="/static/js/libs/rangy-1.2.3/rangy-core.js"> </script>
    <script src="/static/js/libs/rangy-1.2.3/rangy-selectionsaverestore.js"> </script>
    <script src="/static/js/libs/rangy-1.2.3/rangy-cssclassapplier.js"> </script>
    <script src="/static/js/libs/jquery.layout-latest.js"> </script>
    <script src="/static/js/environment.js"> </script>
    <script src="/static/js/utils.js"> </script>
    <script src="/static/js/diff.js"> </script>
    <script src="/static/js/docsnap.js"> </script>
    <heistscripts/>
  </head>
  <body spellcheck="false">
    <ul id="sharemenu">
      <li id="authorshare" data-type="author">with author</li>
      <li id="readershare" data-type="reader">with reader</li>
    </ul>
    <exporters/>
    <div class="ui-layout-north">
      <div id="toolbar" class="ui-widget-header ui-corner-all" >    
        <span class="bigbuttons">  
          <div id="new">New</div>
          <div id="share">Share</div>
          <div id="export">Download</div>
        </span>
        <div id="bold"><b>bold</b></div>
        <div id="italic"><i>italic</i></div>
      </div>
    </div>
    <div class="ui-layout-center">
      <div id="content" class="ui-state-default" style="padding: 3px; text-align: center;">
         Editor 
      </div>
      <div id="editor" class="editorinput ui-layout-content ui-widget-content"> </div>
    </div>
    <div class="ui-layout-east">
      <div class="ui-state-default" style="padding: 3px; text-align: center;">
         Chat
      </div>
      <input id="chatname" placeholder="Enter your name" type="text"  class="ui-widget-content" name="name" value="">
      <div id="chatlog" class="chatlog ui-layout-content ui-widget-content" contenteditable="false"></div>
      <input id="chatfield" placeholder="Type your message" type="text" style="width:100%" class="ui-widget-content" name="message" value="">
    </div>
    <div class="ui-layout-south" style="padding: 0px">
      <div class="ui-state-default" style="padding: 3px; text-align: center;">
        DocSnap © ch0kee 2013
      </div>
    </div>
  </body>
</html>
