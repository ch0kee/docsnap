<!doctype html>
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
    <script src="/static/js/common/jquery-1.9.1.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="/static/js/common/jquerypp/jquerypp.js"> </script>
    <script src="/static/js/common/rangy-1.2.3/rangy-core.js"> </script>
    <script src="/static/js/common/rangy-1.2.3/rangy-selectionsaverestore.js"> </script>
    <script src="/static/js/common/rangy-1.2.3/rangy-cssclassapplier.js"> </script>
    <script src="/static/js/common/jquery.layout-latest.js"> </script>
    <script src="/static/js/common/utils.js"> </script>
    <script src="/static/js/common/diff.js"> </script>
    <script src="/static/js/common/docsnap.js"> </script>
    <newdlg_script/>
    <access_scripts/>
    <sync_scripts/>
  </head>
  <body spellcheck="false">
    <ul id="sharemenu">
      <li><div id="authorshare">with author rights</div></li>
      <li><div id="readershare">with reader rights</div></li>
    </ul>      
    <div class="ui-layout-north">
      <div id="toolbar" class="ui-widget-header ui-corner-all" >    
        <span class="bigbuttons">  
          <div id="new">new</div>
          <div id="share">share</div>
        </span>
        <div id="bold">bold</div>
        <div id="italic">italic</div>
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
      <div id="chatlog" class="chatlog ui-layout-content ui-widget-content" contenteditable="false"></div>
      <input id="chatfield" type="text" style="width:100%" class="ui-widget-content" name="chatInput" value=""><br>
    </div>
    <div class="ui-layout-south" style="padding: 0px">
      <div class="ui-state-default" style="padding: 3px; text-align: center;">
        DocSnap © ch0kee 2013
      </div>
    </div>
  </body>
</html>
