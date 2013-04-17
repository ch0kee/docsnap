<html>
  <head>
    <title>DocSnap Multi-User Document Editor</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <link rel="stylesheet" type="text/css" href="/styles.css"/>
    <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.2/themes/smoothness/jquery-ui.css" />
    <script src="/jquery-1.9.1.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="/jquerypp/jquerypp.js"> </script>
    <script src="/rangy-1.2.3/rangy-core.js"> </script>
    <script src="/rangy-1.2.3/rangy-selectionsaverestore.js"> </script>
    <script src="/rangy-1.2.3/rangy-cssclassapplier.js"> </script>
    <script src="/jquery.layout-latest.js"> </script>

	<style>
	html {
		padding:	0px; /* will add spacing from viewport around BODY */
		display:	block !important;
	}
	body {
		font-family: "Trebuchet MS", "Helvetica", "Arial",  "Verdana", "sans-serif";
	font-size: 62.5%;
}
		/*
		 *	GLOBAL LAYOUT STYLES
		 */
		.ui-layout-content {
			overflow:		auto; /* add scrolling to content-divs (panel-wrappers) */
			border-top:		0 !important; /* tab-buttons above this DIV already has a border-bottom */
		}




		.ui-layout-pane-north {
			background:	#EFE; /* to make pane background stand-out */
			padding:	10px;
			overflow:	hidden;
			}
		.ui-layout-pane-center {
			background:	#EFE; /* to make pane background stand-out */
			padding:	10px;
			overflow:	hidden;
			}
		.ui-layout-pane-east {
			background:	#EFE; /* to make pane background stand-out */
			padding:	10px;
			overflow:	auto; /* scroll - tab-panels taller than pane - without content-div */
			}

		div.buttons {
			float:			right;
			margin-right:	200px;
			margin-top:		0;
		}
	</style>
    <script src="/utils.js"> </script>
    <script src="/diff.js"> </script>
    <script src="/docsnap.js"> </script>
  </head>
  <body spellcheck="false">
    <div class="ui-layout-north" onmouseover="layout.allowOverflow('north')" onmouseout="layout.resetOverflow(this)">
      <h1>[DocSnap Fejléc]</h1>
      <h1>XDocSnap Eszköztár</h1>

      <table>
        <tr>
          <td> <button id="bold">BOLD</button> </td>
          <td> <button id="italic">ITALIC</button> </td>
        </tr>
      </table>
    </div>
    <div class="ui-layout-center">
      <div id="content" class="ui-state-default" style="padding: 3px 5px 5px; text-align: center;">
        DocSnap Editor
      </div>
      <div id="editorTab">
          <div id="editor" class="editorinput ui-layout-content ui-widget-content" contenteditable="true"></div>
      </div>
    </div>


<div class="ui-layout-east">
  This is the east pane.
</div>


<div class="ui-layout-south">

  <DIV class="ui-state-default" style="padding: 3px 5px 5px; text-align: center;"
    onMouseOver="$(this).addClass('ui-state-hover')" onMouseOut="$(this).removeClass('ui-state-hover')">
    DocSnap © ch0kee 2013
  </DIV>
</div>

  </body>
</html>
