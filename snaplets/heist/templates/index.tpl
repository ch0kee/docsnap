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

		/*
		 *	TAB-THEME ADJUSTMENTS
		 */
		.ui-tabs-nav li {
			white-space:	nowrap;
		}
		.ui-tabs-nav li a {
			font-size:		1em !important;
			padding:		4px 1.5ex 3px !important;
		}
		.ui-tabs-panel {
			font-size:		1em !important;
			padding:		0 1em !important;
		}

		/*
		 *	WEST-PANE TABS
		 *
		 *	These tabs 'fill' the pane,
		 *	so the pane-border acts as the tab-border
		 */
		.ui-layout-pane-west {
			padding:	0;
			overflow:	hidden;
			}
			.ui-layout-pane-west .ui-tabs-nav {
				/* don't need border or rounded corners - tabs 'fill' the pane */
				border-top:		0;
				border-left:	0;
				border-right:	0;
				padding-bottom:	0 !important;
				-moz-border-radius:		0;
				-webkit-border-radius:	0;
			}

		/*
		 *	CENTER-PANE TABS
		 *
		 *	These tabs have white-space around them,
		 *	so the content-div provides the border for the tabs
		 */
		.ui-layout-pane-center {
			background:	#FFD; /* to make pane background stand-out */
			padding:	10px;
			overflow:	hidden;
			}
			.ui-layout-pane-center .ui-tabs-nav {
				/* remove rounded corners from bottom of 'tabs'*/
				-moz-border-radius-bottomleft:		0;
				-moz-border-radius-bottomright:		0;
				-webkit-border-bottom-left-radius:	0;
				-webkit-border-bottom-right-radius:	0;
			}

		/*
		 *	EAST-PANE TABS
		 *
		 *	These are ordinary tabs - entire pane scrolls
		 */
		.ui-layout-pane-east {
			background:	#EFE; /* to make pane background stand-out */
			padding:	10px;
			overflow:	auto; /* scroll - tab-panels taller than pane - without content-div */
			}
			.ui-layout-pane-east .ui-tabs-nav {
				/* remove rounded corners from bottom of 'tabs'*/
				-moz-border-radius-bottomleft:		0;
				-moz-border-radius-bottomright:		0;
				-webkit-border-bottom-left-radius:	0;
				-webkit-border-bottom-right-radius:	0;
			}
			.ui-layout-pane-east .ui-tabs-panel {
				background:	#FFF;			/* ui-tab-panel class removes ui-widget-content background - restore it */
				border:		1px solid #AAA;	/* ditto for the ui-widget-content border */
				border-top-color:	#FFF;
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
  <body>
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
      <div id="tabs">
        <ul>
          <li><a href="#editorTab">Editor</a></li>
          <li><a href="#htmlTab">HTML (read-only)</a></li>
        </ul>
        <div id="editorTab">
            <div id="editor" class="editorinput ui-layout-content ui-widget-content" contenteditable="true"></div>
        </div>
        <div id="htmlTab">
            <div id="preview" class="editorinput ui-layout-content ui-widget-content" contenteditable="false"></div>
        </div>
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
