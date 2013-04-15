<html>
  <head>
    <title>DocSnap Multi-User Document Editor</title>
    <link rel="stylesheet" type="text/css" href="/screen.css"/>
    <link rel="stylesheet" type="text/css" href="/styles.css"/>
    <script src="/jquery-1.9.1.js"></script>
    <script src="/jquerypp/jquerypp.js"> </script>
    <script src="/rangy-1.2.3/rangy-core.js"> </script>
    <script src="/rangy-1.2.3/rangy-selectionsaverestore.js"> </script>
    <script src="/rangy-1.2.3/rangy-cssclassapplier.js"> </script>
    <script src="/utils.js"> </script>
    <script src="/diff.js"> </script>
    <script src="/docsnap.js"> </script>
  </head>
  <body>
    <div id="content">

      <h1>[DocSnap Fejléc]</h1>
      <h1>DocSnap Eszköztár</h1>
      <table>
        <tr>
          <td> <button id="bold">BOLD</button> </td>
          <td> <button id="italic">ITALIC</button> </td>
          <td> <button id="hello">HELLO</button> </td>
          <td> <button id="sync">SYNC</button> </td>
        </tr>
        <tr>
          <td>
            <div id="editor" class="editorinput" contenteditable="true"></div>
          </td>
          <td>
            <div id="preview" class="editorinput" contenteditable="false"></div>
          </td>
        </tr>
      </table>

    </div>
  </body>
</html>
