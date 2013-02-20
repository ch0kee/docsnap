<script type="text/javascript">
$(document).ready(function() {
    function filteredEditorText() {
      var v = $("#editor").text();
      //csak ezek a karakterek maradhatnak
      var patt = /([^a-z0-9\s;&\+=-])/gi;
      var vn = v.replace(patt,"");
      return vn;
    }
    //on changed
    var suppressInput = false;
    $("#editor").bind('input', function() {
      if (suppressInput)
        return;
      suppressInput = true;
      $("#editor").text( filteredEditorText() );
      suppressInput = false;
    });

    //input handlers
    /* insert/change attól függően, hogy volt-e kijelölés
    - onpaste
    - onkeydown ? onkeypress ?
    - ondrop
    - oncut (remove)
    - ondrag (remove)
    - onselect kellhet
    */

    //ajax heartbeat
    setInterval(function() {
        var syncobj = new Object();
        syncobj.content = $("#editor").text();

        $.ajax({
            url     : "/cupdate",
            type    : "POST",
            dataType: "json",
            cache   : false,
            data    : {
                d: JSON.stringify(syncobj)
            },
            success : function(json) {

            },
            error : function( xhr, status ) {
                //alert("Sorry, there was a problem!");
            },
            // code to run regardless of success or failure
            complete : function( xhr, status ) {
                //alert("The request is complete!");
            }
        });
    }, 5000);
 });
</script>
