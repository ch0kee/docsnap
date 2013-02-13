<!-- 
<textarea id="idEditorBox" class="editorinput" onkeyup="storeContent()"  />
 -->
<script>
var suppressOnInput = false;
/*
var obj = new Object();
obj.list = new Array();
obj.list[0] = "ListFirstElement";
obj.list[1] = "ListSecondElement";

alert(enyo.json.stringify(obj));

equals to {"list":["ListFirstElement","ListSecondElement"]}
*/

//adatvaltozast kezelo kind
enyo.kind({
	name: "ContentTracker",
  inserts: new Array(),
  removes: new Array(),

	published: {
		content: ""
	},

	getAsJSON: function() {
		var obj = new Object();
		obj.inserts = this.inserts;
    obj.removes = this.removes;
    return JSON.stringify(obj);
	},

	resetChanges: function() {
		this.inserts = new Array();
		this.removes = new Array();
	},

	//valtozott a tartalom, szinkronizaljunk	
	contentChanged: function(inOldValue) {
		this.inserts.push("vupsz");
	}
});

//szerkesztodoboz
enyo.kind({
	  name: "EditorArea",
	  kind: enyo.RichText,
    value: "",
    defaultFocus: true,
    
    syncInterval: 5000,

	  contentTracker: new ContentTracker(),
	  
    handlers: {
        oninput: "inputChange"
    },

	  //sajat modositasok osszeallitasa
	  createSyncData: function() {
		  this.contentTracker.setContent(this.getValue());
		  var so = this.contentTracker.getAsJSON();
		  this.contentTracker.resetChanges();
		  return so;
	  },

	  //sajat textbox frissitese
    updateContent: function(inSender, inResponse) {
      console.log("updateContent");
    },
		  
	  //elkuldjuk a sajat tartalmat es visszakapjuk mi legyen kiirva
	  //de ugy, hogy a kuldes utan tortent modositasok is megmaradnak
	  syncContent: function() {
	    var x = new enyo.Ajax({url:"cupdate"});
    	x.go({
        	d : this.createSyncData()
        	});
    	x.response(this.updateContent);
	  },


	  //ket szinkronizacio kozott folyamatosan gyujtjuk az adatokat,
	  //egymas melletti inserteket osszemergelunk, stb.
      //csak intelligensen..
	  inputChange: function(inSender, inEvent) {
		  //alert(this.sentContent);
		  //alert(lastContent);
		    // retrieve new input value
		    //newInputValue = this.$.myInput.getValue();
		    //var s = enyo.json.stringify(inEvent); 
		    //alert(inEvent.target.value);
		    // do something in response
		    //nem engedjuk tovabb ? 
		    return true; 
		},
		
    create: function() {
        this.inherited(arguments);
        this.syncContent();
        this.start();
        // Call the constructor inherited from Object
    },    
    destroy: function() {
        this.stopTimer();
        this.inherited(arguments);
    },
    start: function() {
        this.job = window.setInterval(enyo.bind(this, "syncContent"), this.syncInterval);
    },
    stop: function() {
        window.clearInterval(this.job);
    }
	});

var editorArea = new EditorArea({});
editorArea.addClass("editorinput");
editorArea.renderInto(document.body);
</script>



<!--
 
<div id="idEditorBox2" style="height:200;width:300;border:3px inset;overflow:scroll;">
	<div style="color:red;">I am red</div>
</div>
-->
<!--
<code contenteditable="true" />
     <span style="color: blue">var</span> foo = <span style="color: green">"bar"</span>;  
</code>
-->