<!-- 
<textarea id="idEditorBox" class="editorinput" onkeyup="storeContent()"  />
 -->
<script>

var dbg = null;

function Insert(index, content) {
	this.index = index;
	this.content = content;
}

function Remove(index, length) {
  this.index = index;
  this.length = length;
}

//adatvaltozast kezelo kind
enyo.kind({
	name: "ContentTracker",
  inserts: new Array(),
  removes: new Array(),

	published: {
		//content: "",
		checkStart: 0, //itt kezdodik az ellenorzes
		checkLength: 0  //ilyen hosszan
		
	},

	getAsJSON: function() {
		var obj = new Object();
		obj.inserts = this.inserts;
    //obj.removes = this.removes;
    return JSON.stringify(obj);
	},


	resetChanges: function() {
		this.inserts = new Array();
		this.removes = new Array();
	},

	insert: function(inIndex, inContent) {
		var i = new Insert(inIndex, inContent);
		this.inserts.push(i);
	},
  remove: function(inIndex, inLength) {
    var r = new Remove(inIndex, inLength);
    this.removes.push(r);	    
  }
/*
	//valtozott a tartalom, szinkronizaljunk	
	contentChanged: function(inOldValue) {
	}*/
});
//szerkesztodoboz
enyo.kind({
	  name: "EditorArea",
	  kind: enyo.RichText,
    value: "",
    defaultFocus: true,
    allowHtml: true,
    syncInterval: 5000,
    suppressInputChange: false,

	  contentTracker: new ContentTracker(),
	  
    handlers: {
        oninput: "inputChange",
    },

	  //sajat modositasok osszeallitasa
	  createSyncData: function() {
		  var so = this.contentTracker.getAsJSON();
		  console.log(so);
		  this.contentTracker.resetChanges();
		  return so;
	  },
	  //sajat textbox frissitese
    updateContent: function(inSender, inResponse) {
      //console.log(this.getValue());
      //
    	//this.setValue(inResponse);
    },
		  
	  //elkuldjuk a sajat tartalmat es visszakapjuk mi legyen kiirva
	  //de ugy, hogy a kuldes utan tortent modositasok is megmaradnak
	  syncContent: function() {
	   /* var x = new enyo.Ajax({
		    url:"cupdate",
		    method:"POST",
		    postBody: 'd=' + this.createSyncData()
			    });
    	x.go();
    	x.response(this.updateContent);*/
	  },

	  //kiszurjuk a nem kezelheto tartalmat
	  filterValue: function() {
		  var v = this.getValue();
		  //csak ezek a karakterek maradhatnak
		  var patt=/([^a-z0-9\s;&\+=-])/gi;
		  var vn=v.replace(patt,"");
		  this.suppressInputChange = true;
		  this.setValue(vn);
      this.suppressInputChange = false;
	  }
	  ,

	  //ket szinkronizacio kozott folyamatosan gyujtjuk az adatokat,
	  //egymas melletti inserteket osszemergelunk, stb.
      //csak intelligensen..
	  inputChange: function(inSender, inEvent) {
		  if (this.suppressInputChange) {
			  return;
		  }
		  this.filterValue();
		  //
		},
		setCaretPosition: function(idx) {
	    this.getSelection().removeAllRanges();
	    var r = document.createRange();
	    r.setStart(this.node, idx);
	    r.setEnd(this.node, idx);     
	    this.getSelection().addRange( r);
		},
		getCaretPosition: function() {
			var s = this.getSelection();
			var startNode = s.anchorNode;
			var loc = s.anchorOffset;

			for(var n = startNode.previousSibling; n != null; n = n.previousSibling) {
				loc += n.textContent.length;
			}
      //console.log('caretpos: '+loc);
			return loc;
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