<!--
<textarea id="idEditorBox" class="editorinput" onkeyup="storeContent()"  />
 -->
<script type="text/javascript">

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
enyo.kind
({
    name: "ContentTracker",
    oldValue: "",
    value: "",
    inserts: new Array(),
    removes: new Array(),

    published: {
        //content: "",
        checkStart: 0, //itt kezdodik az ellenorzes
        checkLength: 0  //ilyen hosszan
    },

    getLongestCommonPrefix: function(v1,v2)
    {
        var commonPrefix = "";
        var cnt = v1.length < v2.length ? v1.length : v2.length;
        var commonPrefixLength = 0;
        for(var i = 0; i < cnt && v1[i] == v2[i]; ++i)
        {
            ++commonPrefixLength;
        }
        return v1.substr(0, commonPrefixLength);
    },

    getAsJSON: function()
    {
        var lcp = this.getLongestCommonPrefix(this.oldValue, this.value);
        var obj = new Object();
        obj.content = this.value;
        obj.index = this.value
        return JSON.stringify(obj);
    },

    setValue: function(inValue)
    {
        this.oldValue = this.value;
        this.value = inValue;
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
enyo.kind
({
    name: "EditorArea",
    kind: enyo.RichText,
    value: "",
    defaultFocus: true,
    allowHtml: true,
    syncInterval: 5000,
    suppressInputEvent: false,

    contentTracker: new ContentTracker(),

    handlers: {
        oninput: "inputChange",
    },

	  //sajat modositasok osszeallitasa
    createSyncData: function()
    {
        var so = this.contentTracker.getAsJSON();
        console.log(so);
        this.contentTracker.resetChanges();
        return so;
    },
    //sajat textbox frissitese
    updateContent: function(inSender, inResponse) {
        this.setValue(inResponse);
    },

    //elkuldjuk a sajat tartalmat es visszakapjuk mi legyen kiirva
    //de ugy, hogy a kuldes utan tortent modositasok is megmaradnak
    syncContent: function() {
        var x = new enyo.Ajax
        ({
            url:"cupdate",
            method:"POST",
            postBody: 'd=' + this.createSyncData()
        });
        x.go();
        x.response(this.updateContent);
    },

    //kiszurjuk a nem kezelheto tartalmat
    filterValue: function()
    {
        var v = this.getValue();
        //csak ezek a karakterek maradhatnak
        var patt=/([^a-z0-9\s;&\+=-])/gi;
        var vn=v.replace(patt,"");
        this.suppressInputEvent = true;
        this.setValue(vn);
        this.suppressInputEvent = false;
    }
    ,

    //ket szinkronizacio kozott folyamatosan gyujtjuk az adatokat,
    //egymas melletti inserteket osszemergelunk, stb.
    inputChange: function(inSender, inEvent)
    {
        if (this.suppressInputChange)
            return;
        this.filterValue();
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
