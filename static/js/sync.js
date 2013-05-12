

//@ Chat modul
DocSnap.Chat = { //implements ISynchronizable
  
  //üzenetek verziója
  __version: -1,
  
  //elküldésre váró üzenetek nem
  __buffer: [],
  
  //chat üzenetek küldése
  generateSyncData: function() {
    var d = {
      messages: DocSnap.Chat.__buffer,
      version: DocSnap.Chat.__version
    };
    DocSnap.Chat.__buffer = [];
    return d;
  },
  
  //chat üzenetek megjelenítése
  useRemoteSyncData: function(response) {
    DocSnap.showChatMessages(response.messages);
    DocSnap.Chat.__version = response.version;
  },
  
  //modul inicializálása
  initModule: function() {
    //@ chat ablakban enter lenyomására puffereljük az üzenetet
    $('input[name="message"]').keypress(function(e) {
      if (e.which == 13) {
        if (/\S/.test($('input[name="message"]').val())) {
          DocSnap.Chat.__buffer.push({
            sender : DocSnap.getChatName(),
            message: $('input[name="message"]').val()
          });
          $('input[name="message"]').val('');
        }
        return false; 
      }
    });
  }
};



//@ Editor modul
DocSnap.Editor = { //implements ISynchronizable
  
  //verzió
  __version: -1,
    
  //legutóbbi szinkronizált tartalom
  __syncContent: "",
  
  //helyi változások elküldése
  generateSyncData: function() {
    var d = { editScript: [], version: DocSnap.Editor.__version };
    if (DocSnap.__CANCOMMIT) {
      var actContent = DocSnap.getActualContent();
      d.editScript = DocSnap.collectEditsSince(DocSnap.Editor.__syncContent, actContent);
      DocSnap.Editor.__syncContent = actContent;
    }
    return d;
  },
  
  //távoli változások beolvasztása
  useRemoteSyncData: function(response) {
    DocSnap.Editor.__version = response.version;
    var needMerge = response.editScript.length > 0;

    if (needMerge) {
      //számítsuk ki az új szinkronizált tartalmat
      var newSyncContent =
        //DocSnap.Differences.executeES1 
        DocSnap.Differences.executeES1v2(DocSnap.Editor.__syncContent
                                      ,response.editScript);
      
      DocSnap.saveSelection();
      
      //a szerver oldali változtatásokkal együtt alkalmazzuk a helyben elkövetett
      //változtatásokat. így megőrizhető a kijelölés is.
      var actContent = DocSnap.getActualContent();
      var localEdits = DocSnap.collectEditsSince (DocSnap.Editor.__syncContent, actContent);
      var combinedContent =
        DocSnap.Differences.executeES2 (DocSnap.Editor.__syncContent
                                      ,localEdits
                                      ,response.editScript);
      DocSnap.Editor.__syncContent = newSyncContent;

      DocSnap.setActualContent(combinedContent);         
      DocSnap.restoreSelection();
    }
  },
  
  //modul inicializálása
  initModule: function() {
  }
};

//@ Synchronize modul
DocSnap.Synchronize = {

  //regisztrált modulok
  __modules: { },
  
  //regisztráció
  bind: function(ISynchronizable, id ) {
    DocSnap.Synchronize.__modules[ id ] = ISynchronizable;
  },
  
  //szinkronizáció
  start: function() {
    //kérések összegyűjtése
    
    var moduleRequests = [ ];
    for(var id in DocSnap.Synchronize.__modules) {
      
      var ISynchronizable = DocSnap.Synchronize.__modules[id];
      var data = ISynchronizable.generateSyncData();
      
      var r = {
        module: id,
        payload: JSON.stringify(data)
      };
      
      moduleRequests.push( r );
    }
    var request = { packages: moduleRequests };
    //küldés
    DocSnap.sendAjaxCommand("update", request, function(response) {
      //válaszok elosztása
      for(var i = 0; i < response.packages.length; ++i) {
        var id = response.packages[i].module;
        var payload = JSON.parse(response.packages[i].payload);
        var ISynchronizable = DocSnap.Synchronize.__modules[id];
        
        ISynchronizable.useRemoteSyncData(payload);        
      }
      
      //újra
      setTimeout(DocSnap.Synchronize.start, DocSnap.__syncInterval);
    });
  }
};

$(document).ready(function() {
  DocSnap.Editor.initModule();
  DocSnap.Chat.initModule();
  
  DocSnap.Synchronize.bind( DocSnap.Editor, "Editor" );
  DocSnap.Synchronize.bind( DocSnap.Chat, "Chat" );

  DocSnap.Synchronize.start();
});






