$(document).ready(function() {
  var context =
    { syncContent:  ""
    , syncVersion: 0
    , syncChatBuffer: []
    , syncChatVersion: -1 };
  
  //@ Központi rutin, a tartalom szinkronizálásáért felelős
  function  synchronizeContent() {
    var request =
      { reqRevision: { version: context.syncVersion }
      , reqChatBuffer: context.syncChatBuffer
      , reqChatName: DocSnap.getChatName()
      , reqChatVersion: context.syncChatVersion };
    context.syncChatBuffer = [];

    //küldendő változatások összegyűjtése
    if (DocSnap.__CANCOMMIT) {
      DocSnap.__sentContent = DocSnap.getActualContent();
      request.reqRevision.editScript = DocSnap.collectEditsSince(context.syncContent);
      context.syncContent = DocSnap.__sentContent;
    } else {
      request.reqRevision.editScript = [];
    }
   
    //saját változtatások küldése, többiek módosításának lekérése 
    DocSnap.sendAjaxCommand("update", request, function(response) {

      DocSnap.showChatMessages(response.rspChatMessages);
      context.syncChatVersion = response.rspChatVersion;

      //kiderítjük, hogy sikerült-e a commit      
      var versionIncremented = response.rspRevision.version > context.syncVersion;
      var receivedEdits = response.rspRevision.editScript.length > 0;
      if (DocSnap.__CANCOMMIT && versionIncremented && !receivedEdits) {
        //elfogadták, amit beküldtünk
        //context.syncContent = DocSnap.__sentContent;
      }
      
      //aktuális verzió beállíŧása
      context.syncVersion = response.rspRevision.version;
      
      //történt-e változás
      if (receivedEdits) {
        //számítsuk ki az új szinkronizált tartalmat
        var newSyncContent =
          DocSnap.Differences.executeES1 (context.syncContent
                                        ,response.rspRevision.editScript);
        
        
        //kijelölés megőrzése        
        DocSnap.saveSelection();
        
        //a szerver oldali változtatásokkal együtt alkalmazzuk a helyben elkövetett
        //változtatásokat. így megőrizhető a kijelölés is.
        var localEdits = DocSnap.collectEditsSince (context.syncContent);
        var combinedContent =
          DocSnap.Differences.executeES2 (context.syncContent
                                        ,localEdits
                                        ,response.rspRevision.editScript);
        context.syncContent = newSyncContent;

        DocSnap.setActualContent(combinedContent); 
          
        //kijelölés visszaállítása
        DocSnap.restoreSelection();
      }
      
      //következő szinkronizáció
      setTimeout(synchronizeContent, DocSnap.__syncInterval);
    });
  }
  
  //@ chat ablakban enter lenyomására buffereljük az üzenetet
  $('input[name="message"]').keypress(function(e) {
    if (e.which == 13) {
      if (/\S/.test($('input[name="message"]').val())) {
        context.syncChatBuffer.push($('input[name="message"]').val());
        $('input[name="message"]').val('');
      }
      return false; 
    }
  });

  //@ Tartalom letöltése
  function  initialCheckout() {
    DocSnap.sendAjaxCommand("init", null, function(response) {
      context.syncContent = "";
      
      var revision = response.rspRevision;
      var srvVersion = revision.version;
      context.syncContent =
        DocSnap.Differences.executeES1 (context.syncContent
                                      ,response.rspRevision.editScript);
      context.syncVersion = response.rspRevision.version;
      context.syncChatVersion = response.rspChatVersion;      
      DocSnap.showChatMessages(response.rspChatMessages);
      
      DocSnap.setActualContent(context.syncContent);
      setTimeout(synchronizeContent, DocSnap.__syncInterval);
    });    
  } 
  
  //indítsuk el a kommunikációs folyamatot
  initialCheckout();
});






