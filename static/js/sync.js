$(document).ready(function() {
  var context =
    { syncContent:  ""
    , syncVersion: 0
    , syncChatBuffer: []
    , syncChatVersion: -1 };
  
  //synchronize
  function  synchronizeContent() {
    var request =
      { reqRevision: { version: context.syncVersion }
      , reqChatBuffer: context.syncChatBuffer
      , reqChatName: DocSnap.getChatName()
      , reqChatVersion: context.syncChatVersion };
    context.syncChatBuffer = [];

    if (DocSnap.__CANCOMMIT) {
      DocSnap.__sentContent = DocSnap.getActualContent();    
      console.log('old: ['+context.syncContent+']');
      request.reqRevision.edits = DocSnap.collectEditsSince(context.syncContent);
      console.log('actual: ['+DocSnap.getActualContent()+']');
    } else {
      request.reqRevision.edits = [];
    }
    console.log('synchronizeContent()::sending: ' + JSON.stringify(request));
  
    DocSnap.sendAjaxCommand("update", request, function(response) {
      console.log('synchronizeContent()::received ' + JSON.stringify(response));

      DocSnap.showChatMessages(response.rspChatMessages);
      context.syncChatVersion = response.rspChatVersion;
      
      var versionIncremented = response.rspRevision.version > context.syncVersion;
      var receivedEdits = response.rspRevision.edits.length > 0;

      if (DocSnap.__CANCOMMIT && versionIncremented && !receivedEdits) {
        //elfogadták, amit beküldtünk
        console.log('commit accepted');
        context.syncContent = DocSnap.__sentContent;
      }
      
      context.syncVersion = response.rspRevision.version;
      
      //van változás
      if (receivedEdits) {
        //számítsuk ki az új tartalmat
        //ez megegyezik a szerver oldalival
        var newSyncContent =
          DocSnap.DiffEngine.executeES1 (context.syncContent
                                        ,response.rspRevision.edits);
        
        
        //a kiválasztás megőrzésével megjelenítjük a módosításokat
        //az időközben elkövetett módosításokat is alkalmazni kell.
        //ha csak olvasni tud, akkor a kiválasztást megőrző nodeok
        //is változtatásnak számítanak, így jelölhet ki
        DocSnap.saveSelection();
        
        var localEdits = DocSnap.collectEditsSince (context.syncContent);
        var combinedContent =
          DocSnap.DiffEngine.executeES2 (context.syncContent
                                        ,localEdits
                                        ,response.rspRevision.edits);
        context.syncContent = newSyncContent;

        DocSnap.setActualContent(combinedContent); 
          
        DocSnap.restoreSelection();
      }
      
      
      setTimeout(synchronizeContent, DocSnap.__syncInterval);
    });
  }
  
  
  $('input[name="message"]').keypress(function(e) {
    if (e.which == 13) {
      console.log('chat: '+$('input[name="message"]').val());
      context.syncChatBuffer.push($('input[name="message"]').val());
      $('input[name="message"]').val('');
      return false; 
    }
  });

  
  function  initialCheckout() {
    //start loading progress bar
    DocSnap.sendAjaxCommand("init", null, function(response) {
      context.syncContent = "";
      console.log('initialCheckout()::received ' + JSON.stringify(response));
      
      var revision = response.rspRevision;
      var srvVersion = revision.version;
      context.syncContent =
        DocSnap.DiffEngine.executeES1 (context.syncContent
                                      ,response.rspRevision.edits);
      context.syncVersion = response.rspRevision.version;
      context.syncChatVersion = response.rspChatVersion;      
      DocSnap.showChatMessages(response.rspChatMessages);
      
      DocSnap.setActualContent(context.syncContent);
      setTimeout(synchronizeContent, DocSnap.__syncInterval);
    });    
  } 
  
  initialCheckout();
});






