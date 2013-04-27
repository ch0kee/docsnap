function  createSyncHelper(context, diffEngine) {
  var sentChanges = []; //data hiding
  var sentContent = "";
  return {
    createUpdatePackage: function() {      
      sentContent = context.getActualContent();
      sentChanges = diffEngine.getShortestEditScript(tokenize(context.syncContent), tokenize(sentContent) );
      //üres listát küldjünk, ha nem változott semmi
      if (sentChanges.length == 1 && sentChanges[0].P !== undefined) {
        sentChanges = [];
      }
      var sentRevision = { version:context.syncVersion, edits: sentChanges};
      return sentRevision;
    },    
    handleResponse: function(revision) {  
      var srvVersion = revision.version;
      
      if (revision.edits.length == 0) {
        //commit successfull
        context.syncContent = sentContent;//diffEngine.executeES1(context.syncContent, sentChanges);
        context.syncVersion = srvVersion;
        //var content = diffEngine.executeES1(oldSyncContent, cliChanges);
        return null;
      } else {
        //checkout only   
        var srvChanges = revision.edits;          
        var oldSyncContent = context.syncContent;
        
        context.syncContent = diffEngine.executeES1(context.syncContent, srvChanges);
        context.syncVersion = srvVersion;
        //todo: ha történt útközben változás
        var actContent = context.getActualContent();
        var cliChanges = diffEngine.getShortestEditScript(tokenize(oldSyncContent), tokenize(actContent));
        
        //merge local changes with repository revision
        var content = diffEngine.executeES2(oldSyncContent, cliChanges, srvChanges);
        return content;
      }
    }
  }
}


$(document).ready(function() {
  var normalizingInput = false;
  $("#editor").attr('contenteditable',true);
  var boldApplier = rangy.createCssClassApplier("ds_bold", { applyToEditableOnly: true, normalize: true });
  var italicApplier = rangy.createCssClassApplier("ds_italic", { applyToEditableOnly: true, normalize: true });
  
  $("#editor").on({
    //TAB must be handled here because keypress in
    //Chrome is already too late
    keydown: function(ev) {
      var code = ev.keyCode || ev.which;
      if (code == 9) {
      //TAB
        pasteHtmlAtSelection('&nbsp;&nbsp;&nbsp;&nbsp;');
        ev.preventDefault();
      }
    },

    keypress: function(ev){
      var code = ev.keyCode || ev.which;
      //ENTER
/*      if (code == 13) {
        pasteHtmlAtSelection('<br>');
        modified = true;
        ev.preventDefault();
      }*/
    },

    input: function() {
    }
  });

  $("#bold").mousedown(function(e) {
    boldApplier.toggleSelection();
    modified = true;
    e.preventDefault();
  });

  $("#italic").mousedown(function(e) {
    italicApplier.toggleSelection();
    modified = true;
    e.preventDefault();
  });
  

});






