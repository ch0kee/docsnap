
test( "Differences test", function() {

  function testEditScript(oldStr, newStr) {
    var es = DocSnap.Differences.getShortestEditScript(oldStr, newStr);
    var res = DocSnap.Differences.executeES1v2(oldStr, es);
    return res == newStr;
  }

  function randomText(len) {
      var text = "";
      var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

      for( var i=0; i < len; i++ )
          text += possible.charAt(Math.floor(Math.random() * possible.length));

      return text;
  }


  var len = 20;
  ok( testEditScript("",""), "empty test ");
  for(var i = 0; i < 5; ++i) {
    ok( testEditScript(randomText(len), ""), "remove test");
  }
  for(var i = 0; i < 5; ++i) {
    var txt = randomText(len);
    ok( testEditScript(txt, txt), "preserve test");
  }
  for(var i = 0; i < 5; ++i) {
    ok( testEditScript("", txt), "insert test");
  }
  
  for(var i = 0; i < 15; ++i) {
    var txt1 = randomText(Math.floor(Math.random() * 30));
    var txt2 = randomText(Math.floor(Math.random() * 30));
    ok( testEditScript(txt1, txt2), "full random test");
  }
});



