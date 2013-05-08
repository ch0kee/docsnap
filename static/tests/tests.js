
test( "Differences test", function() {

  function testEditScript(oldStr, newStr) {
    var es = DocSnap.Differences.getShortestEditScript(oldStr, newStr);
    var res = DocSnap.Differences.executeES1(oldStr, es);
    return res == newStr;
  }



  
  ok( testEditScript("",""), "empty");
  ok( testEditScript("sda",""), "Passed ");
  ok( testEditScript("dasdas",""), "Passed ");
  ok( testEditScript("s",""), "Passed ");
  ok( testEditScript("","ds"), "Passed ");
  ok( testEditScript("","sdf"), "Passed ");
  ok( testEditScript("r","e"), "Passed ");
  ok( testEditScript("vcx",""), "Passed ");
  ok( testEditScript("sd","sf"), "Passed ");
  ok( testEditScript("","sdfsdfsdfsdfs"), "Passed ");
  ok( testEditScript("sdfsdfsdfsdfs",""), "Passed ");
  
});



