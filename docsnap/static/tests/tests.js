
test( "hello test", function() {

  DocSnap.DiffEngine.executeNEWES1 = function(content, es) {
    var result = "";
    for(var s = new ESIterator1(content, es); !s.eos(); s.advance()) {
      result += s.value();
    }
    return result;
  }
  
  function okESIterator(v1, v2) {
    var init = true;
    var es = DocSnap.DiffEngine.getShortestEditScript(v1, v2);
    var result = "";
    for(var s = new ESIterator1(v1, es); !s.eos(); s.advance()) {
      result += s.value();
    }    
    return result == v2;
  }
  
  var v1 = "";
  for(var i = 0; i < 200; ++i) {
    v1 += "QWERTZUIOPŐ";
  }
  var v2 = "";
  for(var i = 0; i < 200; ++i) {
    v2 += "OIUZTREWQ";
  }
  
  function testOld() {
    var init = true;
    for(var i = 0; i < 10; ++i) {
      var es = DocSnap.DiffEngine.getShortestEditScript(v1, v2);
      var res = DocSnap.DiffEngine.executeES1(v1, es);
      init = init && (res == v2);
    }
    return init;
  }
  
    function testNew() {
    var init = true;
    for(var i = 0; i < 10; ++i) {
      var es = DocSnap.DiffEngine.getShortestEditScript(v1, v2);
      var res = DocSnap.DiffEngine.executeNEWES1(v1, es);
      init = init && (res == v2);
    }
    return init;
  }
/*
  ok( testOld(), "Old");
  ok( testOld(), "Old");
  ok( testOld(), "Old");

  ok( testNew(), "New");
  ok( testNew(), "New");
  ok( testNew(), "New");
*/

  
  ok( okESIterator("",""), "Passed ");
  ok( okESIterator("sda",""), "Passed ");
  ok( okESIterator("dasdas",""), "Passed ");
  ok( okESIterator("s",""), "Passed ");
  ok( okESIterator(v1,v2), "Passed ");
  ok( okESIterator("","ds"), "Passed ");
  ok( okESIterator("","sdf"), "Passed ");
  ok( okESIterator("r","e"), "Passed ");
  ok( okESIterator("w","w"), "Passed ");
  ok( okESIterator("vcx",""), "Passed ");
  ok( okESIterator("sd","sf"), "Passed ");
  ok( okESIterator("sdfsdfsdfsdfs",""), "Passed ");
  
});



