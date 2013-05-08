




function typeCharacter(editor, char) {  
  editor.innerHTML = editor.innerHTML + char;
}

function runTestFunc() {

var text = " Lorem ipsum dolor sit amet, consectetur adipiscing eli\nt. Duis ut\n est blandit velit condimentum volutpat.\n Pellentes\nque habitant morbi tristiq\nue senectus et ne\ntus et malesuada fames ac turp\nis egestas. Q\nuisque nunc lacus, lacinia sed porttitor\n quis, iaculis at elit. Integer urna est, interdum qu\nis laoreet eget, porta eget tortor. Vivamus rutrum, velit non condimentum\n auctor, magna velit condimentum nulla, at varius urna magna a l\nibero. Nulla eget lacus massa, sed pellentesque eros. Nunc commo\ndo massa in sapien por\nttitor condimentum. Vestibulum sollicitud\nin vehicula mi vel elementum.\n\
Proin vita\ne dapibus augue. Aenean et purus non quam ultr\nices dapibus vit\nae eget est. Suspendisse sed quam ut urna fringilla dignissim.\n Sed malesuada massa eget eros laoreet ultrices. Maecenas facili\nsis laoreet iaculis. Quisque quis est in massa convallis condimentu\nm quis id eli\nt. Sed fermentum lobortis pellentesque.\n Etiam malesuada, orci nec varius mollis, elit felis egestas urna, non consect\netur mi turpis at nunc. Nulla vel lect\n\nus est.\n Praesent diam nibh, rhoncus eu vestibulum a, tempor a nisl. Duis vitae ligu\nla ipsum, eu lobortis n\nulla. Etiam leo lorem, sem\nper sit amet blandit sed, malesuada sed quam. Pellentesque luctus nisi sed l\norem feugiat et gravida lorem fau\ncibus. Ae\nnean ac magna justo. "



  var editor = selenium.getEval("this.browserbot.getUserWindow().document.getElementById('editor')");
  setInitialText(editor);
  funcs = generateFunctions(editor, text.length);
  runFunctions(funcs, text);
}


function   generateFunctions(editor, num) {
  var funcs = [];
  for(var i = 0; i < num; ++i) {
    funcs.push( function(c) {typeCharacter(editor, c); } );
  }
  
  return funcs;
}


function setInitialText(editor) {
  
  //var editor = document.getElementById('editor');
  editor.innerHTML = "_";
}

function runFunctions(functionArray, txt) {
  runNextFunction(functionArray, txt, 0);
}

function runNextFunction(functionArray, txt, i) {
  if (i < functionArray.length) {
   tim = Math.floor((Math.random()*300)+1); 
    setTimeout( function(){ 
       functionArray[i](txt[i]); 
       runNextFunction(functionArray, txt, i+1);
     }, tim);
  } else {
//    alert('done');
  }
}

var gotoLabels= {};
var whileLabels = {};

// overload the original Selenium reset function
Selenium.prototype.reset = function() {
    // reset the labels
    this.initialiseLabels();
    // proceed with original reset code
    this.defaultTimeout = Selenium.DEFAULT_TIMEOUT;
    this.browserbot.selectWindow("null");
    this.browserbot.resetPopups();
}


/*
* --- Initialize Conditional Elements --- *
* Run through the script collecting line numbers of all conditional elements
* There are three a results arrays: goto labels, while pairs and forEach pairs
*
*/
Selenium.prototype.initialiseLabels = function()
{
    gotoLabels = {};
    whileLabels = { ends: {}, whiles: {} };
    var command_rows = [];
    var numCommands = testCase.commands.length;
    for (var i = 0; i < numCommands; ++i) {
        var x = testCase.commands[i];
        command_rows.push(x);
    }
    var cycles = [];
    var forEachCmds = [];
    for( var i = 0; i < command_rows.length; i++ ) {
        if (command_rows[i].type == 'command')
        switch( command_rows[i].command.toLowerCase() ) {
            case "label":
                gotoLabels[ command_rows[i].target ] = i;
                break;
            case "while":
            case "endwhile":
                cycles.push( [command_rows[i].command.toLowerCase(), i] )
                break;
            case "foreach":
            case "endforeach":
                forEachCmds.push( [command_rows[i].command.toLowerCase(), i] )
                break;
        }
    }
    var i = 0;
    while( cycles.length ) {
        if( i >= cycles.length ) {
            throw new Error( "non-matching while/endWhile found" );
        }
        switch( cycles[i][0] ) {
            case "while":
                if( ( i+1 < cycles.length ) && ( "endwhile" == cycles[i+1][0] ) ) {
                    // pair found
                    whileLabels.ends[ cycles[i+1][1] ] = cycles[i][1];
                    whileLabels.whiles[ cycles[i][1] ] = cycles[i+1][1];
                    cycles.splice( i, 2 );
                    i = 0;
                } else ++i;
                break;
            case "endwhile":
                ++i;
                break;
        }
    }

}

Selenium.prototype.continueFromRow = function( row_num )
{
    if(row_num == undefined || row_num == null || row_num < 0) {
        throw new Error( "Invalid row_num specified." );
    }
    testCase.debugContext.debugIndex = row_num;
}

// do nothing. simple label
Selenium.prototype.doLabel = function(){};

Selenium.prototype.doGotoLabel = function( label )
{
    if( undefined == gotoLabels[label] ) {
        throw new Error( "Specified label '" + label + "' is not found." );
    }
    this.continueFromRow( gotoLabels[ label ] );
};

Selenium.prototype.doGoto = Selenium.prototype.doGotoLabel;

Selenium.prototype.doGotoIf = function( condition, label )
{
    if( eval(condition) ) this.doGotoLabel( label );
}

Selenium.prototype.doWhile = function( condition )
{
    if( !eval(condition) ) {
        var last_row = testCase.debugContext.debugIndex;
        var end_while_row = whileLabels.whiles[ last_row ];
        if( undefined == end_while_row ) throw new Error( "Corresponding 'endWhile' is not found." );
        this.continueFromRow( end_while_row );
    }
}

Selenium.prototype.doEndWhile = function()
{
    var last_row = testCase.debugContext.debugIndex;
    var while_row = whileLabels.ends[ last_row ] - 1;
    if( undefined == while_row ) throw new Error( "Corresponding 'While' is not found." );
    this.continueFromRow( while_row );
}

Selenium.prototype.doPush= function(value, varName)
{
    if(!storedVars[varName]) {
        storedVars[varName] = new Array();
    }
    if(typeof storedVars[varName] !== 'object') {
        throw new Error("Cannot push value onto non-array " + varName);
    } else {
        storedVars[varName].push(value);
    }
}
