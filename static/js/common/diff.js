//edit script iterator
function ESIterator (es) {
  this._index = 0;
  this._resetCharCounter();
  this._es = es;
}

ESIterator.prototype._resetCharCounter = function() {
  this._charCounter = 1;
}

ESIterator.prototype.isEnd = function() {
  return this._index >= this._es.length;
}

ESIterator.prototype.inserting = function () {
  return this._es[ this._index ].I !== undefined;
}

ESIterator.prototype.removing = function () {
  return this._es[ this._index ].R !== undefined;
}

ESIterator.prototype.preserving = function () {
  return this._es[ this._index ].P !== undefined;
}

ESIterator.prototype.value = function() {
  //assert (this.inserting())
  if (this.inserting()) {
    return this._es[ this._index ].I;
  } else {
    alert('error');
    return "";
  }
}

ESIterator.prototype.advance =  function () {
  if (this.isEnd()) {
    alert('error, already isEnd()');
    return;
  }
  
  if (this.inserting()) {
    ++this._index;
  } else {
    if (this.removing()) {
      if (this._charCounter >= this._es[ this._index ].R) {
        this._resetCharCounter();
        ++this._index;
      } else {
        ++this._charCounter;
      }
    } else if (this.preserving()) {
      if (this._charCounter >= this._es[ this._index ].P) {
        this._resetCharCounter();
        ++this._index;
      } else {
        ++this._charCounter;
      }    
    }
  }
}


ESIterator.prototype.valid = function() {
  return !this.isEnd();
}


DiffEngine.prototype.executeES1 = function(content, ses) {
  var s = new ESIterator(ses);

  var result = "";
  for(var i = 0; s.valid();) {
    while (s.valid() && s.inserting()) {
      result = result + s.value();
      s.advance();
    }

    if (s.valid() && i < content.length) {
      var keep = s.preserving();
      if (keep) {
        result += content[i];
      }
      ++i;
      s.advance();
    }
  }
  return result;
}

//ses1-ben lévő insert előlrébb kerül be ugyanazon indexen!
DiffEngine.prototype.executeES2 = function(content, ses1, ses2) {

  var s1 = new ESIterator(ses1);
  var s2 = new ESIterator(ses2);

  var result = "";
  for(var i = 0; s1.valid() || s2.valid();) {
    //bedaraljuk a fuggo inserteket
    while (s1.valid() && s1.inserting()) {
      result = result + s1.value();
      s1.advance();
    }
    while (s2.valid() && s2.inserting()) {
      result = result + s2.value();
      s2.advance();
    }

    if (s1.valid() && s2.valid() && i < content.length) {
      var keep = s1.preserving() && s2.preserving();
      if (keep) {
        result += content[i];
      }
      ++i;
      s1.advance();
      s2.advance();
    }
  }
  return result;
}

function DiffEngine() { }

DiffEngine.prototype._equals = function(left, right) {
  return left === right;
}

DiffEngine.prototype._clonePath = function(path) {
  return { x: path.x, components: path.components.slice(0) };
}

DiffEngine.prototype._createComponent= function(value, type) {
  return { value: value, type: type };
}

DiffEngine.prototype._pushComponent = function(components, value, type) {
  var last = components[components.length-1];
  if (last && last.type === type) {
    components[components.length-1] = this._createComponent(last.value + value, type);
  } else {
    components.push(this._createComponent(value, type));
  }
}

DiffEngine.prototype._extractCommon = function(basePath, newString, oldString, diagonalPath) {
  var newLen = newString.length,
      oldLen = oldString.length,
      x = basePath.x,
      y = x - diagonalPath;
      //basePath_y = basePath.x - diagonalPath;
  while (x+1 < newLen && y+1 < oldLen && this._equals(newString[x+1], oldString[y+1])) {
    ++x;
    ++y;
    this._pushComponent(basePath.components, newString[x], '=');
  }
  basePath.x = x;
  return y;
}

// "[=14|+3:alm|-2]
/*
DiffEngine.prototype._fixify = function(editScript) {
  var value = "[";
  for(var i = 0; i < editScript.length; ++i) {

    if (editScript[i].value.length == 0) {
      continue;
    }


    if (i > 0) {
      value += '|';
    }

    value += editScript[i].type;
//    value += editScript[i].value.length.toString();
// editScript[i].value nem egy sztring, hanem sztringek listája
    var len = 0;
    for(var j = 0; j < editScript[i].value.length; ++j) {
      len += editScript[i].value[j].length;
    }
    value += len.toString();     
    if (editScript[i].type == '+') {
      value += ':';
//      value += editScript[i].value;
      for(var j = 0; j < editScript[i].value.length; ++j) {
        value += editScript[i].value[j];
      }     
    }
  }
  value += ']';
  return value;
}
*/

//invariáns ?
//returns : [ { I: "..." }, { P:24 }, { R:5 } ] style object
DiffEngine.prototype._fixify = function(editScript) {
  var result = [];
  for(var i = 0; i < editScript.length; ++i) {
    /*if (editScript[i].value.length == 0) {
      continue;
    }*/
    var thisEdit = { };
    
    var value = "";
    for(var j = 0; j < editScript[i].value.length; ++j) {
      value += editScript[i].value[j];
    }
//    var value = editScript[i].value;//.join('');
    switch(editScript[i].type) {
      case '+':
        thisEdit.I = value;
        break;
      case '-':
        thisEdit.R = value.length;
        break;
      case '=':
        thisEdit.P = value.length;
        break;
      default:
        alert('error'); //!!!!!!
        return null;
    }
    
    result.push(thisEdit);
  }
  return result;
}


//get Shortest Edit Script
DiffEngine.prototype.getShortestEditScript = function(oldString, newString) {
  if (newString === oldString) {
    return this._fixify( [this._createComponent(newString,'=')] );
  }
  var newLen = newString.length,
      oldLen = oldString.length;

  if (newLen == 0) {
    console.log('no newString');
    return this._fixify( [this._createComponent(oldString,'-')] );
  }
  if (oldLen == 0) {
    console.log('no oldString');
    return this._fixify( [this._createComponent(newString,'+')] );
  }
  var bestPath = [{ x: -1, components: [] }];
  var bestPath_y = this._extractCommon(bestPath[0], newString, oldString, 0);

  if (bestPath[0].x+1 >= newLen && bestPath_y+1 >= oldLen) {
    return this._fixify( bestPath[0].components );
  }

  //
  var maxEditLength = newLen + oldLen;
  for (var editLength = 1; editLength <= maxEditLength; ++editLength) {
    for (var diagonalPath = -1*editLength; diagonalPath <= editLength; diagonalPath+=2) {
      var basePath;
      var addPath = bestPath[diagonalPath-1],
          removePath = bestPath[diagonalPath+1];

      //bestPath_y = (removePath ? removePath.x : 0) - diagonalPath;
      removePath_y = (removePath ? removePath.x : 0) - diagonalPath;

      if (addPath) {
        // No one else is going to attempt to use this value, clear it
        bestPath[diagonalPath-1] = undefined;
      }

      var canAdd = addPath && addPath.x+1 < newLen;
      var canRemove = removePath && 0 <= removePath_y && removePath_y < oldLen;
      if (!canAdd && !canRemove) {
        bestPath[diagonalPath] = undefined;
        continue;
      }

      // Select the diagonal that we want to branch from. We select the prior
      // path whose position in the new string is the farthest from the origin
      // and does not pass the bounds of the diff graph
      if (!canAdd || (canRemove && addPath.x < removePath.x)) {
        basePath = this._clonePath(removePath);
        this._pushComponent(basePath.components, oldString[removePath_y], '-');
      } else {
        basePath = this._clonePath(addPath);
        ++basePath.x;
        this._pushComponent(basePath.components, newString[basePath.x], '+');
      }

      var basePath_y = this._extractCommon(basePath, newString, oldString, diagonalPath);
      if (basePath.x+1 >= newLen && basePath_y+1 >= oldLen) {
        return this._fixify( basePath.components );
      } else {
        bestPath[diagonalPath] = basePath;
      }
    }
  }
}
