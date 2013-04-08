// "[=14|+3:alm|-2]
//edit script iterator
function ESIterator (es) {
  this._index = 1;
  this._characterIndex = 0;
  this._es = es;
}

ESIterator.prototype.isEnd = function() {
  return this._es[ this._index ] == ']';
}

ESIterator.prototype.count = function() {
  return this.isEnd() ? null : parseInt( this._es.substr(this._index+1) );
}

ESIterator.prototype.type = function() {
  return this.isEnd() ? null : this._es[ this._index ];
}

ESIterator.prototype.baseContent = function() {
  var result = "";
  /*var i = 0;
  ++i;
  for(var i = 0; i < this._es.length; ++i) {
    if (this._es[i] != '+') {
      result += this._es[ i ].value;
    }
  }*/
  return result;
}

ESIterator.prototype.inserting = function () {
  return this.type() == '+';
}

ESIterator.prototype.removing = function () {
  return this.type() == '-';
}

ESIterator.prototype.preserving = function () {
  return this.type() == '=';
}

ESIterator.prototype.value = function() {
  if (this.inserting()) {
    var start = this._es.indexOf(':', this._index+1);
    if (start == -1) {
      alert('invalid input');
      return "";
    }
    var len = this.count();
    var val = this._es.substr(start+1, len);
    return val;
  } else {
    alert('not inserting');
    return "";
  }
}

ESIterator.prototype.advance =  function () {
  if (this.isEnd()) {
    alert('already isEnd()');
    return;
  }

  if (this.type() == '+' || this._characterIndex >= this.count()-1) {
    //megkeressük a következő |-t vagy ]-t
    var pat = /[|\]]/;
    var str = this._es.substr(this._index);
    var offset = str.search(pat);
    if (offset == -1) {
      alert('invalid input');
      return;
    }

    if (str[offset] == ']') {
      //nincs több input
      this._index += offset;
    } else {
      //| után jön a köv. input
      this._index += offset + 1;
    }
    this._characterIndex = 0;
  } else {
    ++this._characterIndex;
  }
}


ESIterator.prototype.valid = function() {
  return !this.isEnd();
}


// "[=14|+3:alm|-2]
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

// "[=14|+3:alm|-2]
//ses1-nek magasabb a prioritasa
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
    value += editScript[i].value.length.toString();
    if (editScript[i].type == '+') {
      value += ':';
      value += editScript[i].value;
    }
  }
  value += ']';
  return value;
}

//get Shortest Edit Script
DiffEngine.prototype.getShortestEditScript = function(oldString, newString) {
  if (newString === oldString) {
    return this._fixify( [this._createComponent(newString,'=')] );
  }
  if (!newString) {
    console.log('no newString');
    return this._fixify( [this._createComponent(oldString,'-')] );
  }
  if (!oldString) {
    console.log('no oldString');
    return this._fixify( [this._createComponent(newString,'+')] );
  }
  var bestPath = [{ x: -1, components: [] }];
  var bestPath_y = this._extractCommon(bestPath[0], newString, oldString, 0);

  var newLen = newString.length,
      oldLen = oldString.length;
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

if (typeof module !== 'undefined') {
  module.exports = JsDiff;
}
