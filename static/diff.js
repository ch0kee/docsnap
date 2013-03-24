/*
 * Text diff implementation.

 * These methods are based on the implementation proposed in
 * "An O(ND) Difference Algorithm and its Variations" (Myers, 1986).
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927
 */


//edit script iterator
function ESIterator (es) {
    this._index = 0;
    this._characterIndex = 0;
    this._es = es;
}

ESIterator.prototype.baseContent = function() {
  var result = "";
  for(var i = 0; i < this._es.length; ++i) {
    if (this._es[ i ].type != '+') {
      result += this._es[ i ].value;
    }
  }
  return result;
}


ESIterator.prototype.current = function () {
    return this._es[ this._index ];
}

ESIterator.prototype.isInserting = function () {
    return this.current().type == '+';

}
ESIterator.prototype.isRemoving = function () {
    return this.current().type == '-';
}
ESIterator.prototype.isPreserving = function () {
    return this.current().type == '=';
}

ESIterator.prototype.character = function() {
    return this.current().value[ this._characterIndex ];
}

ESIterator.prototype.advance =  function () {
  var c = this.current();
  if (c.type != '+' && this._characterIndex < c.value.length-1) {
    ++this._characterIndex;
  } else {
    ++this._index;
    this._characterIndex = 0;
  }
}


ESIterator.prototype.valid = function() {
    return this._index < this._es.length;
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


//get Shortest Edit Script
DiffEngine.prototype.getShortestEditScript = function(oldString, newString) {
  if (newString === oldString) {
    return [this._createComponent(newString,'=')];
  }
  if (!newString) {
    console.log('no newString');
    return [this._createComponent(oldString,'-')];
  }
  if (!oldString) {
    console.log('no oldString');
    return [this._createComponent(newString,'+')];
  }
  var bestPath = [{ x: -1, components: [] }];
  var bestPath_y = this._extractCommon(bestPath[0], newString, oldString, 0);

  var newLen = newString.length,
      oldLen = oldString.length;
  if (bestPath[0].x+1 >= newLen && bestPath_y+1 >= oldLen) {
    return bestPath[0].components;
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
          return basePath.components;
      } else {
          bestPath[diagonalPath] = basePath;
      }
    }
  }
}


DiffEngine.prototype.executeES1 = function(content, ses) {
    var s = new ESIterator(ses);

    var result = "";
    for(var i = 0; s.valid();) {
        while (s.valid() && s.isInserting()) {
            result = result + s.current().value;
            s.advance();
        }

        if (s.valid() && i < content.length) {
            var keep = s.isPreserving();
            if (keep) {
                result += s.character();
            }
            ++i;
            s.advance();
        }
    }
    return result;
}

//ses1-nek magasabb a prioritasa
DiffEngine.prototype.executeES2 = function(content, ses1, ses2) {

    var s1 = new ESIterator(ses1);
    var s2 = new ESIterator(ses2);

    //debug-kod
    if (s1.baseContent() != s2.baseContent()) {
      alert('WARNING');
      console.log('INVALID SIMULTANEOUS EXECUTE');
    }


    var result = "";
    for(var i = 0; s1.valid() || s2.valid();) {
        //bedaraljuk a fuggo inserteket
        while (s1.valid() && s1.isInserting()) {
            result = result + s1.current().value;
            s1.advance();
        }
        while (s2.valid() && s2.isInserting()) {
            result = result + s2.current().value;
            s2.advance();
        }

        if (s1.valid() && s2.valid() && i < content.length) {
            var keep = s1.isPreserving() && s2.isPreserving();
            if (keep) {
                result += s1.character();
            }
            ++i;
            s1.advance();
            s2.advance();
        }
    }
    return result;
}

function printBr(element, index, array) {
    document.write("<br />[" + index + "] is " + element );
}

if (typeof module !== 'undefined') {
    module.exports = JsDiff;
}
