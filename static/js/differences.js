//@ edit script iterator teszteléshez
function ESIterator1 (baseString, editScript) {
  this._index = 0;
  this._es = editScript;
  this._baseString = baseString;
}

//@ iterátor léptetése
ESIterator1.prototype.advance =  function () {
  ++this._index;
};

ESIterator1.prototype.value = function() {
  if (this._index >= this._es.length) {
    //@ maradék
    var rest = this._baseString;
    this._baseString = ""; 
    return rest;
  }

  if(!!( this._es[ this._index ].I )) {
    //@beszúrunk
    return this._es[ this._index ].I;
  } else if (!!( this._es[ this._index ].R )) {
    //@ baseString = drop R baseString
    this._baseString = this._baseString.slice( this._es[ this._index ].R );
    return "";
//    this._baseString.
  } else if (!!( this._es[ this._index ].P )) {
    //@ keep = take P baseString
    var keep = this._baseString.slice(0, this._es[ this._index ].P);
    this._baseString = this._baseString.slice( this._es[ this._index ].P );
    return keep;
  }
};

//@ end of script
ESIterator1.prototype.eos = function() {
  return this._baseString.length == 0 && this._index >= this._es.length;
};



//@ edit script iterator
function ESIterator (editScript) {
  this._index = 0;
  this._resetCharCounter();
  this._es = editScript;
}

ESIterator.prototype._resetCharCounter = function() {
  this._charCounter = 1;
};

ESIterator.prototype.isEnd = function() {
  return this._index >= this._es.length;
};

//@ nem lehet insert vagy remove, ha vége van a módosításoknak
ESIterator.prototype.inserting = function () {
  return !this.isEnd() && this._es[ this._index ].I !== undefined;
};

ESIterator.prototype.removing = function () {
  return !this.isEnd() && this._es[ this._index ].R !== undefined;
};

//@ viszont ha nincs több módosítás, akkor preserve van (megőrzés)
ESIterator.prototype.preserving = function () {
  return this.isEnd() || this._es[ this._index ].P !== undefined ;
};

ESIterator.prototype.value = function() {
  //assert (this.inserting())
  return this._es[ this._index ].I;
};

//@ törlendő/megőrzendő karakterek száma
//@ beszúrásra 0-t add vissza
ESIterator.prototype.count = function() {
  return (this._es[ this._index ].R || this._es[ this._index ].P || 0);
}

//@ iterátor léptetése
ESIterator.prototype.advance =  function () {
  if (this.isEnd()) {
    return;
  }
  if (this._charCounter >= this.count()) {
    this._resetCharCounter();
    ++this._index;
  } else {
    ++this._charCounter;
  }
};

DocSnap.Differences = {}

//@ változtatások alkalmazása
DocSnap.Differences.executeES1 = function(content, ses) {
  var s = new ESIterator(ses);

  var result = "";
  for(var i = 0; !s.isEnd();) {
    //@ amíg van mit beszúrni, tegyünk úgy
    while (s.inserting()) {
      result = result + s.value();
      s.advance();
    }
    
    //c ha még nem értünk a végére
    if (i < content.length) {
      //@ őrizzük meg
      if (s.preserving()) {
        result += content[i];
      }
      ++i;
      s.advance();
    }
  }
  return result;
};

//@ változtatások alkalmazása (teszteléshez)
DocSnap.Differences.executeES1v2 = function(content, es) {
  var result = "";
  for(var s = new ESIterator1(content, es); !s.eos(); s.advance()) {
    result += s.value();
  }
  return result;
}

//@ két változtatáslista párhuzamos alkalmazása
//ses1-ben lévő insert előlrébb kerül be ugyanazon indexen!
DocSnap.Differences.executeES2 = function(content, ses1, ses2) {

  var s1 = new ESIterator(ses1);
  var s2 = new ESIterator(ses2);

  var result = "";
  for(var i = 0; !s1.isEnd() || !s2.isEnd();) {
    //bedaraljuk a fuggo inserteket
    while (s1.inserting()) {
      result = result + s1.value();
      s1.advance();
    }
    while (s2.inserting()) {
      result = result + s2.value();
      s2.advance();
    }

    if (i < content.length) {
      if (s1.preserving() && s2.preserving()) {
        result += content[i];
      }
      ++i;
      s1.advance();
      s2.advance();
    }
  }
  return result;
};

//@ útvonal klónozása
DocSnap.Differences._clonePath = function(path) {
  return { x: path.x, components: path.components.slice(0) };
};

//@ komponens létrehozása
DocSnap.Differences._createComponent= function(value, type) {
  return { value: value, type: type };
};

//@ komponens hozzáfűzése
DocSnap.Differences._pushComponent = function(components, value, type) {
  var last = components[components.length-1];
  if (last && last.type === type) {
    components[components.length-1] = this._createComponent(last.value + value, type);
  } else {
    components.push(this._createComponent(value, type));
  }
};

//@ megegyező prefix hozzáfűzése
DocSnap.Differences._pushCommonPrefix = function(basePath, newString, oldString, diagonalPath) {
  var newLen = newString.length,
      oldLen = oldString.length,
      x = basePath.x,
      y = x - diagonalPath;

  while (x+1 < newLen && y+1 < oldLen && newString[x+1] === oldString[y+1]) {
    ++x; ++y;
    this._pushComponent(basePath.components, newString[x], '=');
  }
  
  basePath.x = x;
  return y;
};

//returns : [ { I: "..." }, { P:24 }, { R:5 } ] style object
DocSnap.Differences._fixify = function(editScript) {
  var result = [];
  for(var i = 0; i < editScript.length; ++i) {
    var thisEdit = { };
    
    var value = "";
    for(var j = 0; j < editScript[i].value.length; ++j) {
      value += editScript[i].value[j];
    }
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
        //!!!!!!
        return null;
    }
    
    result.push(thisEdit);
  }
  if (result.length == 1 && result[0].P !== undefined) {
    result = [];
  }
  if (result.length == 1 && result[0].R !== undefined && result[0].R == 0) {
    result = [];
  }  
  if (result.length == 1 && result[0].I !== undefined && result[0].I == "") {
    result = [];
  }  
  return result;
};


//get Shortest Edit Script
DocSnap.Differences.getShortestEditScript = function(oldString, newString) {
  if (newString === oldString) {
    return this._fixify( [this._createComponent(newString,'=')] );
  }

  var newLen = newString.length;
  if (newLen == 0) {
    return this._fixify( [this._createComponent(oldString,'-')] );
  }
  
  var oldLen = oldString.length;
  if (oldLen == 0) {
    return this._fixify( [this._createComponent(newString,'+')] );
  }
  
  var bestPath = [{ x: -1, components: [] }];
  var bestPath_y = this._pushCommonPrefix(bestPath[0], newString, oldString, 0);

  if (bestPath[0].x+1 >= newLen && bestPath_y+1 >= oldLen) {
    return this._fixify( bestPath[0].components );
  }

  var maxEditLength = newLen + oldLen;
  for (var editLength = 1; editLength <= maxEditLength; ++editLength) {
    for (var diagonalPath = -1*editLength; diagonalPath <= editLength; diagonalPath+=2) {
      var basePath;
      var addPath = bestPath[diagonalPath-1],
          removePath = bestPath[diagonalPath+1];

      removePath_y = (removePath ? removePath.x : 0) - diagonalPath;

      if (addPath) {
        bestPath[diagonalPath-1] = undefined;
      }

      var canAdd = addPath && addPath.x+1 < newLen;
      var canRemove = removePath && 0 <= removePath_y && removePath_y < oldLen;
      if (!canAdd && !canRemove) {
        bestPath[diagonalPath] = undefined;
        continue;
      }

      if (!canAdd || (canRemove && addPath.x < removePath.x)) {
        basePath = this._clonePath(removePath);
        this._pushComponent(basePath.components, oldString[removePath_y], '-');
      } else {
        basePath = this._clonePath(addPath);
        ++basePath.x;
        this._pushComponent(basePath.components, newString[basePath.x], '+');
      }

      var basePath_y = this._pushCommonPrefix(basePath, newString, oldString, diagonalPath);
      if (basePath.x+1 >= newLen && basePath_y+1 >= oldLen) {
        return this._fixify( basePath.components );
      } else {
        bestPath[diagonalPath] = basePath;
      }
    }
  }
};

//@ változások összegyűjtése
DocSnap.collectEditsSince = function (since, current) {
  var edits = this.Differences.getShortestEditScript(this.tokenize(since)
                                    ,this.tokenize(current));
//üres listát adjunk vissza, ha nem változott semmi
//todo ezt a getShortestEditScript csinálja

  return edits;
};


//@ tokenize
//@ 'data' - string : feldarabolandó szöveg
//@ Tokenekre bontja a szöveget az összehasonításhoz.
//@ returns: Olyan tömböt ad vissza, amelynek minden eleme
//@ vagy egy karakter, vagy pedig egy html jelölőnyelvi elem.
DocSnap.tokenize = function (data) {
  var tokens = [];
  var len = data.length;
  var idx = 0;
  var tokenizerRegex = new RegExp('<[^>]+>|&[a-z]+;|<\/[^>]+>', 'g');
  var res = null;
  while ( (res = tokenizerRegex.exec(data)) ) {
    //megelozo karakterek
    for(var i = idx; i < res.index; ++i) {
      tokens.push(data[i]);
    }

    tokens.push(res[0]);

    idx = res.index + res[0].length;
  }
  for(var i = idx; i < len; ++i) {
    tokens.push(data[i]);
  }
  return tokens;
};
