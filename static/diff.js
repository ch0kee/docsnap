/*
 * Text diff implementation.

 * These methods are based on the implementation proposed in
 * "An O(ND) Difference Algorithm and its Variations" (Myers, 1986).
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927
 */


//shortest edit script iterator
function SESIterator (ses) {
    this._index = 0;
    this._characterIndex = 0;
    this._ses = ses;
}

SESIterator.prototype.current = function () {
    return this._ses[ this._index ];
}

SESIterator.prototype.stepCharacter =  function () {
    var c = this.current();
    if (c.type != '+' && this._characterIndex < c.value.length-1) {
        ++this._characterIndex;
    } else {
        ++this._index;
        this._characterIndex = 0;
    }
}
/*
SESIterator.prototype.currentCharacter = function () {
    if (!this.current
}*/

SESIterator.prototype.doesntChange = function() {
    return this.currentChangeIs('=');
}

SESIterator.prototype.currentChangeIs = function(t) {
    return this.currentChange().type == t;
}

SESIterator.prototype.valid = function() {
    return this._index < this._ses.length;
}

SESIterator.prototype.insertion = function() {
    if (this.valid() && this.currentChangeIs('+')) {
        return this.currentChange().value;
    } else
        return null;
}

function SESPairIterator(s1, s2) {
    this._s1 = s1;
    this._s2 = s2;
}

SESPairIterator.prototype.insertion() {
    var ins1 = _s1.insertion();
    var ins2 = _s2.insertion();
    return ins1 + ins2;
}

SESPairIterator.prototype.insertion() {
    var ins1 = _s1.insertion();
    var ins2 = _s2.insertion();
    return ins1 + ins2;
}



function DiffEngine() {
    //koordinatak a racson
    //x : elso-beli (remove)
    //y : masodik-beli (insert)
}

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
        basePath_y = basePath.x - diagonalPath;
    var plus = 0;
    for(;basePath.x+plus+1 < newMaxIndex && basePath_y+plus+1 < oldLen
        && equals(newString[basePath.x+plus+1],oldString[basePath_y+plus+1])
        ;++plus);
    this._pushComponent(basePath.components, newString.slice(basePath.x, basePath.x+plus), '=');
    basePath.x += plus;
    return basePath_y + plus;
}


//get Shortest Edit Script
DiffEngine.prototype.getSES = function(oldString, newString) {
    if (newString === oldString) {
        return this._createComponent(newString,'=');
    }
    if (!newString) {
        return this._createComponent(oldString,'-');
    }
    if (!oldString) {
        return this._createComponent(newString,'+');
    }
    var bestPath = [{ x: -1, components: [] }];
    var bestPath_y = this._extractCommon(bestPath[0], newString, oldString, 0);

    var newLen = newString.length,
        oldLen = oldString.length,
    if (bestPath[0].x+1 >= newLen && bestPath_y+1 >= oldLen) {
        return bestPath[0].components;
    }

    //
    var maxEditLength = newLen + oldLen,
    for (var editLength = 1; editLength <= maxEditLength; ++editLength) {
        for (var diagonalPath = -1*editLength; diagonalPath <= editLength; diagonalPath+=2) {
            var basePath;
            var addPath = bestPath[diagonalPath-1],
                removePath = bestPath[diagonalPath+1];
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
                basePath = _clonePath(removePath);
                this._pushComponent(basePath.components, oldString[removePath_y], '-');
            } else {
                basePath = _clonePath(addPath);
                this._pushComponent(basePath.components, newString[++basePath.x], '+');
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

DiffEngine.prototype.convertJsonToSes = function(json) {
    return json.data;
}

DiffEngine.prototype.execute2 = function(ses1, ses2, oldString) {
    var s1 = new SESIterator(ses1);
    var s2 = new SESIterator(ses2);

    var result = "";
    for(var i = 0; i < oldString.length; ++i) {
        if (s1.isInserting()) {
            result = result + s1.current().value;
            s1.advance();
        }
        if (s2.isInserting()) {
            result = result + s2.current().value;
            s1.advance();
        }

        //process insertions
        if (s1[j].type == '+') {
            result = result + s1[j].value;
            ++j;
        }
        if (s2[k].type == '+') {
            result = result + s2[k].value;
            ++k;
        }

        //from here we deal only with '-' and '='
        //longest common
        var commonLen = 0;
        var type = '';
        while(s1[j].type === s2[k].type) {
            type = s1[j].type;
            if (type == '=') {

            } else if (type == '-'){
            }
            ++commonLen;

            if (s1[j].value
        }
        //next

        var s1rem = s1.
        while (s1[j] == s2[k]) {

        }
    }
}

    //egyelőre gagyi, de műxik
    applyCharChanges1: function(oldText, ch) {
        cit = new ChangeIterator(ch);
        var text = "";
        for(var i = 0; cit.valid(); ++i) {
            var ins = cit.insertedText();
            if (ins != null) {
                text += ins;
                cit.stepCharacter();
            }

            //itt az i. karakteren vagyunk
            if (cit.valid()) {

                if (cit.doesntChange()) {
                    text += oldText[i];
                }
                cit.stepCharacter();
            }

        }
        return text;
    },
    applyCharChanges2: function(oldText, ch1, ch2) {
        cit1 = new ChangeIterator(ch1);
        cit2 = new ChangeIterator(ch2);
        var text = "";
        for(var i = 0; cit1.valid() || cit2.valid(); ++i) {
            var ins1 = cit1.insertedText();
            var ins2 = cit2.insertedText();
            if (ins1 != null) {
                text += ins1;
            }
            if (ins2 != null) {
                text += ins2;
            }

            //itt mindkettőben a 0. karakteren vagyunk
            if ((cit1.valid() && cit1.doesntChange()) || (cit2.valid() && cit2.doesntChange())) {
                text += oldText[i];
            }

            if (cit1.valid()) {
                cit1.stepCharacter();
            }
            if (cit2.valid()) {
                cit2.stepCharacter();
            }

        }
        return text;
    },

    function printBr(element, index, array) {
    document.write("<br />[" + index + "] is " + element );
    }


    applyCharChangesN: function(oldText, changes) {
    //returns changedTexts[changes.length] : Array
        var cits = new Array();
        for(var i = 0

        cit1 = new ChangeIterator(ch1);
        cit2 = new ChangeIterator(ch2);
        var text = "";
        for(var i = 0; cit1.valid() || cit2.valid(); ++i) {
            var ins1 = cit1.insertedText();
            var ins2 = cit2.insertedText();
            if (ins1 != null) {
                text += ins1;
            }
            if (ins2 != null) {
                text += ins2;
            }

            //itt mindkettőben a 0. karakteren vagyunk
            if ((cit1.valid() && cit1.doesntChange()) || (cit2.valid() && cit2.doesntChange())) {
                text += oldText[i];
            }

            if (cit1.valid()) {
                cit1.stepCharacter();
            }
            if (cit2.valid()) {
                cit2.stepCharacter();
            }

        }
        return text;
    }

  };
})();

if (typeof module !== 'undefined') {
    module.exports = JsDiff;
}
