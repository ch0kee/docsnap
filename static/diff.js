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


function DiffEngine(st) {
    this._syncText = st;
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
DiffEngine.prototype.getSES = function(newString) {
    var oldString = this._syncText;
    if (newString === oldString) {
        console.log('equal');
        return [this._createComponent(newString,'=')];
    }
    if (!newString) {
        console.log('no newtring');
        return [this._createComponent(oldString,'-')];
    }
    if (!oldString) {
        console.log('no oldString');
        return [this._createComponent(newString,'+')];
    }
    console.log('start algorithm');
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
    return json.diff;
}

DiffEngine.prototype.executeES1 = function(ses) {
    var oldString = this._syncText;

    var s = new ESIterator(ses);

    var result = "";
    for(var i = 0; s.valid();) {
        if (s.isInserting()) {
            result = result + s.current().value;
            s.advance();
        }

        if (i < oldString.length) {
            var keep = s.isPreserving();
            if (keep) {
                result += s.character();
            }
            ++i;
            s.advance();
        }
    }
    this._syncText = result;
    return result;
}


DiffEngine.prototype.executeES2 = function(ses1, ses2) {
    var oldString = this._syncText;

    var s1 = new ESIterator(ses1);
    var s2 = new ESIterator(ses2);

    var result = "";
    for(var i = 0; s1.valid() && s2.valid();) {
        if (s1.isInserting()) {
            result = result + s1.current().value;
            s1.advance();
        }
        if (s2.isInserting()) {
            result = result + s2.current().value;
            s2.advance();
        }

        if (i < oldString.length) {
            var keep = s1.isPreserving() && s2.isPreserving();
            if (keep) {
                result += s1.character();
            }
            ++i;
            s1.advance();
            s2.advance();
        }
    }
    this._syncText = result;
    return result;
}

function printBr(element, index, array) {
    document.write("<br />[" + index + "] is " + element );
}

if (typeof module !== 'undefined') {
    module.exports = JsDiff;
}
