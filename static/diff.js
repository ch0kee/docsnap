/*
 * Text diff implementation.

 * These methods are based on the implementation proposed in
 * "An O(ND) Difference Algorithm and its Variations" (Myers, 1986).
 * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.6927
 */


function ChangeIterator (changes) {
    this.currentChangeIndex = 0;
    this.currentCharacterIndex = 0;
    this.changesArray = changes;
}

ChangeIterator.prototype.currentChange = function() {
    return this.changesArray[ this.currentChangeIndex ];
}

ChangeIterator.prototype.stepCharacter =  function () {
    if (!this.currentChangeIs('+') && this.currentCharacterIndex < this.currentChange().len-1) {
        ++this.currentCharacterIndex;
    } else {
        ++this.currentChangeIndex;
        this.currentCharacterIndex = 0;
    }
}
/*
ChangeIterator.prototype.currentCharacter = function () {
    if (!this.current
}*/

ChangeIterator.prototype.doesntChange = function() {
    return this.currentChangeIs('=');
}

ChangeIterator.prototype.currentChangeIs = function(t) {
    return this.currentChange().typ == t;
}

ChangeIterator.prototype.valid = function() {
    return this.currentChangeIndex < this.changesArray.length;
}

ChangeIterator.prototype.insertedText = function() {
    if (this.valid() && this.currentChangeIs('+')) {
        return this.currentChange().val;
    } else
        return null;
}


var JsDiff = (function() {
  /*jshint maxparams: 5*/
  function clonePath(path) {
    return { newPos: path.newPos, components: path.components.slice(0) };
  }


  var Diff = function() {
  };
  Diff.prototype = {
      diff: function(oldString, newString) {
        // Handle the identity case (this is due to unrolling editLength == 0
        if (newString === oldString) {
          return [{ value: newString }];
        }
        if (!newString) {
          return [{ value: oldString, removed: true }];
        }
        if (!oldString) {
          return [{ value: newString, added: true }];
        }

        var newLen = newString.length, oldLen = oldString.length;
        var maxEditLength = newLen + oldLen;
        var bestPath = [{ newPos: -1, components: [] }];

        // Seed editLength = 0
        var oldPos = this.extractCommon(bestPath[0], newString, oldString, 0);
        if (bestPath[0].newPos+1 >= newLen && oldPos+1 >= oldLen) {
          return bestPath[0].components;
        }

        for (var editLength = 1; editLength <= maxEditLength; editLength++) {
          for (var diagonalPath = -1*editLength; diagonalPath <= editLength; diagonalPath+=2) {
            var basePath;
            var addPath = bestPath[diagonalPath-1],
                removePath = bestPath[diagonalPath+1];
            oldPos = (removePath ? removePath.newPos : 0) - diagonalPath;
            if (addPath) {
              // No one else is going to attempt to use this value, clear it
              bestPath[diagonalPath-1] = undefined;
            }

            var canAdd = addPath && addPath.newPos+1 < newLen;
            var canRemove = removePath && 0 <= oldPos && oldPos < oldLen;
            if (!canAdd && !canRemove) {
              bestPath[diagonalPath] = undefined;
              continue;
            }

            // Select the diagonal that we want to branch from. We select the prior
            // path whose position in the new string is the farthest from the origin
            // and does not pass the bounds of the diff graph
            if (!canAdd || (canRemove && addPath.newPos < removePath.newPos)) {
              basePath = clonePath(removePath);
              this.pushComponent(basePath.components, oldString[oldPos], undefined, true);
            } else {
              basePath = clonePath(addPath);
              basePath.newPos++;
              this.pushComponent(basePath.components, newString[basePath.newPos], true, undefined);
            }

            var oldPos = this.extractCommon(basePath, newString, oldString, diagonalPath);

            if (basePath.newPos+1 >= newLen && oldPos+1 >= oldLen) {
              return basePath.components;
            } else {
              bestPath[diagonalPath] = basePath;
            }
          }
        }
      },

      //vagy az utolsóhoz fűzzük, ha lehet, egyébként új komponens
      pushComponent: function(components, value, added, removed) {
        var last = components[components.length-1];
        if (last && last.added === added && last.removed === removed) {
          // We need to clone here as the component clone operation is just
          // as shallow array clone
          components[components.length-1] =
            {value: this.join(last.value, value), added: added, removed: removed };
        } else {
          components.push({value: value, added: added, removed: removed });
        }
      },
      extractCommon: function(basePath, newString, oldString, diagonalPath) {
        var newLen = newString.length,
            oldLen = oldString.length,
            newPos = basePath.newPos,
            oldPos = newPos - diagonalPath;

        //consume snake
        while (newPos+1 < newLen && oldPos+1 < oldLen && this.equals(newString[newPos+1], oldString[oldPos+1])) {
          newPos++;
          oldPos++;

          this.pushComponent(basePath.components, newString[newPos], undefined, undefined);
        }
        basePath.newPos = newPos;
        return oldPos;
      },

      equals: function(left, right) {
        return left === right;
      },
      join: function(left, right) {
        return left + right;
      }
  };

  var CharDiff = new Diff();

  return {
    Diff: Diff,

    diffChars: function(oldStr, newStr) { return CharDiff.diff(oldStr, newStr); },
    diffCharChanges: function(oldStr, newStr) {
        cdiffs = CharDiff.diff(oldStr, newStr);
        changes = new Array();
        for(var i = 0; i < cdiffs.length; ++i) {
            var change = null;

            change = new Object();
            if (cdiffs[i].added) {
                change.typ = '+';
                change.val = cdiffs[i].value;
            } else if (cdiffs[i].removed) {
                change.typ = '-';
                change.len = cdiffs[i].value.length;
            } else {
                change.typ = '='; //skip
                change.len = cdiffs[i].value.length;
            }
            changes.push(change);
        }
        return changes;
    },

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
    }

  };
})();

if (typeof module !== 'undefined') {
    module.exports = JsDiff;
}
