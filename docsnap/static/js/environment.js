/* environment.js
 környezet beállítása
*/

//@ DocSnap névtér
var DocSnap = {

  //@ alaphelyzetben nem tud commitolni
  __CANCOMMIT: false,
  /* ez a változó mondja meg, hogy küldhetünk-e be módosítást.
   * fontos azonban megjegyezni, hogy a szerver mindenképp visszadobja
   * az olvasók módosításait, ha esetleg valaki megpiszkálja a kódot.
  */

  //@ új verziók beküldése között eltelt idő
  __syncInterval: 1000
};

