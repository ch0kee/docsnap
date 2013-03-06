/*
// Set up a logger.
goog.debug.LogManager.getRoot().setLevel(goog.debug.Logger.Level.ALL);
var logger = goog.debug.Logger.getLogger('demo');
var logconsole = new goog.debug.DivConsole(goog.dom.getElement('log'));
logconsole.setCapturing(true);

var EVENTS = goog.object.getValues(goog.ui.Component.EventType);
logger.fine('Listening for: ' + EVENTS.join(', ') + '.');

function logEvent(e) {
  logger.info('"' + e.target.getCaption() + '" dispatched: ' + e.type);
}
*/

function initPage() {
    var editor = goog.dom.getElement('editor');
    var c3 = new goog.ui.Control();
    c3.decorate(editor);

    function SampleCustomRenderer() {
      goog.ui.ControlRenderer.call(this);
    }
    goog.inherits(SampleCustomRenderer, goog.ui.ControlRenderer);

}
goog.events.listen(window, goog.events.EventType.LOAD, initPage);
