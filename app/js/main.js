function getSelected(id,callback) // javascript
{
    // obtain the object reference for the <textarea>
    var txtarea = document.getElementById(id);
    // obtain the index of the first selected character
    var start = txtarea.selectionStart;
    // obtain the index of the last selected character
    var finish = txtarea.selectionEnd;
    // obtain the selected text
    var sel = txtarea.value.substring(start, finish);
    var row = txtarea.value.substr(0,txtarea.selectionStart).split("\n").length;
     // do something with the selected content
    Shiny.setInputValue(callback,{"text": sel,"line":row}, {priority: "event"});
}
/*
$(function() {
        $(this).bind("contextmenu", function(e) {
            e.preventDefault();
            Shiny.setInputValue('onMouseRightClicked','', {priority: 'event'});
            return false;
        });
}); */

// Handler para tocar áudio por id (com suporte a módulos NS)
Shiny.addCustomMessageHandler("play-audio", ({ id }) => {
  const el = document.getElementById(id);
  if (!el) return;
  try {
    el.currentTime = 0;
    void el.play(); // alguns navegadores exigem gesture prévio
  } catch (e) {
    console.warn("Falha ao tocar áudio:", e);
  }
});

// destroy leaft
Shiny.addCustomMessageHandler('leaflet-destroy', function(id){
    var el = document.getElementById(id);
    if (!el) return;
    try {
      var w = HTMLWidgets.find('#' + id);
      if (w && typeof w.getMap === 'function') {
        var map = w.getMap();
        if (map) { map.off(); map.remove(); }
      }
    } catch(e) { console.warn('leaflet-destroy:', e); }
    // MUITO IMPORTANTE: desliga bindings do Shiny e limpa o container
    try { Shiny.unbindAll(el); } catch(e){}
    el.innerHTML = '';
});
