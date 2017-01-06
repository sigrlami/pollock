window.onload = function() {
  var link = document.createElement('a');
  link.href = 'http://clicks.minimob.com/tracking/click?clickid=[[clickid]]&trafficsource=1373691551&cid=[[cid]]&offerid=24667757535291533';
  var imageT = document.createElement('img');
  link.target = '_blank';
  imageT.setAttribute('src', srcInitialisation());
  imageT.setAttribute('alt', '');
  imageT.setAttribute('border', 0);
  link.appendChild(imageT);
  function srcInitialisation() {
    return 'https://storage.googleapis.com/adscout-ident.appspot.com/img/fishdom_320x50_01.jpg';
  };
  document.body.style.marginTop = '0px';
  document.body.style.marginLeft = '0px';
  document.body.appendChild(link)
};
