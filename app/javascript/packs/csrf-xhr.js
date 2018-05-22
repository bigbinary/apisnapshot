(function(xhr) {
    // h/t http://stackoverflow.com/a/19709846/2334666
    var absoluteUrl = /^(?:[a-z]+:)?\/\//i;

    // h/t http://tosbourn.com/a-fix-for-window-location-origin-in-internet-explorer/
    //
    // Use this instead of location.origin, which is not supported in
    // older browsers. Always do it this way, even when location.origin is
    // available, to ensure consistency across browsers.
    var origin = location.protocol + "//" + location.hostname + (location.port ? ':' + location.port: '');

    var csrfTokenNode, csrfToken;

    try {
      csrfTokenNode = document.head.querySelector('meta[name="csrf-token"]');
    } catch (e){
      // ignore document-based errors
      csrfTokenNode = null;
    }

    if ((csrfTokenNode !== null) && (typeof csrfTokenNode.content === "string")) {
      csrfToken = csrfTokenNode.content;
    } else {
      csrfToken = null;
    }

    // Patch XMLHttpRequest
    var originalOpen = xhr.open;

    xhr.open = function(method, url) {
        // If we're on http://example.com and the URL starts with
        // http://example.com then we're sending to a local origin.
        // Also if we're sending to a relative URL, that's also a local origin.
        var isLocalOrigin = url.indexOf(origin) === 0 || !absoluteUrl.test(url);

        // Call open() before attempting to set headers; if we call
        // setRequestHeader() before the xhr is in the OPENED state,
        // it will throw a runtime exception.
        var originalResult = originalOpen.apply(this, arguments);

        if (isLocalOrigin) {
            // Set the X-CSRF-Token header, if possible.
            if (csrfToken === null) {
                console.warn("csrf-xhr was unable to add a X-CSRF-Token header to a " + method + " to " + url + " because when csrf-xhr originally loaded, it could not find a <meta> element in the <head> with name=\"csrf-token\" and a Rails CSRF token in a content attribute.");
            } else {
                this.setRequestHeader("X-CSRF-Token", csrfToken);
            }
        }

        return originalResult;
    };
})(XMLHttpRequest.prototype);
