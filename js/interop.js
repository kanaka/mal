// Node vs browser behavior
var interop = {};
if (typeof module === 'undefined') {
    var exports = interop,
        GLOBAL = window;
}

function resolve_js(str) {
    if (str.match(/\./)) {
        var re = /^(.*)\.([^\.]*)$/,
            match = re.exec(str);
        return [eval(match[0]), eval(str)];
    } else {
        return [GLOBAL, eval(str)];
    }
}

function js_to_mal(obj) {
    var cache = [];
    var str = JSON.stringify(obj, function(key, value) {
        if (typeof value === 'object' && value !== null) {
            if (cache.indexOf(value) !== -1) {
                // Circular reference found, discard key
                return;
            }
            // Store value in our collection
            cache.push(value);
        }
        return value;
    });
    cache = null; // Enable garbage collection
    return JSON.parse(str);
}

exports.resolve_js = interop.resolve_js = resolve_js;
exports.js_to_mal = interop.js_to_mal = js_to_mal;
