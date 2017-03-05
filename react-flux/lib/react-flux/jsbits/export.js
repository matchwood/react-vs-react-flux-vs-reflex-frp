//Fake export until we start using the improved-base branch

var hsreact$export = function(x) {
    var o = {
        root: x,
        _key: ++h$extraRootsN
    };
    h$retain(o);
    return o;
};

var hsreact$derefExport = function(o) {
    return o.root;
};
