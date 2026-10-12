'use strict';

// Compare designs panel: the Simulated study panel (pickers, Variables list)
// with the designs as checkboxes; those that do not fit the outcome are
// unticked and hidden.

var S = require('./shared');
var study = require('./simstudy');

var self = {};
for (var k in study) self[k] = study[k];
self.noOutcomes = true;   // one outcome per comparison: no rows for the others

self.syncDesigns = function(ui, ok) {
    var cur = S.getOpt(ui, 'designs') || [];
    var keep = cur.filter(function(d) { return ok.indexOf(d) >= 0; });
    if (keep.length === 0) keep = ok.slice();
    S.setOpt(ui, 'designs', keep);
    S.DESIGNS.forEach(function(d) {
        var c = ui['designs_' + d];
        if (c && c.el) c.el.style.display = ok.indexOf(d) < 0 ? 'none' : '';
    });
};

module.exports = self;
