'use strict';

// Simulated study panel: scenario -> outcome -> design (only compatible ones)
// -> variables measured, all from the library catalogue (shared.js).

var S = require('./shared');

// the scenario's variables by the names the student will see in the data
function variablesOf(sc, l, lib) {
    return Object.keys(sc.variables || {}).map(function(v) { return S.dname(v, l, lib, sc); });
}

module.exports = {

    // keep the pickers and the supplier in step with the options; `reset`
    // empties the measured variables (after the user changes the scenario
    // or the outcome, never on a restore)
    refresh: function(ui, reset) {
        var self = this;
        var l = S.lang();
        if (S.customLibrary(ui).error === 'notjson') {
            // only JSON can be read here: ids are typed, the measured variables default to all
            ['scenario', 'outcome'].forEach(function(nm) {
                var $i = ui[nm] && ui[nm].$input;
                if ($i) { $i.prev('select.studysim-pick').remove(); $i.css('display', ''); }
            });
            S.browseButton(ui, function() { self.refresh(ui, true); });
            return;
        }
        var lib = S.library(ui);
        var ids = Object.keys(lib.scenarios);
        var scId = S.getOpt(ui, 'scenario');
        if (!lib.scenarios[scId]) { scId = ids[0]; S.setOpt(ui, 'scenario', scId); reset = true; }
        var sc = lib.scenarios[scId];
        var ocs = Object.keys(sc.outcomes || {});
        var ocId = S.getOpt(ui, 'outcome');
        if (ocs.indexOf(ocId) < 0) { ocId = ocs[0]; S.setOpt(ui, 'outcome', ocId); reset = true; }
        var oc = sc.outcomes[ocId];

        S.syncSelect(ui, 'scenario', ids.map(function(k) { return [k, S.tr(lib.scenarios[k].label, l) || k]; }),
            function(v) { S.setOpt(ui, 'scenario', v); S.setOpt(ui, 'outcome', ''); self.refresh(ui, true); });
        S.syncSelect(ui, 'outcome', ocs.map(function(k) { return [k, S.tr(sc.outcomes[k].label, l) || k]; }),
            function(v) { S.setOpt(ui, 'outcome', v); self.refresh(ui, true); });

        // design: the native ComboBox, with the incompatible designs hidden
        var ok = S.designsFor(sc, oc);
        if (ok.length && ok.indexOf(S.getOpt(ui, 'design')) < 0) S.setOpt(ui, 'design', ok[0]);
        if (ui.design && ui.design.input) {
            var opts = ui.design.input.options;
            for (var i = 0; i < opts.length && i < S.DESIGNS.length; i++) {
                var off = ok.indexOf(S.DESIGNS[i]) < 0;
                opts[i].hidden = off;
                opts[i].disabled = off;
            }
        }

        // one row per scenario variable, by the name the student will see;
        // a new scenario or outcome starts with none measured, a restore keeps
        // the ticks (even if the names were saved in another language)
        var vars = variablesOf(sc, l, lib);
        var ids = Object.keys(sc.variables || {});
        var cur = S.getOpt(ui, 'measured') || [];
        var prev = {};
        cur.forEach(function(m) {
            if (!m) return;
            ids.forEach(function(id) {
                if (m['var'] === id || ['en', 'es', 'ca'].some(function(x) { return S.dname(id, x, lib, sc) === m['var']; })) prev[id] = m;
            });
        });
        var rows = ids.map(function(id, i) {
            var m = (!reset && prev[id]) || {};
            return { 'var': vars[i], include: !!m.include, form: m.form || 'continuous', cuts: m.cuts || '' };
        });
        S.setOpt(ui, 'measured', rows);
        S.browseButton(ui, function() { self.refresh(ui, false); });
    },

    view_loaded: function(ui) { this.refresh(ui, false); },
    view_updated: function(ui) { this.refresh(ui, false); }
};
