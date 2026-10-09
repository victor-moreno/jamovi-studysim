'use strict';

// Simulated study panel: scenario -> outcome -> design (only compatible ones)
// -> variables measured, all from the library catalogue (shared.js).

var S = require('./shared');

function variablesOf(sc) {
    return Object.keys(sc.variables || {});
}

module.exports = {

    // keep the pickers and the supplier in step with the options; `reset`
    // refills the measured variables (after the user changes the scenario
    // or the outcome, never on a restore)
    refresh: function(ui, reset) {
        var self = this;
        var l = S.lang();
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

        var vars = variablesOf(sc);
        if (reset) S.setOpt(ui, 'factors', vars);
        ui.factorSupplier.setValue(utils.valuesToItems(vars, FormatDef.variable));
        S.browseButton(ui, function() { self.refresh(ui, false); });
    },

    view_loaded: function(ui) { this.refresh(ui, false); },
    view_updated: function(ui) { this.refresh(ui, false); },

    factorSupplier_updated: function(ui) {
        var lib = S.library(ui);
        var sc = lib.scenarios[S.getOpt(ui, 'scenario')];
        ui.factorSupplier.setValue(utils.valuesToItems(sc ? variablesOf(sc) : [], FormatDef.variable));
    }
};
