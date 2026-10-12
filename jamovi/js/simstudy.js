'use strict';

// Simulated study panel: scenario -> outcome -> design (only compatible ones)
// -> variables measured, all from the library catalogue (shared.js).

var S = require('./shared');

// the word in the Units column of an outcome row
var OUTCOME_WORD = { en: 'outcome', es: 'resultado', ca: 'resultat' };

// designs whose people can carry the scenario's other outcomes (SECONDARY_DESIGNS in R)
var SECONDARY = ['cross_sectional', 'cohort', 'prognosis', 'rct', 'cluster_rct', 'factorial', 'diagnostic'];
var FOLLOWED = ['cohort', 'prognosis', 'rct', 'cluster_rct', 'factorial'];

function otherOutcomes(sc, ocId, design) {
    if (SECONDARY.indexOf(design) < 0) return [];
    return Object.keys(sc.outcomes || {}).filter(function(o) {
        var x = sc.outcomes[o];
        // death after a first event needs follow-up
        return o !== ocId && !(x.after && FOLLOWED.indexOf(design) < 0);
    });
}

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
        // the design filter (Simulated study only) narrows the scenarios and outcomes
        var flt = 'any';
        try { flt = S.getOpt(ui, 'designFilter') || 'any'; } catch (e) { flt = 'any'; }   // Compare designs has no filter
        var entries = S.scenarioEntries(lib, l, flt);
        var ids = entries.map(function(e) { return e[0]; });
        var scId = S.getOpt(ui, 'scenario');
        if (ids.indexOf(scId) < 0) { scId = ids[0]; S.setOpt(ui, 'scenario', scId); reset = true; }
        var sc = lib.scenarios[scId];
        var ocs = Object.keys(sc.outcomes || {}).filter(function(o) { return S.fitsDesign(sc, flt, o); });
        if (ocs.length === 0) ocs = Object.keys(sc.outcomes || {});
        var ocId = S.getOpt(ui, 'outcome');
        if (ocs.indexOf(ocId) < 0) { ocId = ocs[0]; S.setOpt(ui, 'outcome', ocId); reset = true; }
        var oc = sc.outcomes[ocId];

        S.syncSelect(ui, 'scenario', entries,
            function(v) { S.setOpt(ui, 'scenario', v); S.setOpt(ui, 'outcome', ''); self.refresh(ui, true); });
        S.syncSelect(ui, 'outcome', ocs.map(function(k) { return [k, S.tr(sc.outcomes[k].label, l) || k]; }),
            function(v) { S.setOpt(ui, 'outcome', v); self.refresh(ui, true); });

        var ok = S.designsFor(sc, oc);
        this.syncDesigns(ui, ok);
        if (flt !== 'any' && ok.indexOf(flt) >= 0 && ui.design) S.setOpt(ui, 'design', flt);

        // one row per scenario variable, by the name the student will see;
        // a new scenario or outcome starts with all of them measured (untick
        // to leave one unmeasured), a restore keeps the ticks (even if the
        // names were saved in another language)
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
        var defs = ids.map(function(id) { return S.varDef(lib, sc, id); });
        var rows = ids.map(function(id, i) {
            var m = (!reset && prev[id]) || {};
            var inc = (!reset && prev[id]) ? !!m.include : true;
            return { 'var': vars[i], units: S.unitsText(defs[i], l, lib, sc), include: inc,
                     form: S.isNumeric(defs[i]) ? (m.form || 'continuous') : 'continuous', cuts: S.isNumeric(defs[i]) ? (m.cuts || '') : '',
                     exposure: !!m.exposure, role: '' };
        });
        // the scenario's other outcomes first, unticked: ticked ones are added
        // for the same people (the designs that can carry them; same rule as
        // secondary_columns() in R)
        var outs = this.noOutcomes ? [] : otherOutcomes(sc, ocId, S.getOpt(ui, 'design'));
        var orows = outs.map(function(o) {
            var name = S.dname(o, l, lib, sc);
            var was = cur.filter(function(m) { return m && m.role === 'outcome' &&
                (m['var'] === o || ['en', 'es', 'ca'].some(function(x) { return S.dname(o, x, lib, sc) === m['var']; })); })[0];
            return { 'var': name, units: OUTCOME_WORD[l] || OUTCOME_WORD.en, include: !reset && !!(was && was.include),
                     form: 'continuous', cuts: '', exposure: false, role: 'outcome' };
        });
        S.setOpt(ui, 'measured', orows.concat(rows));
        this._numeric = orows.map(function() { return false; }).concat(defs.map(S.isNumeric));
        this._outcome = orows.map(function() { return true; }).concat(defs.map(function() { return false; }));
        this.lockForms();
        S.browseButton(ui, function() { self.refresh(ui, false); });
    },

    // design: the native ComboBox, with the incompatible designs hidden
    syncDesigns: function(ui, ok) {
        if (ok.length && ok.indexOf(S.getOpt(ui, 'design')) < 0) S.setOpt(ui, 'design', ok[0]);
        if (ui.design && ui.design.input) {
            var opts = ui.design.input.options;
            for (var i = 0; i < opts.length && i < S.DESIGNS.length; i++) {
                var off = ok.indexOf(S.DESIGNS[i]) < 0;
                opts[i].hidden = off;
                opts[i].disabled = off;
            }
        }
    },

    // Form and Cut points only apply to numbers: hide them on categorical rows.
    // The list re-renders its rows on its own schedule, so this runs again
    // shortly after every change.
    lockForms: function() {
        var flags = this._numeric || [];
        var outc = this._outcome || [];
        var withExposure = !this.noOutcomes;   // Simulated study: a Measured and an Exposure tick per row
        var apply = function() {
            var sel = document.querySelectorAll('.list-item-cell select');
            var txt = document.querySelectorAll('.list-item-cell input[type=text]');
            var chk = document.querySelectorAll('.list-item-cell input[type=checkbox]');
            for (var i = 0; i < flags.length; i++) {
                [sel[i], txt[i]].forEach(function(el) {
                    if (!el) return;
                    el.disabled = !flags[i];
                    el.style.visibility = flags[i] ? '' : 'hidden';
                });
                // an outcome row has no exposure
                var ex = withExposure ? chk[2 * i + 1] : null;
                if (ex) { ex.disabled = !!outc[i]; ex.style.visibility = outc[i] ? 'hidden' : ''; }
            }
        };
        apply();
        setTimeout(apply, 50);
        setTimeout(apply, 400);
    },

    measured_changed: function(ui) { this.lockForms(); },
    design_changed: function(ui) { this.refresh(ui, false); },
    designFilter_changed: function(ui) { this.refresh(ui, false); },

    view_loaded: function(ui) { this.refresh(ui, false); },
    view_updated: function(ui) { this.refresh(ui, false); }
};
