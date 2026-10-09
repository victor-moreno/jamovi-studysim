'use strict';

// Panel helpers shared by the studysim analyses: the library catalogue (built-in
// library embedded at build time, plus a custom JSON library read by the
// Browse button), the injected scenario/outcome pickers, and option access.
// Pickers are <select>s over hidden String options because jamovi options are
// static; the factors use a Supplier whose target is an Array of String, so
// their names are never treated as dataset columns (findings.md, Phase 0).

var BUILTIN = require('./builtin');

var DESIGNS = ['case_control', 'matched_cc', 'cohort', 'cross_sectional', 'rct', 'crossover', 'prognosis'];
var LIB_MAX = 700000;   // base64 characters; the options hop to the engine is 1 MiB

function getOpt(ui, name) {
    if (ui[name] && typeof ui[name].value === 'function') return ui[name].value();
    if (typeof ui.getOptionValue === 'function') return ui.getOptionValue(name);
    var o = ui.getOption ? ui.getOption(name) : null;
    return o ? o.value() : undefined;
}

function setOpt(ui, name, value) {
    if (JSON.stringify(getOpt(ui, name)) === JSON.stringify(value)) return;
    if (ui[name] && typeof ui[name].setValue === 'function') return ui[name].setValue(value);
    if (typeof ui.setOptionValue === 'function') return ui.setOptionValue(name, value);
    var o = ui.getOption ? ui.getOption(name) : null;
    if (o && typeof o.setValue === 'function') o.setValue(value);
}

// jamovi's language: the module catalogs translate this marker to their code
function lang() {
    var code = '';
    try { if (typeof _ === 'function') code = _('studysim-language-code'); } catch (e) { code = ''; }
    if (/^[a-z]{2}$/.test(code)) return code;
    var nav = (typeof navigator !== 'undefined' && navigator.language) ? navigator.language.substring(0, 2) : 'en';
    return ['en', 'es', 'ca'].indexOf(nav) >= 0 ? nav : 'en';
}

function tr(x, l) {
    if (x === null || x === undefined) return '';
    if (typeof x === 'object') return x[l] || x.en || x[Object.keys(x)[0]] || '';
    return String(x);
}

// column name shown for an id (inst/library/i18n/<lang>.yaml)
function dname(id, l) {
    var d = BUILTIN.i18n && BUILTIN.i18n[l] && BUILTIN.i18n[l].names;
    return (d && d[id]) || id;
}

function b64ToText(b64) {
    var bin = atob(b64);
    var bytes = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    return new TextDecoder('utf-8').decode(bytes);
}

// The custom library, if it is JSON; {lib, error}
function customLibrary(ui) {
    var b64 = getOpt(ui, 'libContent');
    var name = String(getOpt(ui, 'libName') || '');
    if (!b64) return { lib: null, error: '' };
    if (!/\.json$/i.test(name)) return { lib: null, error: 'notjson' };
    try {
        var doc = JSON.parse(b64ToText(b64));
        return { lib: { variables: doc.variables || {}, scenarios: doc.scenarios || {} }, error: '' };
    } catch (e) {
        return { lib: null, error: 'invalid' };
    }
}

function library(ui) {
    var lib = { variables: {}, scenarios: {} };
    var k;
    for (k in BUILTIN.variables) lib.variables[k] = BUILTIN.variables[k];
    for (k in BUILTIN.scenarios) lib.scenarios[k] = BUILTIN.scenarios[k];
    var c = customLibrary(ui).lib;
    if (c) {
        for (k in c.variables) lib.variables[k] = c.variables[k];
        for (k in c.scenarios) {
            var s = c.scenarios[k];
            lib.scenarios[s.id || k] = s;
        }
    }
    return lib;
}

// same rule as compatible_designs() in R/engine_scenario.R
function designsFor(sc, oc) {
    if (oc.designs) return [].concat(oc.designs).filter(function(d) { return DESIGNS.indexOf(d) >= 0; });
    var effects = Object.keys(oc.effects || {});
    var ivs = sc.interventions || {};
    var hasIv = effects.some(function(v) { return ivs.hasOwnProperty(v); });
    var d = ({ binary: ['cross_sectional', 'cohort', 'case_control', 'matched_cc'],
               event: ['cohort', 'case_control', 'matched_cc'],
               continuous: ['cross_sectional', 'cohort'],
               count: ['cross_sectional', 'cohort'] })[oc.type] || [];
    d = d.slice();
    if (hasIv) {
        d.push('rct');
        if (oc.type === 'continuous') d.push('crossover');
    }
    return d;
}

// <select> placed before the hidden TextBox of a String option
function syncSelect(ui, name, entries, onChange) {
    var ctrl = ui[name];
    if (!ctrl || !ctrl.$input) return;
    var $input = ctrl.$input;
    var jq = $input.constructor;
    var $sel = $input.prev('select.studysim-pick');
    if ($sel.length === 0) {
        $sel = jq('<select class="studysim-pick"></select>').css({ maxWidth: '100%', minWidth: '220px', color: 'inherit' });
        $input.before($sel);
        $sel.on('change', function() { onChange(jq(this).val()); });
    }
    $input.css('display', 'none');
    var cur = getOpt(ui, name);
    $sel.empty();
    for (var i = 0; i < entries.length; i++) {
        var $o = jq('<option></option>').attr('value', entries[i][0]).text(entries[i][1]);
        if (entries[i][0] === cur) $o.attr('selected', 'selected');
        $sel.append($o);
    }
}

function browseButton(ui, onLoaded) {
    var ctrl = ui.libName;
    if (!ctrl || !ctrl.$input) return;
    var $input = ctrl.$input;
    $input.prop('readonly', true).prop('disabled', true);
    var has = !!getOpt(ui, 'libName');
    var jq = $input.constructor;
    if ($input.prev('.studysim-browse').length) {
        $input.next('.studysim-clear').css('display', has ? '' : 'none');
        return;
    }
    var css = { flexShrink: '0', cursor: 'pointer', padding: '2px 10px', color: 'inherit', whiteSpace: 'nowrap',
                border: '1px solid rgba(128, 128, 128, 0.6)', borderRadius: '3px', background: 'rgba(128, 128, 128, 0.12)' };
    var $btn = jq('<button type="button" class="studysim-browse">Browse…</button>').css(css);
    var $clr = jq('<button type="button" class="studysim-clear" title="Use the built-in library only">✕</button>').css(css);
    $input.wrap(jq('<div></div>').css({ display: 'flex', alignItems: 'center', gap: '4px', width: '100%' }));
    $input.css({ flex: '1 1 auto', minWidth: 0 });
    $input.before($btn);
    $input.after($clr);
    $clr.css('display', has ? '' : 'none');
    $clr.on('click', function() {
        setOpt(ui, 'libContent', '');
        setOpt(ui, 'libName', '');
        $clr.css('display', 'none');
        onLoaded();
    });
    $btn.on('click', function() {
        var f = document.createElement('input');
        f.type = 'file';
        f.accept = '.json,.yaml,.yml,.xlsx,.zip';
        f.style.display = 'none';
        document.body.appendChild(f);
        f.addEventListener('change', function() {
            var file = f.files[0];
            document.body.removeChild(f);
            if (!file) return;
            var r = new FileReader();
            r.onload = function() {
                var s = String(r.result);
                var b64 = s.substring(s.indexOf(',') + 1);
                if (b64.length > LIB_MAX) {
                    setOpt(ui, 'libName', file.name + ' — too large (max ~500 KB)');
                    setOpt(ui, 'libContent', '');
                } else {
                    setOpt(ui, 'libContent', b64);
                    setOpt(ui, 'libName', file.name);
                }
                $clr.css('display', '');
                onLoaded();
            };
            r.readAsDataURL(file);
        });
        f.click();
    });
}

module.exports = {
    BUILTIN: BUILTIN, DESIGNS: DESIGNS, getOpt: getOpt, setOpt: setOpt, lang: lang, tr: tr, dname: dname,
    library: library, customLibrary: customLibrary, designsFor: designsFor,
    syncSelect: syncSelect, browseButton: browseButton
};
