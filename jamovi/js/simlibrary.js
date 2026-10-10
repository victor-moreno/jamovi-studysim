'use strict';

// Library panel: Browse button for a custom library and a scenario picker.
// A YAML, Excel or zip library cannot be read here (only JSON), so for those
// the scenario id is typed and the results list the ids.

var S = require('./shared');

module.exports = {
    refresh: function(ui) {
        var self = this;
        var l = S.lang();
        var custom = S.customLibrary(ui);
        S.browseButton(ui, function() { self.refresh(ui); });
        var $input = ui.scenario && ui.scenario.$input;
        if (custom.error === 'notjson') {
            if ($input) { $input.prev('select.studysim-pick').remove(); $input.css('display', ''); }
            return;
        }
        var lib = S.library(ui);
        var ids = Object.keys(lib.scenarios);
        if (!lib.scenarios[S.getOpt(ui, 'scenario')]) S.setOpt(ui, 'scenario', ids[0]);
        S.syncSelect(ui, 'scenario', S.scenarioEntries(lib, l),
            function(v) { S.setOpt(ui, 'scenario', v); });
    },
    view_loaded: function(ui) { this.refresh(ui); },
    view_updated: function(ui) { this.refresh(ui); }
};
