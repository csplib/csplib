/**
 * SyntaxHighlighter for http://alexgorbatchev.com/SyntaxHighlighter/
 * @license
 * Dual licensed under the MIT and GPL licenses.
 */
;(function()
{
    // CommonJS
    typeof(require) != 'undefined' ? SyntaxHighlighter = require('shCore').SyntaxHighlighter : null;

    function Brush()
    {
        var comet_control = 'if|else|while|do|for|forall|return|in|break|continue|try|tryall|until|catch|throw|switch|case|default'
        var comet_type = 'int|float|boolean|bool|string|void|range|set|enum|expr|inc|var|set|ifstream|closure|pclosure|continuation|alias'
        var comet_class = 'Float|Closure|Counter|Process|Boolean|Integer|Continuation|Object|Event|KeyEvent|Condition'
        var comet_operator = 'sum|prod|union|cross|and|or|inter|setof|collect|filter|mapof|all|abs|floor|ceil|sqrt|ln|sin|cos|tab|round|arcsin|arccos|arctan|card|instanceof|new|assert|final'
        var comet_structure = 'model|class|tuple|interface|function|operator|implements|extends|import|include|native|maximizing|minimizing|minimize|maximize|minmax|abstract|soft|hard|implements|subject|to|using|onRestart'
        var comet_selectors = 'select|selectMin|selectMinAsp|selectMax|selectMaxAsp|selectFirst|selectRandom|selectCircular|selectPr'
        var comet_constants = 'this|super|true|false|null|cout|heap'
        var comet_other = 'IntToString|FloatToString|cast|move|max|argMax|min|argMin|lookahead|when|whenever|foreveron|sleepUntil|neighbor|call|once|probe|parall|pardo|parever|onFailure|exploreall|explore|ref|sync|shared|dict|stack|queue|onBacktracking|trail|noevent|by|synchronized|thread|member|interval|with|clearEvents|notify'

        this.regexList = [
            { regex: /\/\/(.*)$/gm,                                           css: 'comments' },
            { regex: new RegExp(this.getKeywords(comet_operator), 'gm'),   css: 'functions' },
            { regex: new RegExp(this.getKeywords(comet_control), 'gmi'),   css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_type),    'gmi'),   css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_class),   'gmi'),   css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_structure), 'gmi'), css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_selectors), 'gmi'), css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_constants), 'gmi'), css: 'keyword' },
            { regex: new RegExp(this.getKeywords(comet_other),    'gmi'),  css: 'keyword' },
            //{ regex: new RegExp(this.getKeywords(operators), 'gmi'), css: 'color1' },
            { regex: /\/\*([\s\S]*?)\*\//g,                                css: 'comments'},
            { regex: SyntaxHighlighter.regexLib.doubleQuotedString,        css: 'string' },
        ]};

    Brush.prototype = new SyntaxHighlighter.Highlighter();
    // Should be the same name as the Title from specification.md of the langauge page converted to lower case.
    Brush.aliases   = ['comet'];

    SyntaxHighlighter.brushes.Comet = Brush;

    // CommonJS
    typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();

