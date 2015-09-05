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
        var funcs     = 'abort abs acosh array_intersect array_union array1d array2d array3d array4d array5d array6d asin assert atan bool2int card ceil concat cos cosh dom dom_array dom exists size fix exp floor forall index_set index_set_1of2 index_set_2of2 index_set_1of3 index_set_2of3 index_set_3of3 int2float is fixed join lb lb_array length ln log log2 log10 min max pow product round set2array show show_int show_float sin sinh sqrt sum tan tanh trace ub ub_array';
        var keywords  = 'ann annotation any array assert bool constraint else elseif endif enum float function if in include int list of op output minimize maximize par predicate record set solve string test then type var where satisfy';
        var operators = '<-> -> <- \\\\/ xor /\\\\ < > <= >= == = != subset superset union diff symdiff \\.\\. intersect \\+\\+ \\+ - \\* / div mod'

        this.regexList = [
            { regex: /%(.*)$/gm,                                     css: 'comments' },
            { regex: new RegExp(this.getKeywords(funcs), 'gm'),      css: 'functions' },
            { regex: new RegExp(this.getKeywords(keywords), 'gmi'),  css: 'keyword' },
            //{ regex: new RegExp(this.getKeywords(operators), 'gmi'), css: 'keyword' },
            { regex: /\/\*([^\*][\s\S]*)?\*\//gm,                    css: 'comments' },
            { regex: SyntaxHighlighter.regexLib.doubleQuotedString,  css: 'string' }
        ]};

    Brush.prototype = new SyntaxHighlighter.Highlighter();
    Brush.aliases   = ['minizinc'];

    SyntaxHighlighter.brushes.MiniZinc = Brush;

    // CommonJS
    typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();

