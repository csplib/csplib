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
        var datatypes = 'matrix int bool  false true'
        // var extras    = ''
        var funcs     = 'max min toSet toInt allDiff atleast atmost gcc alldifferent_except table';
        var keywords  = 'dim maximising minimising forAll exists sum be by domain in find from given image indexed intersect freq letting of partial quantifier representation subset subsetEq such supset supsetEq that new type union where branching on';
        var operators = 'all and any between cross in join like not null or outer some';

        this.regexList = [
            { regex: /\$(.*)$/gm,                                   css: 'comments' },
            { regex: new RegExp(this.getKeywords(funcs), 'gm'),     css: 'functions' },
            { regex: new RegExp(this.getKeywords(operators), 'gm'), css: 'color2' },
            { regex: new RegExp(this.getKeywords(datatypes), 'gm'), css: 'color1 bold' },
            // { regex: new RegExp(this.getKeywords(extras), 'gm'),    css: 'preprocessor' },
            { regex: new RegExp(this.getKeywords(keywords), 'gmi'), css: 'keyword' }
            ];
    };

    Brush.prototype = new SyntaxHighlighter.Highlighter();
    Brush.aliases   = ['essenceprime', "essence'", 'eprime'];

    SyntaxHighlighter.brushes.EssencePrime = Brush;

    // CommonJS
    typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();

