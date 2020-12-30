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
	var keywords =	'count for foreach foreacharg foreachelem fromto loop_name multifor param';
	var control =	'do once call catch findall setof bagof throw bb_min minimize optimize suspend';

        this.regexList = [
	    { regex: SyntaxHighlighter.regexLib.multiLineCComments,	css: 'comments' },
            { regex: /%(.*)$/gm,                                   	css: 'comments' },
	    { regex: /^:-(.*)$/gm,					css: 'preprocessor' },	// directives
            { regex: new RegExp(this.getKeywords(keywords), 'gm'),	css: 'functions' },
            { regex: new RegExp(this.getKeywords(control), 'gm'),	css: 'keyword' },
            { regex: /\s:-$/gm,						css: 'keyword' },
	    { regex: SyntaxHighlighter.regexLib.doubleQuotedString,	css: 'string' },
	    { regex: SyntaxHighlighter.regexLib.singleQuotedString,	css: 'string' },
	    { regex: /\b[A-Z_][A-Za-z0-9_]*\b/g,			css: 'variable' }
            ];
    };

    Brush.prototype = new SyntaxHighlighter.Highlighter();
    // Should be the same name as the Title from specification.md of the langauge page converted to lower case.
    Brush.aliases   = ['eclipse'];

    SyntaxHighlighter.brushes.ECLiPSe = Brush;

    // CommonJS
    typeof(exports) != 'undefined' ? exports.Brush = Brush : null;
})();

