@{%
const moo = require("moo");

let lexer = moo.compile({
	ws: /[ \t]+/,
	comment: /\/\/.*?$/,
	number:  /0|[1-9][0-9]*/,
	
	string:  /"(?:\\["\\]|[^\n"\\])*"/,
	lparen:  '(',
	rparen:  ')',
	semi:  ';',
	comma: ',',
	imp: "import",
	frm: "from",
	as: "as",
	keyword: ['while', 'if', 'else'],
	NL:      { match: /\n/, lineBreaks: true },
	ident:  /[a-zA-Z_][a-zA-Z_0-9]*/,
});

%}

@lexer lexer

import -> %imp __ imports __ %frm __ %string _ %semi {% d => ({symbols: d[2], from: d[6]}) %}

_ -> %ws:*
__ -> %ws:+

identifier -> %ident {% id => id[0] %}

imports -> identifier (__ %as __ identifier):? (_ %comma __ imports):? {% d => [{symbol: d[0], alias: d[1] ? d[1][3] : null}, ...(d[2]?d[2][3]:[])]%}
