\ xpr-fix.fth - xpr-fix in GForth
\ Copyright (C) 2023 Robert Coffey
\ Released under the MIT license.

\ TODO: Add infix expression parser.

\ UTILITY ----------------------------------------------------------------------

true  constant t
false constant f
: not   0= ;

: plural   create , does> @ { xt } 0 do xt execute loop ;
' nip plural #nip
' over plural #over

: error ( str -- )   ." error: " type clearstack quit ;

: .num ( n -- )   s>d swap over dabs <<# #s rot sign #> type #>> ;

\ LEXER ------------------------------------------------------------------------

0 constant TOK.ERR
1 constant TOK.END
2 constant TOK.NUM
3 constant TOK.OP

2variable tok
: !tok ( data type -- )   tok 2! ;
: tok.type ( -- type )   tok ;
: tok.data ( -- data )   tok cell + ;
: .tok ( -- )   tok.type ? tok.data ? ;

: lex.word ( -- str )   parse-name ;
: lex.s/op? ( str -- ? )   s" + - * /" 2swap search 2 #nip ;
: lex.s>op? ( str -- c ? )   2dup lex.s/op? if drop c@ t else drop f then ;
: lex.scan ( str -- )
  dup 0=             if nip TOK.END !tok    else \ no input remaining
  2dup lex.s>op?     if 2 #nip TOK.OP !tok  else \ operator token
  drop s>number? nip if TOK.NUM !tok        else \ number token
                        TOK.ERR !tok             \ invalid token
  then then then ;
: lex.next ( -- )   lex.word lex.scan ;

\ PARSER -----------------------------------------------------------------------

0 constant AST.N
1 constant AST.E

: ast.alloc? ( -- ast ? )   4 cells allocate ;
: ast.free ( ast -- )   free ;

: ast.type ( ast -- ast.type )   ;
: ast.data ( ast -- ast.data )   1 cells + ;
: ast.lhs  ( ast -- ast )        2 cells + ;
: ast.rhs  ( ast -- ast )        3 cells + ;
create ast.fields ' ast.type , ' ast.data , ' ast.lhs , ' ast.rhs ,

: ast.make ( rhs lhs data type -- ast )
  ast.alloc? drop
  4 0 do tuck ast.fields i cells + @ execute ! loop ;
: ast.make.n ( n -- ast )   nil nil rot AST.N ast.make ;
: ast.make.e ( ast ast c -- ast )   AST.E ast.make ;

: ast.dest ( ast -- )
  { ast }
  ast ast.lhs @ ?dup if recurse then
  ast ast.rhs @ ?dup if recurse then
  ast ast.free ;

: ast.n? ( ast -- ? )   ast.type @ AST.N = ;
: ast.e? ( ast -- ? )   ast.type @ AST.E = ;

: .ast ( ast -- )
  { ast }
  ast ast.n? if ast ast.data @ .num                                     else
  ast ast.e? if ." (" ast ast.data @ emit space
                ast ast.lhs @ recurse space ast ast.rhs @ recurse ." )" else
  then then ;

: prs.next ( -- tok.type )   tok.type @ ;
: prs.consume ( -- )   lex.next ;
: prs.expect ( tok.type -- )
  tok.type @ = if prs.consume else s" prs.expect" error then ;

: prs.num ( -- ast )
  prs.next TOK.END = if s" prs.num: missing token" error  else
  prs.next TOK.NUM = if tok.data @ ast.make.n prs.consume else
                        s" prs.num: invalid token" error
  then then ;

: prs.op ( -- ast )
  prs.next TOK.END = if s" prs.op: missing token" error           else
  prs.next TOK.OP =  if nil nil tok.data @ ast.make.e prs.consume else
                        s" prs.op: invalid token" error
  then then ;

\ PREFIX (RD): E -> N | O E E
: prs.&prefix ( -- ast )
  prs.next { t }
  t TOK.END = if s" prs.prefix: missing tokens" error                 else
  t TOK.NUM = if prs.num                                              else
  t TOK.OP =  if prs.op recurse over ast.lhs ! recurse over ast.rhs ! else
                 s" prs.prefix: invalid token" error
  then then then ;
: prs.prefix ( -- ast )
  prs.&prefix
  prs.next TOK.END <> if s" prs.prefix: too many tokens" error then ;

\ POSTFIX (PDA): E -> N | E E O
: prs.postfix ( -- ast )
  depth { depth.orig }
  begin
    prs.next { t }
    t TOK.END = if s" prs.postfix: missing tokens" error else
    t TOK.NUM = if prs.num                               else
    t TOK.OP =  if prs.op tuck ast.rhs ! tuck ast.lhs !  else
                   s" prs.postfix: invalid token" error
    then then then
  prs.next TOK.END = until
  depth depth.orig 1+ <> if s" prs.postfix: too many tokens" error then ;

: prs ( xt -- ast )   prs.consume execute ;
\ e.g. ' prs.prefix prs
