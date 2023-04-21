#! /usr/bin/env gforth

\ xpr-fix.fth - xpr-fix in GForth
\
\ USAGE: ./xpr-fix.fth FIX XPR
\   - FIX: prefix, postfix, infix
\
\ Each syntactic element of an expression must be blank-delimited. That is, each
\ number, operator, and parenthesis must be separated by blanks.
\
\ Copyright (C) 2023 Robert Coffey
\ Released under the MIT license.

false warnings !

\ UTILITY ----------------------------------------------------------------------

\ boolean
: not   if false else true then ;

\ memory
: mxor ( a1 a2 u -- )
  0 do { a1 a2 }  a1 c@ a2 c@ xor  a2 c!  a1 1+ a2 1+ loop 2drop ;
: mswap ( a1 a2 u -- )   { a1 a2 u }  a1 a2 u mxor  a2 a1 u mxor  a1 a2 u mxor ;

\ array
: reverse ( a u s -- a u )
  { a u s }
  u 2 < if exit then
  u 1- s * a + { e }
  u 2 / 0 do a i s * +  e i s * -  s mswap loop ;
: step ( a u s -- adr u )   rot + swap 1- ;

\ string
: -leading   dup 0 do over c@ bl = if 1 step else leave then loop ;
: word-len ( str -- len )
  { a u } 0 ( len )
  begin
      dup u < swap
      dup a + c@ bl <> rot
    and
  while 1+ repeat ;

\ compiler
: (##)   { xt } 0 do xt execute loop ;
: [##]   postpone ['] postpone (##) ; immediate
: ##     ' (##) ;

\ output
: .num ( n -- )   s>d swap over dabs <<# #s rot sign #> type #>> ;

\ misc
: 0exit   postpone 0= postpone if postpone exit postpone then ; immediate
: error ( str -- )   ." error: " type cr clearstack bye ;

\ TOKEN ------------------------------------------------------------------------

0 constant tok-num
1 constant tok-op
2 constant tok-lp
3 constant tok-rp

: tok   [ 2 cells ] literal ;
: toks   tok * ;

: tok-type ( a -- 'a )   ;
: tok-data ( a -- 'a )   cell+ ;

: tok@ ( adr -- data type )   dup tok-data @ swap tok-type @ ;
: tok! ( data type adr -- )   tuck tok-type ! tok-data ! ;

: tok-type/char? ( type -- ? )   { t } t tok-op = t tok-lp = t tok-rp = or or ;
: .tok ( data type -- ) { d t }
  t tok-num =      if d .num   else
  t tok-type/char? if d emit   else
                      'E' emit then then ;
: tok? ( adr -- )   tok@ .tok ;
: .toks ( adr u -- )   dup 0exit  0 do dup i toks + tok? space loop drop ;

\ LEXER ------------------------------------------------------------------------

256 constant max-toks
create tok-arr max-toks toks allot
variable tok-cnt

: reverse-tok-arr   tok-arr tok-cnt @ tok reverse ;
: .tok-arr   tok-arr tok-cnt @ .toks ;

: s/op?   { a u } u 1 = if s" +-*/" a u search 2 [##] nip else false then ;
: s/lp?   { a u } u 1 <> if false else a c@ '(' = then ;
: s/rp?   { a u } u 1 <> if false else a c@ ')' = then ;
: parse-word ( word -- data type )
  { adr u }
  adr u s/op?         if adr c@ tok-op else
  adr u s/lp?         if adr c@ tok-lp else
  adr u s/rp?         if adr c@ tok-rp else
  adr u s>number? nip if tok-num       else drop
                         s\" failed to parse token\nnote: tokens must be blank-delimited"
                         error then then then then ;

: (next-word?) ( str len -- str data type )
  { adr u len }
  adr len +  u len -
  adr len parse-word ;
: next-word? ( str -- str data type ? )
  -leading  2dup word-len  dup 0= if 0 false else (next-word?) true then ;

: (lex) ( str i -- str ? )
  { i }
  i max-toks < if
    next-word? if
      tok-arr i toks + tok! false
    else 2drop true then
  else true then ;
: lex ( str -- )
  0 begin { i }  i (lex)  i 1+ swap until
  1- tok-cnt !  2drop ;

\ AST --------------------------------------------------------------------------

0 constant ast-num
1 constant ast-xpr

4 cells constant /ast
: ast-alloc? ( -- ast )   /ast allocate ;
: ast-free ( ast -- )   free ;

: ast-type ( a -- 'a )   ;
: ast-data ( a -- 'a )   [ 1 cells ] literal + ;
: ast-rhs ( a -- 'a )    [ 2 cells ] literal + ;
: ast-lhs ( a -- 'a )    [ 3 cells ] literal + ;
create ast-fields ' ast-type , ' ast-data , ' ast-rhs , ' ast-lhs ,

: ast-make ( lhs rhs data type -- ast )
  ast-alloc? if s" failed to allocate memory" error then
  4 0 do tuck ast-fields i cells + @ execute ! loop ;
: ast-make-num ( n -- ast )   nil nil rot ast-num ast-make ;
: ast-make-xpr ( lhs rhs data -- ast )   ast-xpr ast-make ;
: ast-dest ( ast -- )
  { ast }
  ast ast-lhs @ ?dup if recurse then
  ast ast-rhs @ ?dup if recurse then
  ast ast-free ;

: ast-num? ( ast -- ? )   ast-type @ ast-num = ;
: ast-xpr? ( ast -- ? )   ast-type @ ast-xpr = ;

: .ast ( ast -- )
  { ast }
  ast ast-num? if ast ast-data @ .num             else
  ast ast-xpr? if ." (" ast ast-data @ emit space
                  ast ast-lhs @ recurse space
                  ast ast-rhs @ recurse ." )"     then then ;

: invert-ast ( ast -- ast )
  { ast }
  ast ast-lhs ast ast-rhs cell mswap
  ast ast-lhs @ ?dup if recurse then
  ast ast-rhs @ ?dup if recurse then ;

\ PARSER -----------------------------------------------------------------------

create tok-i 0 ,
: parse ( str -- )   lex  0 tok-i ! ;
: parse-done? ( -- ? )   tok-i @ tok-cnt @ >= ;

: next-tok? ( -- data type ? )
  tok-i @ tok-cnt @ >= if nil nil false                    else
                          tok-arr tok-i @ toks + tok@ true then ;
: consume-tok ( -- )   1 tok-i +! ;
: expect-tok ( type -- data )
  next-tok? not if s" missing tokens" error else
  rot =         if consume-tok              else
                   s" invalid token" error  then then ;
: expect-tok> ( type -- )   expect-tok drop ;

: parse-num ( -- ast )   tok-num expect-tok ast-make-num ;
: parse-op ( -- ast )   tok-op expect-tok  nil nil rot ast-make-xpr ;

\ PREFIX: E -> N | O E E
: (prefix)
  next-tok? { data type f }
  f not          if s" missing tokens" error                               else
  type tok-num = if parse-num                                              else
  type tok-op =  if parse-op recurse over ast-lhs ! recurse over ast-rhs ! else
                    s" invalid token" error then then then ;
: prefix   (prefix) parse-done? not if s" too many tokens" error then ;

\ POSTFIX: E -> N | E E O
: two-ops? ( ... d0 -- ... ? )
  { d0 } depth d0 - 2 < if s" missing operands" error then ;
: postfix
  depth { d0 }
  begin
    next-tok? { data type f }
    f not          if s" missing tokens" error                           else
    type tok-num = if parse-num                                          else
    type tok-op =  if d0 two-ops? parse-op tuck ast-rhs ! tuck ast-lhs ! else
                      s" invalid token" error then then then
  parse-done? until
  depth d0 1+ <> if s" too many tokens" error then ;

\ INFIX: (XL = X-left)
\   E  -> EL T
\   EL -> e | E [+-]
\   T  -> TL F
\   TL -> e | T [*/]
\   F  -> ( E ) | N

: inf-XL { 'pred 'head }
  next-tok? { data type f }
  f not                   if nil                                   else
  data type 'pred execute if parse-op 'head execute over ast-lhs ! else
                             nil                                   then then ;
: inf-X { 'tail 'head }
  'head execute
  'tail execute ?dup 0<> if
    tuck ast-rhs !
  then ;

: tok-op/E? { d t -- ? }   t tok-op <> if false else d '+' = d '-' = or then ;
: tok-op/T? { d t -- ? }   t tok-op <> if false else d '*' = d '/' = or then ;

defer inf-E
: inf-F
  next-tok? { data type f }
  type tok-rp = if tok-rp expect-tok>  inf-E  tok-lp expect-tok> else
                   parse-num then ;

defer inf-T
: inf-TL   ['] tok-op/T? ['] inf-T inf-XL ;
:noname   ['] inf-TL ['] inf-F inf-X ; is inf-T

: inf-EL   ['] tok-op/E? ['] inf-E inf-XL ;
:noname   ['] inf-EL ['] inf-T inf-X ; is inf-E

: infix   reverse-tok-arr  inf-E  reverse-tok-arr ;

\ e.g. s" 1 - 1 - 1" parse infix

\ main -------------------------------------------------------------------------

: main
  next-arg { a u }
  a u s" prefix" str=  if ['] prefix                 else
  a u s" postfix" str= if ['] postfix                else
  a u s" infix" str=   if ['] infix                  else
                          s" unknown notation" error then then then
  { fix } next-arg parse  fix execute  .ast cr ;

main bye
