(*CSCI 2041 Project2 Jiatan Huang*)
(*

  This file contains definitions for the OCaml type THING, and the OCaml module
  SCANNER, both of which are needed to write the module PARSER for Project 2.

*)

(* THING. Types of the usual Lisp objects. *)

type
  thing =
    Closure of thing * thing * environment |
    Cons of thing * thing |
    Nil |
    Number of int |
    Primitive of (thing -> environment -> thing) |
    Symbol of string
and
  environment = (string * thing) list ;;

(* SCANNER. Lexical scanner for Lisp from Lab 9. It also ignores comments. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT COMMENT. Skip a comment. It starts with a ';' and ends with a newline
   '\n' or an end of file '\000'. We skip the '\n', but not the '\000'. *)

  let rec nextComment () =
    match ! ch
    with '\000' ->
           () |
         '\n' ->
           nextChar () |
         _ ->
           nextChar () ;
           nextComment ()
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)

  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         ';' ->
           nextComment () ;
           nextToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;

module type Parsers = (*signature*)
sig
  val makeParser : string -> unit -> thing(*function makeParser*)
  (*Can'tParse. Raised if can't read a thing*)
  exception Can'tParse of string
end;; 

module Parser:Parsers = 
  struct
    exception Can'tParse of string;;(*exception*)
    let oops message = raise (Can'tParse message)
    let makeParser path = 
      let getToken = Scanner.makeScanner path(*creat a scanner and read next token*)
    in let token = ref(getToken())(*get token*)
    
  in let rec nextThing() = 
      match !token with
      Scanner.CloseParenToken ->
        oops "Unexpected close paren" |
      Scanner.EndToken ->
        oops "Unexpected end of file"|(*when CloseParenToken and EndToken show up, raise exception*)
      Scanner.NumberToken n ->
        token := getToken(); Number n|
      Scanner.OpenParenToken ->
        token := getToken(); nextThings()|
      Scanner.SymbolToken "nil" -> (*nil is a symbol*)
        token := getToken(); Nil|
      Scanner.SymbolToken s ->
        token := getToken(); Symbol s

  and nextThings() =  
    (match !token with
      Scanner.CloseParenToken ->
        token := getToken(); Nil|
      Scanner.EndToken ->
        oops "Unexpected end of file"|
      _ -> (*include three situation, openParen, symbolToken and NumberToken*)
        let left = nextThing()
      in let right = nextThings()
    in Cons(left, right))(*make cons*)
  in
  nextThing;;
end;;


(*test result*)
(*
#use "parser.ml";;
type thing =
    Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and environment = (string * thing) list
module Scanner :
  sig
    type token =
        CloseParenToken
      | EndToken
      | NumberToken of int
      | OpenParenToken
      | SymbolToken of string
    val makeScanner : string -> unit -> token
  end
module type Parsers =
  sig
    val makeParser : string -> unit -> thing
    exception Can'tParse of string
  end
module Parser : Parsers   
*)
(*
#use "testsP2.ml";;
val nextThing : unit -> thing = <fun>
- : thing = Nil
- : thing = Number 7734
- : thing = Symbol "lobyms"
- : thing = Cons (Symbol "a", Nil)
- : thing = Cons (Symbol "a", Cons (Symbol "b", Nil))
- : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol "c", Nil)))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol "c", Nil))
- : thing =
Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", Nil)), Nil))
- : thing =
Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", Nil))), Nil)
- : thing =
Cons (Symbol "define",
 Cons (Symbol "!",
  Cons
   (Cons (Symbol "lambda",
     Cons (Cons (Symbol "n", Nil),
      Cons
       (Cons (Symbol "if",
         Cons (Cons (Symbol "=", Cons (Symbol "n", Cons (Number 0, Nil))),
          Cons (Number 1,
           Cons
            (Cons (Symbol "*",
              Cons (Symbol "n",
               Cons
                (Cons (Symbol "!",
                  Cons
                   (Cons (Symbol "-", Cons (Symbol "n", Cons (Number 1, Nil))),
                   Nil)),
                Nil))),
            Nil)))),
       Nil))),
   Nil)))
Exception: Parser.Can'tParse "Unexpected end of file".
*)