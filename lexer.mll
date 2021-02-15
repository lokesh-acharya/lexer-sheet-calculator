{

type token =
  | Float of float
  | Index of int
  | Lparan
  | Rparan
  | Lbrac
  | Rbarc
  | Comma
  | Colon
  | Indice
  | Range
  | Uop
  | Bop
  | Assign
  | Terminator
  | Cons
  | EOF
  | End

exception Eof
exception Ill

}

let n=(['0']|(['1'-'9'] ['0'-'9']*)) 
let f=['-']?((['0'-'9']?|['1'-'9'] ['0'-'9']*) '.' (['0']|(['1'-'9']?|['0'-'9']*? ['1'-'9']))) 
let i=['['] [' ' '\t']* n [' ' '\t']* [','] [' ' '\t']* n [' ' '\t']* [']']
let x=(['['] [' ' '\t']* f [' ' '\t']* [','] [' ' '\t']* n [' ' '\t']* [']'])|(['['] [' ' '\t']* n [' ' '\t']* [','] [' ' '\t']* f [' ' '\t']* [']'])|(['['] [' ' '\t']* f [' ' '\t']* [','] [' ' '\t']* f [' ' '\t']* [']'])
let c=n|f
let uop="COUNT"|"ROWCOUNT"|"COLCOUNT"|"SUM"|"ROWSUM"|"COLSUM"|"AVG"|"ROWAVG"|"COLAVG"|"MIN"|"ROWMIN"|"COLMIN"|"MAX"|"ROWMAX"|"COLMAX" 
let bop="ADD"|"SUBT"|"MULT"|"DIV"
let r=['('] [' ' '\t']* i [' ' '\t']* [':'] [' ' '\t']* i [' ' '\t']* [')']
let y=(['('] [' ' '\t']* x [' ' '\t']* [':'] [' ' '\t']* i [' ' '\t']* [')'])|(['('] [' ' '\t']* i [' ' '\t']* [':'] [' ' '\t']* x [' ' '\t']* [')'])|(['('] [' ' '\t']* x [' ' '\t']* [':'] [' ' '\t']* x [' ' '\t']* [')'])

rule token = parse
    [' ' '\t']     { token lexbuf }     
  | f as lxm { Float(float_of_string lxm) } 
  | n as lxm { Index(int_of_string lxm) }
  | ')'            {Rparan}
  | '['            {Lbrac}
  | ']'            {Rbarc}
  | ';'            {Terminator}
  | ':'            {Colon}
  | ','            {Comma}
  | ":="           {Assign}
  | c              {Cons}
  | i              {Indice} 
  | r              {Range}
  | x              {raise Ill}
  | y              {raise Ill}
  | ['0']* n       {raise Ill}
  | f ['0']*       {raise Ill}
  | uop            {Uop}
  | bop            {Bop}
  | eof            { raise Eof }
  | _              { raise Eof }

{
let main() = begin
  try
      let rec lexbuf = Lexing.from_channel stdin in
      while true do
        let result = token lexbuf in
          Printf.printf "I saw a token!\n";
      done
    with Eof -> exit 0
        |Ill -> Printf.printf("illegal\n") 
end;;
main() ;;
}
