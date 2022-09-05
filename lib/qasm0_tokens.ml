
type token =
  Comment of string
| Blank
| Qubit of string * string option
| Cbit of string * string option
| Def of string * int * string
| Defbox of string * int * int * string
| Gate of string * string list
| EOF
