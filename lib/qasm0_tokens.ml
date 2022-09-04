
type token =
  Comment of string
| Qubit of string
| Cbit of string
| Def of string * int * string
| Defbox of string * int * int * string
| Gate of string * string list
| EOF
