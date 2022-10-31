
module ME = struct

type t =
  QW
| QWA
| QWX of int
| CW
| CWA
| CDOTS
| LSTICK of string option
| RSTICK of string option
| DSTICK of string * int
| CGHOST of string
| GHOST of string * int option
| MULTIGATE of int * string * int option
| CTRL of int
| TARG
| METER
[@@deriving to_yojson, show, eq, ord]
end
module MatrixElement = ME
