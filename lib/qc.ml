open Qc_misc ;

value with_include_path = Qc_misc.with_include_path ;

module Qasm2 = struct
open Qasm2syntax ;

type t =  list (AST.stmt_t loc) [@@deriving (to_yojson, show, eq, ord);] ;

value pp_hum pps x = Qasmpp.ASTPP.main pps ("2.0", x) ;

value of_string s = Qasm_io.full_to_ast s ;
value of_file f = Qasm_io.full_to_ast_from_file f ;

value lib_of_file f = Qasm2_parser.PA.include_file f ;

end ;

module Qlam = struct
open Qlam_syntax ;

module Circ = struct
type t = SYN.qcirc_t [@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps qc = PP.qcirc pps qc ;

value of_string s = Qlam_parser.qcircuit_of_string s ;
value of_file f = Qlam_parser.read_qcircuit f ;
end ;

module Environ = struct
type t = SYN.environ_t [@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps qc = PP.environ pps qc ;

value of_string s = Qlam_parser.qelib_of_string s ;
value of_file f = Qlam_parser.qelib_of_file f ;
end ;

module Prog = struct
type t = SYN.program_t [@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps p = PP.program pps p ;
value of_string s = Qlam_parser.program_of_string s ;
value of_file f = Qlam_parser.read_program f ;
end ;

end ;

