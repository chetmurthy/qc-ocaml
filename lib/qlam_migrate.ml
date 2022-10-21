
exception Migration_error of string

let migration_error feature =
  raise (Migration_error feature)

let _migrate_list subrw0 __dt__ l =
  List.map (subrw0 __dt__) l

type loc = [%import: Qlam_syntax.loc]
and unique_t = [%import: Qlam_syntax.SYN.Unique.t]
and bit_ident_t = [%import: Qlam_syntax.SYN.bit_ident_t
                  [@with Unique.t := unique_t]
                  ]
and coupling_map_t = [%import: Qlam_syntax.SYN.CouplingMap.t]
and const_t = [%import: Qlam_syntax.SYN.const_t]
and pvar_t = [%import: Qlam_syntax.SYN.pvar_t]
and qvar_t = [%import: Qlam_syntax.SYN.qvar_t]
and cvar_t = [%import: Qlam_syntax.SYN.cvar_t]
and qgn_t = [%import: Qlam_syntax.SYN.qgn_t]
and binop_t = [%import: Qlam_syntax.SYN.binop_t]
and unop_t = [%import: Qlam_syntax.SYN.unop_t]
and ufun_t = [%import: Qlam_syntax.SYN.ufun_t]
and pexpr_t = [%import: Qlam_syntax.SYN.pexpr_t]
and qgatelam_t = [%import: Qlam_syntax.SYN.qgatelam_t]
and qgateargs_t = [%import: Qlam_syntax.SYN.qgateargs_t]
and qcirc_t = [%import: Qlam_syntax.SYN.qcirc_t
              [@with Unique.t := unique_t]
              ]
and qbinding_t = [%import: Qlam_syntax.SYN.qbinding_t]
and item = [%import: Qlam_syntax.SYN.item
            [@with CouplingMap.t := coupling_map_t]
            ]
and gate_item = [%import: Qlam_syntax.SYN.gate_item]
and env_t = [%import: Qlam_syntax.SYN.env_t]
and top = [%import: Qlam_syntax.SYN.top]
[@@deriving migrate
    { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Qlam_syntax.SYN
        ; dstmod = Qlam_syntax.SYN
        ; types = [
            const_t
          ; pvar_t
          ; qvar_t
          ; cvar_t
          ; qgn_t
          ; binop_t
          ; unop_t
          ; ufun_t
          ; pexpr_t
          ; qgatelam_t
          ; qgateargs_t
          ; qcirc_t
          ; qbinding_t
          ; item
          ; gate_item
          ; env_t
          ; top
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_loc = {
          srctype = [%typ: loc]
        ; dsttype = [%typ: loc]
        ; code = fun __dt__ x -> x
        }
      ; migrate_id = {
          srctype = [%typ: Qc_misc.ID.t]
        ; dsttype = [%typ: Qc_misc.ID.t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_unique_t = {
          srctype = [%typ: unique_t]
        ; dsttype = [%typ: unique_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_bit_ident_t = {
          srctype = [%typ: bit_ident_t]
        ; dsttype = [%typ: bit_ident_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_coupling_map_t = {
          srctype = [%typ: coupling_map_t]
        ; dsttype = [%typ: coupling_map_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_file_type_t = {
          srctype = [%typ: Qc_misc.file_type_t]
        ; dsttype = [%typ: Qc_misc.file_type_t]
        ; code = fun __dt__ x -> x
        }
      ; migrate_real_numeral_t = {
          srctype = [%typ: Qc_misc.RealNumeral.t]
        ; dsttype = [%typ: Qc_misc.RealNumeral.t]
        ; code = fun __dt__ x -> x
        }
      }
    }
]

