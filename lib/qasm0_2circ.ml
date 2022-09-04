open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Coll
open Std

module Wire = struct
  type t = Quantum | Classical

  let wire ty = match ty with
      Quantum -> {|\w|}
    | Classical -> {|\W|}
end

(*
def num2name(num):	# convert a number to a name
	if( num == 0 ):
	  return "";
	elif( num <= 26 ):
	  return chr(num+64)
	else:
	  return chr( (num % 26) + 64) + num2name(num/26)
 *)

let rec num2name num =
  if num = 0 then ""
  else if num < 26 then
    String.make 1 (Char.chr (num + 64))
  else (String.make 1 (Char.chr ((num mod 26) + 64))) ^ (num2name (num / 26))

module QGate = struct
  type t = {
        name : string
      ; qubits : string list
      ; timeseq : int
      ; id : int
      ; mutable endtex : string
      ; xy : (string, string) MLM.t
      ; yloc : (string, int) MLM.t
      ; wiretype : (string, Wire.t) MLM.t
      ; loc : Ploc.t
      ; nbits : int
      ; nctrl : int
      ; texsym : string
    }

  let mk loc ~master op qubits =
    let timeseq = 0 in
    let id = 0 in
    let endtex = "" in
    let xy = MLM.mk () in
    let yloc = MLM.mk () in
    let wiretype = MLM.mk () in
    if not (MLM.in_dom master op) then
      Fmt.(raise_failwithf loc "[qgate] OOPS! unknown gate op %s on %a" op (list string) qubits) ;

    let nqubits = List.length qubits in
    let (nbits, nctrl, texsym) = MLM.map master op in
    if nbits <> nqubits then
      Fmt.(raise_failwithf loc "[qgate] OOPS! wrong number of qubits in op %s on %a" op (list string) qubits) ;
    if not (distinct qubits) then
      Fmt.(raise_failwithf loc "[qgate] OOPS! duplicate bit operands in op %s on %a" op (list string) qubits) ;
    {
      name = op
    ; qubits = qubits
    ; timeseq
    ; id
    ; endtex
    ; xy
    ; yloc
    ; wiretype
    ; loc
    ; nbits 
    ; nctrl 
    ; texsym
    }

  let set_bittype g (qb, ty) =
    MLM.add g.wiretype (qb, ty)

(*
    def xid(self):			# return ID string for gate timestep
    return('g%s' % (num2name(self.timeseq)))
 *)

  let xid g =
    Fmt.(str "g%s" (num2name g.timeseq))

(*

    def xyid(self,qubitnum):		# return ID string for gate/qubit
        return('%s%s%s' % (self.xid(),'x',num2name(qubitnum)))

 *)

  let xyid g qubitnum =
    Fmt.(str "%s%s%s" (xid g) "x" (num2name qubitnum))


(*
    def make_id(self,qb2idx):		# make gate ID's, eg gAB
        for qb in self.qubits:
            self.xy[qb] = self.xyid(qb2idx[qb])
            self.yloc[qb] = qb2idx[qb]	# y (vertical) location of qubit
 *)

  let make_id g qb2idx =
    g.qubits
    |> List.iter (fun qb ->
           MLM.add g.xy (qb, xyid g (MLM.map qb2idx qb)) ;
           MLM.add g.yloc (qb, (MLM.map qb2idx qb))
         )


  module Latex = struct

  (*

    def latex(self):			# output latex/xypic/xyqcirc for gate

        def defid(k,op):		# latex def for given gate & qubit
            myid = self.xy[self.qubits[k]]
            wires = ['\w','\W']		# \w = single, \W = double wire
            mywire = wires[self.wiretype[self.qubits[k]]]
            return('\def\%s{%s%s\A{%s}}' % (myid,op,mywire,myid))
   *)
    let defid g k op =
      let qb = List.nth g.qubits k in
      let myid = MLM.map g.xy qb in
      let mywire = Wire.wire (MLM.map g.wiretype qb) in
      Fmt.(str {|\def\%s{%s%s\A{%s}}|} myid op mywire myid)

    (*
        def get_wiretype(qubits):	# figure out wire type for verticals
            # if any control is classical (double-wire) then all should be
            if(sum([ self.wiretype[x] for x in qubits])>0):
                wt = '='		# wire type = cbit
            else:
                wt = '-'		# wire type = qubit
            return(wt)
     *)

    let get_wiretype g qubits =
      if qubits |> List.exists (fun qb -> MLM.map g.wiretype qb = Wire.Classical) then
        Wire.Classical
      else Wire.Quantum

(*
        def do_multiqubit(nbits,nctrl,u):	# multiple-qubit operation
            # first do target qubits (big box)
            s = []
            targets = self.qubits[nctrl:]
            ytab = [ self.yloc[qb] for qb in targets ]
            idx = ytab.index(min(ytab))	# find which qubit is first
            qb = targets[idx]		# handle first qubit specially

            ytop = min(ytab)		# remember y location & ID of top qubit
            xytop = self.xy[qb]
            ybot = max(ytab)		# and bottom
            xybot = self.xy[targets[ytab.index(ybot)]]

            myid = self.xy[qb]		# top qubit gets \gnqubit{u}{ddd...}
            dstr = 'd'*(nbits-nctrl-1)
            wires = ['\w','\W']		# \w = single, \W = double wire
            w = wires[self.wiretype[qb]]
            s.append(r'\def\%s{\gnqubit{%s}{%s}%s\A{%s}}'%(myid,u,dstr,w,myid))
            firstqb = qb
            for qb in targets:		# loop over target bits
                if (qb==firstqb):	# skip first qubit
                    continue
                myid = self.xy[qb]	# non-first bits get \gspace{u}
                w = wires[self.wiretype[qb]]
                s.append(r'\def\%s{\gspace{%s}%s\A{%s}}' % (myid,u,w,myid))
                
            # now do control qubits
            controls = self.qubits[:nctrl]
            for k in range(nctrl):	# loop over all control qubits
                s.append(defid(k,r'\b'))		# bullets on controls

            # create vertical wires
            # if any control is classical (double-wire) then all should be
            wt = get_wiretype(controls)
            for qb in controls: 	# loop over all ctrl qubits
                # endtex = latex commands which appear after xymatrix body
                # such as the vertical wires
                if self.yloc[qb] < ytop:
                    self.endtex += r'\ar@{%c}"%s";"%s"' %(wt,xytop,self.xy[qb])
                else:
                    self.endtex += r'\ar@{%c}"%s";"%s"' %(wt,xybot,self.xy[qb])

            # done with multi-qubit op
            return(join(s,'\n'))		# return with latex def's
 *)

    let do_multiqubit g (nbits, nctrl, u) =
      let sacc = ref [] in
      let (controls, targets) = sep_firstn nctrl g.qubits in
      let qb =
        let min_yloc = MLM.fold (fun minval (k, v) -> min minval v) (-1) g.yloc in
        List.hd (MLM.inv g.yloc min_yloc) in
      let ytop = MLM.map g.yloc qb in
      let xytop = MLM.map g.xy qb in
      let qb_bot =
        let max_yloc = MLM.fold (fun maxval (k, v) -> max maxval v) max_int g.yloc in
        List.hd (MLM.inv g.yloc max_yloc) in
      let ybot = MLM.map g.yloc qb_bot in
      let xybot = MLM.map g.xy qb_bot in
      
      let myid = xytop in
      let dstr = String.make (nbits - nctrl - 1) 'd' in
      let w = Wire.wire (MLM.map g.wiretype qb) in
      push sacc Fmt.(str {|\def\%s{\gnqubit{%s}{%s}%s\A{%s}}|}  myid u dstr w myid) ;

      (except qb targets)
      |> List.iter (fun qb ->
             let myid = MLM.map g.xy qb in
             let w = Wire.wire (MLM.map g.wiretype qb) in
             push sacc Fmt.(str {|\def\%s{\gspace{%s}%s\A{%s}}|} myid u w myid)
           ) ;

      controls
      |> List.iteri (fun k _ ->
             push sacc (defid g k {|\b|})
           ) ;

      let wt = Wire.wire (get_wiretype g controls) in
      controls
      |> List.iter (fun qb ->
             if MLM.map g.yloc qb < ytop then
               g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt xytop (MLM.map g.xy qb))
             else
               g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt xybot (MLM.map g.xy qb))
           ) ;

      String.concat "\n" (List.rev !sacc)

(*
        def ctrl_op(nctrl,u):		# controlled operation
            s = []
            for k in range(nctrl):	# loop over all control qubits
                s.append(defid(k,r'\b'))		# bullets on controls
            s.append(defid(nctrl,u))	# add target op 
            s = join(s,'\n')

            # create vertical wires
            qbtarget = self.xy[self.qubits[-1]]	
            wt = get_wiretype(self.qubits[0:-1])
            for qb in self.qubits[0:-1]: # loop over all ctrl-target pairs
                # endtex = latex commands which appear after xymatrix body
                # such as the vertical wires
                self.endtex += r'\ar@{%c}"%s";"%s"' % (wt,qbtarget,self.xy[qb])

            return(s)
 *)

    let ctrl_op g nctrl u =
      let sacc = ref [] in
      (interval 0 (nctrl-1))
      |> List.iter (fun k ->
             push sacc (defid g k {|\b|})
           ) ;
      push sacc (defid g nctrl u) ;
      let s = String.concat "\n" (List.rev !sacc)  in
      let (last, first) = sep_last g.qubits in
      let qbtarget = MLM.map g.xy last in
      let wt = Wire.wire (get_wiretype g first) in
      first
      |> List.iter (fun qb ->
             g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt qbtarget (MLM.map g.xy qb))
           ) ;
      s

  (*
        def check_multi_qubit_gate_targets(nctrl):
            # gate targets (not controls) must be consecutive bits
            ytab = [self.yloc[qb] for qb in self.qubits[nctrl:]]
            ytab.sort()
            for k in range(len(ytab)-1):
                if (ytab[k+1]-ytab[k]!=1):
                    s = (self.linenum, self.name + " " + self.args)
                    do_error('[qgate] OOPS! line %d multi-qubit gate targets not consecutive %s' % s)
   *)

    let check_multi_qubit_gate_targets g nctrl =
      let (_, targets) = sep_firstn nctrl g.qubits in
      let ytab = List.map (MLM.map g.yloc) targets in
      let ytab = List.sort Stdlib.compare ytab in
      (interval 0 ((List.length ytab) - 2))
      |> List.iter (fun k ->
             if List.nth ytab (k+1) - List.nth ytab k <> 1 then
               Fmt.(raise_failwithf g.loc "[qgate] OOPS! multi-qubit gate targets not consecutive %s %a"
                      g.name (list string) g.qubits)
           )

(*
        def double_sym_gate(texsym):
            wt = get_wiretype(self.qubits)
            qb0 = self.xy[self.qubits[0]]
            qb1 = self.xy[self.qubits[1]]
            self.endtex += r'\ar@{%c}"%s";"%s"' % (wt,qb0,qb1)
            return(defid(0,texsym) + '\n' + defid(1,texsym))
 *)
    let double_sym_gate g texsym =
      let wt = Wire.wire (get_wiretype g g.qubits) in
      let qb0 = MLM.map g.xy (car g.qubits) in
      let qb1 = MLM.map g.xy (cadr g.qubits) in
      g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt qb0 qb1) ;
      (defid g 0 texsym) ^ "\n" ^ (defid g 1 texsym)

    let latex ~master g =
      let (nbits, nctrl, texsym) = MLM.map master g.name in
      if g.name = "zero" then
        let myid = MLM.map g.xy (car g.qubits) in
        Fmt.(str {|\def\%s{%s\A{%s}}|} myid texsym myid)
      else if g.name = "space" then
        let myid = MLM.map g.xy (car g.qubits) in
        Fmt.(str {|\def\%s{\A{%s}}|} myid myid)
      else if g.name ="ZZ" then
        double_sym_gate g texsym
      else if g.name = "SS" then
        double_sym_gate g texsym
      else if g.name = "swap" then
        double_sym_gate g texsym
      else if nbits-nctrl > 1 then begin
        check_multi_qubit_gate_targets g nctrl ;
        do_multiqubit g (nbits,nctrl,texsym)
        end
      else if nctrl = 0 then
        defid g 0 texsym
      else
        ctrl_op g nctrl texsym
        
           (*
        # main routine to generate latex
        (nbits, nctrl, texsym) = GateMasterDef[self.name]
        if(self.name=='zero'):		# special for zero: no wire
            myid = self.xy[self.qubits[0]]
            return('\def\%s{%s\A{%s}}' % (myid,texsym,myid))
        if(self.name=='space'):		# special for space: no wire
            myid = self.xy[self.qubits[0]]
            return('\def\%s{\A{%s}}' % (myid,myid))
        if(self.name=='ZZ'):		# special for ZZ gate
            return(double_sym_gate(texsym))
        if(self.name=='SS'):		# special for SS gate
            return(double_sym_gate(texsym))
        if(self.name=='swap'):		# special for swap gate
            return(double_sym_gate(texsym))
        if(nbits-nctrl>1):			# multi-qubit gate
            check_multi_qubit_gate_targets(nctrl)
            return(do_multiqubit(nbits,nctrl,texsym))
        if(nctrl==0):
            return(defid(0,texsym))		# single qubit op
        else: 
            return(ctrl_op(nctrl,texsym))	# controlled-single-qubit op
   *)
  end

end

module Ast = struct
  type t = {
      bits : (string,  Wire.t) MLM.t
    ; mutable comments : string list
    ; gateMasterDef : (string, (int * int * string)) MLM.t
    ; mutable gates : QGate.t list
    }
        
  let mk () = {
      bits = MLM.mk()
    ; comments  = []
    ; gateMasterDef =
        MLM.ofList () [
            ({|cnot|},     ( 2 , 1 , {|\o|}        ))
          ; ({|c-z|},      ( 2 , 1 , {|\op{Z}|}    ))
          ; ({|c-x|},      ( 2 , 1 , {|\op{X}|}    ))
          ; ({|measure|},  ( 1 , 0 , {|\meter|}    ))
          ; ({|dmeter|},   ( 1 , 0 , {|\dmeter{}|} ))
          ; ({|h|},        ( 1 , 0 , {|\op{H}|}    ))
          ; ({|H|},        ( 1 , 0 , {|\op{H}|}    ))
          ; ({|X|},        ( 1 , 0 , {|\op{X}|}    ))
          ; ({|Y|},        ( 1 , 0 , {|\op{Y}|}    ))
          ; ({|Z|},        ( 1 , 0 , {|\op{Z}|}    ))
          ; ({|S|},        ( 1 , 0 , {|\op{S}|}    ))
          ; ({|T|},        ( 1 , 0 , {|\op{T}|}    ))
          ; ({|U|},        ( 1 , 0 , {|\op{U}|}    ))
          ; ({|ZZ|},       ( 2 , 0 , {|\b|}       ))
          ; ({|SS|},       ( 2 , 0 , {|\sq|}       ))
          ; ({|zero|},     ( 1 , 0 , {|\z|}        ))
          ; ({|nop|},      ( 1 , 0 , {|*-{}|}      ))
          ; ({|discard|},  ( 1 , 0 , {|\discard|}  ))
          ; ({|slash|},    ( 1 , 0 , {|\slash|}    ))
          ; ({|space|},    ( 1 , 0 , {||}          ))
          ; ({|swap|},     ( 2 , 0 , {|\t|}       ))
          ; ({|toffoli|},  ( 3 , 2 , {|\o|}       ))
          ; ({|Utwo|},     ( 2 , 0 , {|U|}         ))
          ]
    ; gates = []
    }

  let comment it s =
    it.comments <- it.comments @ [s]

  let qubit it id =
    MLM.add it.bits (id, Quantum)

  let cbit it id =
    MLM.add it.bits (id, Classical)

  let def it (id, nctrl, qtex) =
    let qtexlen = String.length qtex in
    let tex = String.sub qtex 1 (qtexlen - 2) in
    let texsym = 
      if tex = "bullet" then {|\b|}
      else if Pcre.pmatch ~pat:{|\dmeter|} tex then tex
      else Printf.sprintf {|\op{%s}|} tex in
    if MLM.in_dom it.gateMasterDef id then
      failwithf "[qasm_parser] oops! duplicate def for op %s" id ;
    MLM.add it.gateMasterDef (id, (nctrl+1, nctrl, texsym))

  let defbox it (id, nbits, nctrl, qtex) =
    let qtexlen = String.length qtex in
    let tex = String.sub qtex 1 (qtexlen - 2) in
    if MLM.in_dom it.gateMasterDef id then
      failwithf "[qasm_parser] oops! duplicate def for op %s" id ;
    MLM.add it.gateMasterDef (id, (nbits, nctrl, tex))

  let gate loc it (id, args) =
    let g = QGate.mk loc ~master:it.gateMasterDef id args in
    it.gates <- it.gates @ [g]

end

module Parse = struct
  open Qasm0_tokens
  let rec parse it =
    parser
  | [< '(loc, (Comment line, _)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     parse it strm
  | [< '(loc,(Qubit id, line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.qubit it id ;
     parse it strm
  | [< '(loc,(Cbit id, line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.cbit it id ;
     parse it strm
  | [< '(loc,(Def(id, n, tex), line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.def it (id,n, tex) ;
     parse it strm
  | [< '(loc,(Defbox(id, nbits, nctrl, tex), line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.defbox it (id,nbits, nctrl, tex) ;
     parse it strm
  | [< '(loc,(Gate(id, ids), line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.gate loc it (id,ids) ;
     parse it strm
     
end
