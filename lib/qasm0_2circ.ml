open Pa_ppx_base
open Ppxutil
open Pa_ppx_utils
open Coll
open Std

let append r v = r := !r @ [v]
let vector_wrapped_get v n =
  let n = if n < 0 then
            (Vector.length v) + n
          else n in
  Vector.get v n

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
      ; mutable timeseq : int
      ; mutable id : int
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
      Fmt.(raise_failwithf loc "[qgate] OOPS! unknown gate op %s on %a" op (list ~sep:(const string ",") string) qubits) ;

    let nqubits = List.length qubits in
    let (nbits, nctrl, texsym) = MLM.map master op in
    if nbits <> nqubits then
      Fmt.(raise_failwithf loc "[qgate] OOPS! wrong number of qubits in op %s on %a" op (list ~sep:(const string ",") string) qubits) ;
    if not (distinct qubits) then
      Fmt.(raise_failwithf loc "[qgate] OOPS! duplicate bit operands in op %s on %a" op (list ~sep:(const string ",") string) qubits) ;
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

  let set_bittype_bool g (qb, b) =
    set_bittype g (qb, if b then Wire.Classical else Wire.Quantum)



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
      append sacc Fmt.(str {|\def\%s{\gnqubit{%s}{%s}%s\A{%s}}|}  myid u dstr w myid) ;

      (except qb targets)
      |> List.iter (fun qb ->
             let myid = MLM.map g.xy qb in
             let w = Wire.wire (MLM.map g.wiretype qb) in
             append sacc Fmt.(str {|\def\%s{\gspace{%s}%s\A{%s}}|} myid u w myid)
           ) ;

      controls
      |> List.iteri (fun k _ ->
             append sacc (defid g k {|\b|})
           ) ;

      let wt = Wire.wire (get_wiretype g controls) in
      controls
      |> List.iter (fun qb ->
             if MLM.map g.yloc qb < ytop then
               g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt xytop (MLM.map g.xy qb))
             else
               g.endtex <- g.endtex ^ Fmt.(str {|\ar@{%s}"%s";"%s"|} wt xybot (MLM.map g.xy qb))
           ) ;

      String.concat "\n" !sacc

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
             append sacc (defid g k {|\b|})
           ) ;
      append sacc (defid g nctrl u) ;
      let s = String.concat "\n" !sacc  in
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
                      g.name (list ~sep:(const string ",") string) g.qubits)
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
      bits : (string,  (Wire.t * string option)) MLM.t
    ; comments : string list ref
    ; gateMasterDef : (string, (int * int * string)) MLM.t
    ; gates : QGate.t list ref
    }
        
  let mk () = {
      bits = MLM.mk()
    ; comments  = ref []
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
    ; gates = ref []
    }

  let comment it s = append it.comments s

  let qubit it (id, initval_opt) =
    MLM.add it.bits (id, (Quantum, initval_opt))

  let cbit it (id, initval_opt) =
    MLM.add it.bits (id, (Classical, initval_opt))

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
    append it.gates g

end

module Parse = struct
  open Qasm0_tokens
  let rec parse it =
    parser
  | [< '(loc, (Comment line, _)) ; strm >] ->
     Ast.comment it line ;
     parse it strm
  | [< '(loc,(Qubit (id, initval_opt), line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.qubit it (id, initval_opt) ;
     parse it strm
  | [< '(loc,(Cbit(id, initval_opt), line)) ; strm >] ->
     Ast.comment it ("% "^line) ;
     Ast.cbit it (id, initval_opt) ;
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
  | [< >] -> it
     
end

module Circuit = struct
  type t = {
      ast : Ast.t
    ; initval : (string, string) MLM.t
    ; is_cbit : (string, bool) MLM.t
    ; qubitnames : string Vector.t
    ; qbtab : (string, int Vector.t) MLM.t
    ; qb2idx : (string, int) MLM.t
    ; optab : QGate.t list ref
    ; circuit : int Vector.t Vector.t
    ; mutable matrix : string Vector.t Vector.t
    }

(*
    def setnames(self,names,types):	# set bit names and types (+ initval)

        def do_name(n,type):		# set names & extract initial values
            tmp = n.split(',')			# check for initial value
            self.qubitnames.append(tmp[0])	# add to name list
            self.is_cbit[tmp[0]] = type		# 0 = qubit, 1 = cbit
            if(len(tmp)>1):
                self.initval[tmp[0]] = tmp[1]	# add initial value for qubit

        self.qubitnames = []
        for k in range(len(names)):		# loop over qubit names
            do_name(names[k],types[k])		# process name and type

 *)

  let setnames it bits =
    let do_name n (ty,initval_opt) =
      Vector.push it.qubitnames n ;
      MLM.add it.is_cbit (n, ty = Wire.Classical) ;
      Option.iter (fun initval -> MLM.add it.initval (n, initval)) initval_opt
    in
    MLM.app do_name bits

(*
    def add_op(self,gate):	# add gate to circuit

        self.optab.append(gate)		# put gate into table of gates
        gate.id = len(self.optab)-1	# give the gate a unique ID number
        # print "%% adding op %s(%s) IDs: %s" % (gate.name,gate.args,
        #                                       join(gate.xy.values(),','))
        
        for qb in gate.qubits:		# put gate on qubits it acts upon
            if(self.qbtab.has_key(qb)==0):	# check for syntax error
                s = (qb,gate.linenum,gate.name + ' ' + gate.args)
                do_error('[qcircuit] No qubit %s in line %d: "%s"' % s)
            if(len(self.qbtab[qb])==0):	# if first gate, timestep = 1
                ts = 1
            else:			# otherwise, timestep = last+1
                ts = self.optab[self.qbtab[qb][-1]].timeseq+1
            self.qbtab[qb].append(gate.id)
            if(ts>gate.timeseq):	# set timeseq number for gate
                gate.timeseq = ts	# to be largest of its qubits

        gate.make_id(self.qb2idx)	# make gate ID's (do after timestep)

        if(gate.timeseq > len(self.circuit)):	# add new timestep if necessary
            self.circuit.append([])
        self.circuit[gate.timeseq-1].append(gate.id)	# add gate to circuit

 *)

  let add_op it g =
    let open QGate in
    append it.optab g ;
    g.id <- (List.length !(it.optab) - 1) ;
    g.qubits
    |> List.iter (fun qb ->
           if not(MLM.in_dom it.qbtab qb) then
             Fmt.(raise_failwithf g.loc {|[qcircuit] No qubit %s "%s %a"|} qb g.name (list ~sep:(const string ",") string) g.qubits) ;
           let ts =
             if not (MLM.in_dom it.qbtab qb) || Vector.length (MLM.map it.qbtab qb) = 0 then
               1
             else
               (List.nth !(it.optab) (vector_wrapped_get (MLM.map it.qbtab qb) (-1))).timeseq + 1
           in
           g.timeseq <- ts
         ) ;
    make_id g it.qb2idx ;
    if g.timeseq > Vector.length it.circuit then
      Vector.push it.circuit (Vector.create ~dummy:0) ;
    Vector.push (Vector.get it.circuit (g.timeseq - 1)) g.id

(*
        
    def output_sequence(self):	# output time-sequence of gates
        k = 1				# timestep counter
        for timestep in self.circuit:	# loop over timesteps
            print "%%  Time %02d:" % k
            for g in timestep:		# loop over events in this timestep
                op = self.optab[g]
                print "%%    Gate %02d %s(%s)" % (op.id,
                                                     op.name,op.args)
            k += 1
        print ""

 *)

  let format_sequence it =
    let l = it.circuit
            |> Vector.to_list
            |> List.mapi (fun km1 timestep ->
                   [Fmt.(str "%%  Time %02d:" (km1+1))]@
                     (timestep
                      |> Vector.to_list
                      |> List.map (fun gidx ->
                             let op = List.nth !(it.optab) gidx in
                             Fmt.(str "%%    Gate %02d %s(%a)" op.id op.name (list ~sep:(const string ",") string) op.qubits)))
                 )
            |> List.concat in
    l @ ["\n"]

(*
    def make_matrix(self):	# make circuit matrix, of qubit vs timestep
        
        self.matrix = []
        ntime = len(self.circuit)+2	# total number of timsteps
        wires = ['n','N']		# single or double wire for qubit/cbit

        for qb in self.qubitnames:	# loop over qubits
            self.matrix.append([])	# start with empty row
            k = 1			# timestep counter
            cbit = self.is_cbit[qb]	# cbit=0 means qubit type (single wire)
            gidtab = self.qbtab[qb]	# table of gate IDs
            for gid in gidtab:		# loop over IDs for gates on qubit
                g = self.optab[gid]	# gate with that ID
                while(g.timeseq>k):	# output null ops until gate acts
                    self.matrix[-1].append('%s  ' % wires[cbit])
                    k += 1		# increment timestep  
                g.set_bittype(qb,cbit)	# set qubit type (cbit/qubit)
                self.matrix[-1].append(g.xy[qb])
                k += 1			# increment timestep
                if(g.texsym=='\meter'):	# if measurement gate then cbit=1
                    cbit = 1
                if(g.texsym.find('\dmeter')>=0): # alternative measurement gate
                    cbit = 1
                if(g.name=='measure'):	# if measurement gate then cbit=1
                    cbit = 1		# switch to double wire
                if(g.name=='zero'):	# if zero gate then cbit=0
                    cbit = 0		# switch to single wire
            while(k<ntime):		# fill in null ops until end of circuit
                k += 1			# unless last g was space or discard
                if((g.name!='space')&(g.name!='discard')):
                    self.matrix[-1].append('%s  ' % wires[cbit])
 *)
  let make_matrix it =
    let open QGate in
    Vector.clear it.matrix ;
    let ntime = (Vector.length it.circuit) + 2 in
    let to_wire b = if b then "N" else "n" in
    it.qubitnames
    |> Vector.iter (fun qb  ->
           Vector.push it.matrix (Vector.create ~dummy:"") ;
           let k = ref 1 in
           let cbit = ref (MLM.map it.is_cbit qb) in
           let gidtab = MLM.map it.qbtab qb in
           gidtab
           |> Vector.iter (fun gid ->
                  let g = List.nth !(it.optab) gid in
                  while (g.timeseq > !k) do
                    let txt = Fmt.(str "%s  " (to_wire !cbit)) in
                    Vector.push (vector_wrapped_get it.matrix (-1)) txt ;
                    incr k
                  done ;
                  set_bittype_bool g (qb, !cbit) ;
                  Vector.push (vector_wrapped_get it.matrix (-1)) (MLM.map g.xy qb) ;
                  incr k ;
                  if g.texsym = {|\meter|} then
                    cbit := true
                  else if Pcre.(pmatch ~pat:(quote {|\meter|}) g.texsym) then
                    cbit := true
                  else if g.name = "measure" then
                    cbit := true
                    else if g.name = "zero" then
                    cbit := false ;
                ) ;
           let last_gid = vector_wrapped_get gidtab (-1) in
           let last_g = List.nth !(it.optab) last_gid in
           while !k < ntime do
             incr k ;
             if last_g.name <> "space" && last_g.name <> "discard" then
               let txt = Fmt.(str "%s  " (to_wire !cbit)) in
               Vector.push (vector_wrapped_get it.matrix (-1)) txt
           done
         )

    (*

    def qb2label(self,qb):	# make latex format label for qubit name

        m = re.compile('([A-z]+)(\d+)').search(qb)
        if(m):			# make num subscript if name = alpha+numbers
            label = "%s_{%s}" % (m.group(1),m.group(2))
        else:
            label = qb			# othewise use just what was specified
        if(self.is_cbit[qb]):
            if(self.initval.has_key(qb)):	# qubit has initial value?
                label = r'   {%s = %s}' % (label,self.initval[qb])
            else:
                label = r'   {%s}' % (label)
        else:
            if(self.initval.has_key(qb)):	# qubit has initial value?
                label = r'\qv{%s}{%s}' % (label,self.initval[qb])
            else:
                label = r' \q{%s}' % (label)
        return(label)
     *)
  let qb2label it qb =
    let label = match Pcre.exec ~pat:{|([A-z]+)(\d+)|} qb with
        exception Not_found -> qb
      | ss ->
         let ident = Pcre.get_substring ss 1 in
         let num = Pcre.get_substring ss 2 in
         Fmt.(str "%s_{%s}" ident num) in
    let label =
      if MLM.map it.is_cbit qb then
        if MLM.in_dom it.initval qb then
          Fmt.(str "   {%s = %s}" label (MLM.map it.initval qb))
        else
          Fmt.(str "   {%s}" label)
      else
        if MLM.in_dom it.initval qb then
          Fmt.(str {|\qv{%s}{%s}|} label (MLM.map it.initval qb))
        else
          Fmt.(str {| \q{%s}|} label) in
    label

(*

    def output_matrix(self):	# output circuit matrix, of qubit vs timestep

        if(len(self.matrix)==0):	# make circuit matrix if not done
            self.make_matrix()

        k = 0
        print "% Qubit circuit matrix:\n%"
        for y in self.matrix:	# loop over qubits
            print '%% %s: %s' % (self.qubitnames[k],join(y,', '))
            k += 1
 *)
  let format_matrix it =
    if Vector.length it.matrix = 0 then
      make_matrix it ;
    ["% Qubit circuit matrix:\n%\n"]
    @(it.matrix
      |> Vector.to_list
      |> List.mapi (fun k y ->
             Fmt.(str "%% %s: %s" (Vector.get it.qubitnames k) (String.concat ", " (Vector.to_list y)))
           )
     )

(*

    def output_latex(self):	# output latex with xypic for circuit

        if(len(self.matrix)==0):	# make circuit matrix if not done
            self.make_matrix()

        print ''
        print r'\documentclass[preview]{standalone}'        # output latex header
        print r'\input{xyqcirc.tex}'

        # now go through all gates and output latex definitions
        print ""
        print "% definitions for the circuit elements\n"
        for g in self.optab:
            print g.latex()		# output \def\gXY{foo} lines

        # now output defs for qubit labels and initial states
        print ""
        print "% definitions for bit labels and initial states\n"
        for j in range(len(self.matrix)):
            qb = self.qubitnames[j]
            print r"\def\b%s{%s}" % (num2name(j+1),self.qb2label(qb))

        # now output circuit
        print ""
        # print r'\xymatrix@R=15pt@C=12pt{'
        print "% The quantum circuit as an xymatrix\n"
        print r'\xymatrix@R=5pt@C=10pt{'

        ntime = len(self.circuit)+2	# total number of timsteps
        j = 0				# counter for timestep
        stab = []			# table of strings
        for y in self.matrix:		# loop over qubits
            qb = self.qubitnames[j]	# qubit name
            ops = join(map(lambda(x):'\\'+x,y),' &')
            stab.append(r'\b%s & %s' % (num2name(j+1),ops)) 
            j += 1			# increment timestep
        stab[0] = '    ' + stab[0]
        print join(stab,'\n\\\\  ')

        # now go through all gates and output final latex (eg vertical lines)
        print "%"
        print "% Vertical lines and other post-xymatrix latex\n%"
        for g in self.optab:
            if(g.endtex!=""):
                print g.endtex		# output end latex commands

        # now end the xymatrix & latex document
        print r'}'
        print ''
        print r'\end{document}'
 *)
  let format_latex it =
    let open QGate in
    if Vector.length it.matrix = 0 then
      make_matrix it ;
    let sacc = ref [] in
    append sacc {|
\documentclass[preview]{standalone}
\input{xyqcirc.tex}

% definitions for the circuit elements
|} ;
    !(it.optab)
    |> List.iter (fun g ->
           append sacc (QGate.Latex.latex ~master:it.ast.Ast.gateMasterDef g)
         ) ;
    append sacc {|
% The quantum circuit as an xymatrix
\xymatrix@R=5pt@C=10pt{
|} ;
    let ntime = (Vector.length it.circuit) + 2 in
    let j = ref 0 in
    let stab = Vector.create ~dummy:"" in
    it.matrix
    |> Vector.to_list
    |> List.iter (fun y ->
           let qb = Vector.get it.qubitnames !j  in
           let ops = String.concat " &" (y |> Vector.to_list |>  List.map (fun x -> "\\"^x)) in
           Vector.push stab Fmt.(str {|\b%s & %s|} (num2name(!j+1)) ops) ;
           incr j
         ) ;
    Vector.set stab 0 ("    "^(Vector.get stab 0)) ;
    append sacc (String.concat "\n\\\\  " (Vector.to_list stab)) ;

    append sacc {|
%
% Vertical lines and other post-xymatrix latex
%
|} ;
    !(it.optab)
    |> List.iter (fun g ->
           if g.endtex <> "" then
             append sacc g.endtex ;
         ) ;
    append sacc {|
}

\end{document}
|} ;
    !sacc

  let mk ast =
    let it = {
        ast
        ; initval = MLM.mk()
        ; is_cbit = MLM.mk()
        ; qubitnames = Vector.create ~dummy:""
        ; qbtab = MLM.mk()
        ; qb2idx = MLM.mk()
        ; optab = ref []
        ; circuit = Vector.create ~dummy:(Vector.create ~dummy:0)
        ; matrix = Vector.create ~dummy:(Vector.create ~dummy:"")
      } in
    setnames it ast.Ast.bits ;
    it.qubitnames
    |> Vector.iteri (fun km1 name ->
           MLM.add it.qbtab (name, Vector.create ~dummy:0) ;
           MLM.add it.qb2idx (name, km1+1)
         ) ;
    it

end

module Top = struct

let catch_parse_error pfun tokstrm =
  try pfun tokstrm
  with Stream.Error _ ->
        match Stream.peek tokstrm with
        | None -> Fmt.(failwithf "parse error at EOF")
        | Some(loc, tok) ->
           Fmt.(raise_failwithf loc "parse error")

let parse1 strm =
  let ast = Ast.mk() in
  Parse.parse ast strm

let document strm =
  let ast = parse1 strm in
  let qc = Circuit.mk ast in
  !(ast.Ast.gates)
  |> List.iter (Circuit.add_op qc) ;
  let comments = String.concat "" !(ast.Ast.comments) in
  let comments = Pcre.substitute ~pat:"#" ~subst:(fun _ -> "%") comments in
  [comments]
  @(Circuit.format_sequence qc)
  @(Circuit.format_matrix qc)
  @(Circuit.format_latex qc)

let full_parse pfun ?(fname="") buf =
  let tokstrm = Qasm0_lexer.make_lexer ~fname buf in
  catch_parse_error pfun tokstrm

let full_parse_from_file pfun fname =
let ic = open_in fname in
  let tokstrm = Qasm0_lexer.make_lexer_from_channel ~fname ic in
  catch_parse_error pfun tokstrm
  
end
