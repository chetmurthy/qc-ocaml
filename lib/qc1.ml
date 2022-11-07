
let with_include_path = Qc0.with_include_path

module Qasm2 = struct
include Qc0.Qasm2
end

module Qlam = struct
open Qlam_syntax

module Circ = struct
include Qc0.Qlam.Circ
end

module Gate = struct
include Qc0.Qlam.Gate
end

module Environ = struct
include Qc0.Qlam.Environ
let of_qasm2 l = Qconvert.ToLam.env l
end

module Prog = struct
include Qc0.Qlam.Prog

let of_qasm2 l = Qconvert.ToLam.program l
let to_qasm2 ~env0 p = Qconvert.ToQasm2.program ~env0 p
end

end

module Layout = struct
include Qc0.Layout
end

module CM = struct
include Qc0.CM
end
module CouplingMap = CM
