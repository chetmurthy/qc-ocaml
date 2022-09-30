open Qlam_syntax ;

value g = Grammar.gcreate (Plexer.gmake ());
value (qcirc : Grammar.Entry.e QC.t) = Grammar.Entry.create g "qcirc";
