
module Cookie = struct

(*

set-cookie-header = "Set-Cookie:" SP set-cookie-string
 set-cookie-string = cookie-pair *( ";" SP cookie-av )
 cookie-pair       = cookie-name "=" cookie-value
 cookie-name       = token
 cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
 cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                       ; US-ASCII characters excluding CTLs,
                       ; whitespace DQUOTE, comma, semicolon,
                       ; and backslash
 token             = <token, defined in [RFC2616], Section 2.2>

 cookie-av         = expires-av / max-age-av / domain-av /
                     path-av / secure-av / httponly-av /
                     extension-av
 expires-av        = "Expires=" sane-cookie-date
 sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
 max-age-av        = "Max-Age=" non-zero-digit *DIGIT
                       ; In practice, both expires-av and max-age-av
                       ; are limited to dates representable by the
                       ; user agent.
 non-zero-digit    = %x31-39
                       ; digits 1 through 9
 domain-av         = "Domain=" domain-value
 domain-value      = <subdomain>
                       ; defined in [RFC1034], Section 3.5, as
                       ; enhanced by [RFC1123], Section 2.1
 path-av           = "Path=" path-value
 path-value        = <any CHAR except CTLs or ";">
 secure-av         = "Secure"
 httponly-av       = "HttpOnly"
 extension-av      = <any CHAR except CTLs or ";">


   cookie-header = "Cookie:" OWS cookie-string OWS
   cookie-string = cookie-pair *( ";" SP cookie-pair )

   token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT

       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>


   OWS            = *( [ obs-fold ] WSP )
                    ; "optional" whitespace
   obs-fold       = CRLF


 *)

let semi_space = Pcre.regexp "; "
               
let cookie_string_re = Pcre.regexp "([^=]+)=([^\"]*|\"[^\"]+\")"

let _OWS_left_re = Pcre.regexp ~flags:[`DOTALL] "^(?:(?:\\r\\n)? )*(\\S.*)$"
let _OWS_right_re = Pcre.regexp ~flags:[`DOTALL] "^(.*\\S)(?:(?:\\r\\n)? )*$"

let clean_ows s =
  let rv1 = Pcre.extract ~rex:_OWS_left_re s in
  let s = rv1.(1) in
  let rv2 = Pcre.extract ~rex:_OWS_right_re s in
  rv2.(1)

let parse_cookie_pair pair =
  let a = Pcre.extract ~rex:cookie_string_re pair in
  (a.(1), a.(2))

let parse_cookie s =
  let s = clean_ows s in
  let l = Pcre.split ~rex:semi_space s in
  List.map parse_cookie_pair l

type cookie_attribute_t =
  | Expires of string
  | MaxAge of string
  | Domain of string
  | Path of string
  | Secure
  | HttpOnly
  | Extension of string

let split_av_re = Pcre.regexp "(Expires|Max-Age|Domain|Path)=(.*)"

let parse_cookie_av s =
  try
    let p = Pcre.extract ~rex:split_av_re s in
    let k = p.(1) in
    let v = p.(2) in
    match k with
  | "Expires" -> Expires v
  | "Max-Age" -> MaxAge v
  | "Domain" -> Domain v
  | "Path" -> Path v
  | _ -> Extension s
  with Not_found ->
        if s = "Secure" then Secure
        else if s = "HttpOnly" then HttpOnly
        else Extension s

let parse_set_cookie s =
  let cookie_pair::avl = Pcre.split ~rex:semi_space s in
  let (k,v) = parse_cookie_pair cookie_pair in
  (k,v,List.map parse_cookie_av avl)
end
