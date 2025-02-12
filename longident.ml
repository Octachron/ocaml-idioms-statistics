
let rec length = function
  | Longident.Lident _ -> 1
  | Longident.Ldot (x,_) -> 1 + length x
  | Longident.Lapply(x,y) -> 2 + length x + length y

let add_longident h l =
  let n = length l in
  let count = succ @@ Option.value ~default:0 (Hashtbl.find_opt h n) in
  Hashtbl.replace h n count


module Iterator(H:sig val h: (int,int) Hashtbl.t end) = struct
  let l' lid = add_longident H.h lid
  let l lid = l' lid.Location.txt

  let default = Ast_iterator.default_iterator
  open Parsetree
  open Ast_iterator
  let typ sub t =
    match t.ptyp_desc with
    | Ptyp_constr (lid, tl) -> l lid; List.iter (sub.typ sub) tl
    | Ptyp_class (lid, tl) -> l lid; List.iter (sub.typ sub) tl
    | Ptyp_package (lid, _) -> l lid; default.typ sub t
    | Ptyp_open (mod_ident, t) -> l mod_ident; sub.typ sub t

    | _ -> default.typ sub t

  let type_extension sub te =
    l te.ptyext_path; default.type_extension sub te


  let class_type sub cty =
    match cty.pcty_desc with
    | Pcty_constr (lid, _) -> l lid; default.class_type sub cty
    | _ -> default.class_type sub cty

  let module_type sub mty =
    match mty.pmty_desc with
    | Pmty_ident s
    | Pmty_alias s -> l s
    | _ -> default.module_type sub mty

  let with_constraint sub x = match x with
    | Pwith_modsubst (_, lid)
    | Pwith_type (lid, _)
    | Pwith_modtype (lid, _)
    | Pwith_typesubst (lid, _)
    | Pwith_modtypesubst (lid, _) -> l lid; default.with_constraint sub x
    | Pwith_module (lid, lid2) -> l lid; l lid2

  let module_expr sub me = match me.pmod_desc with
    | Pmod_ident x -> l x
    | _ -> default.module_expr sub me

  let expr sub e = match e.pexp_desc with
    | Pexp_ident lid | Pexp_construct (lid,_)
    | Pexp_field (_, lid) | Pexp_new lid
    | Pexp_setfield (_, lid, _) -> l lid; default.expr sub e
    | _ -> default.expr sub e

  let pat sub p =
    default.pat sub p;
    match p.ppat_desc with
    | Ppat_construct (lid, _) | Ppat_open (lid, _) -> l lid
    | _ -> ()

  let class_expr sub cl =
    default.class_expr sub cl;
    match cl.pcl_desc with
    | Pcl_constr (lid, _) -> l lid
    | _ -> ()

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let open_description sub od =
  l od.popen_expr; default.open_description sub od

let iterator =
  { default with
    typ; module_expr; module_type; with_constraint; class_expr; class_type; expr; pat; type_extension;
    open_description
  }


end


let analyze (it:Ast_iterator.iterator) x =
  let tool_name = "longident-statistic" in
  try match Filename.extension x with
  | ".mli" -> it.signature it (Pparse.parse_interface ~tool_name x)
  | ".ml" -> it.structure it (Pparse.parse_implementation ~tool_name x)
  | _ -> ()
  with Syntaxerr.Error _ -> ()

let average h =
  let m0, m1 = Hashtbl.fold (fun k x (m0,m1) -> m0 + x, m1 + k * x) h (0,0) in
  m0, float m1 /. float m0

let () =
  let h = Hashtbl.create 0 in
  let module It = Iterator(struct let h = h end) in
  let it = It.iterator in
  Arg.parse [] (analyze it) "longident <files>";
  let count, average = average h in
  Format.printf "%d Longident.t counted: average length %.4g@."
    count average
