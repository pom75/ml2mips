open Types;;
open Typeur;;
open Env_typeur;;
open Env_trans;;
open Langinter;;
let compiler_name = ref "ml2mips";;
let object_suffix = ref ".s";;
let count = ref 0;;
let tabVar = ref [];;
let countVar = ref 0;;
let dollar2 = ref false;;
let dollar3 = ref false;;

let cpt_pile = ref 0;;
let max_pile = ref 0;;
let ifcpt = ref 0;;

let etiq_f = ref " " ;;
let tabVarF = ref [];;
let isCreat = ref false;;
let isApply = ref false;;

(* des valeurs pour certains symboles de env_trans *)
pair_symbol:=",";;
cons_symbol:="::";;
ref_symbol:="ref";;

(* l'environnement initial du traducteur en liaison avec la Runtime *)
let build (s,equiv)  = 
  let t = 
    try List.assoc s !initial_typing_env  
    with Not_found -> 
      failwith ("building initial_trans_env : for symbol : "^s)
  in (s,(equiv,type_instance t));;

initial_special_env := 
  List.map build [
    "hd","MLruntime.MLhd";
    "tl","MLruntime.MLtl";
    "fst","MLruntime.MLfst";
    "snd","MLruntime.MLsnd"
  ];;


initial_trans_env:= 


  (*NEEDS TO BE REPLACED*)
  
    let alpha = max_unknown () in
    [",",("MLruntime.MLpair", Fun_type (Pair_type (alpha,alpha),
    Pair_type (alpha,alpha)))]@
    ["::",("MLruntime.MLlist", Fun_type (Pair_type (alpha,alpha),
    List_type (alpha)))]@

    (
    List.map build 
    ["true" ,"true";
    "false","false";
    "+","add";
    "-","sub";
    "*","mul";
    "/","div";
    "=","equal";
    "<","smaller";
    "<=","smallerequal";
    ">","bigger";
    ">=","biggerequal";
    ]
    )
    ;;

let output_channel = ref stdout;;
let change_output_channel oc = output_channel := oc;;
let shift_string = String.make 256 ' ';;
let out s = output_string !output_channel s;;
let out_start s nb = out ("\n"^(String.sub shift_string 0 (2*nb))^s);;
let out_end s nb = out ("\n"^(String.sub shift_string 0 nb)^"}\n");;
let out_line s = out (s^"\n");;

let out_before (fr,sd,nb) = 
  if sd<>"" then out_start (sd^"=") nb
  else if fr then out_start ("return ") nb;;

let out_after  (fr,sd,nb) = 
  if sd<>"" then 
    begin
      out ";";
      if fr then out (("return "^sd^";"))
    end
  else if fr then out ";";;

(* des fonctions utilitaires pour commenter un peu la production *)
let header_main  s = 
  List.iter out 
    ["\n";
     " # "^ s ^ ".s" ^ " engendre par ml2mips \n";
     "\n"]
;;

let footer_main  s = 
  List.iter out
    ["# fin du fichier " ^ s ^ ".s\n"]
;;
let header_one  s = 
  List.iter out
    [];;

let footer_one  s = ();;

(*need to be modified more maybe header_two*)

let header_two  s = 
  List.iter out
    [ "\n";
      "\n";
      "\n";
      ".data \n"
    ]
;;

let footer_two  s = ();;




let footer_three  s = 
  List.iter out
    [ "\n"]
;;




(* on recuoere le  type pour une declaration precise *)
let string_of_const_type ct = match ct with   
    INTTYPE    -> ".word "
  | FLOATTYPE  -> ".float "
  | STRINGTYPE -> ".asciiz "
  | BOOLTYPE   -> "BOOOOL "
  | UNITTYPE   -> ".space 4" (* on garde 4 octet pour stocker*)


let rec string_of_type typ = match typ with 
    CONSTTYPE t -> string_of_const_type t
  | ALPHA    ->  "MLvalue " 
  | PAIRTYPE -> ".word "
  | LISTTYPE -> ".word "
  | FUNTYPE  -> "MLfun "
  | REFTYPE  -> "MLref "
;;


let number_of_var instr = match  instr with
        VAR(v, _) -> v
        | _ -> ""
 ;;



let get_param_type lv = 
  List.map (function (VAR(name,typ)) -> typ 
              | _ -> failwith "get_param_type" ) lv;;


let prod_const c =
    match c with
    INT i ->  
               if(!dollar2) then begin
                            if(!dollar3) then begin
                    
                                out ("ori $26, $0, "^(string_of_int i)^"\n");
                                dollar3 := false;  
                            end 
                            else 
                              begin
                               out ("ori $27, $0, "^(string_of_int i)^"\n");
                                dollar3 := true; 
                              end      
                          end
                        else
                            begin
                              out ("ori $26, $0, "^(string_of_int i)^"\n");
                              dollar2 := true;   
                         end
  | FLOAT f -> 
        if(!dollar2) then begin
                            if(!dollar3) then begin
                                out("ici \n");
                                out ("ori $26, $0, "^(string_of_float f)^"\n");
                                dollar3 := false;  
                            end 
                            else 
                              begin
                               out ("ori $27, $0, "^(string_of_float f)^"\n");
                                dollar3 := true; 
                              end      
                          end
                        else
                            begin
                              out ("ori $26, $0, "^(string_of_float f)^"\n");
                              dollar2 := true;   
                         end
  | BOOL b  -> out "ok"
  | STRING s -> out ("\""^s^"\"")
  | EMPTYLIST -> out ("")
  | UNIT ->      out ("") 
;;



let rec prod_local_var (fr,sd,nb) (v,t) = 
  out_start ("MLvalue "(*(string_of_type t)*)^v^";") nb;;


let convert_var var = 
    let reg = Str.regexp "T_+\\([0-9]+\\)" in
    if  (Str.string_match reg var 0) then 
      let i = (Str.global_replace 
        reg
        ("\\1")
        var)
      in
        let j = int_of_string i in
        ("$t"^(string_of_int( j mod 8)))
    else
      var
;;


let fun_header fn cn  = 
  List.iter out 
    []
;;














let save_pil ar =
    out_line "## SAVE PILE";
    max_pile := ar + 2 + 1;
    cpt_pile := 0;
    out_line ("addiu $29 $29 -"^string_of_int (!max_pile*4));
    out_line ("sw $31, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;
    if ar >= 4 then (
        for i = 0 to ar-1 do
         out_line (" sw $"^string_of_int(i+8)^ ", "^ string_of_int(!cpt_pile*4)^"($29)");
            cpt_pile := !cpt_pile + 1;        
        done;
        for i = 0 to ar-1 do
         out_line (" lw $"^string_of_int(i+8)^ ", -"^ string_of_int((i*4 + !max_pile *4)) ^"($29)");
        done;
        
    )else(
        for i = 0 to ar-1 do
         out_line ("sw $"^string_of_int(i+8)^", "^ string_of_int(!cpt_pile*4)
          ^"($29)");
         out_line ("move $"^string_of_int(i+8)^", $"^string_of_int(i+4));
         cpt_pile := !cpt_pile + 1;
        done;
    );
    out_line ("sw $26, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;
    out_line ("sw $27, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;

;;



let restor_pil ar =
    out_line "## RESTORATION PILE";

    max_pile := ar + 2 + 1;
    cpt_pile := 0;
    
    
    
    out_line ("lw $31, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;
    
    
    if ar >= 4 then (
        for i = 0 to ar-1 do
         out_line ("lw $"^string_of_int(i+8)^ ", "^ string_of_int(!cpt_pile*4)^"($29)");
            cpt_pile := !cpt_pile + 1;        
        done;       
    )else(
      for i = 0 to ar-1 do
         out_line ("lw $"^string_of_int(i+4)^", "^ string_of_int(!cpt_pile*4) ^"($29)");
         cpt_pile := !cpt_pile + 1; 
        done;
    );

    out_line ("lw $26, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;
    out_line ("lw $27, "^ string_of_int(!cpt_pile*4)^"($29)");
    cpt_pile := !cpt_pile + 1;
    
    
    out_line ("addiu $29 $29 "^string_of_int (!max_pile*4));

;;





let prod_const_global c =
    match c with
    INT i ->  out ((string_of_int i));
              count := !count + 1
  | FLOAT f -> out ((string_of_float f) );
              count := !count + 1

  | BOOL b  -> out "ok"
  | STRING s -> out ("\""^s^"\"")
  | EMPTYLIST -> out ("")
  | UNIT ->      out ("") 
;;





let rec prod_glob_var (fr,sd,nb) instr  =
match instr with 
    VAR (v, FUNTYPE)
    -> 
        ();
    | VAR (v,t)
    -> 
        if not(List.exists ((=)v) !tabVar) then(
          tabVar := v::!tabVar;  
          out  ("\n"^v^ ": "); 
          out (string_of_type t) ;
        );

    | CONST c -> 
        prod_const_global c;
  
    | AFFECT (v,i) -> 
        prod_glob_var (false,v,nb) i;
    | BLOCK(l,i) -> 
      List.iter (fun (v,t,i) -> prod_glob_var (false,v,nb+2) i) l;
    | _ -> ();
;;

(* On iter sur chaque noeud principal *)
let prod_GV  ast_li = 
  List.iter (prod_glob_var  (false,"",0) ) ast_li;
  out "\n \n"
;;





let count_Var ast_li =
  countVar := List.length !tabVar
;;

let prod_save_args c nb =
    match c with
    INT i ->  out_line ("li $"^(string_of_int (4+nb))^", "^(string_of_int i));
  | FLOAT f -> out_line ("li $"^(string_of_int (4+nb))^", "^(string_of_float f));
  | _ -> ()
;;

let prod_fun_called nom tabV =


 out_line "## Transmission des argument et sauvgarde";
 if List.length tabV >= 4 then(
 out_line ("addiu $29 $29 -"^string_of_int (((List.length tabV )+1 )*4));
 out_line ("sw $"^string_of_int(8)^", "^ string_of_int(0)^"($29)");
        for j = 0 to (List.length tabV)-1 do
        if String.length ( snd (List.nth tabV j)) != 0 then(
                out_line ("lw $8, "^( snd (List.nth tabV j)));
                out_line ("sw $8, "^ string_of_int((j+1)*4)^"($29)");
                )else(
             match (fst (List.nth tabV j)) with
                INT i ->  
                out_line ("li $8, "^(string_of_int i));
                out_line ("sw $8, "^ string_of_int((j+1)*4)^"($29)");
                | FLOAT f -> 
                out_line ("li $8, "^(string_of_float f));
                out_line ("sw $8, "^ string_of_int((j+1)*4)^"($29)");
                | _ -> ()
            );
    done;
 )
 else(
    out_line ("addiu $29 $29 -"^string_of_int ((List.length tabV  )*4));
    for i = 0 to (List.length tabV)-1 do
            out_line ("sw $"^string_of_int(i+4)^", "^ string_of_int(i*4)^"($29)");
            if String.length ( snd (List.nth tabV i)) != 0 then(
                out_line ("lw $"^(string_of_int (4+i))^", "^( snd (List.nth tabV i)));
                )else
                    prod_save_args (fst (List.nth tabV i)) i;
            
    done;

  ) ;
  
  out_line "## Appelle a la fonction";
  out_line ("jal "^nom);
  
  out_line "## Recupereation des anciens registres et dépile";
   if List.length tabV >= 4 then(
        out_line ("lw $"^string_of_int(8)^", "^ string_of_int(0)^"($29)");
        out_line ("addiu $29 $29 "^string_of_int (((List.length tabV )+1 )*4))
  )  
 else(
    for i = 0 to (List.length tabV)-1 do
         out_line ("lw $"^string_of_int(i+4)^", "^ string_of_int(i*4)^"($29)");
    done;
    
    out_line ("addiu $29 $29 "^string_of_int ((List.length tabV )*4))

  ) ;
  

;;
let rec prod_call_func (fr,sd,nb) instr  =
match instr with 
    CONST c ->
      if nb >1 || ( String.contains sd 'z') then(
        tabVarF := (c,"")::!tabVarF;
        )
  | VAR (v,FUNTYPE)       
    ->  
          etiq_f := (String.sub v ((String.index v '.')+1) ((String.length v)-5) );
          isCreat := true;
  | VAR (v,t)       
    ->  
        if nb > 1 || ( String.contains sd 'z') then(
          tabVarF := (UNIT,v) ::!tabVarF;
          )
  | AFFECT (v,i) -> 
        prod_call_func (false,sd,nb+1) i;
  | RETURN i -> ();
  | BLOCK(l,i) ->
      List.iter (fun (v,t,i) -> prod_call_func (false,sd,nb+1) i) l;
      prod_call_func(fr,sd,nb+1) i;
  | PRIM ((name,typ),instrl) ->(
      (* HACKKKKKKK *)
      if !isCreat then(
        tabVarF := List.rev !tabVarF;
        prod_fun_called !etiq_f !tabVarF;
        tabVarF := [];
        isCreat := false
        
        );
      )
      
  | _ -> ();

;;

let rec prod_call_func2 (fr,sd,nb) instr  =
match instr with 
    CONST c ->
      if nb >1 then(
        tabVarF := (c,"")::!tabVarF;
        )
  | VAR (v,FUNTYPE)       
    ->  
          etiq_f := (String.sub v ((String.index v '.')+1) (((String.length v) - (String.index v '.'))-1) );
  
          isCreat := true;
          tabVarF := [];
  | VAR (v,t)       
    ->  
        if nb > 1  then(
          tabVarF := (UNIT,v) ::!tabVarF;
          
          )
  | AFFECT (v,i) -> 
        prod_call_func2 (false,"",nb+1) i;
  | RETURN i -> ();
  | BLOCK(l,i) ->
      List.iter (fun (v,t,i) -> prod_call_func2 (false,sd,nb+1) i) l;
      prod_call_func2 (fr,sd,nb+1) i;
  | PRIM ((name,typ),instrl) ->(
      if !isCreat   then(
        tabVarF := List.rev !tabVarF;
        prod_fun_called !etiq_f !tabVarF;
        tabVarF := [];
        isCreat := false
        
        );
      )
      
  | _ -> ();

;;

let prod_main  s  ast_li=
  List.iter out
    [  "\n";
        ".text";
        "\n";
       "main:\n";
    ];
    
    List.iter (fun x -> prod_call_func2 (false,"",0) x) ast_li;
  
;;



let rec is_call_fun (fr,sd,nb) instr = 
match instr with 
    CONST c -> ()
  | VAR (v,FUNTYPE)  ->  ()
  | VAR (v,t)  ->  ()
  | AFFECT (v,i) -> 
        is_call_fun (false,"",nb+1) i;
  | RETURN i -> ()
  | BLOCK(l,i) ->
      List.iter (fun (v,t,i) -> is_call_fun (false,sd,nb+1) i) l;
      is_call_fun (fr,sd,nb+1) i;
  | APPLY(i1,i2) ->
      (
        isApply := true;  
     )
      
  | _ -> ();

;;





let rec prod_instr (fr,sd,nb) instr  = 
match instr with 
    CONST c -> 
      prod_const c;
  | VAR (v,t)       (*x = x + 1   *)
    ->  
    if (t = FUNTYPE ) then
      ()
    else(
    if (nb = 0) && ( sd = "") then ()
    else 
      begin 
           match t with
                      FUNTYPE ->
                        if(!dollar2) then begin
                            if(!dollar3) then begin
                                out("ori $26, $0, "^(convert_var v)^" \n");
                                dollar3 := false;  
                            end 
                            else 
                              begin
                                out("ori $27, $0, "^(convert_var v)^" \n");
                                dollar3 := true; 
                              end      
                          end
                        else
                            begin
                              out("ori $26, $0, "^(convert_var v)^" \n");
                              dollar2 := true;   
                            end
                     |   _ ->                     
                        if(!dollar2) then begin
                          if(!dollar3) then begin
                               out ("lw $26, "^(convert_var v)^"($29) \n");
                               dollar3 := false;   
                          end
                          else
                            begin
                              out ("lw $27, "^(convert_var v)^"($29) \n");
                              dollar3 := true; 
                            end 
                          end    
                        else
                            begin
                              out ("lw $26, "^(convert_var v)^"($29) \n"); 
                              dollar2 := true;
                            end   
                                               
      end
    )
  | IF(i1,i2,i3) ->  
       prod_instr (false,"",nb) i1 ;
      out("beq $2,$0,Else"^(string_of_int !ifcpt)^"\n");
      prod_instr (fr,sd,nb+1) i2 ;
      out("j Endif"^(string_of_int !ifcpt)^"\n");
      out("Else"^(string_of_int !ifcpt)^":\n"); 
      prod_instr (fr,sd,nb+1) i3;
      out("Endif"^(string_of_int !ifcpt)^":\n");
      ifcpt := !ifcpt + 1;
  | RETURN i -> prod_instr (true,"",nb) i
  | AFFECT (v,i) -> prod_instr (false,v,nb) i
  | BLOCK(l,i) ->(       
        List.iter (fun (v,t,i) -> is_call_fun (false,v,nb+2) i) l;
       is_call_fun (fr,sd,nb+1) i;
        if !isApply then (
             List.iter (fun (v,t,i) -> prod_call_func (false,"zzz",0) i) l;
             prod_call_func(fr,sd,0) i;
        )else(
            List.iter (fun (v,t,i) -> prod_instr (false,v,nb+2) i) l;
            prod_instr (fr,sd,nb+1) i;
        );
         if !isCreat then(
        tabVarF := List.rev !tabVarF;
        prod_fun_called !etiq_f !tabVarF;
        tabVarF := [];
        isCreat := false
        );
       
       ) 
        
  | APPLY(i1,i2) ->
      (  
     )
  | PRIM ((name,typ),instrl) ->(
      out (" lw $2 "^(convert_var (number_of_var (List.nth instrl 1)))^"($29)\n");
      (match name with
        |"add" -> out(" add $2, $26, $27\n");
        |"sub" -> out(" sub $2, $26, $27\n");
        |"mul" -> out(" mul $26, $27\n");
                  out("mflo $2");
        |"div" -> out(" div $26, $27\n");
                  out("mflo $2");
        |"equal" -> out("seq $2, $26, $27\n");
        |"smaller" -> out "slt $2, $26, $27\n";
        |"smallerequal" -> out "sle $2, $26, $27 \n";
        |"bigger" -> out "sgt $2, $26, $27 \n";
        |"biggerequal" -> out "sge $2, $26, $27";
        |_ -> (););
        out (" sw $2 "^(convert_var (number_of_var (List.nth instrl 1)))^"($29)\n") (*petit bug ici why it doesnt execute*)
      );
  | FUNCTION _ -> out "function";()
;;



let prod_invoke_fun cn ar t lp instr = 
  out_line ("## INSTRUCTION \n\n");
  prod_instr (true,"",2) instr;
  
  out_line ""
;;


let prod_fun instr = match instr with 
    FUNCTION (ns,t1,ar,(lp,t2),instr) -> 
    let class_name = ns in
    out_line ("##### nom fonction : "^ns);
    out_line (class_name^" : ");
	fun_header ns class_name ;
    
    save_pil ar ;
    
    prod_invoke_fun class_name ar t1 lp instr;
    
    restor_pil ar;

    out "addi $v0 $2 0 \n";
    out "jr $ra \n";
	  
  |  _ -> ()
;;

let prod_func  ast_li = 
  List.iter prod_fun ast_li
;;

let prod_three  ast_li = 
  List.iter (prod_instr  (false,"",0) ) ast_li
;;

let prod_file filename ast_li = 
  let obj_name = filename ^ !object_suffix in 
  let oc = open_out obj_name in 
    change_output_channel oc;  
    module_name:=filename;
    try 
      header_main  filename;
      header_two  filename;
      prod_GV ast_li;


      


      count_Var ast_li;
      prod_main filename ast_li;
      out_line "syscall";
      out_line "\n\n";
      prod_func ast_li;

      footer_three  filename; 
      footer_main  filename;
      close_out oc
    with x -> close_out oc; raise x;;

