
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VERIFICATION
    | TYPE
    | TIMES
    | THEN
    | TABLE
    | SYMBOL of (
# 7 "src/parser.mly"
      (string)
# 16 "src/parser.ml"
  )
    | STRING of (
# 7 "src/parser.mly"
      (string)
# 21 "src/parser.ml"
  )
    | SEMICOLON
    | RULE
    | RPAREN
    | REAL
    | RBRACKET
    | RANGE
    | PLUS
    | PENALITY
    | OUTPUT
    | OR
    | ONE
    | NOTIN
    | NEQ
    | MINUS
    | LTE
    | LT
    | LPAREN
    | LBRACKET
    | INTEGER
    | INT of (
# 6 "src/parser.mly"
      (int)
# 45 "src/parser.ml"
  )
    | INPUT
    | INFORMATIVE
    | INCOME
    | IN
    | IF
    | GTE
    | GT
    | GIVEN_BACK
    | FOR
    | FLOAT of (
# 5 "src/parser.mly"
      (string)
# 59 "src/parser.ml"
  )
    | FAMILY
    | ERROR
    | EQUALS
    | EOF
    | ENDIF
    | ELSE
    | DIV
    | DISCORDANCE
    | DATE_YEAR
    | DATE_MONTH
    | DATE_DAY_MONTH_YEAR
    | CONTEXT
    | CONST
    | COMPUTED
    | COMMA
    | COLON
    | CHAINING
    | BOOLEAN
    | BASE
    | APPLICATION
    | ANOMALY
    | AND
    | ALIAS
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState229
  | MenhirState221
  | MenhirState209
  | MenhirState205
  | MenhirState203
  | MenhirState200
  | MenhirState198
  | MenhirState197
  | MenhirState190
  | MenhirState187
  | MenhirState185
  | MenhirState166
  | MenhirState163
  | MenhirState158
  | MenhirState151
  | MenhirState139
  | MenhirState128
  | MenhirState112
  | MenhirState111
  | MenhirState108
  | MenhirState104
  | MenhirState91
  | MenhirState89
  | MenhirState85
  | MenhirState82
  | MenhirState78
  | MenhirState75
  | MenhirState72
  | MenhirState64
  | MenhirState59
  | MenhirState48
  | MenhirState43
  | MenhirState41
  | MenhirState40
  | MenhirState38
  | MenhirState37
  | MenhirState34
  | MenhirState26
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState10
  | MenhirState8
  | MenhirState6
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "src/parser.mly"
  


# 154 "src/parser.ml"

let rec _menhir_goto_loop_variables : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FLOAT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | FOR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | IF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | INT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LPAREN ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | MINUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | SYMBOL _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SYMBOL _v ->
                _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_formula_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_7 : (unit)) = _v in
        let ((((_menhir_stack, _menhir_s), _, (_2 : (unit list))), _, (_4 : (unit))), (_6 : (unit option))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (unit) = 
# 111 "src/parser.mly"
               ( () )
# 233 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 45 "src/parser.mly"
       ( () )
# 241 "src/parser.ml"
         in
        _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (unit)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit) = 
# 115 "src/parser.mly"
                                      ( () )
# 253 "src/parser.ml"
         in
        _menhir_goto_formula_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_loop_variable2_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState203 | MenhirState18 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit list)) = _v in
        let _v : (unit) = 
# 156 "src/parser.mly"
                                                     ( () )
# 269 "src/parser.ml"
         in
        _menhir_goto_loop_variables _menhir_env _menhir_stack _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) = 
# 231 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 281 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_loop_variable2_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_AND_loop_variable1_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState203 | MenhirState18 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit list)) = _v in
        let _v : (unit) = 
# 155 "src/parser.mly"
                                               ( () )
# 297 "src/parser.ml"
         in
        _menhir_goto_loop_variables _menhir_env _menhir_stack _menhir_s _v
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) = 
# 231 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 309 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_loop_variable1_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_formula_kind : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState209
        | SYMBOL _v ->
            _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
        | APPLICATION | CHAINING | EOF | OUTPUT | RULE | VERIFICATION ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 114 "src/parser.mly"
                         ( () )
# 338 "src/parser.ml"
             in
            _menhir_goto_formula_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_function_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 236 "src/parser.mly"
                     ( () )
# 362 "src/parser.ml"
         in
        _menhir_goto_function_call_args _menhir_env _menhir_stack _menhir_s _v
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (unit)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit) = 
# 240 "src/parser.mly"
                                      ( () )
# 374 "src/parser.ml"
         in
        _menhir_goto_function_arguments _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_else_branch_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDIF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (_2 : (unit))), _, (_4 : (unit))), (_5 : (unit option))) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _v : (unit) = 
# 220 "src/parser.mly"
                                                   ( () )
# 398 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 210 "src/parser.mly"
                   ( () )
# 406 "src/parser.ml"
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_function_call_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 430 "src/parser.ml"
        ))), _), _, (_3 : (unit))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (unit) = 
# 232 "src/parser.mly"
                                          ( () )
# 437 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 211 "src/parser.mly"
                ( () )
# 445 "src/parser.ml"
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_enumeration_item_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState43 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit list)) = _v in
        let _v : (unit) = 
# 165 "src/parser.mly"
                                                   ( () )
# 465 "src/parser.ml"
         in
        (match _menhir_s with
        | MenhirState26 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (unit)) = _v in
            let ((_menhir_stack, _menhir_s), (_2 : (
# 7 "src/parser.mly"
      (string)
# 475 "src/parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 159 "src/parser.mly"
                            ( () )
# 482 "src/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AND ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ONE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
                let _v : (unit list) = 
# 229 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 506 "src/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_AND_loop_variable1_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState43 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (unit)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 522 "src/parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 162 "src/parser.mly"
                            ( () )
# 528 "src/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SYMBOL _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
                let _v : (unit list) = 
# 229 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 552 "src/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_SEMICOLON_loop_variable2_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit list) = 
# 231 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 572 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_enumeration_item_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit) = 
# 178 "src/parser.mly"
                                ( () )
# 590 "src/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit) = 
# 177 "src/parser.mly"
                                 ( () )
# 601 "src/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _2 = () in
        let _v : (unit) = 
# 217 "src/parser.mly"
                                  ( () )
# 612 "src/parser.ml"
         in
        (match _menhir_s with
        | MenhirState41 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (unit)) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 208 "src/parser.mly"
                      ( () )
# 624 "src/parser.ml"
             in
            _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
        | MenhirState18 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 235 "src/parser.mly"
                  ( () )
# 634 "src/parser.ml"
             in
            _menhir_goto_function_call_args _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FLOAT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | FOR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | INT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | LPAREN ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | MINUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | SYMBOL _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FLOAT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | FOR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | IF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | LPAREN ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | MINUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | SYMBOL _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | ENDIF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 706 "src/parser.ml"
             in
            _menhir_goto_option_else_branch_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (_2 : (unit))) = _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 223 "src/parser.mly"
                  ( () )
# 723 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (unit)) = _v in
        let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 731 "src/parser.ml"
         in
        _menhir_goto_option_else_branch_ _menhir_env _menhir_stack _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (unit))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 244 "src/parser.mly"
                           ( () )
# 749 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 214 "src/parser.mly"
        ( () )
# 757 "src/parser.ml"
             in
            _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FLOAT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | FOR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | INT _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LPAREN ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | MINUS ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | SYMBOL _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _v : (unit) = 
# 239 "src/parser.mly"
             ( () )
# 800 "src/parser.ml"
             in
            _menhir_goto_function_arguments _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ERROR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SYMBOL _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 848 "src/parser.ml"
        ))), _, (_2 : (unit option))), _, (_4 : (unit))) = _menhir_stack in
        let _3 = () in
        let _v : (unit) = 
# 125 "src/parser.mly"
                                     ( () )
# 854 "src/parser.ml"
         in
        (match _menhir_s with
        | MenhirState205 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (unit)) = _v in
            let ((_menhir_stack, _menhir_s), _, (_2 : (unit))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 122 "src/parser.mly"
                                   ( () )
# 867 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 119 "src/parser.mly"
              ( () )
# 875 "src/parser.ml"
             in
            _menhir_goto_formula_kind _menhir_env _menhir_stack _menhir_s _v
        | MenhirState197 | MenhirState209 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 118 "src/parser.mly"
          ( () )
# 885 "src/parser.ml"
             in
            _menhir_goto_formula_kind _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_enumeration_item : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | SYMBOL _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | AND | COLON | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
        let _v : (unit list) = 
# 229 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 919 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_enumeration_item_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_comparison : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (unit)) = _v in
    let _v : (unit) = 
# 181 "src/parser.mly"
             ( () )
# 937 "src/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AND ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | FOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | IF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | INT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LPAREN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SYMBOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | OR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | FOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | IF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LPAREN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SYMBOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | COMMA | DIV | ELSE | ENDIF | EQUALS | GT | GTE | LT | LTE | MINUS | NEQ | PLUS | RPAREN | SEMICOLON | THEN | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 176 "src/parser.mly"
                  ( () )
# 996 "src/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_comparison_op : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 1035 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RANGE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (
# 7 "src/parser.mly"
      (string)
# 1054 "src/parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 1059 "src/parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 173 "src/parser.mly"
                      ( () )
# 1065 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 168 "src/parser.mly"
           ( () )
# 1073 "src/parser.ml"
             in
            _menhir_goto_enumeration_item _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | AND | COLON | COMMA | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 1087 "src/parser.ml"
        ))) = _menhir_stack in
        let _v : (unit) = 
# 170 "src/parser.mly"
         ( () )
# 1092 "src/parser.ml"
         in
        _menhir_goto_enumeration_item _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
      (int)
# 1105 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 6 "src/parser.mly"
      (int)
# 1113 "src/parser.ml"
    )) = _v in
    let _v : (unit) = 
# 169 "src/parser.mly"
      ( () )
# 1118 "src/parser.ml"
     in
    _menhir_goto_enumeration_item _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_sum_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState200 | MenhirState16 | MenhirState104 | MenhirState18 | MenhirState38 | MenhirState91 | MenhirState89 | MenhirState40 | MenhirState78 | MenhirState75 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 256 "src/parser.mly"
         ( () )
# 1139 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | GT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 254 "src/parser.mly"
     ( () )
# 1150 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | GTE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 251 "src/parser.mly"
       ( () )
# 1161 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | LT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 253 "src/parser.mly"
     ( () )
# 1172 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | LTE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 252 "src/parser.mly"
       ( () )
# 1183 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | NEQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 255 "src/parser.mly"
       ( () )
# 1194 "src/parser.ml"
             in
            _menhir_goto_comparison_op _menhir_env _menhir_stack _v
        | AND | COMMA | DIV | ELSE | ENDIF | MINUS | OR | PLUS | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _v : (unit) = 
# 188 "src/parser.mly"
                 ( () )
# 1203 "src/parser.ml"
             in
            _menhir_goto_comparison _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (unit))), (_2 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 189 "src/parser.mly"
                                              ( () )
# 1219 "src/parser.ml"
         in
        _menhir_goto_comparison _menhir_env _menhir_stack _menhir_s _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (unit))), (_2 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 193 "src/parser.mly"
                                                 ( () )
# 1229 "src/parser.ml"
         in
        _menhir_goto_sum_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_sum_operator : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_goto_option_brackets_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FLOAT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
        | FOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | IF ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | INT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
        | LPAREN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | MINUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | SYMBOL _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "src/parser.mly"
      (string)
# 1301 "src/parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | ONE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState18 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACKET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LPAREN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | AND | COMMA | DIV | EQUALS | GT | GTE | LT | LTE | MINUS | NEQ | OR | PLUS | RPAREN | TIMES ->
            _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (_2 : (
# 7 "src/parser.mly"
      (string)
# 1363 "src/parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 152 "src/parser.mly"
                           ( () )
# 1370 "src/parser.ml"
             in
            (match _menhir_s with
            | MenhirState17 | MenhirState19 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_2 : (unit)) = _v in
                let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 1380 "src/parser.ml"
                ))) = _menhir_stack in
                let _v : (unit) = 
# 212 "src/parser.mly"
                  ( () )
# 1385 "src/parser.ml"
                 in
                _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
            | MenhirState198 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (unit)) = _v in
                let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 1395 "src/parser.ml"
                 in
                _menhir_goto_option_brackets_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 1416 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | SYMBOL _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | SYMBOL _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_value_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((((_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 1497 "src/parser.ml"
            ))), (_4 : (unit))), _, (_5 : (unit list))), (_6 : (unit option))), (_7 : (unit))), (_9 : (
# 7 "src/parser.mly"
      (string)
# 1501 "src/parser.ml"
            ))), _, (_10 : (unit option))) = _menhir_stack in
            let _11 = () in
            let _8 = () in
            let _3 = () in
            let _2 = () in
            let _v : (unit) = 
# 84 "src/parser.mly"
            ( () )
# 1510 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 65 "src/parser.mly"
                  ( () )
# 1518 "src/parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 1539 "src/parser.ml"
            ))), (_3 : (unit option))), _, (_5 : (unit list))), (_7 : (
# 7 "src/parser.mly"
      (string)
# 1543 "src/parser.ml"
            ))), _, (_8 : (unit option))) = _menhir_stack in
            let _9 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _v : (unit) = 
# 72 "src/parser.mly"
                                     ( () )
# 1552 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 63 "src/parser.mly"
                     ( () )
# 1560 "src/parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_value_type_prim : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (unit)) = _v in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : (unit) = 
# 99 "src/parser.mly"
                       ( () )
# 1582 "src/parser.ml"
     in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (unit)) = _v in
    let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 1590 "src/parser.ml"
     in
    _menhir_goto_option_value_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_product_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState200 | MenhirState16 | MenhirState104 | MenhirState18 | MenhirState38 | MenhirState91 | MenhirState89 | MenhirState40 | MenhirState78 | MenhirState75 | MenhirState48 | MenhirState64 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 197 "src/parser.mly"
        ( () )
# 1611 "src/parser.ml"
             in
            _menhir_goto_sum_operator _menhir_env _menhir_stack _v
        | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 196 "src/parser.mly"
       ( () )
# 1622 "src/parser.ml"
             in
            _menhir_goto_sum_operator _menhir_env _menhir_stack _v
        | AND | COMMA | DIV | ELSE | ENDIF | EQUALS | GT | GTE | LT | LTE | NEQ | OR | RPAREN | SEMICOLON | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _v : (unit) = 
# 192 "src/parser.mly"
                     ( () )
# 1631 "src/parser.ml"
             in
            _menhir_goto_sum_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (unit))), (_2 : (unit))), _, (_3 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 201 "src/parser.mly"
                                             ( () )
# 1647 "src/parser.ml"
         in
        _menhir_goto_product_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_product_operator : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run198 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 1682 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState198 in
        let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 1697 "src/parser.ml"
         in
        _menhir_goto_option_brackets_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198

and _menhir_run203 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ONE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | SYMBOL _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 1723 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | AND | COMMA | DIV | ELSE | ENDIF | EQUALS | GT | GTE | LT | LTE | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ONE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SYMBOL _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
      (string)
# 1834 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 5 "src/parser.mly"
      (string)
# 1842 "src/parser.ml"
    )) = _v in
    let _v : (unit) = 
# 226 "src/parser.mly"
        ( () )
# 1847 "src/parser.ml"
     in
    _menhir_goto_factor_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 1856 "src/parser.ml"
     in
    _menhir_goto_option_value_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOLEAN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 102 "src/parser.mly"
          ( () )
# 1874 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | DATE_DAY_MONTH_YEAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 104 "src/parser.mly"
                      ( () )
# 1885 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | DATE_MONTH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 105 "src/parser.mly"
             ( () )
# 1896 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | DATE_YEAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 103 "src/parser.mly"
            ( () )
# 1907 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | INTEGER ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 106 "src/parser.mly"
          ( () )
# 1918 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | REAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 107 "src/parser.mly"
       ( () )
# 1929 "src/parser.ml"
         in
        _menhir_goto_value_type_prim _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_GIVEN_BACK_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALIAS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (
# 7 "src/parser.mly"
      (string)
# 1958 "src/parser.ml"
            )) = _v in
            let _1 = () in
            let _v : (unit) = 
# 87 "src/parser.mly"
               ( () )
# 1964 "src/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | STRING _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | TYPE ->
                        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | SEMICOLON ->
                        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_factor : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState200 | MenhirState16 | MenhirState104 | MenhirState18 | MenhirState38 | MenhirState91 | MenhirState89 | MenhirState40 | MenhirState78 | MenhirState75 | MenhirState48 | MenhirState59 | MenhirState72 | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 205 "src/parser.mly"
      ( () )
# 2031 "src/parser.ml"
             in
            _menhir_goto_product_operator _menhir_env _menhir_stack _v
        | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (unit) = 
# 204 "src/parser.mly"
        ( () )
# 2042 "src/parser.ml"
             in
            _menhir_goto_product_operator _menhir_env _menhir_stack _v
        | AND | COMMA | ELSE | ENDIF | EQUALS | GT | GTE | LT | LTE | MINUS | NEQ | OR | PLUS | RPAREN | SEMICOLON | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
            let _v : (unit) = 
# 200 "src/parser.mly"
         ( () )
# 2051 "src/parser.ml"
             in
            _menhir_goto_product_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (unit))) = _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 209 "src/parser.mly"
               ( () )
# 2068 "src/parser.ml"
         in
        _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_chaining_reference_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | SYMBOL _v ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197

and _menhir_goto_nonempty_list_verification_condition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (unit list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
        let _v : (unit list) = 
# 211 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2101 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_verification_condition_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_8 : (unit list)) = _v in
        let (((((_menhir_stack, _menhir_s), (s : (
# 7 "src/parser.mly"
      (string)
# 2111 "src/parser.ml"
        ))), _, (_3 : (string list))), _, (_5 : (unit))), _, (_7 : (unit))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (unit) = 
# 129 "src/parser.mly"
                                                               ( () )
# 2119 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 46 "src/parser.mly"
               ( () )
# 2127 "src/parser.ml"
         in
        _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | FOR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | IF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | MINUS ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | SYMBOL _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_goto_list_computed_variable_subtype_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TYPE ->
                    _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | SEMICOLON ->
                    _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit))), _, (xs : (unit list))) = _menhir_stack in
        let _v : (unit list) = 
# 201 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2205 "src/parser.ml"
         in
        _menhir_goto_list_computed_variable_subtype_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_computed_variable_subtype : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BASE ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | GIVEN_BACK ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | COLON ->
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState166
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_goto_list_input_variable_attribute_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GIVEN_BACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = () in
            let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 2246 "src/parser.ml"
             in
            _menhir_goto_option_GIVEN_BACK_ _menhir_env _menhir_stack _v
        | ALIAS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 2254 "src/parser.ml"
             in
            _menhir_goto_option_GIVEN_BACK_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit))), _, (xs : (unit list))) = _menhir_stack in
        let _v : (unit list) = 
# 201 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2270 "src/parser.ml"
         in
        _menhir_goto_list_input_variable_attribute_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_symbol_or_int_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (unit))), _, (xs : (unit list))) = _menhir_stack in
        let _v : (unit list) = 
# 211 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2287 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_symbol_or_int_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | APPLICATION ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState190
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_factor_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (unit)) = _v in
    let _v : (unit) = 
# 213 "src/parser.mly"
                 ( () )
# 2323 "src/parser.ml"
     in
    _menhir_goto_factor _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_SYMBOL_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 7 "src/parser.mly"
      (string)
# 2337 "src/parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) = 
# 231 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2343 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_SYMBOL_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (string list)) = _v in
        let _v : (unit) = 
# 259 "src/parser.mly"
                                         ( () )
# 2353 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (unit)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (unit) = 
# 54 "src/parser.mly"
                                       ( () )
# 2364 "src/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState6 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState190 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CHAINING ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | SYMBOL _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | SEMICOLON ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (_menhir_stack, (_3 : (
# 7 "src/parser.mly"
      (string)
# 2423 "src/parser.ml"
                                ))) = _menhir_stack in
                                let _4 = () in
                                let _2 = () in
                                let _1 = () in
                                let _v : (unit) = 
# 60 "src/parser.mly"
                                  ( () )
# 2431 "src/parser.ml"
                                 in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (x : (unit)) = _v in
                                let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 2439 "src/parser.ml"
                                 in
                                _menhir_goto_option_chaining_reference_ _menhir_env _menhir_stack _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | FOR | SYMBOL _ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 2462 "src/parser.ml"
                     in
                    _menhir_goto_option_chaining_reference_ _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState221 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), (s : (
# 7 "src/parser.mly"
      (string)
# 2489 "src/parser.ml"
                ))), _, (_3 : (unit))) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : (unit) = 
# 57 "src/parser.mly"
                                                      ( () )
# 2496 "src/parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (unit)) = _v in
                let _v : (unit) = 
# 43 "src/parser.mly"
           ( () )
# 2504 "src/parser.ml"
                 in
                _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_SYMBOL_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 7 "src/parser.mly"
      (string)
# 2528 "src/parser.ml"
        ))), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 211 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( x :: xs )
# 2533 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_SYMBOL_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | APPLICATION ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (unit))), _, (_5 : (string list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 132 "src/parser.mly"
                                                       ( () )
# 2575 "src/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState15 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState111
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | MenhirState112 | MenhirState111 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | APPLICATION | CHAINING | EOF | OUTPUT | RULE | SYMBOL _ | VERIFICATION ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
                    let _v : (unit list) = 
# 209 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 2603 "src/parser.ml"
                     in
                    _menhir_goto_nonempty_list_verification_condition_ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) = 
# 199 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [] )
# 2626 "src/parser.ml"
     in
    _menhir_goto_list_computed_variable_subtype_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run159 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) = 
# 79 "src/parser.mly"
             ( () )
# 2638 "src/parser.ml"
     in
    _menhir_goto_computed_variable_subtype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (unit) = 
# 78 "src/parser.mly"
       ( () )
# 2650 "src/parser.ml"
     in
    _menhir_goto_computed_variable_subtype _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit list) = 
# 199 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [] )
# 2659 "src/parser.ml"
     in
    _menhir_goto_list_input_variable_attribute_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 2666 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUALS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (
# 6 "src/parser.mly"
      (int)
# 2685 "src/parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 2690 "src/parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (unit) = 
# 90 "src/parser.mly"
                    ( () )
# 2696 "src/parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SYMBOL _v ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | ALIAS | GIVEN_BACK ->
                _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_error_message_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((_menhir_stack, _menhir_s, (n : (
# 7 "src/parser.mly"
      (string)
# 2738 "src/parser.ml"
        ))), (_2 : (unit))), (_4 : (
# 7 "src/parser.mly"
      (string)
# 2742 "src/parser.ml"
        ))), (_6 : (
# 7 "src/parser.mly"
      (string)
# 2746 "src/parser.ml"
        ))), (_8 : (
# 7 "src/parser.mly"
      (string)
# 2750 "src/parser.ml"
        ))), (_10 : (
# 7 "src/parser.mly"
      (string)
# 2754 "src/parser.ml"
        ))), (_12 : (unit option))) = _menhir_stack in
        let _13 = () in
        let _11 = () in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _3 = () in
        let _v : (unit) = 
# 138 "src/parser.mly"
                                         ( () )
# 2765 "src/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (unit)) = _v in
        let _v : (unit) = 
# 47 "src/parser.mly"
         ( () )
# 2773 "src/parser.ml"
         in
        _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((_menhir_stack, _menhir_s, _), _), _), _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce116 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 7 "src/parser.mly"
      (string)
# 2786 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 2792 "src/parser.ml"
    ))) = _menhir_stack in
    let _v : (unit) = 
# 247 "src/parser.mly"
         ( () )
# 2797 "src/parser.ml"
     in
    _menhir_goto_symbol_or_int _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_symbol_or_int : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState200 | MenhirState16 | MenhirState104 | MenhirState18 | MenhirState37 | MenhirState38 | MenhirState91 | MenhirState89 | MenhirState40 | MenhirState78 | MenhirState75 | MenhirState72 | MenhirState64 | MenhirState59 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _v : (unit) = 
# 227 "src/parser.mly"
                ( () )
# 2812 "src/parser.ml"
         in
        _menhir_goto_factor_literal _menhir_env _menhir_stack _menhir_s _v
    | MenhirState187 | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
        | SYMBOL _v ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (unit))) = _menhir_stack in
            let _v : (unit list) = 
# 209 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 2830 "src/parser.ml"
             in
            _menhir_goto_nonempty_list_symbol_or_int_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 2848 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 7 "src/parser.mly"
      (string)
# 2871 "src/parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 229 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 2876 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_SYMBOL_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 2889 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | COLON | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 7 "src/parser.mly"
      (string)
# 2903 "src/parser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 209 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( [ x ] )
# 2908 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_SYMBOL_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_goto_option_computed_variable_table_ : _menhir_env -> 'ttv_tail -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMPUTED ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BASE ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | GIVEN_BACK ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | COLON ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState158
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_input_variable_subtype : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | ALIAS | GIVEN_BACK ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (unit)) = _v in
    let _v : (unit) = 
# 44 "src/parser.mly"
           ( () )
# 2969 "src/parser.ml"
     in
    _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_error : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | STRING _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | COLON ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | STRING _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | SEMICOLON ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | COLON ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | STRING _v ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (_2 : (
# 7 "src/parser.mly"
      (string)
# 3041 "src/parser.ml"
                                                )) = _v in
                                                let _1 = () in
                                                let _v : (unit) = 
# 141 "src/parser.mly"
                  ( () )
# 3047 "src/parser.ml"
                                                 in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (x : (unit)) = _v in
                                                let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 3055 "src/parser.ml"
                                                 in
                                                _menhir_goto_option_error_message_ _menhir_env _menhir_stack _v
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | SEMICOLON ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 3068 "src/parser.ml"
                                             in
                                            _menhir_goto_option_error_message_ _menhir_env _menhir_stack _v
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let ((((((_menhir_stack, _menhir_s, _), _), _), _), _), _) = _menhir_stack in
                                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((((_menhir_stack, _menhir_s, _), _), _), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((((_menhir_stack, _menhir_s, _), _), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((((_menhir_stack, _menhir_s, _), _), _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run186 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 3135 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
      (int)
# 3145 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 6 "src/parser.mly"
      (int)
# 3153 "src/parser.ml"
    )) = _v in
    let _v : (unit) = 
# 248 "src/parser.mly"
      ( () )
# 3158 "src/parser.ml"
     in
    _menhir_goto_symbol_or_int _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_source_file : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "src/parser.mly"
      (unit)
# 3165 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (
# 26 "src/parser.mly"
      (unit)
# 3175 "src/parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (unit))) = _menhir_stack in
        let _v : (
# 26 "src/parser.mly"
      (unit)
# 3181 "src/parser.ml"
        ) = 
# 37 "src/parser.mly"
                               ( () )
# 3185 "src/parser.ml"
         in
        _menhir_goto_source_file _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (
# 26 "src/parser.mly"
      (unit)
# 3194 "src/parser.ml"
        )) = _v in
        Obj.magic _1
    | _ ->
        _menhir_fail ()

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_source_file_item : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APPLICATION ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | CHAINING ->
        _menhir_run220 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | EOF ->
        _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | OUTPUT ->
        _menhir_run214 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | RULE ->
        _menhir_run185 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | SYMBOL _v ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState229 _v
    | VERIFICATION ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState229
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState229

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState229 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState209 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
      (string)
# 3476 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ANOMALY ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 144 "src/parser.mly"
          ( () )
# 3491 "src/parser.ml"
         in
        _menhir_goto_type_error _menhir_env _menhir_stack _v
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONST ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUALS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FLOAT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | SEMICOLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, (_1 : (
# 7 "src/parser.mly"
      (string)
# 3522 "src/parser.ml"
                        ))), (_5 : (
# 5 "src/parser.mly"
      (string)
# 3526 "src/parser.ml"
                        ))) = _menhir_stack in
                        let _6 = () in
                        let _4 = () in
                        let _3 = () in
                        let _2 = () in
                        let _v : (unit) = 
# 68 "src/parser.mly"
                                            ( () )
# 3535 "src/parser.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_1 : (unit)) = _v in
                        let _v : (unit) = 
# 64 "src/parser.mly"
                  ( () )
# 3543 "src/parser.ml"
                         in
                        _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | INPUT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CONTEXT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (unit) = 
# 93 "src/parser.mly"
          ( () )
# 3577 "src/parser.ml"
                 in
                _menhir_goto_input_variable_subtype _menhir_env _menhir_stack _v
            | FAMILY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (unit) = 
# 94 "src/parser.mly"
         ( () )
# 3588 "src/parser.ml"
                 in
                _menhir_goto_input_variable_subtype _menhir_env _menhir_stack _v
            | INCOME ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (unit) = 
# 96 "src/parser.mly"
         ( () )
# 3599 "src/parser.ml"
                 in
                _menhir_goto_input_variable_subtype _menhir_env _menhir_stack _v
            | PENALITY ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = () in
                let _v : (unit) = 
# 95 "src/parser.mly"
           ( () )
# 3610 "src/parser.ml"
                 in
                _menhir_goto_input_variable_subtype _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TABLE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | INT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RBRACKET ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, (_3 : (
# 6 "src/parser.mly"
      (int)
# 3642 "src/parser.ml"
                        ))) = _menhir_stack in
                        let _4 = () in
                        let _2 = () in
                        let _1 = () in
                        let _v : (unit) = 
# 75 "src/parser.mly"
                              ( () )
# 3650 "src/parser.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (x : (unit)) = _v in
                        let _v : (unit option) = 
# 116 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( Some x )
# 3658 "src/parser.ml"
                         in
                        _menhir_goto_option_computed_variable_table_ _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | COMPUTED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (unit option) = 
# 114 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
    ( None )
# 3681 "src/parser.ml"
             in
            _menhir_goto_option_computed_variable_table_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | DISCORDANCE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 145 "src/parser.mly"
              ( () )
# 3698 "src/parser.ml"
         in
        _menhir_goto_type_error _menhir_env _menhir_stack _v
    | INFORMATIVE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (unit) = 
# 146 "src/parser.mly"
              ( () )
# 3709 "src/parser.ml"
         in
        _menhir_goto_type_error _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run185 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | SYMBOL _v ->
        _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185

and _menhir_run214 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SYMBOL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), (s : (
# 7 "src/parser.mly"
      (string)
# 3763 "src/parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (unit) = 
# 149 "src/parser.mly"
                                            ( () )
# 3772 "src/parser.ml"
                     in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_1 : (unit)) = _v in
                    let _v : (unit) = 
# 48 "src/parser.mly"
         ( () )
# 3780 "src/parser.ml"
                     in
                    _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run219 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 26 "src/parser.mly"
      (unit)
# 3815 "src/parser.ml"
    ) = 
# 38 "src/parser.mly"
      ( () )
# 3819 "src/parser.ml"
     in
    _menhir_goto_source_file _menhir_env _menhir_stack _menhir_s _v

and _menhir_run220 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | APPLICATION ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState221
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState221)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run224 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SYMBOL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (s : (
# 7 "src/parser.mly"
      (string)
# 3867 "src/parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (unit) = 
# 51 "src/parser.mly"
                                   ( () )
# 3874 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (unit)) = _v in
            let _v : (unit) = 
# 42 "src/parser.mly"
              ( () )
# 3882 "src/parser.ml"
             in
            _menhir_goto_source_file_item _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and source_file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 26 "src/parser.mly"
      (unit)
# 3913 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | APPLICATION ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CHAINING ->
        _menhir_run220 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OUTPUT ->
        _menhir_run214 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RULE ->
        _menhir_run185 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SYMBOL _v ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | VERIFICATION ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 233 "/home/demerigo/.opam/4.04.2/lib/menhir/standard.mly"
  

# 3949 "src/parser.ml"
