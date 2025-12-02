val open_file_for_text_extraction : Pos.t -> int -> string list
(** [open_file_for_text_extraction pos] returns a lambda
    [nb_lines: int -> string list] that reads nb_lines of the file whose name is
    given in the position *)
