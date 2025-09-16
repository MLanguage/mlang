val handler :
  application_names:string list ->
  int ->
  bool ->
  bool ->
  int option ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  bool ->
  int ->
  bool ->
  bool ->
  bool ->
  bool ->
  Config.Dgfip_options.flags

val process_dgfip_options :
  application_names:string list ->
  string list ->
  Config.Dgfip_options.flags option
