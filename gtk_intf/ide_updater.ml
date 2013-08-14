module type Parse = sig
  val parse :
    int ->
    string ->
    (string -> unit) ->
    (string -> unit) ->
    (string -> unit) -> (string list -> unit) -> unit

  val title : string
  val missing_title : string
  val lang_mime_type : string
  val example : string
end
