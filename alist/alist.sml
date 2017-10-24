signature ALIST =
  sig
    exception AlistExn
    type ('a, 'b) alist
    val Alist : 'a * 'b -> ('a, 'b) alist
    val add : ''a * 'b -> (''a, 'b) alist -> (''a, 'b) alist
    val find : ''a -> (''a, 'b) alist -> 'b
  end

structure Alist :> ALIST =
  struct
    exception AlistExn
    type ('a, 'b) alist = ('a * 'b) list
    fun Alist (k, v) = [(k, v)] : ('a, 'b) alist

    fun add (k, v) ls = if exist k ls then raise AlistExn else (k, v)::ls
    and exist key nil = false
      | exist key ((k, v)::ls) = (k = key) orelse exist key ls
    fun find key nil = raise AlistExn
      | find key ((k, v)::ls) = if k = key then v else find key ls
  end
