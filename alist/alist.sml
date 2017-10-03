structure Alist =
  struct
    exception AlistExn
    type key = int
    type value = string
    type alist = (key * value) list

    fun Alist () = nil : alist
    fun add (k, v) ls = if exist k ls then raise AlistExn else (k, v)::ls
    and exist key nil = false
      | exist key ((k, v)::ls) = (k = key) orelse exist key ls
    fun find key nil = raise AlistExn
      | find key ((k, v)::ls) = if k = key then v else find key ls
  end
