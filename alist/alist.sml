(* Usage:
 * val alist = Alist.Alist (1, "one");
 * val alist = Alist.add (2, "two") alist;
 * val value = Alist.find 1 alist;
 *
 * If you want partial application, do as follows:
 * val find1 = fn x => Alist.find 1 x;
 * val value = find1 alist;
 *)

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
