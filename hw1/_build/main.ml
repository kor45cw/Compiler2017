open Regex
open Hw1

let testcases : (Regex.t * alphabet list) list =
  [
    (Empty, []); (* false *)
    (Epsilon, []); (* true *)
    (Alpha A, [A]); (* true *)
    (Alpha A, [B]); (* false *)
    (OR (Alpha A, Alpha B), [B]); (* true *)
    (CONCAT (STAR (Alpha A), Alpha B), [B]); (* true *)
    (CONCAT (STAR (Alpha A), Alpha B), [A;B]); (* true *)
    (CONCAT (STAR (Alpha A), Alpha B), [A;A;B]); (* true *)
    (CONCAT (STAR (Alpha A), Alpha B), [A;B;B]); (* false *)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B]); (* true *)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]); (* true *)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B;B;B]); (* true *)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B]); (* true *)
    (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;B;B;B]); (* false *) 
    (CONCAT (STAR (OR (Alpha A, Alpha B)), Alpha B), [A;B;B;A]);
  ]

let match_regex : Regex.t -> alphabet list -> bool
=fun regex input -> Hw1.run_dfa (Hw1.regex2dfa regex) input

(* run testcases *)
let _ =
  List.iter (fun (regex, str) ->
    prerr_endline (string_of_bool (match_regex regex str))
  ) testcases
