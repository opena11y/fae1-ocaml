(**
   Tests relating to scripting and automation
*)

let debug = false
let msg = (Testutil.msg debug)
let runmsg = (Testutil.msg debug "Running")

(* ---------------------------------------------------------------- *)
let is_nonfocusable (tag : Html.htmlItem Html.tag) =
  not (Testutil.is_focusable tag)

let count_nonfocusable_with_onclick page =
  let onclick_list = Testutil.get_elements_with_attribute "onclick" page in
  let onclick_nonfocusable = List.length (List.filter is_nonfocusable onclick_list) in
  let total_onclick = List.length onclick_list in
    (onclick_nonfocusable, total_onclick)

let count_focusable_with_onmouseover_and_onfocus page =
  let onmouseover_list = Testutil.get_elements_with_attribute "onmouseover" page in
  let onmouseover_focusable = List.filter Testutil.is_focusable onmouseover_list in
  let onfocus = Testutil.count_elements_with_attribute "onfocus" onmouseover_focusable in
  let total_onmouseover_focusable = List.length onmouseover_focusable in
    (onfocus, total_onmouseover_focusable);;

let count_focusable_with_onmouseout_and_onblur page =
  let onmouseout_list = Testutil.get_elements_with_attribute "onmouseout" page in
  let onmouseout_focusable = List.filter Testutil.is_focusable onmouseout_list in
  let onblur = Testutil.count_elements_with_attribute "onblur" onmouseout_focusable in
  let total_onmouseout_focusable = List.length onmouseout_focusable in
    (onblur, total_onmouseout_focusable);;

let count_nonfocusable_with_onmouseover_or_onmouseout page =
  let onmouseover_list = Testutil.get_elements_with_attribute "onmouseover" page in
  let onmouseover_nonfocusable = List.length (List.filter is_nonfocusable onmouseover_list) in
  let onmouseout_list = Testutil.get_elements_with_attribute "onmouseout" page in
  let onmouseout_nonfocusable = List.length (List.filter is_nonfocusable onmouseout_list) in
    (onmouseover_nonfocusable, onmouseout_nonfocusable);;

(* ---------------------------------------------------------------- *)
(**
   001p: Number of nonfocusable elements with onclick attribute.
*)
let test001p site page =
  let test_id = "auto001p" in
    runmsg test_id;
    let (count, total) = count_nonfocusable_with_onclick page in
    let results = [
      ("cnt1", count);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(**
   002p: Number of focusable elements with onmouseover attribute that
   do not have onfocus attribute.
*)
let test002p site page =
  let test_id = "auto002p" in
    runmsg test_id;
    let (count, total) = count_focusable_with_onmouseover_and_onfocus page in
    let missing = total - count in
    let results = [
      ("cnt1", missing);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(**
   003p: Number of focusable elements with mouseout attribute that
   do not have onblur attribute.
*)
let test003p site page =
  let test_id = "auto003p" in
    runmsg test_id;
    let (count, total) = count_focusable_with_onmouseout_and_onblur page in
    let missing = total - count in
    let results = [
      ("cnt1", missing);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;

(* ---------------------------------------------------------------- *)
(**
   004p: Number of nonfocusable elements with onmouseover or
   onmouseout attribute.
*)
let test004p site page =
  let test_id = "auto004p" in
    runmsg test_id;
    let (onmouseover, onmouseout) = count_nonfocusable_with_onmouseover_or_onmouseout page in
    let total = onmouseover + onmouseout in
    let results = [
      ("cnt1", onmouseover);
      ("cnt2", onmouseout);
      ("tot1", total)
    ] in
      Wamtml.create_wamt_test test_id results;;
