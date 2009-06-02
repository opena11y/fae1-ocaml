(**
   Lists that define which tests are currently run by the analysis engine
   and a version string that identifies the current set of tests.
*)

let version = "0906-1"

let page_tests = [
  Navigation.test001p; (* no site test *)
  Navigation.test002p; (* no site test *)
  Navigation.test004p;
  Navigation.test005p;

  Navigation.test010p;
  Navigation.test011p;

  Navigation.test020p;
  Navigation.test021p;
  Navigation.test022p;
  Navigation.test023p;
  Navigation.test024p;

  Navigation.test030p;
  Navigation.test031p;
  Navigation.test032p;

  Navigation.test041p;
  Navigation.test042p;
  Navigation.test043p;

  Navigation.test050p;
  Navigation.test051p;
  Navigation.test052p;
  Navigation.test053p;

  Navigation.test060p; (* no site test; debug only *)
  Navigation.test061p; (* no site test *)
  Navigation.test062p; (* no site test *)
  Navigation.test063p; (* no site test *)
  Navigation.test064p; (* no site test *)
  Navigation.test065p; (* no site test *)

  Text.test001p;
  Text.test002p;
  Text.test003p;
  Text.test004p;

  Text.test010p;
  Text.test011p;
  Text.test012p;

  Automation.test001p; (* no site test *)
  Automation.test002p; (* no site test *)
  Automation.test003p; (* no site test *)
  Automation.test004p; (* no site test *)

  Style.test001p; (* no site test *)
  Style.test002p; (* no site test *)
  Style.test003p; (* no site test *)
  Style.test004p; (* no site test *)
  Style.test005p; (* no site test *)
  Style.test006p;

  Standards.test001p;
  Standards.test002p;
]

let site_tests = [
  (* Navigation.test004s; *)
  (* Navigation.test005s; *)

  (* Navigation.test010s; *)
  (* Navigation.test011s; *)
  (* Navigation.test012s; *) (* no page test *)

  (* Navigation.test020s; *)
  (* Navigation.test021s; *)
  (* Navigation.test022s; *)
  (* Navigation.test023s; *)
  (* Navigation.test024s; *)

  (* Navigation.test030s; *)
  (* Navigation.test031s; *)
  (* Navigation.test032s; *)

  (* Navigation.test041s; *)
  (* Navigation.test042s; *)
  (* Navigation.test043s; *)

  (* Navigation.test050s; *)
  (* Navigation.test051s; *)
  (* Navigation.test052s; *)
  (* Navigation.test053s; *)

  (* Text.test001s; *)
  (* Text.test002s; *)
  (* Text.test003s; *)
  (* Text.test004s; *)

  (* Text.test010s; *)
  (* Text.test011s; *)
  (* Text.test012s; *)

  (* Style.test006s; *)

  (* Standards.test001s; *)
  (* Standards.test002s; *)
]
