(**
   Lists that define which tests are currently run by the analysis engine
*)

let site_tests = [
  Navigation.test001s;
  Navigation.test002s;
  Navigation.test004s;

  Navigation.test010s;
  Navigation.test011s;
  Navigation.test012s; (* unique titles: no page test *)

  Navigation.test020s;
  Navigation.test021s;
  Navigation.test022s;
  Navigation.test023s;

  Navigation.test030s;
  Navigation.test031s;

  Navigation.test041s;

  Text.test001s;
  Text.test002s;
  Text.test003s;
  Text.test004s;
  Text.test005s;
  Text.test006s;

  Automation.test001s;
  Automation.test002s;
  (* Automation.test003s; *)

  Style.test001s;
  Style.test002s;
  Style.test003s;
  Style.test004s;
  Style.test005s;

  Standards.test001s;
  Standards.test002s;
  Standards.test003s;
];;

let page_tests = [
  Navigation.test001p; (* nav menus & h1s *)
  Navigation.test002p; (* area elements *)
  Navigation.test003p; (* data tables: no site test *)
  Navigation.test004p; (* frame titles *)

  Navigation.test010p;
  Navigation.test011p;

  Navigation.test020p;
  Navigation.test021p;
  Navigation.test022p;
  Navigation.test023p;

  Navigation.test030p;
  Navigation.test031p;

  Navigation.test041p;

  Text.test001p;
  Text.test002p;
  Text.test003p;
  Text.test004p;
  Text.test005p;
  Text.test006p;

  Automation.test001p;
  Automation.test002p;
  (* Automation.test003p; *)

  Style.test001p;
  Style.test002p;
  Style.test003p;
  Style.test004p;
  Style.test005p;

  Standards.test001p;
  Standards.test002p;
  Standards.test003p;
];;
