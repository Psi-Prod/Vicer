open Mirage

let port =
  let doc = Key.Arg.info ~doc:"Gemini listen port." [ "port" ] in
  Key.(create "port" Arg.(opt (some int) None doc))

let hook =
  let doc =
    Key.Arg.info ~doc:"Webhook for pulling the repository." [ "hook" ]
  in
  Key.(create "hook" Arg.(opt string "/hook" doc))

let certs_remote =
  let doc =
    Key.Arg.info ~doc:"Certificate remote repository url" [ "certs-remote" ]
  in
  Key.(create "certs-remote" Arg.(required string doc))

let blog_remote =
  let doc =
    Key.Arg.info ~doc:"Blog content remote repository url" [ "blog-remote" ]
  in
  Key.(create "blog-remote" Arg.(required string doc))

let comments_remote =
  let doc =
    Key.Arg.info ~doc:"Comments remote repository url" [ "comments-remote" ]
  in
  Key.(create "comments-remote" Arg.(required string doc))

let ssh_key =
  let doc =
    Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)."
      [ "ssh-key" ]
  in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc =
    Key.Arg.info ~doc:"SSH host key authenticator." [ "ssh-authenticator" ]
  in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let packages =
  [
    package ~min:"0.2" "mehari";
    package ~min:"0.2" "mehari-mirage";
    package "git-kv";
    package "ezjsonm";
  ]

let vicer =
  let keys =
    [
      Key.v port;
      Key.v hook;
      Key.v certs_remote;
      Key.v blog_remote;
      Key.v comments_remote;
    ]
  in
  foreign "Unikernel.Main" ~packages ~keys
    (git_client @-> pclock @-> random @-> stackv4v6 @-> time @-> job)

let stack = generic_stackv4v6 default_network

let git_client =
  let dns = generic_dns_client stack in
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (git_ssh ~key:ssh_key ~authenticator:ssh_authenticator tcp git)

let () =
  register "vicer"
    [
      vicer $ git_client $ default_posix_clock $ default_random $ stack
      $ default_time;
    ]
