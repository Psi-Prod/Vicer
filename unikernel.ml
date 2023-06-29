open Lwt.Infix
open Lwt.Syntax

let src = Logs.Src.create "vicer"

module Log = (val Logs.src_log src)

module Comment = struct
  type t = { author : string; date : date; body : string }
  and date = (int * int * int) * (int * int * int)

  let of_json json =
    let open Ezjsonm in
    let d = get_dict json in
    let author = List.assoc "author" d |> decode_string_exn in
    let body = List.assoc "body" d |> decode_string_exn in
    let date =
      let ints = get_triple get_int get_int get_int in
      List.assoc "date" d |> get_pair ints ints
    in
    { author; date; body }

  let to_json { author; date; body } =
    let open Ezjsonm in
    dict
      [
        ("author", encode_string author);
        ("date", pair (triple int int int) (triple int int int) date);
        ("body", encode_string body);
      ]

  let make ~author ~date body = { author; date; body }

  let compare { date = date, time; _ } { date = date', time'; _ } =
    let open Ptime in
    compare
      (Option.get (of_date_time (date, (time, 0))))
      (Option.get (of_date_time (date', (time', 0))))

  let pp_date fmt ((year, month, day), (hour, min, sec)) =
    Format.fprintf fmt "%i-%i-%i-%i-%i-%i" year month day hour min sec

  let pp_date_hum fmt ((y, m, d), _) = Format.fprintf fmt "%02i-%02i-%02i" d m y

  let pp fmt { author; date; body } =
    Format.fprintf fmt "{ name = %S; date = %a; body = %S }" author pp_date date
      body
end

module CommentStore (P : Mirage_clock.PCLOCK) = struct
  module Store = Git_kv.Make (P)

  let post store article_url com =
    let push f =
      Store.change_and_push store ~author:"vicer"
        ~message:(Format.asprintf "Post comment %a" Comment.pp com)
        f
      >|= function
      | Ok (Ok ()) -> ()
      | Ok (Error err) | Error err ->
          Log.warn (fun l -> l "%a" Store.pp_write_error err)
    in
    let key = Mirage_kv.Key.v article_url in
    Store.exists store key >>= function
    | Ok (Some `Value) -> (
        Store.get store key >>= function
        | Ok json ->
            let json =
              Ezjsonm.from_string json
              |> Ezjsonm.get_list Comment.of_json
              |> List.cons com
              |> Ezjsonm.list Comment.to_json
              |> Ezjsonm.to_string
            in
            push (fun store -> Store.set store key json)
        | Error err ->
            Log.warn (fun l -> l "%a" Store.pp_error err);
            Lwt.return_unit)
    | Ok None ->
        push (fun store ->
            let json =
              Ezjsonm.list Comment.to_json [ com ] |> Ezjsonm.to_string
            in
            Store.set store key json)
    | Ok (Some `Dictionary) ->
        Log.warn (fun l -> l "A value is expected!");
        Lwt.return_unit
    | Error err ->
        Log.warn (fun l -> l "%a" Store.pp_error err);
        Lwt.return_unit

  let fetch store article_url =
    let key = Mirage_kv.Key.v article_url in
    Store.get store key >|= function
    | Ok json -> Ezjsonm.from_string json |> Ezjsonm.get_list Comment.of_json
    | Error (`Not_found _) -> []
    | Error err ->
        Log.warn (fun l -> l "%a" Store.pp_error err);
        []
end

module Template = struct
  open Mehari.Gemtext

  let com_header url n =
    [
      newline;
      heading `H1 (Printf.sprintf "Commentaires (%i)" n);
      newline;
      link (Filename.concat url "comment") ~name:"Écrire un commentaire";
      newline;
      newline;
    ]
    |> to_string

  let no_com url =
    [
      newline;
      heading `H1 "Commentaires (0)";
      newline;
      text "Pas encore de commentaires";
      newline;
      link (Filename.concat url "comment") ~name:"Écrire un commentaire";
      newline;
    ]
    |> to_string

  let comment (c : Comment.t) =
    [
      text
        (Format.asprintf "Par %s, le %a :" c.author Comment.pp_date_hum c.date);
      quote c.body;
      newline;
    ]
    |> to_string
end

module Main (_ : sig end)
(P : Mirage_clock.PCLOCK)
(R : Mirage_random.S)
(S : Tcpip.Stack.V4V6)
(T : Mirage_time.S) =
struct
  module Store = Git_kv.Make (P)
  module X = Tls_mirage.X509 (Store) (P)
  module M = Mehari_mirage.Make (P) (S) (T)
  module ComStore = CommentStore (P)

  let not_found = Mehari.(response not_found) ""
  let gemini_en = Mehari.gemini ~charset:"utf-8" ~lang:[ "en" ] ()

  let coms_feed =
    object (self)
      val entries : (string * Comment.t) Queue.t = Queue.create ()
      val mutable updated = Ptime.epoch

      method add_entry url ({ Comment.date = date, time; _ } as com) =
        updated <- Ptime.of_date_time (date, (time, 0)) |> Option.get;
        if Queue.length entries > 10 then
          let _ = Queue.take entries in
          Queue.add (url, com) entries
        else Queue.add (url, com) entries

      method entry_of_com article_url
          { Comment.author; date = date, time; body } =
        let title =
          Printf.sprintf "New comment of %s on article %S" author article_url
        in
        let id =
          Printf.sprintf "heyplzlookat.me/articles/%s/comment" article_url
          |> Uri.of_string
        in
        Syndic.Atom.entry ~id ~title:(Text title) ~content:(Text body)
          ~authors:(Syndic.Atom.author author, [])
          ~updated:(Ptime.of_date_time (date, (time, 0)) |> Option.get)
          ()

      method to_atom =
        let open Syndic.Atom in
        let xml =
          feed ~generator:(generator "Vicer")
            ~id:(Uri.of_string "heyplzlookat.me/articles/comments.xml")
            ~title:(Text "Heyplzlookat's comments feed") ~updated
            (Queue.fold
               (fun es (url, com) -> self#entry_of_com url com :: es)
               [] entries)
          |> Syndic.Atom.to_xml
        in
        Format.asprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>%s"
          (Syndic.XML.to_string xml ~ns_prefix:(function
            | "http://www.w3.org/2005/Atom" -> Some ""
            | _ -> Some "http://www.w3.org/2005/Atom"))
    end

  let post_com blog coms req =
    let article_url = Mehari.param req 1 in
    Store.exists blog
      (Mirage_kv.Key.v (Filename.concat "/articles" article_url))
    >>= function
    | Ok (Some `Value) -> (
        match Mehari.query req with
        | None | Some "" -> M.respond Mehari.input "Votre commentaire :"
        | Some body ->
            let author =
              (match Mehari.client_cert req with
              | [] -> None
              | hd :: _ ->
                  X509.Certificate.issuer hd
                  |> X509.Distinguished_name.common_name)
              |> Option.value ~default:"Anonyme"
            in
            let date =
              P.now_d_ps () |> Ptime.unsafe_of_d_ps |> Ptime.to_date_time
              |> fun (d, (t, _)) -> (d, t)
            in
            let com = Comment.make ~author ~date (Uri.pct_decode body) in
            let+ () = ComStore.post coms article_url com in
            coms_feed#add_entry article_url com;
            let redirect = Filename.concat "/articles" article_url in
            Mehari.(response redirect_temp) redirect)
    | Ok (Some `Dictionary | None) -> Lwt.return not_found
    | Error err ->
        Log.warn (fun l -> l "%a" Store.pp_error err);
        Lwt.return not_found

  let serve_misc _ =
    let year, month, day =
      P.now_d_ps () |> Ptime.unsafe_of_d_ps |> Ptime.to_date
    in
    let body =
      let open Mehari.Gemtext in
      [
        heading `H1 "Misc";
        newline;
        heading `H2 (Format.asprintf "Today: %02i/%02i/%02i" year month day);
        newline;
        link "/" ~name:"Back to home";
      ]
    in
    M.respond_body (Mehari.gemtext body) gemini_en

  let serve_article blog coms req =
    let article_url = Mehari.param req 1 in
    let target = Mehari.target req in
    Store.get blog (Mirage_kv.Key.v target) >>= function
    | Ok body ->
        let+ coms =
          ComStore.fetch coms article_url >|= function
          | [] -> Template.no_com target
          | coms ->
              let header = Template.com_header target (List.length coms) in
              let comments =
                List.map Template.comment coms |> String.concat "\n"
              in
              header ^ comments
        in
        let mime = Mehari.gemini ~charset:"utf-8" ~lang:[ "fr" ] () in
        Mehari.(response_body (string (body ^ coms)) mime)
    | Error _ -> Lwt.return not_found

  let serve_static blog req =
    let find path =
      let lookup path = Store.get blog (Mirage_kv.Key.v path) in
      lookup path >>= function
      | Ok resp -> Lwt.return_ok resp
      | Error _ -> lookup (Filename.concat path "index.gmi")
    in
    let target = Mehari.target req |> Uri.pct_decode in
    find target >|= function
    | Ok body ->
        let mime =
          Mehari.from_filename target |> Option.value ~default:gemini_en
        in
        Mehari.(response_body (string body)) mime
    | Error _ -> not_found

  let sync ~blog ~coms _ =
    let* content_msg =
      Git_kv.pull blog >|= function
      | Ok _ -> "Succefully pulled content repository.\n"
      | Error err ->
          Format.asprintf "Error while pulling content repository:\n%a\n"
            Store.pp_error err
    in
    let+ coms_msg =
      Git_kv.pull coms >|= function
      | Ok _ -> "Succefully pulled commentaries repository."
      | Error err ->
          Format.asprintf "Error while pulling commentaries repository: %a"
            Store.pp_error err
    in
    Mehari.(response_body (string (content_msg ^ coms_msg)) plaintext)

  let comments_feed _ =
    M.respond_body
      (Mehari.string coms_feed#to_atom)
      (Mehari.make_mime ~charset:"utf-8" "application/atom+xml")

  let serve_random_banners blog _ =
    Store.list blog (Mirage_kv.Key.v "/banners") >>= function
    | Ok banners -> (
        let rindex = Randomconv.int ~bound:(List.length banners) R.generate in
        match List.nth banners rindex with
        | banner, `Value -> (
            Store.get blog banner >|= function
            | Ok body ->
                let mime = Magic_mime.lookup (Mirage_kv.Key.to_string banner) in
                Mehari.(response_body (string body) (make_mime mime))
            | Error err ->
                Log.warn (fun l -> l "%a" Store.pp_error err);
                not_found)
        | _, `Dictionary -> Lwt.return not_found)
    | Error err ->
        Log.warn (fun l -> l "%a" Store.pp_error err);
        Lwt.return not_found

  let router blog coms =
    M.router
      [
        M.route ("/" ^ Key_gen.hook ()) (sync ~blog ~coms);
        M.route "/misc.gmi" serve_misc;
        M.route "/articles" (fun _ ->
            M.respond Mehari.redirect_temp "/gemlog.gmi");
        M.route "/articles/comments.xml" comments_feed;
        M.route ~regex:true {|/articles/([a-zA-Z0-9_-]+\.gmi)/comment|}
          (post_com blog coms);
        M.route ~regex:true {|/articles/([a-zA-Z0-9_-]+\.gmi)|}
          (serve_article blog coms);
        M.route "/randbanner" (serve_random_banners blog);
        M.route ~regex:true "/(.*)" (serve_static blog);
      ]

  let start git_ctx _default_clock () stack _default_time =
    let* certs_remote = Git_kv.connect git_ctx (Key_gen.certs_remote ()) in
    let* certs = X.certificate certs_remote `Default in
    let* blog = Git_kv.connect git_ctx (Key_gen.blog_remote ()) in
    let* coms = Git_kv.connect git_ctx (Key_gen.comments_remote ()) in
    router blog coms |> M.logger
    |> M.run ?port:(Key_gen.port ()) ~certchains:[ certs ] stack
end
