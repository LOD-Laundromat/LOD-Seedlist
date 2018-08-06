:- module(ll_seedlist_server, []).

/** <module> LOD Laundromat Seedlist Server

Debug flag `ll(seedlist_server)'.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(debug)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).

:- use_module(library(atom_ext)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/http_server)).
:- use_module(library(ll/ll_seedlist)).
:- use_module(library(pagination)).
:- use_module(library(rocks_ext)).

:- discontiguous
    seed_method/3.

:- dynamic
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    http:media_types/2,
    http:params/2.

:- http_handler(/,
                home_handler,
                [id(home),methods([get,head,options])]).
:- http_handler(root(seed),
                seed_handler,
                [id(seed),methods([delete,get,head,options,post])]).
:- http_handler(root(seed/idle),
                seed_idle_handler,
                [id(seed_idle),methods([get,head,options,patch])]).
:- http_handler(root(seed/processing),
                seed_processing_handler,
                [id(seed_processing),methods([get,head,options,patch])]).
:- http_handler(root(seed/stale),
                seed_stale_handler,
                [id(seed_stale),methods([get,head,options,patch])]).

:- multifile
    html:handler_description/2,
    html:menu_item/2,
    html:menu_item/3,
    html:page_exception/2,
    http:error_status_message_hook/3,
    http:media_types/2,
    user:body//2,
    user:head//2.

html:handler_description(seed_handler, "Seed").
html:handler_description(seed_idle_handler, "Idle seed").
html:handler_description(seed_processing_handler, "Processing seed").
html:handler_description(seed_stale_handler, "Stale seed").

html:menu_item(seed_idle_handler, "Idle").
html:menu_item(seed_processing_handler, "Processing").
html:menu_item(seed_stale_handler, "Stale").

http:error_status_message_hook(error(existence_error(seed_hash,Hash),_Context), 404, Msg) :-
  format(
    string(Msg),
    "😿 Your request is incorrect!  Seed with hash ‘~a’ does not exist.",
    [Hash]
  ).

http:media_types(home_handler, [media(text/html)]).
http:media_types(seed_handler, [media(application/json),
                                media(text/html)]).
http:media_types(seed_idle_handler, [media(application/json),
                                     media(text/html)]).
http:media_types(seed_processing_handler, [media(application/json),
                                           media(text/html)]).
http:media_types(seed_stale_handler, [media(application/json),
                                      media(text/html)]).

:- multifile
    http:param/2.

http:param(hash, [atom,description("Hash key of the requested seed.")]).

:- set_setting(http:products, ["LOD-Seedlist"-"v0.0.1"]).
:- set_setting(pagination:default_page_size, 20).





% /:
home_handler(Request) :-
  rest_method(Request, home_method).

% /: GET,HEAD
home_method(Method, MediaTypes) :-
  http_is_get(Method),
  rest_media_type(MediaTypes, home_get_media_type).

% /: GET,HEAD: text/html
home_get_media_type(media(text/html,_)) :-
  html_page(page(_,["Home"]), [], []).





% /seed
seed_handler(Request) :-
  rest_method(Request, seed_method(Request)).


% /seed: DELETE
seed_method(Request, delete, MediaTypes) :-
  rest_media_type(MediaTypes, seed_delete_media_type(Request)).

% /seed: DELETE: application/json
seed_delete_media_type(Request, media(application/json,_)) :-
  (   auth_(Request)
  ->  rest_parameters(Request, [hash(Hash)]),
      with_mutex(seedlist,
        (   rocks_key(seedlist, Hash)
        ->  rocks_delete(seedlist, Hash),
            Status = 200
        ;   Status = 404
        )
      ),
      reply_json_dict(_{}, [status(Status)])
  ;   reply_json_dict(_{}, [status(403)])
  ).


% /seed: GET
seed_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  seed_get_(Request, MediaTypes, _).

seed_get_(Request, MediaTypes, Status) :-
  debug(ll(seedlist_server), "GET /seed", []),
  rest_parameters(
    Request,
    [hash(Hash,[atom,optional(true)]),page(PageNumber),page_size(PageSize)]
  ),
  (   var(Hash)
  ->  % Return a list of seeds.
      memberchk(request_uri(RelUri), Request),
      http_absolute_uri(RelUri, Uri),
      Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
      pagination(
        Seed,
        seed(Status, Hash, Seed),
        number_of_seeds(Status),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, list_seeds_media_type(Status, Page))
  ;   % Return the requested seed.
      seed(Status, Hash, Seed)
  ->  rest_media_type(MediaTypes, seed_media_type(Seed))
  ;   % The requested seed does not exist.
      throw(error(existence_error(seed_hash,Hash),_))
  ).

%! list_seeds_media_type(?Status:oneof([idle,processing,stale]), +Page:dict,
%!                       +MediaType:compound) is det.
%
% /seed: GET,HEAD: application/json, text/html

list_seeds_media_type(_, Page, media(application/json,_)) :-
  http_pagination_json(Page).
list_seeds_media_type(Status, Page, media(text/html,_)) :-
  (var(Status) -> T = [] ; atom_capitalize(Status, Label), T = [Label]),
  html_page(
    page(Page,["Seed"|T]),
    [],
    [\html_pagination_result(Page, html_seed_table)]
  ).

%! seed_media_type(+Seed:dict, +MediaType:compound) is det.

% /seed/$(HASH): GET, HEAD: application/json, text/html
seed_media_type(Seed, media(application/json,_)) :-
  reply_json_dict(Seed).
seed_media_type(Seed, media(text/html,_)) :-
  _{hash: Hash} :< Seed,
  atom_string(Hash, Subtitle),
  html_page(page(_,[Subtitle]), [], [\html_seed(Seed)]).


% /seed: POST
seed_method(Request, post, MediaTypes) :-
  rest_media_type(MediaTypes, seed_post_media_type(Request)).

% /seed: POST: application/json
seed_post_media_type(Request, media(application/json,_)) :-
  (   auth_(Request)
  ->  (   http_read_json_dict(Request, Dict, [value_string_as(atom)]),
          assert_seeds(Dict)
      ->  Status = 200
      ;   Status = 400
      )
  ;   Status = 403
  ),
  reply_json_dict(_{}, [status(Status)]).



% /seed/idle
seed_idle_handler(Request) :-
  rest_method(Request, seed_idle_method(Request)).


% /seed/idle: GET,HEAD
seed_idle_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  seed_get_(Request, MediaTypes, idle).
% /seed/idle: PATCH
seed_idle_method(Request, patch, MediaTypes) :-
  rest_media_type(MediaTypes, seed_idle_media_type(Request)).


% /seed/idle: PATCH: application/json
%
% Reset the seed to its original state.
seed_idle_media_type(Request, media(application/json,_)) :-
  (   auth_(Request)
  ->  rest_parameters(Request, [hash(Hash)]),
      (   rocks_key(seedlist, Hash)
      ->  with_mutex(seedlist,
            rocks_merge(
              seedlist,
              Hash,
              _{processed: 0.0, processing: false}
            )
          ),
          reply_json_dict(_{}, [])
      ;   reply_json_dict(_{}, [status(404)])
      )
  ;   reply_json_dict(_{}, [status(403)])
  ).



% /seed/processing
seed_processing_handler(Request) :-
  rest_method(Request, seed_processing_method(Request)).


% /seed/processing: GET,HEAD
seed_processing_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  seed_get_(Request, MediaTypes, processing).
% /seed/processing: PATCH
seed_processing_method(Request, patch, MediaTypes) :-
  rest_media_type(MediaTypes, seed_processing_media_type(Request)).


% /seed/processing: PATCH: application/json
%
% Finish processing the seed.
seed_processing_media_type(Request, media(application/json,_)) :-
  (   auth_(Request)
  ->  rest_parameters(Request, [hash(Hash)]),
      (   rocks_key(seedlist, Hash)
      ->  get_time(Now),
          with_mutex(
            seedlist,
            rocks_merge(seedlist, Hash, _{processed: Now, processing: false})
          ),
          reply_json_dict(_{}, [])
      ;   reply_json_dict(_{}, [status(404)])
      )
  ;   reply_json_dict(_{}, [status(403)])
  ).



% /seed/stale
seed_stale_handler(Request) :-
  rest_method(Request, seed_stale_method(Request)).


% /seed/stale: GET,HEAD
seed_stale_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  seed_get_(Request, MediaTypes, stale).
% /seed/stale: PATCH
seed_stale_method(_, patch, MediaTypes) :-
  rest_media_type(MediaTypes, seed_stale_media_type).


% /seed/stale: PATCH: application/json
%
% Start processing the seed.
seed_stale_media_type(media(application/json,_)) :-
  debug(ll(seedlist_server), "PATCH /seed/stale", []),
  (   with_mutex(seedlist, (
        seed(stale, Hash, Seed),
        rocks_merge(seedlist, Hash, _{processing: true})
      ))
  ->  reply_json_dict(Seed, [])
  ;   reply_json_dict(_{}, [status(404)])
  ).





% HTML %

html_seed(Seed) -->
  html_table(\html_seed_row(Seed)).

html_seed_table(Seeds) -->
  html_table(
    \html_table_header_row([
      "Hash",
      "Interval",
      "Processed",
      "Processing",
      "Staleness",
      "URL"
    ]),
    \html_maplist(html_seed_row, Seeds)
  ).

html_seed_row(Seed) -->
  {
    _{
      hash: Hash,
      interval: Interval,
      processed: Processed,
      processing: Processing,
      url: Uri
    } :< Seed,
    http_link_to_id(seed, [hash(Hash)], Link),
    Staleness is Interval + Processed
  },
  html(
    tr([
      td(a(href=Link, code(Seed.hash))),
      td(\format_time_(Interval)),
      td(\format_time_(Processed)),
      td(\html_boolean(Processing)),
      td(\format_time_(Staleness)),
      td(a(href=Uri, code(Uri)))
    ])
  ).

format_time_(N) -->
  {format_time(string(Str), "%FT%T%:z", N)},
  html(Str).





% HTML STYLE %

html:page_exception(Status, Msg) :-
  html_page(
    page(_,["HTTP error",Status]),
    [],
    [
      p(Msg),
      p(a(href='/',"Return to root"))
    ]
  ).

user:head(page(Page,Subtitles), Content_0) -->
  {atomics_to_string(["Seedlist"|Subtitles], " ― ", Title)},
  html(
    head([
      \html_root_attribute(lang, en),
      meta(charset='utf-8', []),
      \meta_ie_latest,
      \meta_viewport,
      \favicon,
      \html_if_then(ground(Page), html_pagination_links(Page)),
      title(Title),
      \html_requires(html_ext)
    | Content_0
    ])
  ).

user:body(page(_,_), Content_0) -->
  html(body([\navbar("Seedlist", \menu), \row_1(Content_0)])).





% HELPERS %

%! auth_(+Request:list(compound)) is semidet.

auth_(Request) :-
  setting(ll_seedlist:password_file, File),
  catch(http_authenticate(basic(File), Request, _), _, true).
