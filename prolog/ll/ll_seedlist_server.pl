:- module(ll_seedlist_server, []).

/** <module> LOD Laundromat Seedlist Server

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(memfile)).
:- use_module(library(semweb/rdf_db), [
     rdf_load/2,
     rdf_transaction/3
   ]).
:- use_module(library(semweb/turtle), []).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/http_server)).
:- use_module(library(ll/ll_seedlist)).
:- use_module(library(pagination)).
:- use_module(library(pp)).
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
    html:rest_exception/1,
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

:- set_setting(http:products, ["LOD-Seedlist"-"v0.0.0"]).
:- set_setting(pagination:default_page_size, 100).





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
  rest_parameters(
    Request,
    [hash(Hash, [atom,optional(true)]),page(PageNumber),page_size(PageSize)]
  ),
  (   var(Hash)
  ->  memberchk(request_uri(RelUri), Request),
      http_absolute_uri(RelUri, Uri),
      Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
      pagination(
        Seed,
        rocks_value(seedlist, Seed),
        rocks_size(seedlist),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, list_seeds_media_type(_, Page))
  ;   seed_by_hash(Hash, Seed)
  ->  rest_media_type(MediaTypes, seed_media_type(Seed))
  ;   format(string(Msg), "Hash ‘~a’ does not exist.", [Hash]),
      rest_media_type(MediaTypes, existence_error_media_type(Hash, Msg))
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

%! existence_error_media_type(+Hash:atom, +MediaType:compound) is det.
%
% /seed/$(HASH): application/json, text/html

existence_error_media_type(Hash, Msg, media(application/json,_)) :-
  reply_json_dict(_{hash: Hash, message: Msg}, [status(404)]).
existence_error_media_type(Hash, Msg, media(test/html,_)) :-
  html_page(
    page(_,["Seed",Hash]),
    [],
    [h1("Existence error"),p(Msg)]
  ).



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
  seed_by_status_method(idle, Request, MediaTypes).
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
  seed_by_status_method(processing, Request, MediaTypes).
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
            rocks_merge(
              seedlist,
              Hash,
              _{processed: Now, processing: false}
            )
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
  seed_by_status_method(stale, Request, MediaTypes).
% /seed/stale: PATCH
seed_stale_method(_, patch, MediaTypes) :-
  rest_media_type(MediaTypes, seed_stale_media_type).

% /seed/stale: PATCH: application/json
%
% Start processing the seed.
seed_stale_media_type(media(application/json,_)) :-
  (   with_mutex(seedlist, (
        seed_by_status(stale, Hash, Seed),
        rocks_merge(seedlist, Hash, _{processing: true})
      ))
  ->  reply_json_dict(Seed, [])
  ;   reply_json_dict(_{}, [status(404)])
  ).

% /seed/*: GET,HEAD
seed_by_status_method(Status, Request, MediaTypes) :-
  rest_parameters(Request, [page(PageNumber),page_size(PageSize)]),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
  pagination(
    Seed,
    seed_by_status(Status, _, Seed),
    {Status}/[N]>>aggregate_all(count, seed_by_status(Status, _, _), N),
    Options,
    Page
  ),
  rest_media_type(MediaTypes, list_seeds_media_type(Status, Page)).





% HTML %

html_seed(Seed) -->
  html(table(tbody(\html_seed_row(Seed)))).

html_seed_table(Seeds) -->
  html(
    table([
      thead([
        th("Hash"),
        th("Interval"),
        th("Processed"),
        th("Processing"),
        th("Staleness"),
        th("URL")
      ]),
      tbody(\html_maplist(html_seed_row, Seeds))
    ])
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

html:rest_exception(Dict) :-
  html_page(
    page(_,["HTTP error",Dict.status]),
    [],
    [
      p(Dict.message),
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
