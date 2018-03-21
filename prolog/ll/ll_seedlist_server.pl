:- module(ll_seedlist_server, []).

/** <module> LOD Laundromat Seedlist Server

@author Wouter Beek
@version 2018
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/http_server)).
:- use_module(library(pagination)).
:- use_module(library(pp)).
:- use_module(library(rocks_ext)).

:- use_module(library(ll/ll_seedlist)).

:- dynamic
    http:media_types/2.

:- http_handler(/,
                home_handler,
                [id(home),methods([get,head,options])]).
:- http_handler(root(seed),
                seed_handler,
                [id(seed),methods([delete,get,head,options,patch,post])]).

:- multifile
    html:handler_description/2,
    html:menu_item/3,
    http:media_types/2,
    user:body//2,
    user:head//2.

html:handler_description(seed_handler, "Seed").

html:menu_item(seed_handler, "Seed").

http:media_types(home_handler, [media(text/html)]).
http:media_types(seed_handler, [media(application/json),media(text/html)]).

:- set_setting(http:products, ["LOD-Seedlist"-"v0.0.0"]).





% /
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
  rest_parameters(
    Request,
    [
      hash(Hash, [atom,
                  description("Hash key of the seed that is to be deleted.")])
    ]
  ),
  with_mutex(seedlist,
    (   rocks_key(seedlist, Hash)
    ->  rocks_delete(seedlist, Hash),
        Success = 200
    ;   Success = 404
    )
  ),
  rest_media_type(MediaTypes, seed_delete_media_type(Success)).
% /seed: GET,HEAD
seed_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  rest_parameters(
    Request,
    [
      hash(Hash, [atom,
                  description("Hash key of the requested seed, if any."),
                  optional(true)]
      ),
      page(PageNumber),
      page_size(PageSize)
    ]
  ),
  (   var(Hash)
  ->  memberchk(request_uri(RelUri), Request),
      http_absolute_uri(RelUri, Uri),
      Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
      pagination(
        Seed,
        rocks_value(seedlist, Seed),
        [N]>>rocks_size(seedlist, N),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, seeds_get_media_type(Page))
  ;   rocks(seedlist, Hash, Seed),
      rest_media_type(MediaTypes, seed_get_media_type(Seed))
  ).
% /seed: PATCH
seed_method(Request, patch, _) :-
  rest_parameters(
    Request,
    [
      hash(Hash, [atom,
                  description("Hash key of the patched seed, if any."),
                  optional(true)])
    ]
  ),
  (   var(Hash),
      (   next_seed(Seed)
      ->  reply_json_dict(Seed, [])
      ;   reply_json_dict(_{}, [status(404)])
      )
  ;   with_mutex(seedlist, rocks_merge(seedlist, Hash, _{status: idle}))
  ).
% /seed: POST
seed_method(Request, post, MediaTypes) :-
  http_read_json_dict(Request, Seed, [value_string_as(atom)]),
  rest_media_type(MediaTypes, seed_post_media_type(Seed)).

% /seed: DELETE: application/json
seed_delete_media_type(Status, media(application/json,_)) :-
  reply_json_dict(_{}, [status(Status)]).

% /seed: GET,HEAD: application/json
seed_get_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /seed/$(HASH): GET, HEAD: application/json
seed_get_media_type(Seed, media(application/json,_)) :-
  reply_json_dict(Seed).
% /seed/$(HASH): GET, HEAD: text/html
seed_get_media_type(Seed, media(text/html,_)) :-
  _{hash: Hash} :< Seed,
  atom_string(Hash, Subtitle),
  html_page(page(_,[Subtitle]), [], [\html_seed(Seed)]).

% /seed: POST: application/json
seed_post_media_type(Seed, media(application/json,_)) :-
  catch(add_seed(Seed), E, true),
  (   var(E)
  ->  reply_json_dict(_{}, [status(201)])
  ;   E = error(existence_error(seed,_Hash),_Context)
  ->  reply_json_dict(_{message: "A seed with the same hash already exists."})
  ).

% /seed: GET,HEAD: application/json
seeds_get_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /seed: GET,HEAD: text/html
seeds_get_media_type(Page, media(text/html,_)) :-
  html_page(
    page(Page,["Seed"]),
    [],
    [\html_pagination_result(Page, html_seed_table)]
  ).
  




% HTML %

html_seed_table(Seeds) -->
  html(ul(\html_maplist(html_seed_row, Seeds))).

html_seed_row(Seed) -->
  {
    _{dataset: Dataset, hash: Hash, organization: Org} :< Seed,
    _{name: DName, url: Url} :< Dataset,
    _{name: OName} :< Org,
    http_link_to_id(seed, [hash(Hash)], Uri)
  }, !,
  html(li(a(href=Uri, [OName,"/",DName," ",\external_link(Url)]))).
html_seed_row(Seed) -->
  {_{hash: Hash} :< Seed},
  html(li(code(Hash))).

html_seed(Seed) -->
  {
    _{
      dataset: Dataset,
      documents: Docs,
      hash: Hash,
      organization: Org,
      scrape: Scrape,
      status: Status
    } :< Seed,
    _{name: Name, url: Url} :< Dataset,
    _{name: OrgName} :< Org,
    _{added: Added, interval: Interval, processed: Processed} :< Scrape,
    format_time(string(AddedStr), "%FT%T%:z", Added),
    Staleness is Interval + Processed,
    format_time(string(StalenessStr), "%FT%T%:z", Staleness)
  },
  html([
    h1([OrgName,": ",Name]),
    dl([
      dt("Staleness time"),
      dd(StalenessStr),
      dt("URL"),
      dd(a(href=Url, Url)),
      dt("Added"),
      dd(AddedStr),
      dd("Status"),
      dt(Status),
      dt("Documents"),
      dd(ul(\html_maplist(html_seed_document, Docs))),
      dt("Hash"),
      dd(Hash)
    ])
  ]).

html_seed_document(Doc) -->
  html(li(a(href=Doc, Doc))).





% HTML style %

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
