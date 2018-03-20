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
    http:media_types/2,
    http:param/2,
    http:params/2.

http:media_types(home_handler, [media(application/json,[]),
                                media(text/html,[])]).

http:param(hash, [
  atom,
  description("Optional hash indicating a specific seed."),
  optional(true)
]).
http:param(stale, [
  boolean,
  default(false),
  description("Return only stale seeds.")
]).

http:params(home_handler, [hash,page,page_size,stale]).

:- http_handler(/,
                home_handler,
                [id(home),methods([get,head,options])]).
:- http_handler(root(seed),
                seed_handler,
                [id(seed),methods([get,head,options,patch,post])]).

:- multifile
    html:handler_description/2,
    html:menu_item/3,
    http:media_types/2,
    http:param/2,
    http:params/2,
    user:body//2,
    user:head//2.

html:handler_description(seed_handler, "Seed").

html:menu_item(seed_handler, "Seed").

http:media_types(home_handler, [media(text/html)]).
http:media_types(seed_handler, [media(application/json),media(text/html)]).

http:param(hash, [description("Hash key of a specific seed."),
                  atom,
                  optional(true)]).
http:param(stale, [default(true),
                   description("Whether the retrieved seeds should be stale or not.  Default value is ‘false’."),
                   boolean]).

http:params(seed_handler, [hash,page,page_size,stale]).

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

% /seed: GET,HEAD
seed_method(Request, Method, MediaTypes) :-
  http_is_get(Method), !,
  rest_parameters(
    Request,
    [hash(Hash),page(PageNumber),page_size(PageSize),stale(Stale)]
  ),
  (   var(Hash)
  ->  memberchk(request_uri(RelUri), Request),
      http_absolute_uri(RelUri, Uri),
      Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
      pagination(
        Seed,
        seed(Stale, Seed),
        [N]>>rocks_size(seedlist, N),
        Options,
        Page
      ),
      rest_media_type(MediaTypes, seeds_get_media_type(Page))
  ;   rocks(seedlist, Hash, Seed),
      rest_media_type(MediaTypes, seed_get_media_type(Seed))
  ).
% /seed: PATCH
seed_method(_, patch, MediaTypes) :-
  next_seed(Seed),
  rest_media_type(MediaTypes, seed_patch_media_type(Seed)).
% /seed: POST
seed_method(Request, post, MediaTypes) :-
  http_read_json_dict(Request, Seed, [value_string_as(atom)]),
  rest_media_type(MediaTypes, seed_post_media_type(Seed)).

% /seed: GET,HEAD: application/json
seed_get_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /seed/$(HASH): GET, HEAD: application/json
seed_get_media_type(Seed, media(application/json,_)) :-
  reply_json_dict(Seed).
% /seed/$(HASH): GET, HEAD: text/html
seed_get_media_type(Seed, media(text/html,_)) :-
  Hash{} :< Seed,
  atom_string(Hash, Subtitle),
  html_page(page(_,[Subtitle]), [], [\html_seed(Seed)]).

% /seed: PATCH: application/json
seed_patch_media_type(Seed, media(application/json,_)) :-
  reply_json_dict(Seed, []).

% /seed: POST: application/json
seed_post_media_type(Seed, media(application/json,_)) :-
  catch(add_seed(Seed), E, true),
  (   var(E)
  ->  reply_json_dict(_{}, [status(201)])
  ;   gtrace,
      writeln(E)
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
    Hash{dataset: Dataset, organization: Org} :< Seed,
    _{name: DName, url: Url} :< Dataset,
    _{name: OName} :< Org,
    http_link_to_id(seed, [hash(Hash)], Uri)
  }, !,
  html(li(a(href=Uri, [OName,"/",DName," ",\external_link(Url)]))).
html_seed_row(Seed) -->
  {Hash{} :< Seed},
  html(li(code(Hash))).

html_seed(Seed) -->
  {
    Hash{dataset: Dataset, documents: Docs, organization: Org} :< Seed,
    _{name: Name, url: Url} :< Dataset,
    _{name: OrgName} :< Org
  },
  html([
    h1([OrgName,": ",Name]),
    dl([
      dt("URL"),
      dd(a(href=Url, Url)),
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
