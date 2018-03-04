:- module(ll_seedlist_server, []).

/** <module> LOD Laundromat Seedlist Server

@author Wouter Beek
@version 2018
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(settings)).

:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pagination)).
:- use_module(library(http/http_pagination)).
:- use_module(library(http/http_resource)).
:- use_module(library(http/http_server)).
:- use_module(library(pagination)).
:- use_module(library(rocks_ext)).

:- use_module(library(ll/ll_seedlist)).

:- dynamic
    http:media_types/2,
    http:param/2,
    http:params/2.

http:media_types(home_handler, [media(application/json,[]),
                                media(text/html,[])]).

http:param(stale, [
  boolean,
  default(false),
  description("Return only stale seeds.")
]).

http:params(home_handler, [page,page_size,stale]).

:- http_handler(/, home_handler, [methods([get,head,options])]).

:- set_setting(http:products, ["LOD-Seedlist"-"v0.0.0"]).

home_handler(Request) :-
  rest_method(Request, home_method(Request)).

% /: GET,HEAD
home_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  rest_parameters(
    Request,
    [page(PageNumber),page_size(PageSize),stale(Stale)]
  ),
  memberchk(request_uri(RelUri), Request),
  http_absolute_uri(RelUri, Uri),
  Options = _{page_number: PageNumber, page_size: PageSize, uri: Uri},
  pagination(Seed, seed_(Stale, Seed), number_of_seeds_, Options, Page),
  rest_media_type(MediaTypes, home_media_type(Page)).

seed_(false, Seed) :- !,
  seed(Seed).
seed_(true, Seed) :-
  stale_seed(Seed).

number_of_seeds_(N) :-
  rocks_size(seedlist, N).

% /: GET,HEAD: application/json
home_media_type(Page, media(application/json,_)) :-
  http_pagination_json(Page).
% /: GET,HEAD: text/html
home_media_type(Page, media(text/html,_)) :-
  html_page(
    page(Page,[]),
    [],
    [\html_pagination_result(Page, html_seed_table)]
  ).

html_seed_table(Seeds) -->
  html(ul(\html_maplist(html_seed_row, Seeds))).

html_seed_row(Seed) -->
  {
    _{name: DName, organization: Org, url: Url} :< Seed,
    _{name: OName} :< Org
  },
  html(li(a(href=Url, [OName,"/",DName]))).





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
