:- use_module(library(aggregate)).
:- use_module(library(thread)).

:- use_module(library(http/ckan_api)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).

:- use_module(library(ll/ckan_seedlist)).
:- use_module(library(ll/ll_seedlist_server)).

populate_seedlist :-
  aggregate_all(set(Uri), ckan_site_uri(Uri), Uris),
  %concurrent_maplist(ckan_scrape_init, Uris),
  member(Uri, Uris),
  writeln(Uri),
  ckan_scrape_init(Uri).
