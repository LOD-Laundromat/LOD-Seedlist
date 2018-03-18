:- use_module(library(aggregate)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).

:- use_module(library(http/ckan_api)).
:- use_module(library(ll/ckan_seedlist)).
:- use_module(library(ll/ll_seedlist_server)).
:- use_module(library(thread_ext)).

populate_seedlist :-
  thread_monitor,
  aggregate_all(set(Uri), ckan_site_uri(Uri), Uris),
  threaded_maplist(4, ckan_scrape_init, Uris).
