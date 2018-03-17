:- use_module(library(http/ckan_api)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/thread_httpd)).

:- use_module(library(ll/ckan_seedlist)).
:- use_module(library(ll/ll_seedlist_server)).

populate_seedlist :-
  forall(
    ckan_site_uri(Uri),
    thread_create(ckan_scrape_init(Uri), _, [alias(Uri)])
  ).
