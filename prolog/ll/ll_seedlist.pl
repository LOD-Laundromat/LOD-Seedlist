:- module(
  ll_seedlist,
  [
    assert_seed/3,   % +Uri, +Hash, -Seed
    retract_seed/1,  % +Hash
    seed_by_hash/2,  % +Hash, -Seed
    seed_by_status/3 % +Status, -Hash, -Seed
  ]
).

/** <module> LOD Laundromat seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(settings)).

:- use_module(library(conf_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(http/http_client2)).
:- use_module(library(rocks_ext)).
:- use_module(library(sw/rdf_mem)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- at_halt(maplist(rocks_close, [seedlist])).

:- initialization
   ll_seedlist_init.

merge_dicts(partial, _, New, In, Out) :-
  merge_dicts(New, In, Out).
merge_dicts(full, _, Initial, Additions, Out) :-
  merge_dicts([Initial|Additions], Out).

:- maplist(rdf_assert_prefix, [
     dcat-'http://www.w3.org/ns/dcat#',
     dct-'http://purl.org/dc/terms/',
     rdfs-'http://www.w3.org/2000/01/rdf-schema#'
   ]).

:- setting(default_interval, float, 86400.0,
           "The default interval for recrawling.").
:- setting(password_file, any, _,
           "The file containing basic authentication user-password pairs.").





%! assert_seed(+Uri:atom, +Hash:atom, -Seed:dict) is det.

assert_seed(Uri, Hash, Seed) :-
  % Number of seconds in 100 days: (* 100 24 60 60.0)
  Seed = _{
    hash: Hash,
    interval: 8640000.0,
    processed: 0.0,
    processing: false,
    url: Uri
  },
  rocks_put(seedlist, Hash, Seed).



%! retract_seed(+Hash:atom) is det.

retract_seed(Hash) :-
  rocks_delete(seedlist, Hash).



%! seed_by_hash(+Hash:atom, -Seed:dict) is semidet.
%! seed_by_hash(-Hash:atom, -Seed:dict) is nondet.

seed_by_hash(Hash, Seed) :-
  rocks(seedlist, Hash, Seed).



%! seed_by_status(+Status:oneof([idle,processing,stale]), ?Hash:atom,
%!                -Seed:dict) is nondet.

seed_by_status(Status, Hash, Seed) :-
  rocks(seedlist, Hash, Seed),
  (   Status = processing
  ->  _{processing: true} :< Seed
  ;   get_time(Now),
      _{processing: false, interval: Interval, processed: Processed} :< Seed,
      N is Processed + Interval,
      (Status == idle -> N >= Now ; Status == stale -> N < Now)
  ).





% HELPERS %

%! uri_host(+Uri:atom, -Host:atom) is det.

uri_host(Uri, Host) :-
  uri_components(Uri, UriComps),
  uri_data(authority, UriComps, Auth),
  uri_authority_components(Auth, AuthComps),
  uri_authority_data(host, AuthComps, Host).





% INITIALIZATION %

%! ll_seedlist_init is det.

ll_seedlist_init :-
  conf_json(Conf),
  % interval
  _{'default-interval': Interval} :< Conf,
  set_setting(default_interval, Interval),
  % password file
  _{'password-file': File} :< Conf,
  set_setting(password_file, File),
  rocks_init(seedlist, [key(atom),merge(ll_seedlist:merge_dicts),value(term)]).
