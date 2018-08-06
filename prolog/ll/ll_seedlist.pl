:- module(
  ll_seedlist,
  [
    assert_seeds/1,    % +Dict
    number_of_seeds/2, % ?Status, -N
    retract_seed/1,    % +Hash
    seed/3             % ?Status, ?Hash, -Seed
  ]
).

/** <module> LOD Laundromat seedlist

Debug flag `ll(seedlist)'.

---

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(settings)).

:- use_module(library(call_ext)).
:- use_module(library(conf_ext)).
:- use_module(library(dict)).
:- use_module(library(hash_ext)).
:- use_module(library(rocks_ext)).

:- at_halt(maplist(rocks_close, [seedlist])).

:- initialization
   ll_seedlist_init.

merge_dicts(partial, _, New, In, Out) :-
  merge_dicts(New, In, Out).
merge_dicts(full, _, Initial, Additions, Out) :-
  merge_dicts([Initial|Additions], Out).

:- setting(default_interval, float, 86400.0,
           "The default interval for recrawling.").
:- setting(password_file, any, _,
           "The file containing basic authentication user-password pairs.").





%! assert_seed(+Url:atom) is det.

assert_seed(Uri) :-
  md5(Uri, Hash),
  Seed = _{
    hash: Hash,
    % Number of seconds in 100 days: (* 100 24 60 60.0)
    interval: 8640000.0,
    processed: 0.0,
    processing: false,
    url: Uri
  },
  debug(ll(seedlist), "ASSERT ~a", [Uri]),
  rocks_put(seedlist, Hash, Seed).



%! assert_seeds(+Dict:dict) is det.

assert_seeds(Dict) :-
  maplist(assert_seed, Dict.urls).



%! number_of_seeds(?Status:oneof([idle,processing,stale]), -Seed:dict) is det.

number_of_seeds(Status, N) :-
  var(Status), !,
  debug(ll(seedlist), "COUNT", []),
  rocks_size(seedlist, N).
number_of_seeds(Status, N) :-
  must_be(oneof([idle,processing,stale]), Status),
  debug(ll(seedlist), "COUNT ~a", [Status]),
  aggregate_all(count, seed(Status, _, _), N).



%! retract_seed(+Hash:atom) is det.

retract_seed(Hash) :-
  debug(ll(seedlist), "RETRACT ~a", [Hash]),
  rocks_delete(seedlist, Hash).



%! seed(?Status:oneof([idle,processing,stale]), +Hash:atom, -Seed:dict) is semidet.
%! seed(?Status:oneof([idle,processing,stale]), -Hash:atom, -Seed:dict) is nondet.

seed(Status, Hash, Seed) :-
  call_det_when_ground(Hash, (
    rocks(seedlist, Hash, Seed),
    debug(ll(seedlist), "LOOKUP ~a", [Hash]),
    (ground(Status) -> check_status_(Status, Seed) ; true)
  )).

check_status_(processing, Seed) :- !,
  _{processing: true} :< Seed.
check_status_(Status, Seed) :-
  get_time(Now),
  _{processing: false, interval: Interval, processed: Processed} :< Seed,
  N is Processed + Interval,
  (Status == idle -> N >= Now ; Status == stale -> N < Now).





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
