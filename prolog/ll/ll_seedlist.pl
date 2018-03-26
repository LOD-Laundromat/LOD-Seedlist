:- module(
  ll_seedlist,
  [
    assert_seed/1,   % +Seed
    retract_seed/1,  % +Hash
    seed_by_hash/2,  % +Hash, -Seed
    seed_by_status/3 % +Status, -Hash, -Seed
  ]
).

/** <module> LOD Laundromat: Seedlist

Seed keys:

  * dataset(dict)
    * description(string)
    * image(uri)
    * license(uri)
    * name(atom)
    * url(uri)
  * documents(list(uri))
  * hash(atom)
  * organization(dict)
    * name(atom)
    * image(uri)
    * url(uri)
  * processing(boolean)
  * scrape
    * added(float)
    * interval(float)
    * last-modified(float)
    * processed(float)

---

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
:- use_module(library(hash_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(rocks_ext)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

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





%! assert_seed(+Seed:dict) is det.

assert_seed(Seed0) :-
  _{name: DName0, url: Url} :< Seed0.dataset,
  get_time(Now),
  % interval
  Interval is Now - Seed0.'last-modified',
  organization_name(Url, Seed0, OName0),
  % Normalize names.
  maplist(normalize_name, [OName0,DName0], [OName,DName]),
  % hash
  md5(OName-DName, Hash),
  (   % The URL has already been added to the seedlist.
      rocks_key(seedlist, Hash)
  ->  existence_error(seed, Hash)
  ;   % prefixes
      bnode_prefix_([OName,DName], BNodePrefix),
      Dataset1 = _{
        name: DName,
        prefixes: [_{iri: BNodePrefix, prefixLabel: bnode}],
        url: Url
      },
      % license
      seed_license(Seed0.dataset, Dataset1, Dataset2),
      Seed = _{
        dataset: Dataset2,
        documents: Seed0.documents,
        hash: Hash,
        organization: _{name: OName},
        processing: false,
        scrape: _{
          added: Now,
          interval: Interval,
          'last-modified': Seed0.'last-modified',
          processed: 0.0
        }
      },
      rocks_put(seedlist, Hash, Seed)
  ).

organization_name(_, Seed0, OName0) :-
  (   _{name: OName0} :< Seed0.organization
  ->  true
  ;   _{url: Url} :< Seed0.organization
  ->  uri_host(Url, OName0)
  ), !.
organization_name(Url, _, OName0) :-
  uri_host(Url, OName0).

bnode_prefix_(Segments, BNodePrefix) :-
  setting(rdf_term:bnode_prefix_scheme, Scheme),
  setting(rdf_term:bnode_prefix_authority, Auth),
  uri_comps(BNodePrefix, uri(Scheme,Auth,['.well-known',genid|Segments],_,_)).

seed_license(Dataset, Dict1, Dict2) :-
  get_dict(license, Dataset, License), !,
  Dict2 = Dict1.put(_{license: License}).
seed_license(_, Dict, Dict).



%! retract_seed(+Hash:atom) is det.

retract_seed(Hash) :-
  rocks_delete(seedlist, Hash).



%! seed_by_hash(+Hash:atom, -Seed:dict) is semidet.
%! seed_by_hash(-Hash:atom, -Seed:dict) is nondet.

seed_by_hash(Hash, Seed) :-
  rocks(seedlist, Hash, Seed).



%! seed_by_status(+Status:oneof([idle,processing,stale]), -Hash:atom,
%!                -Seed:dict) is nondet.

seed_by_status(processing, Hash, Seed) :- !,
  rocks_value(seedlist, Seed),
  _{hash: Hash, processing: true} :< Seed.
seed_by_status(Status, Hash, Seed) :-
  get_time(Now),
  rocks_value(seedlist, Seed),
  _{hash: Hash, processing: false, scrape: Scrape} :< Seed,
  _{interval: Interval, processed: Processed} :< Scrape,
  N is Processed + Interval,
  (Status == idle -> N >= Now ; Status == stale -> N < Now).





% HELPERS %

%! uri_host(+Uri:atom, -Host:atom) is det.

uri_host(Uri, Host) :-
  uri_components(Uri, UriComps),
  uri_data(authority, UriComps, Auth),
  uri_authority_components(Auth, AuthComps),
  uri_authority_data(host, AuthComps, Host).



%! normalize_name(+Name:atom, -NormalizedName:atom) is det.
%
% Normalized names only contain alpha-numeric characters and hyphens.

normalize_name(Name1, Name3) :-
  atom_phrase(normalize_name_, Name1, Name2),
  atom_length(Name2, Length),
  (Length =< 40 -> Name3 = Name2 ; sub_atom(Name2, 0, 40, _, Name3)).

normalize_name_, [Code] -->
  [Code],
  {code_type(Code, alnum)}, !,
  normalize_name_.
normalize_name_, "-" -->
  [_], !,
  normalize_name_.
normalize_name_--> "".





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
