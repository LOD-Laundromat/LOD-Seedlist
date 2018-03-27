:- module(
  ll_seedlist,
  [
    assert_seed/1,   % +Seed
    retract_seed/1,  % +Hash
    seed_by_hash/2,  % +Hash, -Seed
    seed_by_status/3 % +Status, -Hash, -Seed
  ]
).

/** <module> LOD Laundromat seedlist

  * approach(atom) REQUIRED
  * dataset(dict)
    * description(string)
    * image(uri)
    * 'last-modified'(float) REQUIRED
    * license(uri)
    * name(atom) REQUIRED NORMALIZED
    * url(uri)
  * documents(list(uri)) REQUIRED
  * hash(atom) GENERATED
  * organization(dict) GENERATED
    * name(atom) NORMALIZED
    * image(uri)
    * url(uri)
  * processing(boolean) GENERATED
  * scrape GENERATED
    * added(float)
    * interval(float)
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

assert_seed(Seed1) :-
  normalize_name(Seed1.dataset.name, DName),
  organization_name(Seed1, OName),
  % hash
  md5(OName-DName, Hash),
  (   % The URL has already been added to the seedlist.
      rocks_key(seedlist, Hash)
  ->  existence_error(seed, Hash)
  ;   seed_dataset(Seed1.dataset, DName, Dataset),
      seed_organization(Seed1, OName, Org),
      seed_scrape(Seed1, Scrape),
      Seed2 = _{
        approach: Seed1.approach,
        dataset: Dataset,
        documents: Seed1.documents,
        hash: Hash,
        organization: Org,
        processing: false,
        scrape: Scrape
      },
      rocks_put(seedlist, Hash, Seed2)
  ).

%! organization_name(+Seed:dict, -Name:atom) is det.
organization_name(Seed, Name) :-
  get_dict(organization, Seed, Org),
  (   _{name: Name0} :< Org
  ->  true
  ;   _{url: Url} :< Org
  ->  uri_host(Url, Name0)
  ), !,
  normalize_name(Name0, Name).
organization_name(Seed, Name) :-
  _{url: Url} :< Seed,
  uri_host(Url, Name0), !,
  normalize_name(Name0, Name).
organization_name(_, noname).

%! seed_dataset(+Seed:dict, +Name:atom, -Dataset:dict) is det.
seed_dataset(Dataset1, Name, Dataset2) :-
  Dataset2 = Dataset1.put(_{name: Name}).

%! seed_organization(+Seed:dict, +Name:atom, -Organization:dict) is det.
seed_organization(Seed, Name, Org2) :-
  (get_dict(organization, Seed, Org1) -> true ; Org1 = _{}),
  Org2 = Org1.put(_{name: Name}).

%! seed_scrape(+Seed:dict, -Scrape:dict) is det.
seed_scrape(Seed, Scrape) :-
  get_time(Now),
  Interval is Now - Seed.dataset.'last-modified',
  Scrape = _{added: Now, interval: Interval, processed: 0.0}.



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
