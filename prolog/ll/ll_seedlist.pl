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
    * image(atom)
    * license(atom)
    * name(atom)
    * url(atom)
  * documents(list(atom))
  * hash(atom)
  * organization(dict)
    * name(atom)
    * image(atom)
    * url(atom)
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
:- use_module(library(debug)).
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
  _{dataset: Dataset0, documents: Urls, 'last-modified': LMod} :< Seed0,
  _{name: DName0, url: Url} :< Dataset0,
  get_time(Now),
  % interval
  Interval is Now - LMod,
  % hash
  uri_hash(Url, Hash),
  (   % The URL has already been added to the seedlist.
      rocks_key(seedlist, Hash)
  ->  existence_error(seed, Hash)
  ;   organization_name(Url, Seed0, OName0),
      % Normalize names for Triply Cloud.
      maplist(triply_name, [OName0,DName0], [OName,DName]),
      % prefixes
      bnode_prefix_([OName,DName], BNodePrefix),
      Dataset1 = _{
        name: DName,
        prefixes: [_{iri: BNodePrefix, prefixLabel: bnode}],
        url: Url
      },
      % license
      seed_license(Seed0, Dataset1, Dataset2),
      Seed = _{
        dataset: Dataset2,
        documents: Urls,
        hash: Hash,
        organization: _{name: OName},
        processing: false,
        scrape: _{
          added: Now,
          interval: Interval,
          'last-modified': LMod,
          processed: 0.0
        }
      },
      debug(ll, "Added seed: ~a/~a", [OName0,DName0]),
      rocks_put(seedlist, Hash, Seed)
  ).

organization_name(_, Seed0, OName0) :-
  _{organization: Org} :< Seed0,
  (   _{name: OName0} :< Org
  ->  true
  ;   _{url: Url} :< Org
  ->  uri_host(Url, OName0)
  ), !.
organization_name(Url, _, OName0) :-
  uri_host(Url, OName0).

bnode_prefix_(Segments, BNodePrefix) :-
  setting(rdf_term:bnode_prefix_scheme, Scheme),
  setting(rdf_term:bnode_prefix_authority, Auth),
  uri_comps(BNodePrefix, uri(Scheme,Auth,['.well-known',genid|Segments],_,_)).

seed_license(Seed0, Dict1, Dict2) :-
  _{license: License0} :< Seed0,
  (   triply_license(License0, License)
  ->  Dict2 = Dict1.put(_{license: License})
  ;   debug(ll, "No license for ~w", [Seed0]),
      Dict2 = Dict1
  ).
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

%! triply_license(+Url:atom, -Label:string) is nondet.
%
% http://portal.opendata.dk/dataset/open-data-dk-licens
% http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
% https://creativecommons.org/licenses/by/3.0/at/deed.de

triply_license(Url, Label) :-
  license_(Prefix, Label),
  atom_prefix(Url, Prefix), !.

% non-mapped
license_('').
license_('http://data.surrey.ca/pages/open-government-licence-surrey').
license_('http://www.data.gouv.fr/license-Ouverte-Open-license').
license_('http://www.nationalarchives.gov.uk/doc/non-commercial-government-licence/').
license_('http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/').
license_('https://www.agesic.gub.uy/innovaportal/file/6327/1/licencia-de-datos-abiertos.pdf').

% mapped
license_('http://creativecommons.org/licenses/by-nc/', "CC-BY-NC").
license_('http://creativecommons.org/licenses/by-sa/', "CC-BY-SA").
license_('http://creativecommons.org/licenses/by/', "CC-BY").
license_('http://creativecommons.org/publicdomain/zero/1.0', "CC0").
license_('http://reference.data.gov.uk/id/open-government-licence', "OGL").
license_('http://www.opendefinition.org/licenses/cc-by', "CC-BY").
license_('http://www.opendefinition.org/licenses/cc-by-sa', "CC-BY-SA").
license_('http://www.opendefinition.org/licenses/cc-zero', "CC0").
license_('http://www.opendefinition.org/licenses/gfdl', "GFDL").
license_('http://www.opendefinition.org/licenses/odc-by', "ODC-BY").
license_('http://www.opendefinition.org/licenses/odc-odbl', "ODC-ODBL").
license_('http://www.opendefinition.org/licenses/odc-pddl', "ODC-PDDL").
license_('https://creativecommons.org/licenses/by-sa/3.0/', "CC-BY-SA").
license_('https://creativecommons.org/licenses/by/', "CC-BY").
license_('https://creativecommons.org/publicdomain/zero/1.0', "CC0").



%! uri_host(+Uri:atom, -Host:atom) is det.

uri_host(Uri, Host) :-
  uri_components(Uri, UriComps),
  uri_data(authority, UriComps, Auth),
  uri_authority_components(Auth, AuthComps),
  uri_authority_data(host, AuthComps, Host).



%! triply_name(+Name:atom, -TriplyName:atom) is det.
%
% Triply names can only contain alpha-numeric characters and hyphens.

triply_name(Name1, Name3) :-
  atom_phrase(triply_name_, Name1, Name2),
  atom_length(Name2, Length),
  (Length =< 40 -> Name3 = Name2 ; sub_atom(Name2, 0, 40, _, Name3)).

triply_name_, [Code] -->
  [Code],
  {code_type(Code, alnum)}, !,
  triply_name_.
triply_name_, "-" -->
  [_], !,
  triply_name_.
triply_name_--> "".





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
