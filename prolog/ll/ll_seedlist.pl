:- module(
  ll_seedlist,
  [
    add_seed/1,       % +Seed
    clear_seedlist/0,
    seed/1,           % -Seed
    stale_seed/1      % -Seed
  ]
).

/** <module> LOD Laundromat: Seedlist

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(settings)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(rocks_ext)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- at_halt(maplist(rocks_close, [seedlist])).

:- initialization
   rocks_init(seedlist, [key(atom),merge(ll_seedlist:merge_dicts),value(term)]).

merge_dicts(partial, _, New, In, Out) :-
  merge_dicts(New, In, Out).
merge_dicts(full, _, Initial, Additions, Out) :-
  merge_dicts([Initial|Additions], Out).

:- setting(default_interval, float, 86400.0,
           "The default interval for recrawling.").





%! add_seed(+Seed:dict) is det.
%
% Seed contains the following key/value pairs:
%
%   * description(string)
%   * documents(list(atom))
%   * image(atom)
%   * license(atom)
%   * name(atom)
%   * organization(dict)
%     * name(atom)
%     * image(atom)
%   * url(atom)

add_seed(Seed1) :-
  Url{
    documents: Urls,
    name: DName0,
    organization: Org,
    source: Source
  } :< Seed1,
  get_time(Now),
  % interval
  (   catch(http_metadata_last_modified(Url, LMod), _, fail)
  ->  Interval is Now - LMod
  ;   setting(default_interval, Interval)
  ),
  uri_hash(Url, Hash),
  (   % The URL has already been added to the seedlist.
      rocks_key(seedlist, Hash)
  ->  print_message(informational, existing_seed(Url,Hash))
  ;   (Org == null -> OName0 = Source ; _{name: OName0} :< Org),
      % Normalize for Triply names.
      maplist(triply_name, [OName0,DName0], [OName,DName]),
      % prefixes
      bnode_prefix_([OName,DName], BNodePrefix),
      L1 = [
        added-Now,
        documents-Urls,
        interval-Interval,
        name-DName,
        organization-_{name: OName},
        prefixes-[_{iri: BNodePrefix, prefixLabel: bnode}],
        processed-0.0,
        url-Url
      ],
      % license
      seed_license(Seed1, L1, L2),
      dict_pairs(Seed2, Hash, L2),
      format(string(Msg), "Added seed: ~a/~a", [OName0,DName0]),
      print_message(informational, Msg),
      rocks_put(seedlist, Hash, Seed2)
  ).

bnode_prefix_(Segments, BNodePrefix) :-
  setting(bnode_prefix_scheme, Scheme),
  setting(bnode_prefix_authority, Auth),
  uri_comps(BNodePrefix, uri(Scheme,Auth,['.well-known',genid|Segments],_,_)).

seed_license(Seed, T, L) :-
  _{license: License0} :< Seed,
  (   triply_license(License0, License)
  ->  L = [license-License|T]
  ;   print_message(warning, unrecognized_license(License0)),
      L = T
  ).
seed_license(_, T, T).



%! clear_seedlist is det.

clear_seedlist :-
  rocks_clear(seedlist).



%! seed(-Seed:dict) is nondet.

seed(Seed) :-
  rocks_value(seedlist, Seed).



%! stale_seed(-Seed:dict) is det.
%
% Gives the next stale seed for processing.

stale_seed(Seed) :-
  get_time(Now),
  with_mutex(seedlist, (
    rocks_value(seedlist, Seed),
    Hash{interval: Interval, processed: Processed} :< Seed,
    Processed + Interval < Now,
    rocks_merge(seedlist, Hash, Hash{processed: Now})
  )).





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
