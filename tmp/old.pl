html_seed(Seed) -->
  html([
    \header(Seed),
    \dataset(Seed.dataset),
    \documents(Seed.documents),
    \organization(Seed.organization),
    \processing(Seed.processing),
    \scrape(Seed.scrape),
    \source(Seed.source)
  ]).

dataset(Dataset) -->
  {
    ignore(get_dict(description, Dataset, Description)),
    ignore(get_dict(image, Dataset, Image)),
    ignore(get_dict(license, Dataset, License)),
    ignore(get_dict(url, Dataset, Url))
  },
  html(
    section([
      h2("Dataset"),
      dd(
        dl([
          \description(Description),
          \image(Image),
          \last_modified(Dataset.'last-modified'),
          \license(License),
          \name(Dataset.name),
          \url(Url)
        ])
      )
    ])
  ).

description(Description) -->
  {var(Description)}, !.
description(Description) -->
  html([dt("Description"),dd(Description)]).

documents(Docs) -->
  html(
    section([
      h2("Documents"),
      dd(ol(\html_maplist(html_seed_document, Docs)))
    ])
  ).

header(Seed) -->
  html(
    h1([
      Seed.organization.name,
      "/",
      Seed.dataset.name,
      " (",
      code(Seed.hash),
      ")"
    ])
  ).

image(Image) -->
  {var(Image)}, !.
image(Image) -->
  html([dt("Image"),dd(a(href=Image,img(src=Image,[])))]).

license(License) -->
  {var(License)}, !.
license(License) -->
  html([dt("License"),dd(a(href=License,code(License)))]).

name(Name) -->
  html([dt("Name"),dd(code(Name))]).

organization(Org) -->
  {
    ignore(get_dict(image, Org, Image)),
    ignore(get_dict(url, Org, Url))
  },
  html(
    section([
      h2("Organization"),
      dd(
        dl([
          \name(Org.name),
          \image(Image),
          \url(Url)
        ])
      )
    ])
  ).

source(Source) -->
  html([h2("Source"),p(code(Source))]).

html_seed_document(Doc) -->
  html(li(a(href=Doc, Doc))).

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
