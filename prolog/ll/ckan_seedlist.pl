:- module(
  ckan_scrape,
  [
    ckan_scrape_init/1 % +Source
  ]
).

/** <module> LOD Laundromat: RDF datasets from CKAN

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(dcg)).
:- use_module(library(file_ext)).
:- use_module(library(http/ckan_api), []).
:- use_module(library(media_type)).

:- use_module(library(ll/ll_seedlist)).





%! ckan_add_seed(+Source:atom, +Package:dict) is det.

ckan_add_seed(Source, Package) :-
  Uri{name: DName, resources: Resources} :< Package,
  maplist(ckan_resource_url, Resources, Urls),
  % organization
  ckan_organization(Source, Package, [documents-Urls,name-DName], L1),
  % description
  ckan_description(Package, L1, L2),
  % license
  ckan_license(Package, L2, L3),
  dict_pairs(Seed, Uri, L3),
  add_seed(Source, Seed).

ckan_description(Package, T, [description-Desc|T]) :-
  _{notes: Desc0} :< Package,
  Desc0 \== null, !,
  atom_string(Desc0, Desc).
ckan_description(_, T, T).

ckan_license(Package, T, [license-License|T]) :-
  _{license_url: License} :< Package, !.
ckan_license(_, T, T).

ckan_organization(_, Package, T, [organization-Org|T]) :-
  is_dict(Package.organization), !,
  % name
  _{name: Name} :< Package.organization,
  % image
  (   _{image_url: Url} :< Package.organization
  ->  Org = _{image: Url, name: Name}
  ;   Org = _{name: Name}
  ).
ckan_organization(Source, _, T, [organization-_{name: Source}|T]).

ckan_resource_url(Resource, Url) :-
  _{url: Url} :< Resource.



%! ckan_package(+Source:atom, -Package:dict) is multi.

% DEBUG: read from file
ckan_package(File, Package) :-
  exists_file(File), !,
  call_stream_file(
    File,
    {Package}/[In]>>(
      json_read_dict(In, Packages, [value_string_as(atom)]),
      member(Package, Packages)
    )
  ).
% read from API
ckan_package(Site, Package) :-
  ckan_api:ckan_package(Site, Package).



%! ckan_package_resource(+Package:dict, -Resource:dict,
%!                       -MediaTypes:list(compound)) is multi.

ckan_package_resource(Package, Resource, MediaTypes) :-
  _{resources: Resources} :< Package,
  member(Resource, Resources),
  _{format: Format1, mimetype: Format2} :< Resource,
  aggregate_all(
    set(MediaType),
    (
      member(Format, [Format1,Format2]),
      clean_media_type(Format, MediaType)
    ),
    MediaTypes
  ).



%! ckan_rdf_package(+Source:atom, -Package:dict) is nondet.

ckan_rdf_package(Source, Package) :-
  ckan_package(Source, Package),
  (   % The dataset contains at least one RDF document.
      ckan_package_resource(Package, _, MediaTypes),
      member(MediaType, MediaTypes),
      rdf_media_type_(MediaType)
  ->  format("✓")
  ;   format("❌")
  ).



%! ckan_scrape_init(+Source:atom) is det.

ckan_scrape_init(Source) :-
  forall(
    ckan_rdf_package(Source, Package),
    ckan_add_seed(Source, Package)
  ).

rdf_media_type_(media(application/'n-quads',[])).
rdf_media_type_(media(application/'n-triples',[])).
rdf_media_type_(media(application/'rdf+xml',[])).
rdf_media_type_(media(application/trig,[])).
rdf_media_type_(media(text/turtle,[])).





% CLEANUP CODE %

clean_media_type(Format1, MediaType) :-
  downcase_atom(Format1, Format2),
  atom_strip(Format2, Format3),
  \+ memberchk(Format3, ['',null]),
  (   atom_phrase(media_type(MediaType0), Format3)
  ->  (   % known known Media Type
          media_type_media_type_(MediaType0, MediaType)
      ->  true
      ;   % known unknown Media Type
          media_type_(MediaType0)
      ->  fail
      ;   % unknown unknown Media Type
          print_message(warning, unknown_media_type(MediaType0)),
          fail
      )
  ;   % Remove the leading dot, if any.
      (   sub_atom(Format3, 0, 1, _, .)
      ->  sub_atom(Format3, 1, _, 0, Format4)
      ;   Format4 = Format3
      ),
      (   % knwon known format
          format_media_type_(Format4, MediaType)
      ->  true
      ;   % known unknown format
          format_format_(Format4, _)
      ->  fail
      ;   % unknown unknown format
          print_message(warning, unknown_format(Format4)),
          fail
      )
  ).

% Access ACCDB [Microsoft]
format_(accdb).
format_(adf).
% Application Programming Interface (API).
format_(api).
% ArcGIS [ESRI]
format_(arcgis).
% ASCII
format_(ascii).
% ASP.NET [Microsoft]
format_(asp).
% ASP.NET [Microsoft]
format_(aspx).
format_(bin).
% AutoCAD
format_(autocad).
% Biological Pathway Exchange (BioPAX) [RDF/OWL-based]
format_(biopax).
format_(creole).
format_(dat).
% Common file extension of dBase database files.
% ShapeFile-related [ESRI]
format_(dbf).
% DGN (design) is the name used for CAD file formats supported by
% Bentley Systems, MicroStation and Intergraph's Interactive Graphics
% Design System (IGDS) CAD programs.
format_(dgn).
format_(dia).
format_(document).
% Document Type Definition (DTD)
format_(dtd).
% ArcInfo interchange file (E00) [ESRI]
format_(e00).
format_(emme).
% executable
format_(exe).
% File Geodatabase (FileGDB) [ESRI]
format_(fgdb).
format_(file).
% File Transfer Protocol (FTP)
format_(ftp).
format_(gdb).
% TerraGo GeoPDF
format_(geopdf).
format_(getdata).
% Geographic Infromation System (GIS)
format_(gis).
% Git
format_(git).
% Google Doc [Google]
format_('google doc').
% Google Sheet [Google]
format_('google sheet').
% GeoPackage (GPKG) [OGC]
format_(gpkg).
% General Transit Feed Specification (GTFS)
format_(gtfs).
% Shuttle Radar Topography Mission Height (SRTM) height (HGT) file
format_(hgt).
format_(hydra).
format_(hyperlink).
format_(image).
format_(index).
format_(iso).
format_(labpal).
format_(link).
format_('linked data').
format_(log).
format_(mabxml).
format_(map).
% MARC 21: redefinition of MARC for the 21st century
format_('marc21').
% MARCXML
format_(marcxml).
% MBTiles: file format for storing map tiles in a single file;
% technically, an SQLite database.
format_(mbtiles).
format_(none).
% ODATA
format_(odata).
% Open Geospatial Consortium [OGC]
format_(ogc).
% Open Streetmap (OSM)
format_(osm).
format_(other).
% Web Ontology Language (OWL) [W3C]
format_(owl).
% Protocolbuffer Binary Format (PBF)
format_(pbf).
% PHP
format_(php).
% ShapeFile-related [ESRI]
format_(prj).
% pixel image
format_(px).
% Python
format_(py).
% QGIS
format_(qgis).
% ArcGIS-related [ESRI]
format_(qlr).
format_(qual).
% R
format_(r).
% Resource Description Framework (RDF) [W3C]
format_(rdf).
% SPSS save file
format_(sav).
format_(scraper).
format_(sdf).
format_(search).
format_(service).
% ShapeFile [ESRI]
format_(shp).
% ShapeFile-related [ESRI]
format_(shx).
format_(sid).
format_('sisis export format').
% Solr search engine?
format_(solr).
% This could be either the query string format, or one of the result
% set formats.
format_(sparql).
% (document type)
format_(spreadsheet).
% SPSS Statistics
format_(spss).
% Structured Query Language (SQL)
format_(sql).
% SQLite
format_(sqlite).
% Search/Retrieve via URL (SRU): a standard search protocol for
% Internet search queries, utilizing Contextual Query Language (CQL),
% a standard query syntax for representing queries.
format_(sru).
format_(text).
% Translation Memory eXchange (TMX) is an XML specification for the
% exchange of translation memory data between computer-aided
% translation and localization tools with little or no loss of
% critical data.
format_(tmx).
format_(tomtom).
% TopoJSON: extension of GeoJSON
format_(topojson).
% [ESRI]
format_(twf).
% Universal Resource Locator (URL)
format_(url).
format_(void).
% Web Coverage Service (WCS)
format_(wcs).
format_(webapp).
% Web Application Resource (WAR)
format_(war).
format_(webgis).
% Web Feature Service (WFS)
format_(wfs).
% Web Map Service (WMS)
format_(wms).
% Web Map Tile Service (WMTS) [OGC]
format_(wmts).
% Web Services Description Language (WSDL)
format_(wsdl).
% World Wide Web (WWW)
format_(www).
% Excel toolbars file [Microsoft]
format_(xlb).
% XML Schema Definition (XSD) [W3C]
format_(xsd).

format_format_(data, dat).
format_format_(dbase, dbf).
format_format_('esri shape', shapefile).
format_format_('esri shape file', shapefile).
format_format_('esri shape files', shapefile).
format_format_(gdocs, 'google doc').
format_format_(geopackage, gpkg).
format_format_('google spreadsheet', 'google sheet').
format_format_(googlespreadsheet, 'google sheet').
format_format_(img, image).
format_format_(multiformat, multipart).
format_format_('ogc:wfs', wfs).
format_format_('ogc:wms', wms).
% PASW (Predictive Analytics SoftWare)
format_format_(pasw, spss).
format_format_('plain text', text).
format_format_(shape, shp).
format_format_(shapefile, shp).
format_format_('sql server', sql).
format_format_(sqlitedb, sqlite).
format_format_(txt, text).
format_format_(Format, Format) :-
  format_(Format).

format_media_type_(Format, MediaType) :-
  media_type_extension(MediaType, Format).
format_media_type_('7z',                        media(application/'x-7z-compressed',[])).
format_media_type_('atom feed',                 media(application/'atom+x',[])).
format_media_type_(biopax,                      media(application/'vnd.biopax.rdf+xml',[])).
format_media_type_('bz2:nt',                    media(application/'n-triples',[])).
format_media_type_('bz2:xml',                   media(application/xml,[])).
format_media_type_(cvs,                         media(text/csv,[])).
format_media_type_('data file in excel',        media(application/'vnd.ms-excel',[])).
format_media_type_(excel,                       media(application/'vnd.ms-excel',[])).
% JSON-stat
format_media_type_('json-stat',                 media(application/json,[])).
format_media_type_(geojsom,                     media(application/'vnd.geo+json',[])).
format_media_type_(georss,                      media(application/'rss+xml',[])).
format_media_type_(geotiff,                     media(image/tiff,[])).
format_media_type_(gzip,                        media(application/gzip,[])).
format_media_type_('gzip::nquads',              media(application/'n-quads',[])).
format_media_type_('gzip:ntriples',             media(application/'n-triples',[])).
format_media_type_('html+rdfa',                 media(application/'xhtml+xml',[])).
format_media_type_(html5,                       media(text/html,[])).
% International Aid Transparency Initiative (IATI) XML
format_media_type_('iati-xml',                  media(text/xml,[])).
format_media_type_(jpg,                         media(image/jpeg,[])).
format_media_type_('json-ld',                   media(application/'ld+json',[])).
format_media_type_('microsoft access',          media(application/'vnd.ms-access',[])).
format_media_type_('microsoft access database', media(application/'vnd.ms-access',[])).
format_media_type_('microsoft excel',           media(application/'vnd.ms-excel',[])).
format_media_type_(mol,                         media(chemical/'x-mdl-molfile',[])).
format_media_type_('ms access',                 media(application/'vnd.ms-access',[])).
format_media_type_('ms access mdb',             media(application/'vnd.ms-access',[])).
format_media_type_('n-quads',                   media(application/'n-quads',[])).
format_media_type_('n-triples',                 media(application/'n-triples',[])).
format_media_type_(netcdf,                      media(application/netcdf,[])).
format_media_type_('ods format',                media(application/'vnd.oasis.opendocument.spreadsheet',[])).
format_media_type_('rdf trig',                  media(application/trig,[])).
format_media_type_('rdf+xml',                   media(application/'rdf+xml',[])).
format_media_type_('rdf-n3',                    media(text/n3,[])).
format_media_type_('rdf-turtle',                media(text/turtle,[])).
format_media_type_('rdf-xml',                   media(application/'rdf+xml',[])).
format_media_type_(rdfa,                        media(application/'xhtml+xml',[])).
format_media_type_(rdfxml,                      media(application/'rdf+xml',[])).
format_media_type_('rss + xml',                 media(application/'rss+xml',[])).
format_media_type_('sparql-query',              media(application/'sparql-query',[])).
format_media_type_('sparql-json',               media(application/'sparql-results+json',[])).
format_media_type_('tab-separated-values',      media(text/'tab-separated-values',[])).
format_media_type_('tar.gz',                    media(application/'x-tar',[])).
% typo of `tiff'.
format_media_type_(tif,                         media(image/tiff,[])).
format_media_type_('trig gzip',                 media(application/trig,[])).
format_media_type_('ttl.bz2',                   media(text/turtle,[])).
format_media_type_('ttl.bzip2',                 media(text/turtle,[])).
format_media_type_('xls (zip)',                 media(application/'vnd.ms-excel',[])).
format_media_type_('xml (zipped)',              media(text/xml,[])).
format_media_type_('zip archive',               media(application/zip,[])).
format_media_type_('zip:csv',                   media(text/csv,[])).
format_media_type_('sparql-xml',                media(application/'sparql-results+xml',[])).
format_media_type_(tab,                         media(text/'tab-separated-values',[])).
format_media_type_(tar,                         media(application/'x-tar',[])).
format_media_type_(tgz,                         media(application/'x-tar',[])).
% @tbd Generalize: `xxx.gz' has the Media Type of `xxx'.
format_media_type_('tsv.gz',                    media(text/'tab-separated-values',[])).
format_media_type_(turtle,                      media(text/turtle,[])).
% typo of `xlsx'
format_media_type_(xlxs,                        media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[])).
format_media_type_(xsl,                         media(application/'vnd.ms-excel',[])).
% typo of `xlsx'
format_media_type_(xslx,                        media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',[])).
format_media_type_(yaml,                        media(application/'x-yaml',[])).

media_type_(media(api/'linked-data',_)).
media_type_(media(api/dcat,_)).
media_type_(media(api/git,_)).
media_type_(media(api/search,_)).
media_type_(media(api/sparql,_)).
media_type_(media(application/'octet-stream',_)).
media_type_(media(application/'sql+gzip',_)).
media_type_(media(application/'vnd.lotus-1-2-3',_)).
media_type_(media(application/'vnd.lotus-approach',_)).
media_type_(media(application/'vnd.lotus-freelance',_)).
media_type_(media(application/'vnd.lotus-notes',_)).
media_type_(media(application/'vnd.lotus-organizer',_)).
media_type_(media(application/'vnd.lotus-screencam',_)).
media_type_(media(application/'vnd.lotus-wordpro',_)).
% Hierarchical Data Format (HDF)
media_type_(media(application/'x-hdf',_)).
% TROFF
media_type_(media(application/'x-troff-man',_)).
media_type_(media(application/'x-zip-compressed',_)).
media_type_(media(application/'zip+vnd.ms-excel',_)).
media_type_(media(application/download,_)).
media_type_(media(example/'*',_)).
media_type_(media(example/html,_)).
media_type_(media(example/rdf,_)).
media_type_(media(gdocs/spreadsheet,_)).
media_type_(media(index/ftp,_)).
media_type_(media(mapping/owl,_)).
media_type_(media(mapping/rdfs,_)).
media_type_(media(meta/'rdf+schema',_)).
media_type_(media(meta/'rdf-schema',_)).
media_type_(media(meta/owl,_)).
media_type_(media(meta/sitemap,_)).
media_type_(media(meta/void,_)).
media_type_(media(multipart/'form-data',_)).
media_type_(media(rdf/'xml, html, json',_)).
media_type_(media(rdf/void,_)).
media_type_(media(text/plain,_)).
media_type_(media(video/'x-msvideo',_)).

media_type_media_type_(media(Super/Sub,_), media(Super/Sub,Params)) :-
  media_type(media(Super/Sub,Params)).
media_type_media_type_(media(application/csv,L),               media(text/csv,L)).
media_type_media_type_(media(application/'gpx-xml',L),         media(application/'gpx+xml',L)).
media_type_media_type_(media(application/msexcel,L),           media(application/'vnd.ms-excel',L)).
media_type_media_type_(media(application/rar,L),               media(application/'vnd.rar',L)).
media_type_media_type_(media(application/'rdf xml',L),         media(application/'rdf+xml',L)).
media_type_media_type_(media(application/'vnd.openxmlformates-officedocument.spreadsheetml.sheet',L), media(application/'vnd.openxmlformats-officedocument.spreadsheetml.sheet',L)).
media_type_media_type_(media(application/'x-bzip',L),          media(application/'x-bzip2',L)).
media_type_media_type_(media(application/'x-bzip2',L),         media(application/'x-bzip2',L)).
media_type_media_type_(media(application/'x-gzip',L),          media(application/gzip,L)).
media_type_media_type_(media(application/'x-netcdf',L),        media(application/netcdf,L)).
media_type_media_type_(media(application/'x-nquads',L),        media(application/'n-quads',L)).
media_type_media_type_(media(application/'x-ntriples',L),      media(application/'n-triples',L)).
media_type_media_type_(media(application/'x-pdf',L),           media(application/pdf,L)).
media_type_media_type_(media(application/'x-tgz',L),           media(application/'x-tar',L)).
media_type_media_type_(media(application/'x-trig',L),          media(application/trig,L)).
media_type_media_type_(media(application/'x-turtle',L),        media(text/turtle,L)).
media_type_media_type_(media(application/'x-vnd.oasis.opendocument.spreadsheet',[]), media(application/'vnd.oasis.opendocument.spreadsheet',[])).
media_type_media_type_(media(application/xml,L),               media(text/xml,L)).
media_type_media_type_(media(application/'xml+atom',L),        media(application/'atom+x',L)).
media_type_media_type_(media(application/'xml+rdf',L),         media(application/'rdf+xml',L)).
media_type_media_type_(media(example/'application/rdf+xml',L), media(application/'rdf+xml',L)).
media_type_media_type_(media(example/'html+rdfa',L),           media(application/'xhtml+xml',L)).
media_type_media_type_(media(example/'rdf xml',L),             media(application/'rdf+xml',L)).
media_type_media_type_(media(example/'rdf+json',L),            media(application/'ld+json',L)).
media_type_media_type_(media(example/'rdf+ttl',L),             media(text/turtle,L)).
media_type_media_type_(media(example/'rdf+xml',L),             media(application/'rdf+xml',L)).
media_type_media_type_(media(example/'x-turtle',L),            media(text/turtle,L)).
media_type_media_type_(media(example/n3,L),                    media(text/n3,L)).
media_type_media_type_(media(example/ntriples,L),              media(application/'n-triples',L)).
media_type_media_type_(media(example/rdfa,L),                  media(application/'xhtml+xml',L)).
media_type_media_type_(media(example/turtle,L),                media(text/turtle,L)).
media_type_media_type_(media(html/doc,L),                      media(text/html,L)).
media_type_media_type_(media(html/rdf,L),                      media(application/'xhtml_xml',L)).
media_type_media_type_(media(kml/kmz,L),                       media(application/'vnd.google-earth.kml+xml',L)).
media_type_media_type_(media(marc/xml,L),                      media(text/xml,L)).
media_type_media_type_(media(rdf/'n-triples',L),               media(application/'n-triples',L)).
media_type_media_type_(media(rdf/'xml example',L),             media(application/'rdf+xml',L)).
media_type_media_type_(media(rdf/n3,L),                        media(text/n3,L)).
media_type_media_type_(media(rdf/nt,L),                        media(application/'n-triples',L)).
media_type_media_type_(media(rdf/turtle,L),                    media(text/turtle,L)).
media_type_media_type_(media(sparql/json,L),                   media(application/'sparql-results+json',L)).
media_type_media_type_(media(sparql/xml,L),                    media(application/'sparql-results+xml',L)).
media_type_media_type_(media(text/'comma-separated-values',L), media(text/csv,L)).
media_type_media_type_(media(text/'rdf+n3',L),                 media(text/n3,L)).
media_type_media_type_(media(text/'rdf+ttl',L),                media(text/turtle,L)).
media_type_media_type_(media(text/'x-csv',L),                  media(text/csv,L)).
media_type_media_type_(media(text/javascript,L),               media(application/javascript,L)).
media_type_media_type_(media(text/n3,L),                       media(text/n3,L)).
media_type_media_type_(media(text/sql,L),                      media(application/sql,L)).
media_type_media_type_(media(wfs/gml,L),                       media(application/'gml+xml',L)).
media_type_media_type_(media(xml/rdf,L),                       media(application/'rdf+xml',L)).
media_type_media_type_(media(zip/json,L),                      media(application/json,L)).
media_type_media_type_(media(zip/tsv,L),                       media(text/'tab-separated-values',L)).
