:- module(
  create_password_file,
  [
    create_password_file/2 % +User, +Password
  ]
).

/** <module> Create a password file for basic authentication

@author Wouter Beek
@version 2018
*/

:- use_module(library(http/http_authenticate)).
:- use_module(library(settings)).

:- use_module(library(ll/ll_seedlist)).





%! create_password_file(+User:atom, +Password:atom) is det.

create_password_file(User, Password) :-
  setting(ll_seedlist:password_file, File),
  http_write_passwd_file(File, [passwd(User,Password,[])]).
