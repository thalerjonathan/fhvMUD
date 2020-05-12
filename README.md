fhvMUD
=====

- text based
- dumb client: telnet
- every client can login
- server in Erlang

- inspired by the FHV
  -> NPCs roaming around: e.g. the CEO
  -> elevator is useable and is also used by the NPCs
  -> similar architecture

- add some adventure
  => what about some recursive game? like eXistenZ or Inception? for example 
   in the FHV is a room where we can log in into the same game. this would
   require an indirection in player without sockets...

Build
-----

    $ rebar3 compile

Run
---

    $ rebar3 shell --apps fhvMUD
