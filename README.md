Haskell-Erlang bridge, a la [peb](http://code.google.com/p/mypeb/).

    $ erl -sname other_node -setcookie node_cookie
    (other_node@other_host)> register(shell, self()).
    true
    (other_node@other_host)> flush().
    ok

    $ vim Makefile   # set ERLDIR=
    $ make c2hs example
    $ ./example

    (other_node@other_host)> flush().
    Shell got hi
    ok
