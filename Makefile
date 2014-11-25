ERLDIR=erl_interface-3.7.17

IDIR=/usr/lib/erlang/lib/${ERLDIR}/include
LDIR=/usr/lib/erlang/lib/${ERLDIR}/lib

c2hs: 
	c2hs --cppopts='-I${IDIR}' Erlang/Interface.chs

example:
	ghc -L${LDIR} -lei -lerl_interface -lpthread example.hs
