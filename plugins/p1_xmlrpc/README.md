# p1_xmlrpc

[![Build Status](https://travis-ci.org/processone/p1_xmlrpc.svg?branch=master)](https://travis-ci.org/processone/p1_xmlrpc) [![Coverage Status](https://coveralls.io/repos/processone/p1_xmlrpc/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/p1_xmlrpc?branch=master) [![Hex version](https://img.shields.io/hexpm/v/p1_xmlrpc.svg "Hex version")](https://hex.pm/packages/p1_xmlrpc)

Erlang XMLRPC implementation with SSL, cookies, Authentication.

This is an HTTP 1.1 compliant XML-RPC library for Erlang. It is
designed to make it easy to write XML-RPC Erlang clients and/or
servers. The library is compliant with the XML-RPC specification
published by http://www.xmlrpc.org/.

Prior to using this library you need a recent installation of
Erlang (Erlang/OTP R12).

In order to compile the library run make on top level.

You are now ready to try the client and server examples in the
examples/ directory.

Do not forget to read doc/xmlrpc.3 (or xmlrpc.txt, xmlrpc.ps,
xmlrpc.pdf) for a detailed API description.

Get the latest version of this library at
https://github.com/rds13/xmlrpc/ .

This library is a hack over 
http://ejabberd.jabber.ru/files/contributions/xmlrpc-1.13-ipr2.tgz 
to add support for custom HTTP headers, https transport and
case insensitive HTTP header support.
Custom HTTP headers allow to send cookies between client and server.

## Building

XMLRPC library can be build as follow:

    make

It is a rebar-compatible OTP application. Alternatively, you can build
it with rebar:

    rebar compile

## Development

### Test

#### Unit test

You can run eunit test with the command:

    $ rebar eunit
