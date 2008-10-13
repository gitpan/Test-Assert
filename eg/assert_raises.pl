#!/usr/bin/perl -I../lib

use Test::Assert 'assert_raises';

use Exception::Base;

assert_raises qr/test/, sub { die 'test'; }, 'regexp';

assert_raises 'Exception::Base', sub { Exception::Base->throw; }, 'string';

assert_raises ['NoSuchClass', 'Exception::Base', 'AnotherClass'], sub { Exception::Base->throw; }, 'arrayref';
