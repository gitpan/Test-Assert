#!/usr/bin/perl

use strict;
use warnings;

use File::Spec;
use Cwd;

BEGIN {
    unshift @INC, map { /(.*)/; $1 } split(/:/, $ENV{PERL5LIB}) if defined $ENV{PERL5LIB} and ${^TAINT};

    my $cwd = ${^TAINT} ? do { local $_=getcwd; /(.*)/; $1 } : '.';
    unshift @INC, File::Spec->catdir($cwd, 'inc');
    unshift @INC, File::Spec->catdir($cwd, 'lib');
}

use Test::Unit::Lite 0.11;
use Test::Assert;

use Exception::Base max_arg_nums => 0, max_arg_len => 200, verbosity => 4;
use Exception::Assertion verbosity => 4;

local $SIG{__WARN__} = sub { require Carp; Carp::confess( $_[0] ) };

Test::Unit::HarnessUnit->new->start('Test::Unit::Lite::AllTests');
