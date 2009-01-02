#!/usr/bin/perl

use 5.006;
use Module::Build;

use strict;
use warnings;

my $build = Module::Build->new(
    module_name => 'Test::Assert',
    dist_author => 'Piotr Roszatycki <dexter@debian.org>',
    license => 'perl',
    requires => {
        'perl'                    => 5.006,
        'constant::boolean'       => 0,
        'Exception::Base'         => 0.21,
    },
    optional => {
    },
    build_requires => {
        'Test::Unit::Lite'        => 0.11,
        'parent'                  => 0,
    },
    create_makefile_pl => 'traditional',
    create_readme => 1,
    recursive_test_files => 1,
    add_to_cleanup => [
        'META.yml', '*.bak', '*.gz', '*.tgz', '*.ppd', '*.deb', '*.bat',
        'Makefile.PL', 'Makefile', 'pm_to_blib', 'README', 'cover_db',
        'Debian_CPANTS.txt',
        'Test-Assert-*',
    ],
);

$build->create_build_script;
