use strict;
use inc::Module::Install;
my $plmake = "script/plmake";

name            ('Makefile-Parser');
version_from    ('lib/Makefile/Parser.pm');
abstract       ( 'Parser for Makefiles' );
author          ('Agent Zhang <agent2002@126.com>');
license         ('perl');
#install_script  ($plmake);
#requires        ('Data::Dumper');
build_requires  ('Test::More');
#build_requires  ('File::Compare');

&WriteAll;
