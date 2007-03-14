use strict;
use inc::Module::Install;

name            ('Makefile-Parser');
all_from        ('lib/Makefile/Parser.pm');

install_script  ('script/plmake');

requires        ('Text::Balanced');
requires        ('List::MoreUtils');
requires        ('File::Spec');
requires        ('Cwd');

build_requires  ('Test::More' => 0.66);

auto_install;
WriteAll;

