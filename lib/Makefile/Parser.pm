#: Makefile/Parser.pm
#: Implementation for Makefile::Parser
#: v0.07
#: Copyright (c) 2005 Agent Zhang
#: 2005-09-24 2005-10-06

package Makefile::Parser;

use strict;
#use warnings;
#use Data::Dumper;

our $Debug = 0;
our $VERSION = '0.07';
our $Error;

# usage: $class->new;
sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $self = bless {
        _vars    => {},       # all the definitions of variables
        _tars    => undef,    # all the targets
        _default => undef,    # default target
        _depends => {},       # all the dependencies
    }, $class;
    return $self;
}

# usage: $obj->parse($filename);
sub parse {
    my ($self, $file) = @_;
    $file ||= 'Makefile';
    $self->{_file} = $file;
    my $rvars = $self->{_vars};
    my $in;
    unless (open $in, $file) {
        $Error = "Cannot open $file for reading: $!";
        return undef;
    }

    my $state = 'S_IDLE';
    my ($var, $value, $tar_name, $tar, $colon_type, $depends, $cmd);
    my @cmds;
    my %tars;
    %$rvars = ();
    my $first_tar = 1;
    while (<$in>) {
        next if /^\s*#/;
        #$tar_name = '' unless defined $var;
        #warn "(tar: $tar_name) Switching to tate $state with $_";
        #warn $state if $state ne 'S_IDLE';
        chomp;
        while (my ($key, $val) = each %$rvars) {
            s/\$\($key\)/$val/g;
        }
        if (($state eq 'S_IDLE' or $state eq 'S_CMD') and /^([A-Za-z_]\w+) \s* = \s* (.*)$/xo) {
            $var = $1;
            $value = $2 || '';
            if ($value =~ s/\s+\\$//o) {
                $state = 'S_IN_VAL' ;
            } else {
                $value =~ s/\^\\$/\\/;
                $rvars->{$var} = $value;
                #warn "$var <=> $value\n";
                $state = 'S_IDLE';
            }
            #warn "$1 * $2 * $3";
        }
        elsif ($state eq 'S_IN_VAL' and /^\s+ (.*)$/xo) {
            #warn $1;
            $value .= " $1";
            if ($value !~ s/\s+\\$//o) {
                $state = 'S_IDLE' ;
                $value =~ s/\^\\$/\\/;
                $rvars->{$var} = $value;
                #warn "$var <=> $value\n";
            }
        }
        elsif (($state eq 'S_IDLE' or $state eq 'S_CMD') and /^(\S[^:]*) (::?) \s* (.*)$/xo) {
            $tar_name = $1;
            $colon_type = $2;
            $depends = $3;
            $tar_name =~ s/^\s+|\s+$//;
            #warn "Adding target $tar_name...\n";
            $tar = Makefile::Target->new($tar_name, $colon_type);
            $tars{$tar_name} = $tar;
            if ($first_tar) {
                $self->{_default} = $tar;
                $first_tar = 0;
            }
            if ($depends =~ s/\s+\\$//o) {
                $state = 'S_IN_DEPENDS';
            } else {
                $depends =~ s/\^\\$/\\/;
                $state = 'S_CMD';
            }
            my @depends = split /\s+/, $depends;
            map { $self->{_depends}->{$_} = 1 } @depends;
            $tar->add_depend(@depends);
        }
        elsif ($state eq 'S_IN_DEPENDS' and /^\s+ (.*)$/xo) {
            $depends = $1;
            if ($depends !~ s/\s+\\$//o) {
                $depends =~ s/\^\\$/\\/;
                my @depends = split /\s+/, $depends;
                map { $self->{_depends}->{$_} = 1 } @depends;
                $tar->add_depend(@depends);
                $state = 'S_CMD';
            }
        }
        elsif ($state eq 'S_CMD' and /^\s+(.*)/o) {
            $cmd = $1;
            if ($cmd =~ s/\s+\\$//o) {
                $state = 'S_IN_CMD';
            } else {
                $tar->add_command($cmd);
            }
        }
        elsif ($state eq 'S_IN_CMD' and /^\s+(.*)/o) {
            $cmd .= " $1";
            if ($cmd !~ s/\s+\\$//o) {
                $tar->add_command($cmd);
                $state = 'S_CMD';
            }
        }
        #else {
        #}
    }
    $self->{_tars} = \%tars;
    #warn Data::Dumper->Dump([\%tars], ['TARGETS']);
    close $in;
    return $self;
}

sub var {
    my ($self, $var) = @_;
    $self->parse if !defined $self->{_file};
    return $self->{_vars}->{$var};
}

sub vars {
    my $self = shift;
    $self->parse if !defined $self->{_file};
    return keys %{$self->{_vars}};
}

sub target {
    my ($self, $tar_name) = @_;
    $self->parse if !defined $self->{_file};
    return $self->{_default} if !defined $tar_name;
    return $self->{_tars}->{$tar_name};
}

sub targets {
    my $self = shift;
    $self->parse if !defined $self->{_file};
    return values %{$self->{_tars}};
}

sub roots {
    my $self = shift;
    $self->parse if !defined $self->{_file};
    my %depends = %{$self->{_depends}};
    my %tars = %{$self->{_tars}};
    my @roots = ();
    my ($key, $val);
    while (($key, $val) = each %tars) {
        next if $depends{$key};
        push @roots, $key;
    }
    return @roots;
}

sub error {
    return $Error;
}

package Makefile::Target;

use overload '""' => sub { shift->name };

# usage: $class->new($name, $colon_type)
sub new {
    my $class = shift;
    my $self = {
        _name => shift,
        _colon_type => shift,
        _commands  => [],
        _depends => [],
    };
    return bless $self, $class;
}

sub name {
    return shift->{_name};
}

sub colon_type {
    return shift->{_colon_type};
}

sub depends {
    return @{shift->{_depends}};
}

sub add_depend {
    push @{shift->{_depends}}, @_;
}

sub commands {
    return @{shift->{_commands}};
}

sub add_command {
    push @{shift->{_commands}}, @_;
}

1;
__END__

=head1 NAME

Makefile::Parser - A Simple Parser for Makefiles

=head1 SYNOPSIS

  use Makefile::Parser;

  # Equivalent to ->new('Makefile');
  $parser = Makefile::Parser->new or
      die Makefile::Parser->error;

  # Get last value assigned to the specified variable 'CC':
  print $parser->var('CC');

  # Get all the variable names defined in the Makefile:
  @vars = $parser->vars;
  print join(' ', sort @vars);

  @roots = $parser->roots; # Get all the "root targets"
  print $roots[0]->name;

  @tars = $parser->targets;  # Get all the targets
  $tar = join("\n", $tars[0]->commands);

  # Get the default target, say, the first target defined in Makefile:
  $tar = $parser->target;

  $tar = $parser->target('install');
  # Get the name of the target, say, 'install' here:
  print $tar->name;

  # Get the dependencies for the target 'install':
  @depends = $tar->depends;

  # Access the shell command used to build the current target.
  @cmds = $tar->commands;

  # Parse another file using the same Parser object:
  $parser->parse('Makefile.old') or
    die Makefile::Parser->error;

  # Get the target who is specified by variable EXE_FILE
  $tar = $parser->target($parser->var('EXE_FILE'));

=head1 DESCRIPTION

This is a parser for Makefiles. At this very early stage, the parser
only supports a very limited set of features, so it may not do what
you expected. Currently its main purpose is to provide basic
support for another module named L<Makefile::GraphViz>, which is aimed
to render the building processes specified by a Makefile using
the amazing GraphViz library. The L<Make> module is not satisfactory
for this purpose, so I decided to build one of my own.

B<IMPORTANT!>
This stuff is highly experimental and is currently at B<ALPHA> stage, so
production use is strongly discouraged. Anyway, I have the plan to 
improve this stuff unfailingly.

=head2 SYNTAX SUPPORTED

The ultimate goal of this parser is supporting all the syntax of Win32
NMAKE and GNU MAKE. But currently just a small common set of features are
implemented:

=over

=item VARIABLES

Variable assignments and variable substitutions are now supported:

    MIN_T_FILES = $(PAT_COVER_FILES) t\optest.t t\my_perl.exe.t t\types.cod.t \
        t\catln.t t\exe2hex.t t\hex2bin.t t\bin2hex.t t\bin2asm.t t\ndisasmi.t \
        t\Idu.t t\pat_tree.t t\state_mac.t t\Idu-Util.t t\cidu.t \
        t\opname.t t\error.t t\operand.t t\01disasm.t t\02disasm.t t\03disasm.t \
        t\disasm_cover.t t\ndisasm.t
    T_FILES = t\main.cod.t t\bin2hex.exe.t t\hex2bin.exe.t $(MIN_T_FILES)
    DIRFILESEP = ^\

Currently, environments and special variables like $@, $*, and $< are left 
untouched.

=item NORMAL RULES

    $(CIDU_DLL) : C\idu.obj C\idu.def
        link /dll /nologo /debug /out:$@ /def:C\idu.def C\idu.obj

    $(CIDU_LIB) : $(CIDU_DLL)

    C\idu.obj : C\idu.c C\idu.h
        cd C
        cl /nologo /c /I . idu.c
        cd ..

    smoke : all pat_cover t\pat_cover.t \
            t/pat_cover.ast.ast
        perl util\run-smoke.pl . smoke.html
        perl txt2html.pl t\*.t t\*.ast

    clean:
        copy t\pat_cover.ast.ast.html ..\ /Y 
        $(RM_F) encoding.html encoding.pod state_mac.xml encoding.ast \
            pat_tree.ast state_mac.ast \
            main.cod pat_cover.pod pat_cover.html types.cod \
            hex2bin.exe hex2bin.obj

=back

For the list of features which will be added very soon, take a look at the L</TODO>
section.

=head1 The Makefile::Parser Class

This class provide the interface to the Makefile parser.

=head2 METHODS

=over

=item $class-E<gt>new()

It's the constructor for the Parser class. You may provide the path
of your Makefile as the argument which . It
is worth mentioning that the constructor will I<not> call ->parse method
internally, so please remember calling ->parse after you construct
the parser object.

=item $obj-E<gt>parse(I<Makefile-name>)

This method parse the specified Makefile (default to 'Makefile').

When an error occurs during the parsing procedure, ->parse will return
undef. Otherwise, a reference to Parser object itself is returned.
It is recommended to check the return value every time you call this
method. The detailed error info can be obtained by calling the ->error
method.

=item $obj-E<gt>error

It returns the error info set by the most recent failing operation, such
as a parsing failure.

=item $obj-E<gt>var($variable_name)

The var method returns the value of the given variable. Since the value of
variables can be reset multiple times in the Makefile, so what you get
is always the last value set to the variable. It's worth noting that
variable reassignment can be handled appropriately during parsing since
the whole parsing process is a one-pass operation compared to the multiple-pass
strategy used by the CPAN module L<Make>.

=item $obj-E<gt>vars

This will return all the variables defined in the Makefile. The order may be
quite different from the order they appear in the Makefile.

=item $obj-E<gt>target($target_name)

This method returns a Makefile::Target object with the name specified.
It will returns undef if the rules for the given target is not described
in the Makefile. It is worth noting that only targets with a definition
body will be considered as a I<target> here.

When $target_name is omitted, this method will return the default target,
say, the first target defined in Makefile, to the user. This can be handy
if you try to build a make tool on top of this module.

It is important not to send something like "$(MY_LIB)" as the target name.
Only raw values are acceptable. If you really want to do something like
this, please use the following code:

    my $tar = $parser->target($parser->var('MY_LIB'));

but this code will break if you have reassigned values to variable MY_LIB in
your Makefile.

=item $obj-E<gt>targets

This returns all the targets in Makefile. The order can be completely
different from the order they appear in Makefile. So the following code
will not work if you want to get the default target (the first target):

    @tars = $parser->targets;
    print $tars[0];

Please use the following syntax instead:

    print $parser->target;

The type of the returned list is an array of Makefile::Target objects.

=item $obj-E<gt>roots

The -E<gt>roots method returns the "root targets" in Makefile. The targets
which there're no other targets depends on are called the I<root targets>.
For example, I<install>, I<uninstall>, and I<veryclean>
are all root targets in the Makefile generated by the I<ExtUtils::MakeMaker>
module. On the other hand, I<clean> and I<test> are not, which may be
somewhat counterintuitive. That's because there're some other targets
depend on I<clean>, I<test>, or both.

The type of the returned list is an array of Makefile::Target objects.

=back

=head1 The Makefile::Target Class

This class overloads the "" operator so its instances can be automatically
converted to strings using their names.

=head2 METHODS

=over

=item $class-E<gt>new($target_name, $colon_type)

This is the constructor for class Makefile::Target. The first argument is the 
target name which can't be a Makefile variable, the second one is a single
colon or a double colon which is used by the rule definition in Makefile.

This method is usually called internally by the Makefile::Parser class. It
doesn't make much sense to me if the user has a need to call it manually.

=item $obj-E<gt>name

It will return the name of the current Target object.

=item $obj-E<gt>depends

You can get the list dependencies for the current target. If no dependencies are
specified in the Makefile for the target, an empty array will be returned.

=item $obj-E<gt>commands

This method returns a list of shell commands used to build the current target.
If no shell commands is given in the Makefile, an empty array will be returned.

=back

=head1 EXPORT

None by default.

=head1 CODE COVERAGE

I use L<Devel::Cover> to test the code coverage of my tests, below is the 
L<Devel::Cover> report on this module test suite.

    ---------------------------- ------ ------ ------ ------ ------ ------ ------
    File                           stmt   bran   cond    sub    pod   time  total
    ---------------------------- ------ ------ ------ ------ ------ ------ ------
    blib/lib/Makefile/Parser.pm   100.0   95.5   87.1  100.0  100.0  100.0   97.3
    Total                         100.0   95.5   87.1  100.0  100.0  100.0   97.3
    ---------------------------- ------ ------ ------ ------ ------ ------ ------

=head1 REPOSITORY

For the very latest version of this module, check out the source from
L<https://svn.openfoundry.org/makefileparser> (Subversion). There is
anonymous access to all.

=head1 TODO

The following syntax will be implemented at the first priority:

=over

=item Nmake-Style Referencing Rules:

    obj.asm :
        masm /t $<

    exe.obj :
        link /nologo $<

=item Make-Style Referencing Rules:

    %.obj : %.asm
        masm /t $<;

    %.exe : %.obj
        link /BATCH /NOLOGO $<;

=item Import environment variables

A quick example on Win32:

    C:\> set RM_F=perl -MExtUtils::Command -e rm_f
    C:\> nmake clean

=item Substitute special variables, $@, $*, and $< with their values

=back

=head1 BUGS

Please feel free to report bugs or send your wish-list
to L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Makefile-Parser>.

=head1 SEE ALSO

L<Makefile::GraphViz>.

=head1 AUTHOR

Agent Zhang, E<lt>agent2002@126.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2005 Agent Zhang.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
