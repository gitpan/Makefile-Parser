#: Makefile/Parser.pm
#: Implementation for Makefile::Parser
#: v0.02
#: Copyright (c) 2005 Agent Zhang
#: 2005-09-24 2005-09-24

package Makefile::Parser;

use strict;
#use Data::Dumper;

our $Debug = 0;
our $VERSION = '0.02';
our $Error;

# usage: $class->new($makefile);
sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $makefile = shift;
    $makefile = 'Makefile' unless $makefile;
    my $self = bless {
        _vars => {},
        _tars => undef,
    }, $class;
    return $self->parse($makefile);
}

# usage: $obj->parse($filename);
sub parse {
    my ($self, $file) = @_;
    $self->{_file} = $file;
    my $rvars = $self->{_vars};
    my $in;
    unless (open $in, $file) {
        $Error = "Cannot open $file for reading: $!";
        return undef;
    }

    my $state = 'S_IDLE';
    my ($var, $value, $tar, $colon_type, $depends, $cmd);
    my @cmds;
    my %tars;
    %$rvars = ();
    while (<$in>) {
        next if /^\s*#/;
        if ($Debug) {
            $tar = '' unless defined $var;
            warn "(tar: $tar) Switching to tate $state with $_";
        }
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
                $rvars->{$var} = $value;
                warn "$var <=> $value\n" if $Debug;
                $state = 'S_IDLE';
            }
            #warn "$1 * $2 * $3";
        }
        elsif ($state eq 'S_IN_VAL' and /^\s+ (.*)$/xo) {
            #warn $1;
            $value .= " $1";
            if ($value !~ s/\s+\\$//o) {
                $state = 'S_IDLE' ;
                $rvars->{$var} = $value;
                warn "$var <=> $value\n" if $Debug;
            }
        }
        elsif (($state eq 'S_IDLE' or $state eq 'S_CMD') and /^(\S[^:]*) (::?) \s* (.*)$/xo) {
            $tar = $1;
            $colon_type = $2;
            $depends = $3;
            $tar =~ s/^\s+|\s+$//;
            warn "Adding target $tar...\n" if $Debug;
            $tars{$tar} = Makefile::Target->new($tar, $colon_type);
            if ($depends =~ s/\s+\\$//o) {
                $state = 'S_IN_DEPENDS';
            } else {
                $state = 'S_CMD';
            }
            $tars{$tar}->add_depend(split /\s+/, $depends);
        }
        elsif ($state eq 'S_IN_DEPENDS' and /^\s+ (.*)$/xo) {
            $depends = $1;
            if ($depends !~ s/\s+\\$//o) {
                $tars{$tar}->add_depend(split /\s+/, $depends);
                $state = 'S_CMD';
            }
        }
        elsif ($state eq 'S_CMD' and /^\s+(.*)/o) {
            $cmd = $1;
            if ($cmd =~ s/\s+\\$//o) {
                $state = 'S_IN_CMD';
            } else {
                $tars{$tar}->add_command($cmd);
            }
        }
        elsif ($state eq 'S_IN_CMD' and /^\s+(.*)/o) {
            $cmd .= " $1";
            if ($cmd !~ s/\s+\\$//o) {
                $tars{$tar}->add_command($cmd);
                $state = 'S_CMD';
            }
        }
        #else {
        #}
    }
    $self->{_tars} = \%tars;
    warn Data::Dumper->Dump([\%tars], ['TARGETS']) if $Debug;
    close $in;
    return $self;
}

sub var {
    my ($self, $var) = @_;
    return $self->{_vars}->{$var};
}

sub target {
    my ($self, $tar) = @_;
    return $self->{_tars}->{$tar};
}

sub error {
    return $Error;
}

package Makefile::Target;

# usage: $class->new($name, $colon_type)
sub new {
    my $proto = shift;
    my $class = ref $proto || $proto;
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

Makefile::Parser - A simple Parser for Makefiles

=head1 SYNOPSIS

  use Makefile::Parser;

  # Equivalent to ->new('Makefile');
  $parser = Makefile::Parser->new or
      die Makefile::Parser->error;

  # Get last value assigned to the specified variable 'CC':
  print $parser->var('CC');

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
  my $tar = $parser->target($parser->var('EXE_FILE'));

=head1 DESCRIPTION

This is a parser for Makefiles. At this very early stage, the parser
only support a very limited set of features, so it may not do what
you expected it to do. Currently its main purpose is to provide basic
support for another module named L<GraphViz::Make>, which is aimed
to render the building processes specified by a Makefile using
the amazing GraphViz library. The L<Make> module is not satisfactory
for this purpose, so I decided to build one of my own.

I have a plan to improve this parser constantly.

=head1 The Makefile::Parser Class

This class provide the interface to the Makefile parser.

=head2 METHODS

=over

=item $class-E<gt>new(I<Makefile-name>)

It's the constructor for the Parser class. You may provide the path
of your Makefile as the argument which is default to 'Makefile'. It
is worth mentioning that the constructor will call ->parse method
internally.

=item $obj-E<gt>parse(I<Makefile-name>)

This method parse the specified Makefile (not optional). It is only
useful when you decide to parse another Makefile using the same
Parser object. Because the invocation of ->new will call this method
automatically, please don't call this method immediately after you
create the Parser object. There's no harm to do this, but you'll be
repeating the parsing process.

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

=item $obj-E<gt>target($target_name)

This method returns a Makefile::Target object with the name specified.
It will returns undef if the rules for the given target is not described
by the Makefile.

It is important not to send something like "$(MY_LIB)" as the target name.
Only raw values are acceptable. If you really want to do something like
this, please use the following code:

    my $tar = $parser->target($parser->var('MY_LIB'));

but this code will break if you have reassigned values to variable MY_LIB in
your Makefile.

=back

=head1 The Makefile::Target Class

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

=head1 SEE ALSO

L<GraphViz::Make>, L<Make>.

=head1 AUTHOR

Agent Zhang, E<lt>agent2002@126.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 Agent Zhang.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
