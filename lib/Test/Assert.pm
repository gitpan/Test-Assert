#!/usr/bin/perl -c

package Test::Assert;
use 5.006;
our $VERSION = 0.01;

=head1 NAME

Test::Assert - Assertion methods for those who like JUnit.

=head1 SYNOPSIS

  package My::TestMethod;
  use base 'Test::Assert';
  sub test_method {
    my $self = shift;
    $self->assert_true(1, "pass");
    $self->assert_true(0, "fail");
  }

  package My::Test;
  use Test::Assert ':all';
  assert_true(1, "pass");
  assert_true(0, "fail");
  use Test::More;
  assert_test(sub { require_ok($module) });

  package My::Package;
  use Test::Assert 'fail';
  my $state = do_something();
  if ($state == 1) {
     # 1st state
     do_foo();
  } elsif ($state == 2) {
     # 2nd and last state
     do_bar();
  } else {
     # this shouldn't happen: assert default value
     fail("Unknown state: $state");
  }
  my $a = get_a();
  my $b = get_b();
  assert_num_not_equals(0, $b, 'Division by zero');
  my $c = $a / $b;

=head1 DESCRIPTION

This class provides a set of assertion methods useful for writing tests.  The
API is based on JUnit4 and L<Test::Unit> and the methods die on failure.

The assertion methods can be used in class which is derived from
B<Test::Assert> or used as standard Perl functions after importing them into
user's namespace.

B<Test::Assert> can also wrap standard L<Test::Simple>, L<Test::More> or other
L<Test::Builder>-based tests.

The assertions can be also used for run-time checking.

=for readme stop

=cut


use strict;
use warnings;


use Exception::Base
    '+ignore_class' => [ __PACKAGE__, 'Test::Builder' ],
    'Exception::Assertion';


use Exporter ();
*import = \&Exporter::import;
our @EXPORT_OK = grep { /^(assert_|fail)/ } keys %{*Test::Assert::};
our %EXPORT_TAGS = (all => [ @EXPORT_OK ]);


# Global and local variables required for assert_deep_equal
our %Seen_Refs = ();
our @Data_Stack;
my $DNE = bless [], 'Does::Not::Exist';


# Fails a test with the given name.
sub fail (;$$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($message, $reason) = @_;

    Exception::Assertion->throw(
        message   => $message,
        reason    => $reason,
    );
}


# Asserts that a condition is true.
sub assert_true ($;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($boolean, $message) = @_;

    $self->fail($message, 'Boolean assertion failed') unless $boolean;
}


# Asserts that a condition is false.
sub assert_false ($;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($boolean, $message) = @_;

    $self->fail($message, 'Boolean assertion failed') unless not $boolean;
}


# Asserts that a value is null.
sub assert_null ($;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($value, $message) = @_;

    $self->fail($message, "'$value' is defined") unless not defined $value;
}


# Asserts that a value is not null.
sub assert_not_null ($;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($value, $message) = @_;

    $self->fail($message, 'undef unexpected') unless defined $value;
}


# Assert that two values are equal
sub assert_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($value1, $value2, $message) = @_;

    return 1 if (not defined $value1 and not defined $value2);
    $self->fail(
        $message, 'Expected value was undef; should be using assert_null?'
    ) unless defined $value1;
    $self->fail($message, "Expected '$value1', got undef") unless defined $value2;
    if ($value1 =~ /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/ and
        $value2 =~ /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/)
    {
        no warnings 'numeric';
        $self->fail($message, 'Expected ' . (0+$value1) . ', got ' . (0+$value2)) unless $value1 == $value2;
    }
    else {
        $self->fail($message, "Expected '$value1', got '$value2'") unless $value1 eq $value2;
    }
}


# Assert that two values are not equal
sub assert_not_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
    my ($value1, $value2, $message) = @_;

    if (not defined $value1 and not defined $value2) {
        $self->fail($message, 'Both values were undefined');
    }
    return 1 if (not defined $value1 xor not defined $value2);
    if ($value1 =~ /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/ and
           $value2 =~ /^[+-]?(\d+\.\d+|\d+\.|\.\d+|\d+)([eE][+-]?\d+)?$/)
    {
        no warnings 'numeric';
        $self->fail($message, (0+$value1) . ' and ' . (0+$value2) . ' should differ') unless $value1 != $value2;
    }
    else {
        $self->fail($message, "'$value1' and '$value2' should differ") unless $value1 ne $value2;
    }
}


# Assert that two values are numerically equal
sub assert_num_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    return 1 if (not defined $value1 and not defined $value2);
    no warnings 'numeric';
    $self->fail($message, 'Expected undef, got ' . (0+$value2)) if not defined $value1;
    $self->fail($message, 'Expected ' . (0+$value1) . ', got undef') if not defined $value2;
    $self->fail($message, 'Expected ' . (0+$value1) . ', got ' . (0+$value2)) unless $value1 == $value2;
}


# Assert that two values are numerically not equal
sub assert_num_not_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    if (not defined $value1 and not defined $value2) {
        $self->fail($message, 'Both values were undefined');
    }
    return 1 if (not defined $value1 xor not defined $value2);
    no warnings 'numeric';
    $self->fail($message, (0+$value1) . ' and ' . (0+$value2) . ' should differ') unless $value1 != $value2;
}


# Assert that two strings are equal
sub assert_str_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    return 1 if (not defined $value1 and not defined $value2);
    $self->fail(
        $message, 'Expected value was undef; should be using assert_null?'
    ) unless defined $value1;
    $self->fail($message, "Expected '$value1', got undef") unless defined $value2;
    $self->fail($message, "Expected '$value1', got '$value2'") unless "$value1" eq "$value2";
}


# Assert that two strings are not equal
sub assert_str_not_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    if (not defined $value1 and not defined $value2) {
        $self->fail($message, 'Both values were undefined');
    }
    return 1 if (not defined $value1 xor not defined $value2);
    $self->fail($message, "'$value1' and '$value2' should differ") unless "$value1" ne "$value2";
}


# Assert that string matches regexp
sub assert_matches ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($regexp, $value, $message) = @_;
    $self->fail(
        $message, 'Expected value was undef; should be using assert_null?'
    ) unless defined $regexp;
    $self->fail(
        $message, 'Argument 1 to assert_matches() must be a regexp'
    ) unless ref $regexp eq 'Regexp';
    $self->fail($message, "Expected /$regexp/, got undef") unless defined $value;
    $self->fail($message, "'$value' didn't match /$regexp/") unless $value =~ $regexp;
}


# Assert that string matches regexp
sub assert_not_matches ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($regexp, $value, $message) = @_;
    $self->fail(
        $message, 'Expected value was undef; should be using assert_null?'
    ) unless defined $regexp;
    return 1 if not defined $value;
    $self->fail(
        $message, 'Argument 1 to assert_not_matches() must be a regexp'
    ) unless ref $regexp eq 'Regexp';
    $self->fail($message, "'$value' matched /$regexp/") unless $value !~ $regexp;
}


# Assert that data structures are deeply equal
sub assert_deep_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    $self->fail($message, 'Both arguments were not references') unless ref $value1 or ref $value2;
    $self->fail($message, 'Argument 1 to assert_deep_equals() must be a regexp') unless ref $value1;
    $self->fail($message, 'Argument 2 to assert_deep_equals() must be a regexp') unless ref $value2;
    local @Data_Stack = ();
    local %Seen_Refs = ();
    $self->fail(
        $message, $self->_format_stack(@Data_Stack)
    ) unless $self->_deep_check($value1, $value2);
}


# Assert that data structures are deeply equal
sub assert_deep_not_equals ($$;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($value1, $value2, $message) = @_;
    $self->fail($message, 'Both arguments were not references') unless ref $value1 or ref $value2;
    $self->fail($message, 'Argument 1 to assert_deep_equals() must be a regexp') unless ref $value1;
    $self->fail($message, 'Argument 2 to assert_deep_equals() must be a regexp') unless ref $value2;
    local @Data_Stack = ();
    local %Seen_Refs = ();
    $self->fail(
        $message, 'Both structures should differ'
    ) unless not $self->_deep_check($value1, $value2);
}


# Assert that code throws an exception
sub assert_raises ($&;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($expected, $code, $message) = @_;

    eval {
        $code->();
    };
    if ($@) {
        my $e = $@;
        if (ref $e and eval { $e->isa('Exception::Base') } ) {
            return 1 if $e->matches($expected);
        }
        else {
            if (ref $expected eq 'Regexp') {
                return 1 if "$e" =~ $expected;
            }
            elsif (ref $expected eq 'ARRAY') {
                return 1 if grep { eval { $e->isa($_) } } @{ $expected };
            }
            elsif (not ref $expected) {
                my $message = "$e";
                while ($message =~ s/\t\.\.\.propagated at (?!.*\bat\b.*).* line \d+( thread \d+)?\.\n$//s) { }
                $message =~ s/( at (?!.*\bat\b.*).* line \d+( thread \d+)?\.)?\n$//s;
                return 1 if $message eq $expected;
            }
        }
        # Rethrow an exception
        die $e;
    }
    else {
        $self->fail(
            $message, 'Expected exception was not raised'
        );
    }
}


# Assert that Test::Builder method is ok
sub assert_test (&;$) {
    # check if called as function
    my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;

    my ($code, $message) = @_;

    my $diag_message = '';
    my $ok_message = '';
    my $ok_return = 1;

    no warnings 'redefine';
    local *Test::Builder::diag = sub {
        $diag_message .= $_[1] if defined $_[1];
    };
    local *Test::Builder::ok = sub {
        $ok_message .= $_[2] if defined $_[2];
        return $ok_return = $_[1];
    };

    $code->();
    if (not $ok_return) {
        my $new_message = (defined $message ? $message : '')
                        . (defined $message && $message ne '' && $ok_message ne '' ? ': ' : '')
                        . ($ok_message =~ /\n/s ? "\n" : '')
                        . $ok_message
                        . ($ok_message ne '' && $diag_message ne '' ? ': ' : '')
                        . ($diag_message =~ /\n/s ? "\n" : '')
                        . $diag_message;
        $self->fail(
            $new_message, 'assert_test failed'
        ) unless $ok_return;
    }
    return 1;
}


# Checks if deep structures are equal
sub _deep_check {
    my ($self, $e1, $e2) = @_;

    if ( ! defined $e1 || ! defined $e2 ) {
        return 1 if !defined $e1 && !defined $e2;
        push @Data_Stack, { vals => [$e1, $e2] };
        return '';
    }

    return 1 if $e1 eq $e2;
    if ( ref $e1 && ref $e2 ) {
        my $e2_ref = "$e2";
        return 1 if defined $Seen_Refs{$e1} && $Seen_Refs{$e1} eq $e2_ref;
        $Seen_Refs{$e1} = $e2_ref;
    }

    if (ref $e1 eq 'ARRAY' and ref $e2 eq 'ARRAY') {
        return $self->_eq_array($e1, $e2);
    }
    elsif (ref $e1 eq 'HASH' and ref $e2 eq 'HASH') {
        return $self->_eq_hash($e1, $e2);
    }
    elsif (ref $e1 eq 'REF' and ref $e2 eq 'REF') {
        push @Data_Stack, { type => 'REF', vals => [$e1, $e2] };
        my $ok = $self->_deep_check($$e1, $$e2);
        pop @Data_Stack if $ok;
        return $ok;
    }
    elsif (ref $e1 eq 'SCALAR' and ref $e2 eq 'SCALAR') {
        push @Data_Stack, { type => 'REF', vals => [$e1, $e2] };
        return $self->_deep_check($$e1, $$e2);
    }
    else {
        push @Data_Stack, { vals => [$e1, $e2] };
        return '';
    }
}


# Checks if arrays are equal
sub _eq_array  {
    my ($self, $a1, $a2) = @_;
    return 1 if $a1 eq $a2;

    my $ok = 1;
    my $max = $#$a1 > $#$a2 ? $#$a1 : $#$a2;
    for (0..$max) {
        my $e1 = $_ > $#$a1 ? $DNE : $a1->[$_];
        my $e2 = $_ > $#$a2 ? $DNE : $a2->[$_];

        push @Data_Stack, { type => 'ARRAY', idx => $_, vals => [$e1, $e2] };
        $ok = $self->_deep_check($e1,$e2);
        pop @Data_Stack if $ok;

        last unless $ok;
    }
    return $ok;
}


# Checks if hashes are equal
sub _eq_hash {
    my ($self, $a1, $a2) = @_;
    return 1 if $a1 eq $a2;

    my $ok = 1;
    my $bigger = keys %$a1 > keys %$a2 ? $a1 : $a2;
    foreach my $k (keys %$bigger) {
        my $e1 = exists $a1->{$k} ? $a1->{$k} : $DNE;
        my $e2 = exists $a2->{$k} ? $a2->{$k} : $DNE;

        push @Data_Stack, { type => 'HASH', idx => $k, vals => [$e1, $e2] };
        $ok = $self->_deep_check($e1, $e2);
        pop @Data_Stack if $ok;

        last unless $ok;
    }

    return $ok;
}


# Dumps the differences for deep structures
sub _format_stack {
    my ($self, @Stack) = @_;

    my $var = '$FOO';
    my $did_arrow = 0;
    foreach my $entry (@Stack) {
        my $type = $entry->{type} || '';
        my $idx  = $entry->{'idx'};
        if( $type eq 'HASH' ) {
            $var .= "->" unless $did_arrow++;
            $var .= "{$idx}";
        }
        elsif( $type eq 'ARRAY' ) {
            $var .= "->" unless $did_arrow++;
            $var .= "[$idx]";
        }
        elsif( $type eq 'REF' ) {
            $var = "\${$var}";
        }
    }

    my @vals = @{$Stack[-1]{vals}}[0,1];

    my @vars = ();
    ($vars[0] = $var) =~ s/\$FOO/  \$a/;
    ($vars[1] = $var) =~ s/\$FOO/  \$b/;

    my $out = "Structures begin differing at:\n";
    foreach my $idx (0..$#vals) {
        my $val = $vals[$idx];
        $vals[$idx] = !defined $val ? 'undef' :
                      $val eq $DNE  ? 'Does not exist'
                                    : "'$val'";
    }

    $out .= "$vars[0] = $vals[0]\n";
    $out .= "$vars[1] = $vals[1]";

    return $out;
}


1;


__END__

=begin umlwiki

= Component Diagram =

[ Test::Assert |
  [Test::Assert {=}]
  [Exception::Assertion {=}] ]

= Class Diagram =

[                          <<utility>>
                          Test::Assert
 ------------------------------------------------------------------------------
 ------------------------------------------------------------------------------
 fail( message : Str = undef, reason : Str = undef )
 assert_true( boolean : Bool, message : Str = undef )
 assert_false( boolean : Bool, message : Str = undef )
 assert_null( value : Any, message : Str = undef )
 assert_not_null( value : Any, message : Str = undef )
 assert_equals( value1 : Defined, value2 : Defined, message : Str = undef )
 assert_not_equals( value1 : Defined, value2 : Defined, message : Str = undef )
 assert_num_equals( value1 : Num, value2 : Num, message : Str = undef )
 assert_num_not_equals( value1 : Num, value2 : Num, message : Str = undef )
 assert_str_equals( value1 : Str, value2 : Str, message : Str = undef )
 assert_str_not_equals( value1 : Str, value2 : Str, message : Str = undef )
 assert_matches( regexp : RegexpRef, value : Str, message : Str = undef )
 assert_not_matches( regexp : RegexpRef, value : Str, message : Str = undef )
 assert_deep_equals( value1 : Ref, value2 : Ref, message : Str = undef )
 assert_deep_not_equals( value1 : Ref, value2 : Ref, message : Str = undef )
 assert_raises( expected : Any, code : CoreRef, message : Str = undef )
 assert_test( code : CoreRef, message : Str = undef )                           ]

[Test::Assert] ---> [<<exception>> Exception::Assertion]

[Exception::Assertion] ---|> [Exception::Base]

=end umlwiki

=head1 EXCEPTIONS

=over

=item Exception::Assertion

Thrown whether an assertion failed.

=back

=head1 IMPORTS

By default, the class does not export its symbols.

=over

=item use Test::Assert ':all';

Imports all available symbols.

=back

=head1 METHODS

=over

=item fail([I<message> [, I<reason>]])

Immediate fail the test.  The L<Exception::Assertion> object will have set
I<message> and I<reason> attribute based on arguments.

=item assert_true(I<boolean> [, I<message>])

Checks if I<boolean> expression returns true value.

=item assert_false(I<boolean> [, I<message>])

Checks if I<boolean> expression returns false value.

=item assert_null(I<value> [, I<message>])

=item assert_not_null(I<value> [, I<message>])

Checks if I<value> is defined or not defined.

=item assert_equals(I<value1>, I<value2> [, I<message>])

=item assert_not_equals(I<value1>, I<value2> [, I<message>])

Checks if I<value1> and I<value2> are equals or not equals.  If I<value1> and
I<value2> look like numbers then they are compared with '==' operator,
otherwise the string 'eq' operator is used.

=item assert_num_equals(I<value1>, I<value2> [, I<message>])

=item assert_num_not_equals(I<value1>, I<value2> [, I<message>])

Force numeric comparition.

=item assert_str_equals(I<value1>, I<value2> [, I<message>])

=item assert_str_not_equals(I<value1>, I<value2> [, I<message>])

Force string comparition.

=item assert_matches(qr/I<pattern>/, I<value> [, I<message>])

=item assert_not_matches(qr/I<pattern>/, I<value> [, I<message>])

Checks if I<value> matches I<pattern> regexp.

=item assert_deep_equals(I<value1>, I<value2> [, I<message>])

=item assert_deep_not_equals(I<value1>, I<value2> [, I<message>])

Checks if reference I<value1> is a deep copy of reference I<value2> or not.
The references can be deep structure.  If they are different, the message will
display the place where they start differing.

=item assert_raises(I<expected>, I<code> [, I<message>])

Runs the I<code> and checks if it raises the I<expected> exception.

If raised exception is an L<Exception::Base> object, the assertion passes if
the exception B<matches> I<expected> argument (via
L<Exception::Base>-E<gt>B<matches> method).

If raised exception is not an L<Exception::Base> object, several conditions
are checked.  If I<expected> argument is a string or array reference, the
assertion passes if the raised exception is a given class.  If the argument is
a regexp, the string representation of exception is matched against regexp.

  use Test::Assert 'assert_raises';

  assert_raises( 'foo', sub { die 'foo' } );
  assert_raises( ['Exception::Base'], sub { Exception::Base->throw } );

=item assert_test(I<code> [, I<message>])

Wraps L<Test::Builder> based test function and throws L<Exception::Assertion>
if the test is failed.  The plan test have to be disabled manually.  The
L<Test::More> module imports the B<fail> method by default which conflicts
with B<Test::Assert> B<fail> method.

  use Test::Assert ':all';
  use Test::More ignore => [ '!fail' ];

  Test::Builder->new->no_plan;
  Test::Builder->new->no_ending(1);

  assert_test( sub { cmp_ok($got, '==', $expected, $test_name) } );

=back

=head1 SEE ALSO

L<Exception::Assertion>, L<Test::Unit::Lite>.

=head1 BUGS

If you find the bug, please report it.

=for readme continue

=head1 AUTHOR

Piotr Roszatycki E<lt>dexter@debian.orgE<gt>

=head1 COPYRIGHT

Copyright (C) 2008 by Piotr Roszatycki E<lt>dexter@debian.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>
