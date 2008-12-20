#!/usr/bin/perl -c

package Exception::Assertion;

=head1 NAME

Exception::Assertion - Thrown when assertion failed

=head1 SYNOPSIS

  use Exception::Assertion;

  sub assert_foo {
      my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
      my ($condition, $message) = @_;
      Exception::Assertion->throw(
          message => $message,
          reason  => 'foo failed',
      );
  }

  assert_foo( 0, 'assert_foo failed' );

=head1 DESCRIPTION

This class extends standard L<Exception::Base> and is thrown when assertion is
failed.

=for readme stop

=cut

use 5.006;
use strict;
use warnings;

our $VERSION = 0.04;


use Exception::Base 0.21 (
    'Exception::Assertion' => {
        has       => 'reason',
        message   => 'Unknown assertion failed',
        verbosity => 3,
        string_attributes => [ 'message', 'reason' ],
    },
);


1;


__END__

=begin umlwiki

= Class Diagram =

[                    <<exception>>
                  Exception::Assertion
 ----------------------------------------------------------
 +message : Str = "Unknown assertion failed"          {new}
 +verbosity : Int = 3                                 {new}
 +reason : Str                                        {new}
 #string_attributes : ArrayRef[Str] = ["message", "reason"]
 ----------------------------------------------------------]

[Exception::Assertion] ---|> [Exception::Base]

=end umlwiki

=head1 BASE CLASSES

=over

=item *

L<Exception::Base>

=back

=head1 CONSTANTS

=over

=item ATTRS

Declaration of class attributes as reference to hash.

See L<Exception::Base> for details.

=back

=head1 ATTRIBUTES

This class provides new attributes.  See L<Exception::Base> for other
descriptions.

=over

=item message : Str = "Unknown assertion failed" {rw}

Contains the message of the exception.  This class overrides the default value
from L<Exception::Base> class.

=item verbosity : Int = 3 {rw}

The default verbosity for assertion exception is raised to 3.  This class
overrides the default value from L<Exception::Base> class.

=item reason : Str {rw}

Contains the additional message filled by assertion method.

=item string_attributes : ArrayRef = ['message', 'reason']

Meta-attribute contains the format of string representation of exception
object.  This class overrides the default value from L<Exception::Base>
class.

=back

=head1 SEE ALSO

L<Exception::Base>, L<Test::Assertion>.

=head1 BUGS

If you find the bug, please report it.

=for readme continue

=head1 AUTHOR

Piotr Roszatycki E<lt>dexter@debian.orgE<gt>

=head1 LICENSE

Copyright (C) 2008 by Piotr Roszatycki E<lt>dexter@debian.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>
