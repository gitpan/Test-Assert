#!/usr/bin/perl -c

package Exception::Assertion;
use 5.006;
our $VERSION = 0.01;

=head1 NAME

Exception::Assertion - Thrown when assertion failed

=head1 SYNOPSIS

  use Exception::Base 'Exception::Assertion';

  sub assert_foo {
      my $self = eval { $_[0]->isa(__PACKAGE__) } ? shift : __PACKAGE__;
      my ($condition, $message) = @_;
      Exception::Assertion->throw(
          message => $message,
          reason  => 'foo failed',
      );
  }

  assert_foo(0, 'assert_foo(0)');

=head1 DESCRIPTION

This class extends standard L<Exception::Base> and is thrown when assertion is
failed.

=for readme stop

=cut


use Exception::Base 0.19
    'Exception::Assertion' => {
        has       => 'reason',
        message   => 'Unknown assertion failed',
        verbosity => 3,
        stringify_attributes => [ 'message', 'reason' ],
    };

1;


__END__

=begin umlwiki

= Class Diagram =

[                  <<exception>>
                Exception::Assertion
 -------------------------------------------------
 +message : Str = "Unknown assertion failed" {new}
 +verbosity : Int = 3                        {new}
 +reason : Str                               {new}
 -------------------------------------------------]

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

=item message (rw, default: 'Unknown assertion failed')

Contains the message of the exception.  This class overrides the default value
from L<Exception::Base> class.

=item verbosity (rw, default: 3)

The default verbosity for assertion exception is raised to 3.  This class
overrides the default value from L<Exception::Base> class.

=item reason (rw)

Contains the additional message filled by assertion method.

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
