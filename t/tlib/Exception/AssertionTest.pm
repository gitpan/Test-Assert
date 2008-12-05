package Exception::AssertionTest;

use strict;
use warnings;

use base 'Test::Unit::TestCase';
use Test::Assert ':all';

use Exception::Assertion;

sub test___isa {
    my $obj = Exception::Assertion->new;
    assert_not_null($obj);
    assert_true($obj->isa("Exception::Assertion"), '$obj->isa("Exception::Assertion")');
    assert_true($obj->isa("Exception::Base"), '$obj->isa("Exception::Base")');
}

sub test_attribute {
    my $obj = Exception::Assertion->new(message=>'Message', reason=>'Reason');
    assert_equals('Message', $obj->{message});
    assert_equals('Reason', $obj->{reason});
}

sub test_accessor {
    my $obj = Exception::Assertion->new(message=>'Message', reason=>'Reason');
    assert_equals('Message', $obj->message);
    assert_equals('New message', $obj->message = 'New message');
    assert_equals('New message', $obj->message);
    assert_equals('Reason', $obj->reason);
    assert_equals('New reason', $obj->reason = 'New reason');
    assert_equals('New reason', $obj->reason);
}

sub test_to_string {
    my $obj = Exception::Assertion->new(message=>'Message', reason=>'Reason');
    assert_not_null($obj);
    assert_true($obj->isa("Exception::Assertion"), '$obj->isa("Exception::Assertion")');
    assert_true($obj->isa("Exception::Base"), '$obj->isa("Exception::Base")');
    $obj->{verbosity} = 0;
    assert_equals('', $obj->to_string);
    $obj->{verbosity} = 1;
    assert_equals("Message: Reason\n", $obj->to_string);
    $obj->{verbosity} = 2;
    assert_matches(qr/Message: Reason at .* line \d+.\n/s, $obj->to_string);
    $obj->{verbosity} = 3;
    assert_matches(qr/Exception::Assertion: Message: Reason at .* line \d+\n/s, $obj->to_string);
}

1;
