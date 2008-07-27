#! perl

my $id = "85-dump";

# Test midi-dump

use strict;
use warnings;
use MIDI;

my @dnlist;
BEGIN {
    @dnlist
      = ( qw(24 34 44 54 64 74 84 94),
	  qw(28 38 48 58 68 78 88 98 ),
	);
}

use constant INLINE => 0;

use Test::More tests => scalar(@dnlist) + INLINE;
use File::Spec;
-d "t" && chdir "t";
require "tools.pl";

# Get platform-independent file names.
my $dumper = File::Spec->catfile("../blib/script", "midi-dump");
require_ok($dumper) if INLINE;
#midi_rgscale(1);

for my $dn ( @dnlist ) {
    my $t = $id . $dn;

  SKIP: {

    skip "No test for $dn", 1 unless -s "$t.mid" && -s "$t.ref";

    my @cln = ("$t.out");
    unlink(@cln);

    if ( INLINE ) {
	open(my $fh, '>', "$t.out");
	select($fh);
	midi_dump(MIDI::Opus->new({ from_file => "$t.mid" }));
    }
    else {
	system("$^X $dumper $t.mid > $t.out");
    }

    if ( differ("$t.out", "$t.ref", 1) ) {
	fail("compare $dn");
    }
    else {
	pass("compare $dn");
	# Cleanup.
	unlink(@cln);
    }
  }
}
