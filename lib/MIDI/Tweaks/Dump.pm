#! perl

package MIDI::Tweaks::Dump;

use warnings;
use strict;

=head1 NAME

MIDI::Tweaks::Dump - Dump enhancements to MIDI.pm.

=head1 SYNOPSIS

    use MIDI::Tweaks::Dump;
    $opus->midi_dump;

=head1 DESCRIPTION

This module provides an enhanced dump facility for MIDI data, as
contained in a MIDI Opus structure.

The dump is very similar to MIDI::Opus::dump with options
C<< dump_tracks => 1, flat => 0 >>, but each line contains additional
comments to explain what the data is.

For example, some lines from MIDI::Opus::dump:

    MIDI::Track->new({
      'type' => 'MTrk',
      'events' => [  # 248 events.
        ['track_name', 0, 'Guitar'],
        ['patch_change', 0, 0, 27],
        ['note_on', 0, 0, 45, 90],
        ['note_on', '211.2', 0, 52, 80],
        ['note_on', '134.4', 0, 57, 80],
        ['note_on', '76.80', 0, 60, 80],
        ['note_on', '153.6', 0, 64, 90],

The same lines from midi_dump:

    MIDI::Track->new({
      'type' => 'MTrk',
      'events' => [  # 248 events.
                                                  #       0  001-01-00-00  Time = 6/8, Click = 24, NoteQ = 8
        ['track_name', 0, 'Guitar'],              #       0  001-01-00-00
        ['patch_change', 0, 0, 27],               #       0  001-01-00-00  Channel 1, patch = Electric Guitar(clean)
        ['note_on', 0, 0, 45, 90],                #       0  001-01-00-00  A1
        ['note_on', '211.2', 0, 52, 80],          #     211  001-01-17-35  E2
        ['note_on', '134.4', 0, 57, 80],          #     345  001-02-04-45  A2
        ['note_on', '76.80', 0, 60, 80],          #     422  001-02-11-10  C3
        ['note_on', '153.6', 0, 64, 90],          #     576  002-01-00-00  E3

The dump is written to the currently selected file handle, usually
standard output.

=cut

################ Common stuff ################

use MIDI;
use MIDI::Tweaks ();

our $VERSION = $MIDI::Tweaks::VERSION;

################ Parameters ################

my $rgscale = 0;
my $rgtime  = 1;		# rosegarden compliant time

################ The Process ################

use constant EV_TYPE => 0;
use constant EV_TIME => 1;
use constant EV_NOTE_PITCH => 3;
use constant EV_NOTE_VELO => 4;
use constant EV_MARKER_NAME => 2;

# Note that qw() does not like # in the strings. Use split().
my @keys   = (split(/ /, "C G D A E B F# C# Cb Gb Db Ab Eb Bb F"));
my @snotes = (split(/ /, "C C# D D# E F F# G G# A A# B"));
my @fnotes = (split(/ /, "C Db D Eb E F Gb G Ab A Bb B"));
my $notes = \@snotes;
my $ticks;			# ticks per 1/4 note.

sub drumset {
    my ( $kit ) = @_;
    # Drum kit names (GM + GS).
    return "Standard Drum Kit"  if $kit ==  0;
    return "Room Drum Kit"      if $kit ==  8;
    return "Power Drum Kit"     if $kit == 16;
    return "Electric Drum Kit"  if $kit == 24;
    return "TR808 Drum Kit"     if $kit == 25;
    return "Jazz Drum Kit"      if $kit == 32;
    return "Brush Drum Kit"     if $kit == 40;
    return "Orchestra Kit"      if $kit == 48;
    return "SFX Kit"            if $kit == 56;
    return "CM-64/CM-32L Kit"   if $kit == 127;
    return "$kit";
}

sub MIDI::Opus::midi_dump {
    my ( $op, $options ) = @_;

    $rgtime = $options->{rgtime} if exists $options->{rgtime};
    $rgscale = $options->{rgscale} if exists $options->{rgscale};

    no warnings 'redefine';
    *MIDI::Event::dump = \&event_dump;
    *MIDI::Track::dump = \&track_dump;
    *MIDI::Opus::dump  = \&opus_dump;

    $ticks = $op->ticks;		# ticks per 1/4 note
    $rgscale = 960/$ticks if $rgscale;

    # Defaults time signature according to the MIDI spec.
    init_time_signature(1);
    set_time_signature(0, 4, 2, 24, 8);

    $op->dump({ dump_tracks => 1, flat => 0});
}

sub midi_rgscale {
    $rgscale = shift;
}

################ Convenience routines ################

sub is_note_event {
    my ($e) = shift;
    $e->[EV_TYPE] =~ /^note_o(n|ff)$/;
}

sub is_note_on {
    my ($e) = shift;
    $e->[EV_TYPE] eq 'note_on' && $e->[EV_NOTE_VELO];
}

sub is_note_off {
    my ($e) = shift;
    $e->[EV_TYPE] eq 'note_off'
      || $e->[EV_TYPE] eq 'note_on' && !$e->[EV_NOTE_VELO];
}

################ MIDI::* overriding subroutines ################

my $etime;			# cumulative event time

sub event_dump {		# MIDI::Event::dump

    my @event = ref($_[0]) ? @{ $_[0] } : @_;
    $event[1] *= $rgscale if $rgscale;

    my $t = "[" . MIDI::_dump_quote(@event) . "],";
    $etime += $event[EV_TIME];
    check_time_signature($etime);

    my $extra = "";

    if ( $event[EV_TYPE] eq 'time_signature' ) {
	set_time_signature($etime, @event[2..5]);
	$extra = sprintf("Time = %d/%d, Click = %d, NoteQ = %d",
			 $event[2], 2**($event[3]), $event[4], $event[5]);
    }

    elsif ( $event[EV_TYPE] eq 'key_signature' ) {
	$extra = sprintf("Key = %s%s",
			 $keys[$event[2]], $event[3] eq 'minor' ? "m" : "");
	$notes = $event[2] < 0 ? \@fnotes : \@snotes;
    }

    elsif ( $event[EV_TYPE] eq 'set_tempo' ) {
	# Number of microseconds per quarter note.
	my $qpm = sprintf("%.2f", 60000000 / $event[2]);
	$qpm =~ s/[,.]00$//;
	$extra = "Tempo: q = " . $qpm;
    }

    elsif ( $event[EV_TYPE] eq 'patch_change' ) {
	$extra = sprintf("Channel %d, patch = %s",
			 $event[2]+1,
			 $event[2] == 9 ? drumset($event[3])
			 : $MIDI::number2patch{$event[3]});
    }

    elsif ( $event[EV_TYPE] eq 'control_change' ) {
	if ( defined $MIDI::controllers[$event[3]] ) {
	    $extra = $MIDI::controllers[$event[3]] . ' = '
	      . $event[4];
	}
    }

    elsif ( is_note_event(\@event) ) {
	if ( $event[2] == 9 ) {
	    if ( is_note_on(\@event) ) {
		$extra = $MIDI::notenum2percussion{$event[3]};
	    }
	    else {
		$extra = "(Off: " . $MIDI::notenum2percussion{$event[3]} . ")";
	    }
	}
	else {
	    $extra = is_note_on(\@event) ? "On:  " : "Off: ";
	    $extra .= sprintf("%s%d",
			      $notes->[$event[3]%12], int($event[3]/12)-2);
	}
    }

    $extra = "  ".$extra if $extra;
    printf("        %-40s  #%8d  %s%s\n",
	   $t, $etime, scalar(timestamp($etime)), $extra);
}

# This is copied from MIDI::Track, with one change (see #JV comment).
sub track_dump {       # MIDI::Track::dump

  my $this = $_[0];
  my $options_r = ref($_[1]) eq 'HASH' ? $_[1] : {};
  my $type = $this->type;

  my $indent = '    ';
  my @events = $this->events;
  print(
	$indent, "MIDI::Track->new({\n",
	$indent, "  'type' => ", &MIDI::_dump_quote($type), ",\n",
	defined($this->{'data'}) ?
	  ( $indent, "  'data' => ",
	    &MIDI::_dump_quote($this->{'data'}), ",\n" )
	  : (),
	$indent, "  'events' => [  # ", scalar(@events), " events.\n",
       );

  init_time_signature();	# JV

  foreach my $event (@events) {
    &MIDI::Event::dump(@$event);
    # was: print( $indent, "    [", &MIDI::_dump_quote(@$event), "],\n" );
  }
  print( "$indent  ]\n$indent}),\n$indent\n" );
  return;
}

# This is copied from MIDI::Opus, with one change (see #JV comment).
sub opus_dump {		      # MIDI::Opus::dump

  my $this = $_[0];
  my %info = $this->info();
  my $options_r = ref($_[1]) eq 'HASH' ? $_[1] : {};

  if($options_r->{'flat'}) { # Super-barebones dump mode
    my $d = $options_r->{'delimiter'} || "\t";
    foreach my $track ($this->tracks) {
      foreach my $event (@{ $track->events_r }) {
	print( join($d, @$event), "\n" );
      }
    }
    return;
  }

  print "MIDI::Opus->new({\n",
    "  'format' => ", &MIDI::_dump_quote($this->{'format'}), ",\n";
  if ( $rgscale ) {		# JV
      print "  'ticks'  => 960,\t\t# was: ", &MIDI::_dump_quote($this->{'ticks'}), "\n";
  }
  else {
      print "  'ticks'  => ", &MIDI::_dump_quote($this->{'ticks'}), ",\n";
  }

  my @tracks = $this->tracks;
  if( $options_r->{'dump_tracks'} ) {
    print "  'tracks' => [   # ", scalar(@tracks), " tracks...\n\n";
    foreach my $x (0 .. $#tracks) {
      my $track = $tracks[$x];
      print "    # Track \#", $x+1, " ...\n"; # JV
      if(ref($track)) {
        $track->dump($options_r);
      } else {
        print "    # \[$track\] is not a reference!!\n";
      }
    }
    print "  ]\n";
  } else {
    print "  'tracks' => [ ],  # ", scalar(@tracks), " tracks (not dumped)\n";
  }
  print "});\n";
  return 1;
}

################ Subroutines ################

my $bpm;			# beats per measure
my $tpm;			# ticks per measure
my $bnd;			# duration of beat note
my $frac;			# number of 1/64 notes per beat
my $lts_bar;			# bar of last time sig change
my $lts_time;			# time of last time sig change

my @tsigs;			# track of time signatures
my $csig;			# current time signature

sub timestamp {
    return "" unless $tpm;	# no time sig seen
    return rg_timestamp(@_) if $rgtime;

    my $d = $_[0];
    my $b1 = $tpm / $bpm;
    return sprintf("d=%d, b1=%d, bpm=%d, tpm=%d, bnd=%d, $frac=%.2f",
	    $d, $b1, $bpm, $tpm, $bnd, $frac);
    my @ret = duration($_[0]-$lts_time, $_[1], $_[2], $_[3]);
    $ret[0] += $lts_bar;

    # Measures and beats count from 1.
    $ret[0]++;
    $ret[1]++;

    return @ret if wantarray;
    sprintf("%03d-%02d-%02d-%02.0f", @ret);
}

sub duration {
    (0,0,0,0);
}

################ Rosegarden compliant time subroutines ################

# From: "Chris Cannam" <cannam@all-day-breakfast.com>
# Subject: Re: 33840 -> 024-02-08-00
# Date: Thu, 12 Jun 2008 15:14:14 +0100
#
# On Thu, Jun 12, 2008 at 2:27 PM, D. Michael McIntyre
# <michael.mcintyre@rosegardenmusic.com> wrote:
# > On Thursday 12 June 2008, Johan Vromans wrote:
# >> Any pointers to where this is documented?
# >
# > I never documented it because I don't really understand it myself.  I just
# > muddle along semi-randomly.
# >
# > Hopefully Chris can offer the real explanation.
#
# Oh no!  The pressure is on!  Will I be able to remember what it's all
# intended to mean?
#
# OK, these four values MMM-BB-XX-YY are referred to in the code as
# "bar", "beat", "fraction", and "remainder".
#
# "Bar" is the bar or measure number as you would expect.
#
# "Beat" is the count of beats within the bar.  The total number of
# beats is defined by the time signature, although it is not always
# equal to the numerator of the time sig (e.g. in 6/8 it is 2).
#
# "Fraction" is the count of hemidemisemiquavers (sixty-fourth notes)
# within the beat.  In 4/4 there are 16 of these per beat, but for other
# time signatures that will vary.
#
# "Remainder" is the count of "Rosegarden's basic note timing units"
# within the hemidemisemiquaver.  The base timing unit is 1/960 of a
# crotchet (quarter note), so there are 60 of these for each increment
# of the fraction counter.
#
# So, it's bars; beats (according to time signature); 64th-notes; and
# then whatever finer timing Rosegarden may happen to have a record of.

# From: "Chris Cannam" <cannam@all-day-breakfast.com>
# Subject: Re: [Rosegarden-user] 33840 -> 024-02-08-00
# Date: Thu, 12 Jun 2008 20:40:00 +0100
#
# On Thu, Jun 12, 2008 at 8:21 PM, Johan Vromans <jvromans@squirrel.nl> wrote:
# > I create a piece in 6/8, and generate midi, this will have tick = 480
#
# Ah, introducing MIDI export complicates the matter.
#
# The timebase used for MIDI export is different from the internal
# resolution.  The original intention, I imagine, was to make the MIDI
# export timebase configurable, but as it happens it is currently
# hardcoded to 480 ppq -- only half the internal resolution.
#
# > Since it is 6/8, there are 2 beats per measure, so the number of ticks
# > per measure is 2 * 480 = 960.
#
# Again, this is not the same in MIDI export.  MIDI timebase is always
# per quarter note, not per beat, and there are three quarter notes in a
# 6/8 bar, which is where your 1440 comes from.
#
# Rosegarden doesn't "think" in MIDI time (or MIDI anything much) internally.
#
# >> "Fraction" is the count of hemidemisemiquavers (sixty-fourth notes)
# >> within the beat.  In 4/4 there are 16 of these per beat, but for other
# >> time signatures that will vary.
# >
# > In 6/8, this will be 24, right?
#
# Right -- as far as display in the event list (or transport window in
# musical time mode) is concerned.


sub init_time_signature {
    my $reset = shift;
    $lts_bar = 0;
    $lts_time = 0;
    $etime = 0;
    $csig = 0;
    @tsigs = () if $reset;
}

sub check_time_signature {
    my $et = shift;

    while ( 1 ) {
	return if $csig >= @tsigs;
	my ($t, $nn, $dd, $cc, $bb) = @{$tsigs[$csig]};
	return if $et < $t;
	set_time_signature($t, $nn, $dd, $cc, $bb);
	printf("%s#%8d  %s  Time = %d/%d, Click = %d, NoteQ = %d\n",
	       " " x 50, $t,
	       scalar(timestamp($t)),
	       $nn, 2**$dd, $cc, $bb) if $csig;
	$csig++;
    }
    return 1;
}

sub set_time_signature {
    my ($t, $nn, $dd, $cc, $bb) = @_;

    # Register where the time sig happened.
    my @a = rg_duration($t-$lts_time);
    # By definition, time sig implies a new bar.
    $a[0]++ if $a[1] || $a[2] || $a[3];
    $lts_bar += $a[0];
    $lts_time = $t;

    if ( $csig < @tsigs && $tsigs[$csig]->[0] == $t ) {
	$tsigs[$csig] = [$t, $nn, $dd, $cc, $bb];
    }
    else {
	push(@tsigs, [$t, $nn, $dd, $cc, $bb]);
	$csig++;
    }

    if ( $nn > 5 && $nn % 3 == 0 ) { # compound signature
	$bpm = $nn / 3;
	$tpm = ($nn / 2**$dd) * 4 * ($rgscale ? 960 : $ticks); # midi always ticks qn
	$bnd = $tpm / $bpm;
	$frac = 3 * (64 / 2**$dd);
    }
    else {			# simple time signature
	$bpm = $nn;
	$tpm = $bpm * ($rgscale ? 960 : $ticks) / 2**($dd-2);
	$bnd = $tpm / $bpm;
	$frac = 64 / 2**$dd;
    }
#    print("time sig: bpm = $bpm, tpm = $tpm, frac = $frac, bnd = $bnd, csig=$csig\n");
}

sub rg_duration {
    my ($t) = @_;

    # The time signature must be issued in the first track, at time 0.
    return ( 0, 0, 0, 0 ) unless $t;	# time = 0
    return ( 0, 0, 0, 0 ) unless $tpm;	# no time sig seen

    # 1408 = 1*1024 + 1*256 + 8*16 + 0

    my $bar = int($t / $tpm);
    $t %= $tpm;

    my $beat = int($t / $bnd);
    $t %= $bnd;

    my $fraction = int(($t / $bnd) * $frac);

    $t = sprintf("%.0f", (((($t / $bnd) * $frac) - $fraction) * 60));

    return ($bar, $beat, $fraction, $t) if wantarray;
    sprintf("%03d-%02d-%02d-%02.0f", $bar, $beat, $fraction, $t);
}

sub rg_timestamp {
    return ( 0, 0, 0, 0 ) unless $tpm;	# no time sig seen

    my @ret = rg_duration($_[0]-$lts_time, $_[1], $_[2], $_[3]);
    $ret[0] += $lts_bar;

    # Measures and beats count from 1.
    $ret[0]++;
    $ret[1]++;

    return @ret if wantarray;
    sprintf("%03d-%02d-%02d-%02.0f", @ret);
}

=head1 AUTHOR

Johan Vromans <jvromans@squirrel.nl>

=head1 COPYRIGHT

This programs is Copyright 2008 Squirrel Consultancy.

This program is free software; you can redistribute it and/or modify
it under the terms of the Perl Artistic License or the GNU General
Public License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

=cut

INIT {
    $MIDI::controllers[  0] = "Bank Select";
    $MIDI::controllers[  1] = "Modulation";
    $MIDI::controllers[  2] = "Breath Controller";

    $MIDI::controllers[  4] = "Foot Controller";
    $MIDI::controllers[  5] = "Portamento Time";
    $MIDI::controllers[  6] = "Data Entry (MSB)";
    $MIDI::controllers[  7] = "Main Volume";
    $MIDI::controllers[  8] = "Balance";

    $MIDI::controllers[ 10] = "Pan";
    $MIDI::controllers[ 11] = "Expression Controller";
    $MIDI::controllers[ 12] = "Effect Control 1";
    $MIDI::controllers[ 13] = "Effect Control 2";

    $MIDI::controllers[ 15+$_ ] = "General-Purpose Controller $_" for 1..4;

    $MIDI::controllers[ 32+$_ ] = "LSB for controller $_" for 0 .. 31;

    $MIDI::controllers[ 64] = "Damper pedal (sustain)";
    $MIDI::controllers[ 65] = "Portamento";
    $MIDI::controllers[ 66] = "Sostenuto";
    $MIDI::controllers[ 67] = "Soft Pedal";
    $MIDI::controllers[ 68] = "Legato Footswitch";
    $MIDI::controllers[ 69] = "Hold 2";
    $MIDI::controllers[ 70] = "Sound Controller 1 (default: Timber Variation)";
    $MIDI::controllers[ 71] = "Sound Controller 2 (default: Timber/Harmonic Content)";
    $MIDI::controllers[ 72] = "Sound Controller 3 (default: Release Time)";
    $MIDI::controllers[ 73] = "Sound Controller 4 (default: Attack Time)";

    $MIDI::controllers[ 68+$_ ] = "Sound Controller $_" for 6 .. 10;

    $MIDI::controllers[ 75+$_ ] = "General-Purpose Controller $_" for 5 .. 8;

    $MIDI::controllers[ 84] = "Portamento Control";

    $MIDI::controllers[ 91] = "Effects 1 Depth (formerly External Effects Depth)";
    $MIDI::controllers[ 92] = "Effects 2 Depth (formerly Tremolo Depth)";
    $MIDI::controllers[ 93] = "Effects 3 Depth (formerly Chorus Depth)";
    $MIDI::controllers[ 94] = "Effects 4 Depth (formerly Celeste Detune)";
    $MIDI::controllers[ 95] = "Effects 5 Depth (formerly Phaser Depth)";
    $MIDI::controllers[ 96] = "Data Increment";
    $MIDI::controllers[ 97] = "Data Decrement";
    $MIDI::controllers[ 98] = "Non-Registered Parameter Number (LSB)";
    $MIDI::controllers[ 99] = "Non-Registered Parameter Number (MSB)";
    $MIDI::controllers[100] = "Registered Parameter Number (LSB)";
    $MIDI::controllers[101] = "Registered Parameter Number (MSB)";

    $MIDI::controllers[120+$_] = "Mode Message $_" for 1 .. 7;

    # Additional drum sounds. GS. Not official GM.
    @MIDI::notenum2percussion{25 .. 34} = (
      "Snare Roll", "Finger Snap", "High Q", "Slap", "Scratch Push",
      "Scratch Pull", "Sticks", "Square Click", "Metronome Click", "Metronome Bell",
    );
    @MIDI::notenum2percussion{82 .. 87} = (
      "Shaker", "Jingle Bell", "Belltree", "Castanets", "Mute Surdo", "Open Surdo",
    );
    %MIDI::percussion2notenum = reverse %MIDI::notenum2percussion;
}

=head1 AUTHOR

Johan Vromans, C<< <jvromans at squirrel.nl> >>

=head1 SEE ALSO

L<MIDI>, L<MIDI::Opus>, L<midi-dump>, L<midi-tweak>.

=head1 ISSUES AND SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc MIDI::Tweaks::Dump

This module is part of MIDI::Tweaks. Its development is hosted on GitHub:
L<https://github.com/sciurius/perl-MIDI-Tweaks>. Feel free to fork and
contribute.

Please report issues on the bug tracker on GitHub.

=head1 COPYRIGHT & LICENSE

Copyright 2008,2017,2021 Johan Vromans, Squirrel Consultancy. All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;
