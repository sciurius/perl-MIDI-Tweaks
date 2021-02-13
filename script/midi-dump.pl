#!/usr/bin/perl

# Author          : Johan Vromans
# Created On      : Wed Jun 11 16:02:35 2008
# Last Modified By: Johan Vromans
# Last Modified On: Sat Feb 13 21:20:49 2021
# Update Count    : 335
# Status          : Unknown, Use with caution!

################ Common stuff ################

use strict;

use MIDI::Tweaks::Dump;

our $VERSION = $MIDI::Tweaks::VERSION;

# Package name.
my $my_package = 'Sciurix';
# Program name and version.
my $my_name = "midi-dump";

################ Command line parameters ################

use Getopt::Long 2.13;

# Command line options.
my $rgscale = 0;
my $rgtime  = 1;		# rosegarden compliant time
my $verbose = 0;		# verbose processing

# Development options (not shown with -help).
my $debug = 0;			# debugging
my $trace = 0;			# trace (show process)
my $test = 0;			# test mode.

# Process command line options.
app_options();

# Post-processing.
$trace |= ($debug || $test);

################ Presets ################

my $TMPDIR = $ENV{TMPDIR} || $ENV{TEMP} || '/usr/tmp';

################ The Process ################

MIDI::Opus->new({from_file => $ARGV[0]})->midi_dump;

################ Command line subroutines ################

sub app_options {
    my $help = 0;		# handled locally
    my $ident = 0;		# handled locally

    # Process options, if any.

    if ( !GetOptions
	 (
	  'rgscale'	  => \$rgscale,
	  'rgtime'	  => \$rgtime,
	  'ident'	  => \$ident,
	  'verbose'	  => \$verbose,
	  'trace'	  => \$trace,
	  'help|?'	  => \$help,
	  'debug'	  => \$debug,
	 ) or $help )
    {
	app_usage(2);
    }
    app_ident() if $ident;
    app_usage(1) unless @ARGV == 1 && -f $ARGV[0] && -r _ && -s _;
}

sub app_ident {
    print STDERR ("This is $my_package [$my_name $VERSION]\n");
}

sub app_usage {
    my ($exit) = @_;
    app_ident();
    print STDERR <<EndOfUsage;
Usage: $0 [options] file
    --rgscale		scale to RoseGarden units
    --rgtime		use RoseGarden beat calculation
    --help		this message
    --ident		show identification
    --verbose		verbose information
EndOfUsage
    exit $exit if defined $exit && $exit != 0;
}

1;

=head1 NAME

midi-dump - Dump contents of MIDI files

=head1 SYNOPSIS

  midi-dump [options] file

Options:

   --rgscale		scale ticks to RoseGarden convention
   --ident		show identification
   --help		brief help message
   --verbose		verbose information

=head1 OPTIONS

=over 8

=item B<--rgscale>

RoseGarden always uses a tick unit of 960. When B<--rgscale> is used,
all times are scaled to 960 tick units.

=item B<--verbose>

More verbose information (default).

=item B<--version>

Print a version identification to standard output and exits.

=item B<--help>

Print a brief help message to standard output and exits.

=item B<--ident>

Prints a program identification.

=item I<file>

Input file, which must be a valid MIDI files.

=back

=head1 DESCRIPTION

B<This program> will read the given MIDI file and writes a readable
and understandable representation of the contents to standard output.

The format of the output is a Perl structure similiar to the one
produced by the dump function of the L<MIDI> module. The output has
been enhanced with additional information.

For example:

 MIDI::Opus->new({
  'format' => 1,
  'ticks'  => 256,
  'tracks' => [   # 5 tracks...

    # Track #1 ...
    MIDI::Track->new({
      'type' => 'MTrk',
      'events' => [  # 3 events.
        ['time_signature', 0, 4, 2, 24, 8],       #       0  001-01-00-00  Time = 4/4, Click = 24, NoteQ = 8
        ['key_signature', 0, 0, 0],               #       0  001-01-00-00  Key = C
        ['set_tempo', 0, 600000],                 #       0  001-01-00-00  Tempo: q = 100
      ]
    }),
    
    # Track #2 ...
    MIDI::Track->new({
      'type' => 'MTrk',
      'events' => [  # 346 events.
        ['control_change', 0, 0, 0, 0],           #       0  001-01-00-00
        ['control_change', 0, 0, 32, 0],          #       0  001-01-00-00
        ['patch_change', 0, 0, 52],               #       0  001-01-00-00
        ['lyric', 128, '1.If '],                  #     128  001-01-08-00
        ['note_on', 0, 0, 70, 68],                #     128  001-01-08-00  A#3 on
        ['note_off', 128, 0, 70, 0],              #     256  001-02-00-00  A#3 off
        ['lyric', 0, 'ev'],                       #     256  001-02-00-00
        ['note_on', 0, 0, 70, 75],                #     256  001-02-00-00  A#3 on
        ['note_off', 128, 0, 70, 0],              #     384  001-02-08-00  A#3 off
        ...
        ['note_off', 256, 1, 62, 0],              #   31616  031-04-08-00  D3 off
        ['note_off', 1152, 1, 60, 0],             #   32768  033-01-00-00  C3 off
      ]
    }),
    
  ]
 });

The added information is at the right side, after the C<< # >> mark.
It consists of the accumulated time so far, a timestamp in the form
measure-beat-fraction-remainder (as used e.g. by the Rosegarden
program), and some event specific details.

The output is a valid Perl structure, that can be loaded to create a
new MIDI::Opus object. See L<MIDI> for details.

=head1 REQUIREMENTS

L<MIDI> 0.80 or later.

L<MIDI::Tweaks>

=head1 AUTHOR

Johan Vromans <jvromans@squirrel.nl>

=head1 COPYRIGHT

This programs is Copyright 2008,2021 Squirrel Consultancy.

This program is free software; you can redistribute it and/or modify
it under the terms of the Perl Artistic License or the GNU General
Public License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

=cut

# Local Variables:
# compile-command: "perl -I ../lib -c midi-dump && perl pp.PL midi-dump"
# End:
