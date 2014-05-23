#!/bin/sh
# -*- cperl -*-

case $* in (''|--*) exec perl -x "$0" "$@" ;; (???*) ;; (*) exit 0 ;; esac
grep -aiF "$*" "${XDG_CONFIG_HOME:-$HOME/.config}/nottoomuch/addresses.active"
case $? in 0|1) exit 0; esac
exit $?

# $ nottoomuch-addresses.sh $
#
# Created: Thu 27 Oct 2011 17:38:46 EEST too
# Last modified: Sat 29 Mar 2014 17:12:14 +0200 too

# Add this to your notmuch elisp configuration file:
#
# (require 'notmuch-address)
# (setq notmuch-address-command "/path/to/nottoomuch-addresses.sh")
# (notmuch-address-message-insinuate)

# Documentation at the end. Encoding: utf-8.

#!perl
# line 24
# - 24 -^

# HISTORY
#
# Version 2.2  2014-03-29 15:12:14 UTC
#   * In case there is both {phrase} and (comment) in an email address,
#     append comment to the phrase. This will make more duplicates to be
#     removed. Now there can be:
#	<user@host>
#	"phrase" <user@host>
#	"phrase (comment)" <user@host>
#	<user@host> (comment)
#   * In case email address is in form "someuser@somehost" <someuser@somehost>
#     i.e. the phrase is exactly the same as <address>, phrase is dropped.
#
# Version 2.1  2012-02-22 14:58:58 UTC
#   * Fixed a bug where decoding matching but unknown or malformed =?...?=-
#     encoded parts in email addresses lead to infinite loop.
#
# Version 2.0  2012-01-14 03:45:00 UTC
#   * Added regexp-based ignores using /regexp/[i] syntax in ignore file.
#   * Changed addresses file header to v4; 'addresses' file now contains all
#     found addresses plus some metainformation added at the end of the file.
#     Filtered (by ignores) address list is now in new 'addresses.active'
#     file and the fgrep code at the beginning now uses this "active" file.
#     Addresses file with header v2 and v3 are supported for reading.
#   * Encoded address content is now recursively decoded.
#
# Version 1.6  2011-12-29 06:42:42 UTC
#   * Fixed 'encoded-text' recognition and concatenations, and underscore
#     to space replacements. Now quite RFC 2047 "compliant".
#
# Version 1.5  2011-12-22 20:20:32 UTC
#   * Changed search to exit with zero value (also) if no match found.
#   * Changed addresses file header (v3) to use \t as separator. Addresses
#     file containing previous version header (v2) can also be read.
#   * Removed outdated information about sorting in ASCII order.
#
# Version 1.4  2011-12-14 19:24:28 UTC
#   * Changed to run notmuch search --sort=newest-first --output=files ...
#     (instead of notmuch show ...) and read headers from files internally.
#   * Fixed away joining uninitialized $phrase value to address line.
#
# Version 1.3  2011-12-12 15:41:05 UTC
#   * Changed to store/show addresses in 'newest first' order.
#   * Changed addresses file header to force address file rebuild.
#
# Version 1.2  2011-12-06 18:00:00 UTC
#   * Changed search work case-insensitively -- grep(1) does it locale-aware.
#   * Changed this program execute from /bin/sh (wrapper).
#
# Version 1.1  2011-12-02 17:11:33 UTC
#   * Removed Naïve assumption that no-one runs update on 'dumb' terminal.
#   * Check address database file first line whether it is known to us.
#
#   Thanks to Bart Bunting for providing a good bug report.
#
# Version 1.0  2011-11-30 20:56:10 UTC
#   * Initial release.

use 5.8.1;
use strict;
use warnings;

use Encode qw/encode_utf8 find_encoding/;
use MIME::Base64 'decode_base64';
use MIME::QuotedPrint 'decode_qp';

no encoding;

my $configdir = ($ENV{XDG_CONFIG_HOME}||$ENV{HOME}.'/.config').'/nottoomuch';
my $adbpath = $configdir . '/addresses';
my $ignpath = $configdir . '/addresses.ignore';
my $actpath = $configdir . '/addresses.active';

unless (@ARGV)
{
    require Pod::Usage;
    Pod::Usage::pod2usage( -verbose => 0, -exitval => 0 );
    exit 1;
}

if ($ARGV[0] eq '--help')
{
    $SIG{__DIE__} = sub {
     	$SIG{__DIE__} = 'DEFAULT';
     	require Pod::Usage;
     	Pod::Usage::pod2usage( -verbose => 2, -exitval => 0, -noperldoc => 1 );
     	exit 1;
    };
    require Pod::Perldoc;
    $SIG{__DIE__} = 'DEFAULT';
    # in case PAGER is not set, perldoc runs /usr/bin/perl -isr ...
    if ( ($ENV{PAGER} || '') eq 'less') {
	$ENV{LESS} .= 'R' if ($ENV{LESS} || '') !~ /[rR]/;
    }
    @ARGV = ( $0 );
    exit ( Pod::Perldoc->run() );
}

my @list;

if ($ARGV[0] eq '--update')
{
    sub mkdirs($);
    sub mkdirs($) {
	die "'$_[0]': not a (writable) directory\n" if -e $_[0];
	return if mkdir $_[0]; # no mode: 0777 & ~umask used
	local $_ = $_[0];
	mkdirs $_ if s|/?[^/]+$|| and $_;
	mkdir $_[0] or die "Cannot create '$_[0]': $!\n";
    }

    mkdirs $configdir unless -d $configdir;

    unlink $adbpath if defined $ARGV[1] and $ARGV[1] eq '--rebuild';

    my ($sstr, $acount) = (0, 0);
    if (-s $adbpath) {
	die "Cannot open '$adbpath': $!\n" unless open I, '<', $adbpath;
	sysread I, $_, 18;
	# new header: "v4/dd/dd/dd/dd/dd\n" where / == '\t' (but match also v2)
	if (/^v[234]\s(\d\d)\s(\d\d)\s(\d\d)\s(\d\d)\s(\d\d)\n$/) {
	    $sstr = "$1$2$3$4$5" - 86400 * 7; # one week extra to (re)look.
	    $sstr = 0 if $sstr < 0;
	}
	close I if $sstr == 0;
    }
    if ($sstr > 0) {
	print "Updating '$adbpath', since $sstr.\n";
	$sstr .= '..';
    }
    else {
	print "Creating '$adbpath'. This may take some time...\n";
	$sstr = '*';
    }
    my (%ign_hash, @ign_relist);
    if (-f $ignpath) {
	die "Cannot open '$ignpath': $!\n" unless open J, '<', $ignpath;
	while (<J>) {
	    next if /^\s*#/;
	    if (m|^/(.*)/(\w*)\s*$|) {
		if ($2 eq 'i') {
		    push @ign_relist, qr/$1/i;
		}
		else {
		    push @ign_relist, qr/$1/;
		}
	    }
	    else {
		s/\s+$/\n/;
		$ign_hash{$_} = 1;
	    }
	}
	close J;
    }

    my $sometime = time;
    die "Cannot open '$adbpath.new': $!\n" unless open O, '>', $adbpath.'.new';
    die "Cannot open '$actpath.new': $!\n" unless open A, '>', $actpath.'.new';
    $_ = $sometime; s/(..)\B/$1\t/g; # FYI: s/..\B\K/\t/g requires perl 5.10.
    print O "v4\t$_\n";

    # The following code block is from Email::Address, almost verbatim.
    # The reasons to snip code I instead of just 'use Email::Address' are:
    #  1) Some systems ship Mail::Address instead of Email::Address
    #  2) Every user doesn't have ability to install Email::Address
    # --8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<--

    ## no critic RequireUseWarnings
    # support pre-5.6

    #$VERSION             = '1.889';
    my $COMMENT_NEST_LEVEL = 2;

    my $CTL            = q{\x00-\x1F\x7F};
    my $special        = q{()<>\\[\\]:;@\\\\,."};

    my $text           = qr/[^\x0A\x0D]/;

    my $quoted_pair    = qr/\\$text/;

    my $ctext          = qr/(?>[^()\\]+)/;
    my ($ccontent, $comment) = (q{})x2;
    for (1 .. $COMMENT_NEST_LEVEL) {
	$ccontent = qr/$ctext|$quoted_pair|$comment/;
	$comment  = qr/\s*\((?:\s*$ccontent)*\s*\)\s*/;
    }
    my $cfws           = qr/$comment|\s+/;

    my $atext          = qq/[^$CTL$special\\s]/;
    my $atom           = qr/$cfws*$atext+$cfws*/;
    my $dot_atom_text  = qr/$atext+(?:\.$atext+)*/;
    my $dot_atom       = qr/$cfws*$dot_atom_text$cfws*/;

    my $qtext          = qr/[^\\"]/;
    my $qcontent       = qr/$qtext|$quoted_pair/;
    my $quoted_string  = qr/$cfws*"$qcontent+"$cfws*/;

    my $word           = qr/$atom|$quoted_string/;

# XXX: This ($phrase) used to just be: my $phrase = qr/$word+/; It was changed
# to resolve bug 22991, creating a significant slowdown.  Given current speed
# problems.  Once 16320 is resolved, this section should be dealt with.
# -- rjbs, 2006-11-11
    #my $obs_phrase     = qr/$word(?:$word|\.|$cfws)*/;

# XXX: ...and the above solution caused endless problems (never returned) when
# examining this address, now in a test:
#   admin+=E6=96=B0=E5=8A=A0=E5=9D=A1_Weblog-- ATAT --test.socialtext.com
# So we disallow the hateful CFWS in this context for now.  Of modern mail
# agents, only Apple Web Mail 2.0 is known to produce obs-phrase.
# -- rjbs, 2006-11-19
    my $simple_word    = qr/$atom|\.|\s*"$qcontent+"\s*/;
    my $obs_phrase     = qr/$simple_word+/;

    my $phrase         = qr/$obs_phrase|(?:$word+)/;

    my $local_part     = qr/$dot_atom|$quoted_string/;
    my $dtext          = qr/[^\[\]\\]/;
    my $dcontent       = qr/$dtext|$quoted_pair/;
    my $domain_literal = qr/$cfws*\[(?:\s*$dcontent)*\s*\]$cfws*/;
    my $domain         = qr/$dot_atom|$domain_literal/;

    my $display_name   = $phrase;

    my $addr_spec  = qr/$local_part\@$domain/;
    my $angle_addr = qr/$cfws*<$addr_spec>$cfws*/;
    my $name_addr  = qr/$display_name?$angle_addr/;
    my $mailbox    = qr/(?:$name_addr|$addr_spec)$comment*/;

    # --8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<--

    # In this particular purpose the cache code used in...
    my %seen; # ...Email::Address is "replaced" by %seen & %hash.
    my %hash;

    my $ptime = $sometime + 5;
    my $addrcount = 0;
    $| = 1;
    open P, '-|', qw/notmuch search --sort=newest-first --output=files/, $sstr;
    while (<P>) {
      chomp;
      open M, '<', $_ or next;

      while (<M>) {
	last if /^\s*$/;
	next unless s/^(From|To|Cc|Bcc):\s+//i;
	s/\s+$//;
	my @a = ( $_ );
	while (<M>) {
	    # XXX leaks to body in case empty line is found in this loop...
	    # XXX Note that older code leaked to mail body always...
	    if (s/^\s+// or s/^(From|To|Cc|Bcc):\s+/,/i) {
		s/\s+$//;
		push @a, $_;
		next;
	    }
	    last;
	}
	$_ = join ' ', @a;

	if (time > $ptime) {
	    my $c = qw(/ - \ |)[int ($ptime / 5) % 4];
	    print $c, ' active addresses gathered: ', $addrcount, "\r";
	    $ptime += 5;
	}

	# The parse function from Email::Address heavily modified
	# to fit ok in this particular purpose. New bugs are mine!
	# --8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<--

	s/[ \t]+/ /g;
	s/\?= =\?/\?==\?/g;
	my (@mailboxes) = (/$mailbox/go);
	L: foreach (@mailboxes) {
	    next if $seen{$_};
	    $seen{$_} = 1;

	    my @comments = /($comment)/go;
	    s/$comment//go if @comments;

	    my ($user, $host);
	    ($user, $host) = ($1, $2) if s/<($local_part)\@($domain)>//o;
	    if (! defined($user) || ! defined($host)) {
		s/($local_part)\@($domain)//o;
		($user, $host) = ($1, $2);
	    }

	    sub decode_substring ($) {
		my $t = lc $2;
		my $s;
		if ($t eq 'b') { $s = decode_base64($3); }
		elsif ($t eq 'q') { $s = decode_qp($3);	}
		else {
		    $_[0] = 0;
		    return "=?$1?$2?$3?=";
		}
		$s =~ tr/_/ /;

		return $s if lc $1 eq 'utf-8';

		my $o = find_encoding($1);
		$_[0] = 0, return "=?$1?$2?$3?=" unless ref $o;
		return encode_utf8($o->decode($s));
	    }
	    sub decode_data () {
		my $loopmax = 5;
		while ( s{ =\?([^?]+)\?(\w)\?(.*?)\?= }
			 { decode_substring($loopmax) }gex ) {
		    last if --$loopmax <= 0;
		};
	    }

	    my @phrase       = /($display_name)/o;
	    decode_data foreach (@phrase);

	    for ( @phrase, $host, $user, @comments ) {
		next unless defined $_;
		s/^[\s'"]+//; ## additions 20111123 too
		s/[\s'"]+$//; ## additions 20111123 too
		$_ = undef unless length $_;
	    }
	    # here we want to have email address always // 20111123 too
	    next unless defined $user and defined $host;

	    my $userhost = lc "<$user\@$host>";
	    #my $userhost = "<$user\@$host>";

	    @comments = grep { defined or return 0; decode_data; 1; } @comments;

	    # "trim" phrase, if equals to user@host after trimming, drop it.
	    if (defined $phrase[0]) {
		#$phrase[0] =~s/\A"(.+)"\z/$1/;
		$phrase[0] =~ tr/\\//d; ## 20111124 too
		$phrase[0] =~ s/\"/\\"/g;
		@phrase = () if lc "<$phrase[0]>" eq $userhost;
	    }

	    # In case we would have {phrase} <user@host> (comment),
	    # make that "{phrase} (comment)" <user@host> ...
	    if (defined $phrase[0]) {
		if (@comments) {
		    $phrase[0] = qq/"$phrase[0] / . join(' ', @comments) . '"';
		    @comments = ();
		}
		else {
		    $phrase[0] = qq/"$phrase[0]"/;
		}
	    }
	    else {
		@phrase = ();
	    }
	    $_ = join(' ', @phrase, $userhost, @comments) . "\n";
	    next if defined $hash{$_};
	    print O $_;
	    $hash{$_} = 1;
	    next if defined $ign_hash{$_};
	    foreach my $re (@ign_relist) {
		next L if $_ =~ $re;
	    }
	    print A $_;
	    $addrcount++;
	}
	# --8<----8<----8<----8<----8<----8<----8<----8<----8<----8<----8<--
      }
      close M;
    }
    undef %seen;
    close P;
    my $oldaddrcount = 0;
    if ($sstr ne '*') {
	L: while (<I>) {
	    last if /^---/;
	    next if defined $hash{$_};
	    print O $_;
	    next if defined $ign_hash{$_};
	    foreach my $re (@ign_relist) {
		next L if $_ =~ $re;
	    }
	    print A $_;
	    $addrcount++;
	}
	while (<I>) {
	    $oldaddrcount = ($1 + 0), next if /^active:\s+(\d+)\s*$/;
	}
	close I;
    }
    print O "---\n";
    print O "active: ", $addrcount, "\n";
    close O;
    close A;
    undef %hash;
    #link $adbpath, $adbpath . '.' . $sometime;
    rename $adbpath . '.new', $adbpath or
      die "Cannot rename '$adbpath.new' to '$adbpath': $!\n";
    rename $actpath . '.new', $actpath or
      die "Cannot rename '$actpath.new' to '$actpath': $!\n";
    if ($oldaddrcount or $sstr eq '*') {
	$sometime = time - $sometime;
	my $new = $addrcount - $oldaddrcount;
	print "Added $new active addresses in $sometime seconds.\n";
    }
    print "Total number of active addresses: $addrcount.\n";
    exit 0;
}

die "$0: '$ARGV[0]': unknown option.\n";

__END__

=encoding utf8

=head1 NAME

nottoomuch-addresses.sh -- address completion/matching (for notmuch)

=head1 SYNOPSIS

nottoomuch-addresses.sh ( --update [--rebuild] | <search string> )

B<nottoomuch-addresses.sh --help>  for more help

=head1 VERSION

2.2 (2014-03-29)

=head1 OPTIONS

=head2 B<--update>

This option is used to initially create the "address database" for
searches to be done, and then incrementally update it with new
addresses that are available in mails received since last update.

In case you want to rebuild the database from scratch, add
B<--rebuild> after --update on command line. This is necessary if some
of the new emails received have Date: header point too much in the
past (one week before last update). Update used emails Date:
information to go through new emails to be checked for new addresses
with one week's overlap. Other reason for rebuild could be
enhancements in new versions of this program which change the email
format in database.

=head2 <SEARCH STRING>

In case no option argument is given on command line, the command line
arguments are used as fixed search string. Search goes through all
email addresses in database and outputs every address (separated by
newline) where a substring match with the given search string is
found. No wildcard of regular expression matching is used.

Search is not done unless there is at least 3 octets in search string.

=head1 IGNORE FILE

Some of the addresses collected may be valid but those still seems to
be noisy junk. One may additionally want to just hide some email
addresses.

When running B<--update> the output shows the path of address database
file (usually C<$HOME/.config/nottoomuch/addresses>). If there is file
C<addresses.ignore> in the same directory that file is read as
newline-separated list of addresses which are not to be included in
address database file.

Use your text editor to open both of these files. Then move address
lines to be ignored from B<addresses> to B<addresses.ignore>. After
saving these 2 files the moved addresses will not reappear in
B<addresses> file again.

Version 2.0 of nottoomuch-addresses.sh supports regular expressions in
ignore file. Lines in format I</regexp/> or I</regexp/i> defines (perl)
I<regexp>s which are used to match email addresses for ignoring. The
I</i> format makes regular expression case-insensitive -- although this
is only applied to characters in ranges I<A-Z> and I<a-z>. Remember that
I</^.*regexp.*$/> and I</regexp/> provides same set of matching lines.

=head1 LICENSE

This program uses code from Email::Address perl module. Inclusion of
that makes it easy to define license for the whole of this code base:

Terms of Perl itself

a) the GNU General Public License as published by the Free
   Software Foundation; either version 1, or (at your option) any
   later version, or

b) the "Artistic License"

=head1 SEE ALSO

L<notmuch>, L<Email::Address>

=head1 AUTHOR

Tomi Ollila -- too ät iki piste fi

=head1 ACKNOWLEDGMENTS

This program uses code from Email::Address, Copyright (c) by Casey West
and maintained by Ricardo Signes. Thank you. All new bugs are mine,
though.
