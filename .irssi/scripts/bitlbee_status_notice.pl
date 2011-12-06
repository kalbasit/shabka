# bitlbee_status_notice.pl
# Adds detailed information about status changed to bitlbee query windows
# Information about known offline, online, and away durations will be printed
# to open query windows of buddies. Away messages will also be asked for when
# using the Oscar network.

# To use:
# Set the correct values for $bitlbee_* below, and then:
#   /script load bitlbee_status_notice.pl

# Settings:
#   /set bitlbee_hide_joins ON|OFF
#       Prevents joins from showing up in #bitlbee control channel when buddies
#       sign on
#   /set bitlbee_hide_quits ON|OFF
#       Same for buddies signing off, except it also applies to query windows,
#       because Irssi shows quit notices in query windows automatically.
#
# As of version 1.4, these settings default to OFF.
# If you wish also to ignore mode changes (voicing/devoicing):
#   /ignore &bitlbee MODES
use strict;
use Irssi;
use Time::Duration;
use Data::Dumper;

use vars qw($VERSION %IRSSI);

$VERSION = '1.4';
%IRSSI = (
  authors     => 'Matt "f0rked" Sparks',
  contact     => 'ms+irssi@quadpoint.org',
  name        => 'bitlbee_status_notice',
  description => 'Adds detailed information about status changes to bitlbee query windows',
  license     => 'GPLv2',
  url         => 'http://quadpoint.org',
  changed     => '2010-03-04',
);

my $bitlbee_channel = "&bitlbee";
my $bitlbee_server_tag = "IM";

my %away_watch;
my %away_times;
my %online_times;
my %offline_times;

my $hide_it;
my $requested_info;

Irssi::theme_register([
  'state_away', '{channick $0} {chanhost $1} has gone away',
  'state_back', '{channick_hilight $0} {chanhost_hilight $1} has come back$2',
  'away_msg', '{chanhost msg} $0',
  'join', '{channick_hilight $0} {chanhost_hilight $1} has signed on$2',
  'quit', '{channick $0} {chanhost $1} has signed off$2',
]);


Irssi::signal_add_last 'channel sync' => sub
{
  my($channel) = @_;
  if ($channel->{topic} eq "Welcome to the control channel. Type \x02help\x02 for help information.") {
    $bitlbee_server_tag = $channel->{server}->{tag};
    $bitlbee_channel = $channel->{name};
  }
};


sub get_channel
{
  my @channels = Irssi::channels();
  foreach my $channel (@channels) {
    if ($channel->{topic} eq "Welcome to the control channel. Type \x02help\x02 for help information.") {
      $bitlbee_channel = $channel->{name};
      $bitlbee_server_tag = $channel->{server}->{tag};
      return 1;
    }
  }
  return 0;
}


sub event_join
{
  my($server, $channel, $nick, $address) = @_;
  if ($channel eq $bitlbee_channel && $server->{tag} eq $bitlbee_server_tag) {
    my $off_time;
    $off_time = time - $offline_times{$nick} if $offline_times{$nick};
    delete $offline_times{$nick} if $offline_times{$nick};

    my $str;
    $str = " (last seen: " . ago_exact($off_time) . ")" if $off_time;
    $online_times{$nick} = time;

    my $window = $server->query_find($nick);
    if ($window) {
      $window->printformat(MSGLEVEL_JOINS, "join", $nick, $address, $str);
    }

    Irssi::signal_stop()  # don't print the join announcement in &bitlbee
      if Irssi::settings_get_bool("bitlbee_hide_joins");
  }
}


sub event_quit
{
  my($server, $nick, $address, $reason) = @_;
  if ($server->{tag} eq $bitlbee_server_tag) {
    my $on_time;
    $on_time = time - $online_times{$nick} if $online_times{$nick};
    delete $online_times{$nick} if $online_times{$nick};

    my $str;
    $str = " (duration: " . duration($on_time) . ")" if $on_time;
    $offline_times{$nick} = time;

    my $window = $server->query_find($nick);
    if ($window) {
      $window->printformat(MSGLEVEL_QUITS, "quit", $nick, $address, $str);
    }

    Irssi::signal_stop()  # don't print the quit announcement anywhere
      if Irssi::settings_get_bool("bitlbee_hide_quits");
  }
}


sub event_mode
{
  my($channel, $nick, $setby, $mode, $type) = @_;
  #print Dumper $nick;
  #print Dumper $channel;
  if ($mode eq "+" && $channel->{name} eq $bitlbee_channel &&
      $channel->{server}->{tag} eq $bitlbee_server_tag) {
    my $window = $channel->{server}->query_find($nick->{nick});
    my $gone_time;

    if ($type eq "-") {
      $away_times{$nick->{nick}} = time;
    } elsif ($type eq "+") {
      if (my $time = $away_times{$nick->{nick}}) {
        $gone_time = time-$time;
        delete $away_times{$nick->{nick}};
      }
    }

    if ($window) {
      if ($type eq "+") {
        my $gone_str;
        $gone_str = " (gone: ".duration($gone_time).")" if $gone_time;
        $window->printformat(MSGLEVEL_MODES, "state_back", $nick->{nick},
                             $nick->{host}, $gone_str)
          if (time-$online_times{$nick->{nick}} > 2);
      } elsif ($type eq "-") {
        $window->printformat(MSGLEVEL_MODES, "state_away", $nick->{nick},
                             $nick->{host});
        if ($nick->{host} =~ /login\.oscar\.aol\.com$/) {
          $away_watch{nick} = $nick->{nick};
          $channel->{server}->send_message($channel->{name},
                                           "info $nick->{nick}", 0);
          $requested_info = 1;
        }
      }
    }
  }
}


sub pub_msg
{
  my($server, $msg, $nick, $address, $target) = @_;
  #print "$msg $nick $address $target";
  if ($nick eq "root" && $server->{tag} eq $bitlbee_server_tag &&
      $target eq $bitlbee_channel) {
    my $window = $server->channel_find($target);
    if ($window) {
      my $qwin;
      $qwin = $server->query_find($away_watch{nick}) if $away_watch{nick};
      if ($msg =~ /^TOC\(?.*\)? \- Away Message/g ||
          $msg =~ /^oscar \- Away Message/) {
        $away_watch{watch} = 1;
        $hide_it = 1 if $requested_info;
        Irssi::timeout_add_once(400,
                                sub { $hide_it = 0; $requested_info = 0; },
                                "");
        #$qwin->print("Away message:",MSGLEVEL_CRAP) if $qwin;
      } elsif ($msg =~ /^TOC\(?.*\)? \- .+$/ || $msg =~ /^oscar \- .+$/) {
        delete $away_watch{watch};
        delete $away_watch{nick};
      } elsif ($away_watch{watch} && $qwin) {
        $qwin->printformat(MSGLEVEL_CRAP, "away_msg", $msg) if $qwin;
      }
    }

    Irssi::signal_stop if $hide_it;
  }
}


get_channel();


Irssi::settings_add_bool("bitlbee", "bitlbee_hide_joins", 0);
Irssi::settings_add_bool("bitlbee", "bitlbee_hide_quits", 0);

Irssi::signal_add("message public", "pub_msg");
Irssi::signal_add("nick mode changed", "event_mode");
Irssi::signal_add("message join", "event_join");
Irssi::signal_add("message quit", "event_quit");
