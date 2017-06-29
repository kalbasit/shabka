#!/usr/bin/perl -w

use strict;
use Cwd;
use Digest::MD5 qw(md5);
use Dumpvalue;
use File::Compare qw(compare);
use File::Copy qw(cp);
use File::Find;
use File::Temp qw(tempfile);
use Getopt::Long;
our %Opt;
GetOptions(\%Opt,
           "maxlinks=i",
          ) or die;

my @dirs = @ARGV or die "Usage: $0 OPTIONS directories";
our $DEBUG = 0;
our $Signal = 0;
our %MD5;
our $Usedspc = 0;
our $Savedspc = 0;

$SIG{INT} = sub {
  warn "Caught SIGINT; please stand by, I'm leaving as soon as possible...\n";
  $Signal++;
};
our $DV = Dumpvalue->new(tick => qq{"},
                         quoteHighBit => 1,
                         printUndef => 1,
                        );
our $WD = Cwd::cwd;

sub fmt ($) {
  local $_ = shift;
  s/(\d)(?=(\d{3})+$)/$1_/g;
  $_;
}

# $success = melt($some_file,$another)

# $some_file is always absolute, $another is always in the current
# directory

# melt dies on severe errors, returns 1 on success and 0 if it could
# not melt the files. It warns on suspect error conditions, but does
# not warn if they have no severe consequences.
sub melt ($$) {
  my($first,$basename) = @_;
  my($fh,$tempfile) = tempfile("trimtrees1-XXXXXXXX", DIR => ".");
  unless ($tempfile) {
    die "Could not create a temporary file: $!";
  }
  unless (rename $basename, $tempfile){
    warn sprintf(
                 "Cannot rename %s to %s (%s); Skipping.",
                 $DV->stringify($basename),
                 $DV->stringify($tempfile),
                 $!,
                );
    unlink $tempfile or die "Could not unlink '$tempfile': $!";
    return 0;
  }
  unless (link $first, $basename){
    my $link_err = $!;
    if (rename $tempfile, $basename){
      # We could rename back and no harm should be done. Don't warn.
      return 0;
    } else {
      die sprintf(
                  "Could neither link %s to %s (%s) ".
                  "nor rename %s back to %s (%s).",
                  $DV->stringify($first),
                  $DV->stringify($basename),
                  $link_err,
                  $DV->stringify($tempfile),
                  $DV->stringify($basename),
                  $!
                 );
    }
  }
  close $fh or warn "Could not close the temporary filehandle: $!";
  unless (unlink $tempfile) {
    die sprintf(
                "Could not unlink %s (was %s): %s",
                $DV->stringify($tempfile),
                $DV->stringify($basename),
                $!
               );
  }
  return 1;
}

{
  my %INODE;
  sub register ($$;$) {
    my($md5,$cand,$candstat) = @_;
    $MD5{$md5} = $cand;
    my @stat = $candstat ? @$candstat : stat $cand;
    my $size = $stat[7];
    warn "size undefined" unless defined $size;
    warn "usedspc undefined" unless defined $Usedspc;
    if ($DEBUG) {
      warn(sprintf "\nDEBUG: cand[%s]size[%s]stat[%s]", $cand, $size, join(":",@stat));
    }
    return if $INODE{$stat[1]}++; # don't count twice
    $Usedspc += $size;
  }
}

undef $/;
my %reported;
my $files = 0;
my $dirs = @dirs;
my $tl_dirs_todo = 0;
my $tl_dirs_doing = 0;

sub xreport () {
  my $uniq_files = keys %MD5;
  printf(
         "\rtlds[%s]cur[%s]uniq[%s]fils[%s]spcused[%s]saved[%s]",
         map {
           fmt($_)
         }
         (
          $tl_dirs_todo,
          $tl_dirs_doing,
          $uniq_files,
          $files,
          $Usedspc,
          $Savedspc
         )
        );
}

$| = 1;
for my $diri (0..$#dirs) {
  my $root = $dirs[$diri];
  find(
       {
        wanted => sub {
          if ($Signal){
            $File::Find::prune = 1;
            return;
          }
          if ($File::Find::name eq $root) {
            my $td = $_;
            opendir my($dh), $td;
            my(@tl) = grep { !/^\./ && -d "$td/$_" } readdir $dh;
            $tl_dirs_todo = @tl;
          } elsif (-d) {
            my $slashes = $File::Find::name =~ tr|/||;
            if ($slashes == 1) {
              $tl_dirs_doing++;
            }
          }
          return if -l; # relative links would need special treatment that does not pay off
          return unless -f _;
          return unless -s _; # empty files more risk that files with
                              # content and no gain
          $files++;
          my $basename = $_;
          my $fh;
          unless (open $fh, "<", $basename) {
            warn sprintf(
                         "Cannot read %s (%s); Skipping.",
                         $DV->stringify($File::Find::name),
                         $!,
                        );
            $Usedspc += -s $basename;
            return;
          }
          my $data = <$fh>;
          close $fh;
          my $md5 = md5 $data;
          my $cand = $File::Find::name;
          if ($Opt{maxlinks}) {
            my(@maxlstat) = stat($cand);
            if ($maxlstat[3] > $Opt{maxlinks}) {
              # the case that we have to make a new file from a link
              my($fh,$tempfile) = tempfile("trimtrees2-XXXXXXXX", DIR => ".");
              unless ($tempfile) {
                die "Could not create a temporary file: $!";
              }
              unless (rename $cand, $tempfile){
                die sprintf(
                            "Could not rename %s to %s: %s",
                            $DV->stringify($cand),
                            $DV->stringify($tempfile),
                            $!
                           );
              }
              cp $tempfile, $cand or die sprintf(
                                                 "Could not cp %s to %s: %s",
                                                 $DV->stringify($tempfile),
                                                 $DV->stringify($cand),
                                                 $!,
                                                );
              unlink $tempfile;
              $Savedspc -= $maxlstat[7];
            }
          }
          if (my $first = $MD5{$md5}) {
            unless (File::Spec->file_name_is_absolute($first)) {
              $first = File::Spec->catfile($WD, $first);
            }
            my(@firststat) = stat($first);
            die sprintf(
                        "illegal firststat[%s]first[%s]",
                        join(":",@firststat),
                        $DV->stringify($first),
                       ) unless $firststat[1];
            my $different = compare $first, $basename;
            if ($different != 0 && $File::Compare::VERSION<1.1005) {
              # workaround bug # 37716 in File::Compare
              $different = compare "$first\0", "$basename\0";
            }
            if ($different<0) {
              warn sprintf(
                           "Cannot compare %s and %s (%s); Skipping.",
                           $DV->stringify($first),
                           $DV->stringify($cand),
                           $!,
                          );
              goto XREPORT; # some error occurred
            }
            die sprintf(
                        "Sensation, %s and %s are not equal with same MD5",
                        $DV->stringify($first),
                        $DV->stringify($cand),
                       )
                if $different;
            my(@candstat) = stat($basename);
            goto XREPORT unless $candstat[0] == $firststat[0]; # different file system
            if ($candstat[1] == $firststat[1]){ # already same inode
              if (0 && $Opt{maxlinks} && $firststat[3] > $Opt{maxlinks}) {
              } else {
                goto XREPORT;
              }
            }
            if ($Opt{maxlinks} && $firststat[3] >= $Opt{maxlinks}){
              register($md5,$cand,\@candstat) if $candstat[3] < $Opt{maxlinks};
            } elsif (melt($first,$basename)) {
              if ($candstat[3]==1) { # we don't save space otherwise
                $Savedspc += $firststat[7];
              }
            } else {
              register($md5,$cand,\@candstat);
            }
          } else {
            register($md5,$cand);
          }
        XREPORT:
          return if $files % 100;
          xreport;
        },
        no_chdir => 1,
       },
       $root
      );
  last if $Signal;
}
xreport;
print "\nDONE\n";

__END__

=head1 NAME

trimtrees - traverse directories, find identical files, replace with hard links

=head1 SYNOPSIS

 trimtrees.pl OPTIONS directory...

 OPTIONS:

  --maxlinks N            limit the amount of links per file

=head1 DESCRIPTION

Traverse all directories named on the command line, compute MD5
checksums and find files with identical MD5. IF they are equal, do a
real comparison if they are really equal, replace the second of two
files with a hard link to the first one.

Special care is taken to cope with C<Too many links> error conditions.
The inode that is overbooked in such a way, is taken out of the pool
and replaced with the another one such that the minimum of files
needed is kept on disk.

The C<--maxlinks> option can be used to reduce the linkcount on all
files within a tree, thus preparing the tree for a subsequent call to
C<cp -al>. This operation can be thought of the reverse of the normal
trimtrees operation (--maxlinks=1 produces a tree without hard links).

=head1 SIGNALS

SIGINT is caught and the script stops as soon as the current file
is finished.

=head2 RISKS

The whole idea of replacing identical files with hard links has
inherent dangers. Once two files have turned into one inode other
processes may accidentally change both although they intend to alter
only one. Please consider if this can happen in your environment.

=cut

	Local Variables:
	mode: cperl
	cperl-indent-level: 2
	End:
