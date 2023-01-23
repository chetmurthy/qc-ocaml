#!/usr/bin/env perl

use strict ;
use String::ShellQuote ;

our @cmd ;
our @files ; 

while (@ARGV) {
  if ($ARGV[0] eq "--") { shift @ARGV ; @files = @ARGV ; last ; }
  elsif (int(@ARGV) == 1) {
    @files = @ARGV ;
    last ;
  }
  else { push(@cmd, shift @ARGV) ; }
}


{
  @cmd = map { shell_quote($_) } @cmd ;
  foreach my $f (@files) {
    my $extra = discover_args($f) ;
    my $cmd = "@cmd $extra $f\n" ;
    print STDERR $cmd ;
    system($cmd) ;
  }
}

sub discover_args {
  my $f = shift ;
  open(F, "<$f") || die "$0: cannot open $f for read (to sense extra args)" ;
  my $line1 = <F> ;
  close(F) ;
  if ($line1 =~ m,^\(\*\*(.*?)\*\),) {
    my $extra = $1 ;
    $extra =~ s/(?:\$\(([^)]+)\)|\$\{([^}]+)\})/ envsubst($1, $2) /ge ;
    return $extra ;
  }
  else {
    return "" ;
  }
}

sub envsubst {
  my $varna = shift ;
  die "env var $varna is not set"
      unless exists $ENV{$varna} ;
  return $ENV{$varna} ;
}
