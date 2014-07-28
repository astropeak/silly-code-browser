#!/usr/bin/perl

use strict;
use warnings;

my $pattern = shift;

my $file;
foreach $file (@ARGV)
{
   $file =~ s/^"(.*)"[\x0A\x0D]*$/$1/g;	#remove the surrounding "
   open my $fh, '<', $file or die "unable to open file '$file' for reading : $!";

   my $line_count = 0;
   while (my $line = <$fh>){
       $line_count++;
       print $file, ':', $line_count, ':', $line if $line =~ $pattern;
   }
   close($fh);
}
