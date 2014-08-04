#!/usr/bin/perl

use strict;
use warnings;

my $pattern = shift;

my $file;
foreach $file (@ARGV)
{
   $file =~ s/^"(.*)"[\x0A\x0D]*$/$1/g;	#remove the surrounding "
   if (open my $fh, '<', $file){
       my $line_count = 0;
       while (my $line = <$fh>){
	   $line_count++;
	   print $file, ':', $line_count, ':', $line if $line =~ $pattern;
       }
       close($fh);
   } else {
       print "Can not open '$file'\n"
   }
}
