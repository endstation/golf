#!/usr/bin/perl
# Pass in one argument - a line of grep output from a label/symbol file
# produced by acme.  It will write that label's decimal value to stdout.

use Modern::Perl '2015';

my $line = shift @ARGV;
chomp $line;
if ( $line =~ /\$([0-9a-zA-Z]{2,4})$/ ) {
    print hex($1);
} # if


