#!/usr/bin/env perl

use Modern::Perl '2015';
use Const::Fast;

const our $PKG_DATA => "../assets";

my $splash_src = $PKG_DATA . "/pictures/mysplash.txt";
my $splash_dest = $PKG_DATA . "/pictures/mysplash.asm";
say "\$splash_src=$splash_src";
say "\$splash_dest=$splash_dest";

open my $fh, '<', $splash_src or die "Cannot read $splash_src\n";
open my $fout, '>', $splash_dest or die "Cannot write $splash_dest\n";

print $fout "splash_c_PIXELS\n";

while (<$fh>)
{
    print $fout "splash_c_VIDEO_RAM\n" if /Colors/;

    my $line = $_;
    # Make sure any non-data line is commented out, and that '.db'
    # declarations are changed to '!byte' (i.e. ACME syntax).
    if ( $line =~ /^\s+\.db/ ) {
        $line =~ s/\.db/!byte/;
    }
    else {
        $line = ";" . $line;
    } # if ... else
    
    print $fout "$line";
} # while

close $fout;
close $fh;




