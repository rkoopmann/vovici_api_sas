#!/usr/bin/perl -w

#perl fixVovici.pl file TRUE False true F t
my $file = $ARGV[0];
my $tidy = $ARGV[1];
my $quotes = $ARGV[2];
my $unencapsulate = $ARGV[3];
my $skipCdata = $ARGV[4];

print $file; 
my $flag= 0; 
my $hold; 

open IN, "$file.xml" || die "Can't open for reading: $!";
open OUT, ">$file.parsed.xml" || die "Can't open for writing: $!"; 

foreach(<IN>) {
	$hold .=$_; 
	$hold =~ s/\n//g;             # new lines within line
}

if($tidy =~ /[tT]/) {
  $hold =~ s/&lt;/\</g;         # "&lt;"  ==> "<"
  $hold =~ s/&gt;/\>/g;         # "&gt;"  ==> ">"
  $hold =~ s/(<\/.+?>)/$1\n/g;  # add line breaks
  $hold =~ s/utf-16/utf-8/g;    # switch encoding
  }

if($quotes =~ /[tT]/) {
  $hold =~ s/’/'/g;             # right curly single quote
  $hold =~ s/’/'/g;             # another right curly quote?
  $hold =~ s/“/"/g;             # left curly double quote
  $hold =~ s/”/"/g;             # right curly double quote
  $hold =~ s/–/-/g;             # endash
  $hold =~ s/—/--/g;            # emdash
  $hold =~ s/ / /g;             # spaces
  $hold =~ s/©/\(C\)/g;         # copyright
  $hold =~ s/®/\(R\)/g;         # registered
  $hold =~ s/™/\(TM\)/g;        # trademark
  $hold =~ s/â€™//g;            # mysterious replacement that must die a slow, painful death
  $hold =~ s/â€ƒ//g;            # ibid
  }
  
if($unencapsulate =~ /[tT]/) {
  $hold =~ s/.+?Get.*Result\>(\<.+\>)\<\/.+?Get.*?Result.+/$1/;

  # remove <?xml version="1.0" encoding="utf-8"?>
  $hold =~ s/<.xml version="1.0" encoding="utf-8".\>//g;
  }

if($skipCdata =~ /[tT]/) {
  $hold =~ s/\<\[CDATA\[.+?\]\]\>//g;
  }

print OUT "$hold\n";
close IN; 
close OUT; 
