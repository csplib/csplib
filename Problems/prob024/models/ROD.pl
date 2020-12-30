#!/usr/local/bin/perl

# Uses Roy O. Davies pattern to generate a single Langford chain for given n.
#
# John Miller, Feb 1997

$n = $ARGV[0];

if ( $n < 8 ) {exit;} # otherwise program degenerates
if ( ! ($n%4 || ($n+1)%4) ) {exit;} # n must be a multiple of 4 or one less.

$even = $n % 2 == 0;
$odd = $n % 2 == 1;

$m = $odd ? ($n+1)/4 : $n/4;  # m is the basis for patterns below
$mm = 2*$n;

$i = 0; # i$ will be incremented in the gen & add routines.

gen (4*$m-4, 2*$m);
add (4*$m-2);
gen (2*$m-3, 1);
add (4*$m-1);
gen (1, 2*$m-3);
gen (2*$m, 4*$m-4);

if ($odd) { add (2*$m-1); } # 2m-1 goes in the middle
if ($even) { add (4*$m); }  # one n goes in the middle

gen (4*$m-3, 2*$m+1);
add (4*$m-2);
gen (2*$m-2, 2);

add (2*$m-1); # this 2m-1 always here
add (4*$m-1);
gen (2, 2*$m-2);
gen (2*$m+1, 4*$m-3);

if ($even) {
	add (2*$m-1); # 2m-1 goes to the end
	add (4*$m);   # the other n at the end
}


#print the arrangement
#=====================
#for ($i=1; $i<=$mm; $i++) { print " $L[$i]";} print "\n";

#check for correctness, but destroy in the process
for ($p=1; $p<=$mm; $p++) {

	$k = $L[$p];
	if ($k == 0) {next}
	if ($L[$p+$k+1] != $k) {die};

	$L[$p+$k+1] = 0;
}

print "checks out OK!\n";
exit;


# add a sequence of odd or even numbers to L
sub gen {

	($a,$b) = @_; # range is passed in argument list.

	if ($a<$b) { for ($k=$a; $k<=$b; $k+=2) { $L[++$i] = $k; }}

	if ($a>$b) { for ($k=$a; $k>=$b; $k-=2) { $L[++$i] = $k; }}
}


#add a single number to L
sub add {

	($a) = @_;

	$L[++$i] = $a;
}
