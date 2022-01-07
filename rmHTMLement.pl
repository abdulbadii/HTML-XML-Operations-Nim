#!/usr/bin/perl -w
use strict;

sub getNthElem {			# $_[0]= now searched el  $_[1] = nth-1 $_[2]= whole elem to search
	if ($_[2]=~/^(<[a-z]\w*[^>]*+>(?:(?'cont'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|(?'node'<(\w++)[^>]*+>(?&cont)*+<\/\g-1>)))*?(?'tnode'(?=<$_[0]\b[^>]*+>)(?&node))){$_[1]}(?&cont)*?)((?&tnode))/) {
		 @{$_[3]}=[$1,$6];
		 return}
	return 1
}

sub getAllNthE {
	my $pre='';	my ($h,$b)=$_[1]=~ /^(<[a-z]\w*[^>]*+>)(.*)/s;
	while ($b=~/\G((?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?)((?=<$_[0]\b[^>]*+>)(?&node))/g) {
		push (@{$_[3]}, [$_[2].($h.=$pre.$1), $5]);
		$pre=$5
	}
	return !$pre
}

sub getAllDepth {			# $_[0]= now searched el  $_[1] = nth-1 $_[2]= whole elem to search  $_[4] path stub
	my $minD=()=$_[4]=~/\//g;
	my ($M,$d, $curE)=($_[2]);
	while ($curE	=~/^(<[a-z]\w*[^>]*+>(?:(?'cont'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|(?'node'<(\w++)[^>]*+>(?&cont)*+<\/\g-1>)))*?(?=<$_[0]\b[^>]*+>)(?&node)){$_[1]}(?&cont)*?)(?{$M=$d=0})((?=<$_[0]\b[^>]*+>)<(\w++)(?{$M=$d if ++$d>$M})[^>]*+>(?&cont)*+<\/\g-1>(?{--$d}))/) {
		push (@{$_[3]}, [$1,$curE=$5]) if $M>=$minD;
		last if --$M<$minD
	}
	return !@{$_[3]}
}


# These subs return 1 if fails to find, else 0 and offset & same-name node pairs in the 4rd arg

my @res;my $MUL;
sub getE_Path_Rec { my $iOffNode = $_[1];
	my ($ADepth, $tag, $nth, $path) = $_[0]=~ m#^/(/)?([^[/,]+)(?|\[(\d+|@[^]]+)\]|,(\d+))?(.*)#;
	$MUL |= !$nth;
	for (@$iOffNode) {
		my @OffNode;
		if ($ADepth) {
			if ($nth) {
				if (&getAllDepth ($tag, $nth-1, $_->[1], \@OffNode, $path)) {
					next if @res or $MUL;
					return 1}
			}else {
				if (&getAllDNth ($tag, $path, $_->[1], \@OffNode)) {
					next if @res or $MUL;
					return 1}
			}
		}elsif ($nth) {
			if (&getNthElem ($tag, $nth-1, $_->[1], \@OffNode)) {			# offset-node pair return is in @OffNode
				next if @res or $MUL;
				return 1}
			${$OffNode[0]}[0]=$_->[0].${$OffNode[0]}[0];
		}else {
			if (&getAllNthE ($tag, $_->[1], $_->[0], \@OffNode )) {
				next if @res or $MUL;
				return 1}
		}
		if ($path) {	return &getE_Path_Rec ($path, \@OffNode)			# to be propagated to the next depth
		}	else {				push (@res, @OffNode)
		}
	}
	return
}

my ($whole, $trPath, @valid, $O, $CP);
if (@ARGV) {
	$trPath=shift;
	$O=shift;
	undef local $/;$whole=<>
}else {
	print "Element path is of Xpath form e.g:\n\t\t\t/html/body/div[1]/div[3]\n\n[1] may be replaced with ,1 e.g: html/body/div,1/div,3\nTo put multiply at once, put one after another delimited by ;\nPut element path: ";
	die "No any Xpath given\n" if ($trPath=<>)=~/^\s*$/;
	for (split(/;/,$trPath)) {
		L:if (m#^\s*/?/?([a-z]\w*+(?>\[(?>\d+|@[a-z]+(?:=\w+)?)\]|,\d+)?|@[a-z]\w*)(?://?(?1)?)*+\s*$#i) {
			s/\s|\/$//g;
			if (/^[^\/]/) {
				if(!$CP){
					print "\nRelative path '$_'\nneeds the current path as base. Put one: ";
					$CP=<>=~s/\/$//r;
				}
				$_=	"$CP/$_";goto L
			}
			push (@valid, s/\/$//r =~s/,(\d+)/\[$1\]/gr);
		}else {
			print "'$_' is invalid Xpath\nEdit it or skip it? (E/Enter: Edit  S: Skip.  any else: Abort) ";my $y=<>;
			if($y=~/^e?$/i){
				print "Edit: ";$_=<>;goto L
			}elsif ($y=~/^s/i) { next
			}else{	die "Aborted\n"}
	}}
	print "HTML/XML file name to process: ";
	my $file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "\n'$file' not exist\n";
	$!=2;open R,"$file" or die "\nCannot open '$file'\n";
	undef local $/;$whole=<R>;close R;
	print "\nProcessing HTML document on '$file'...\n"
}

die "\nCan't parse the HTML with ill form\nBecause likely of unbalanced tag pair\n" unless
$whole=~/^(<!DOCTYPE[^>]*+>[^<]*)(<([a-z]\w*)[^>]*+>(?'cont'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|<(\w++)[^>]*+>(?&cont)*+<\/\g-1>))*?<\/\g3>)(.*)/s;

my @in=[$1,$2]; my $wTAG=$3;

my ($E, @path, @fpath, @miss, @short);
for (sort{length $a cmp length $b} @valid) {
	if ($E) {
		print "\nSkip it to process the next path? (Y/Enter: yes. any else: Abort) ";
		<>=~/^(?:\h*y)?$/i or die "Aborting\n"}
	@res=();
	s#^/([a-z]\w*)(/.*)#$2#;
	if ($wTAG ne $1 or $E=&getE_Path_Rec ($_, \@in) or not @res and $E=1) {
		push(@miss,$_);
		print "\nCan't find '$_'"
	}else {
		push (@path, [$_, [@res]]);
		my $cur=$_;			# Optimize removal: filter out longer path whose head is the same as shorter one
		for (@short) {
			goto P if $cur =~ /^\Q$_\E/}
		push (@fpath, @res);
		P:
		push (@short, $_)
	}
}

if (@miss){
	if (@path){
		print "\nKeep processing ones found? (Y/Enter: yes. any else: Abort) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborting\n";
	}else{	print "\nNothing was done\n";exit
}}
unless	(@ARGV){
	print "\nWhich operation will be done :\n- Remove\n- Get\n(R: remove. Else: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name to save the result: (hit Enter to standard output) ';
	my $of=<>=~s/^\h+//r=~ s/\s+$//r;
	open W,">","$of" or die "Cannot open '$of'\n" if($of)
}
print "\nProcessing the path:";print "\n$_->[0]" for(@path);

for ($O){
if (! /^r/i) {
	my $o;for (@path) {
		$o.="\n\n$_->[0]:";
		$o.="\n$_->[1]" for @{$_->[1]}
	}
	fileno W? print W $o:print $o;
	last
}

@fpath=sort {length $b->[0] <=> length $a->[0]} @fpath;
$whole=~ s/\A(\Q$_->[0]\E)\Q$_->[1]\E(.*)\Z/$1$2/s	for (@fpath);
fileno W? print W $whole:print "\n\nRemoval result:\n$whole"
}
close W;
