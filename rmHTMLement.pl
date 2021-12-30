#!/usr/bin/perl -w
use strict;

sub getElem {			# $_[0]= searched elem tag  $_[1] = nth-1 $_[2]= whole elem to search
	if ($_[2]=~/^(<(?>[a-z]\w*|!DOC)[^>]*+>(?:(?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?(?'tnode'(?=<$_[0]\b[^>]*+>)(?&node))){$_[1]}(?>(?&at)|(?&node))*?)((?&tnode))/) {
		 @{$_[3]}=[$1,$6];
		 return}
	return 1
}

sub getMulNthE {
	my ($off,$b)=$_[1]=~ /^(<(?>[a-z]\w*)[^>]*+>)(.*)/s;	my $pre='';
	while ($b=~/\G((?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?)((?=<$_[0]\b[^>]*+>)(?&node))/g) {
		push (@{$_[3]}, [$_[2].($off.=$pre.$1), $5]);
		$pre=$5
	}
	return !$pre
}
# These subs return 1 if cannot find, and offset & same-name elem pairs in the 4rd arg

my (@res, $MUL);
sub getE_Path_Rec { my $iOffNode = $_[1];
	my ($mDepth,$tag,$nth,$path)=$_[0]=~ m#^/(/)?([^[/,]+)(?|\[(\d+|@[^]]+)\]|,(\d+))?(.*)#;
	$MUL |= !$nth;
	for (@$iOffNode) {	my @OffNode;
		if ($nth) {
			if (&getElem ($tag, $nth-1, $_->[1], \@OffNode)) {			# offset-node pair return is in @OffNode
				next if @res or $MUL;
				return 1}
			${$OffNode[0]}[0]=$_->[0].${$OffNode[0]}[0];
		}else {
			if (&getMulNthE ($tag, $_->[1], $_->[0], \@OffNode )) {
				next if @res or $MUL;
				return 1}
		}
		if ($path) {		return 1 if &getE_Path_Rec ($path, \@OffNode)			# then propagate it to the next depth
		}	else {					push (@res, @OffNode)
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
	print "Element path is form of Xpath e.g: /html/body/div[1]/div[3]\n\n[1] may be replaced with ,1 e.g: html/body/div,1/div,3\nTo put multiply, delimit each by ;\nFile name to work on: ";
	my $file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "\n'$file' not exist\n";
	$!=2;open R,"$file" or die "\nCannot open '$file'\n";
	print '\nThe element path(s): ';
	die "No any Xpath given\n" if ($trPath=<>)=~/^\s*$/;
	for (split(/;/,$trPath)) {
		L:if (m#^\s*/?/?([a-z]\w*+(?>\[(?>\d+|@[a-z]+(?:=\w+)?)\]|,\d+)?|@[a-z]\w*)(?://?(?1)?)*+\s*$#i)	{
			s/\s|\/$//g;
			if (/^[^\/]/){
				if(!$CP) {
					print "\nRelative path '$_'\nneeds the current path as base. Put one: ";
					$CP=<>=~s/\/$//r;
				}
				$_=	"$CP/$_";goto L
			}
			push (@valid, $_=~ s/\/$//r=~ s/,(\d+)/\[$1\]/gr);
		}else {
			print "'$_' is invalid Xpath\nEdit it or skip it? (E/Enter: Edit  S: Skip.  Else: Abort) ";my $y=<>;
			if($y=~/^e?$/i){
				print "Edit: ";$_=<>;goto L
			}elsif ($y=~/^s/i) {next
			}else{	die "Aborted\n"}
	}}
	undef local $/;$whole=<R>;close R;
	print "\nProcessing on file '$file'...\n";
}

my ($E, @path, @fpath, @miss, @short);
for (sort{length $a cmp length $b} @valid) {
	if ($E) {
		print "\nSkip it to process the next path? (Y/Enter: yes. Else: abort) ";
		<>=~/^(?:\h*y)?$/i or die "Aborting\n"}
	@res=();
	my @i=['',$whole];
	s#(^/html|(?<=^/html/)body)/#$1\[1\]/#g;
	if ($E= &getE_Path_Rec ($_, \@i) or not @res and $E=1) {
		push(@miss,$_);
		print "\nCan't find '$_'"
	}else{
		push (@path, [$_, [@res]]);
		my $cur=$_;			# Optimize removal: filter out longer path whose head is as the shorter one's
		for (@short) {
			goto P if $cur =~ /^\Q$_\E/}
		push (@fpath, @res);
		P:
		push (@short, $_)
	}
}

if (@miss){
	if (@path){
		print "\nKeep processing ones else found? (Y/Enter: yes. any else: abort) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborting\n";
		print "\nSkipping not found '$_'" for(@miss)
	}else{	print "\nNothing was done\n";exit
}}
print "\nProcessing:";print "\n$_->[0]" for(@path);
unless	(@ARGV){
	print "\nWhich operation will be done :\n- Remove\n- Get\n(R: remove. Else: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name to save the result: (hit Enter to standard output) ';
	my $of=<>=~s/^\h+//r=~ s/\s+$//r;
	open W,">","$of" or die "Cannot open '$of'\n" if($of)
}

for ($O){
if (! /^r/i) {
	my $o;for (@path) {
		$o.="\n\n$_->[0]:";
		$o.="\n$_->[1]" for @{$_->[1]}
	}
	fileno W? print W $o:print $o;
	last
}
# Removal, etc is from long to shorter el. offset of array fpath, so they're sorted descendingly
@fpath=sort {length $b->[0] <=> length $a->[0]} @fpath;

print "\nRemoval result:";
for (@fpath) {
	$whole=~ s/\A(\Q$_->[0]\E)\Q$_->[1]\E(.*)\Z/$1$2/s}
fileno W? print W $whole:print $whole;
}
close W;
