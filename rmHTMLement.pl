#!/usr/bin/perl -w
use strict;

my $whole;
sub getElem{	# $_[0]= searched element tag  $_[1] = nth-1 $_[2]= string under to search
	 $_[2]=~ /\A(<\w+[^>]*+>(?:(?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base|!DOCTYPE)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?(?'tnode'(?=<$_[0]\b[^>]*+>)(?&node))){$_[1]}(?>(?&at)|(?&node))*?)((?&tnode))/s;
	return ($1,$6);
}
sub getElePath{	my $under="<D>$whole";
	my ($off,$offset);
	for (split(/[\/>]/,$_[0])) {
		/^(\w++)(?|\[(\d+)\]|,(\d+))?/;
		($off,$under)=getElem($1, $2?$2-1:0, $under);
		$offset.=$off;
	}
	return ($offset=~s/^<D>//r,$under);
}

my (@opath, @path, @offElem, $O);
{my ($file, $trPath, @validP);
if (@ARGV) {
	$trPath=shift;
	$O=shift;
	undef local $/;$whole=<>;}
else {
	print "Element path is form of Xpath e.g:\thtml/body/div/div[2]/div[3]\n\n/ may be replaced with > and [2] with ,2 e.g:\n\nhtml>body>div>div,2>div,3\n\nIt may be put multiply, delimited by ;\n";
	print "File name to work on: ";
	$file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "'$file' not exist\n";
	$!=2;open R,"$file" or die "Cannot open '$file'\n";
	print 'The element path(s): ';
	my $i;for (split(/;/,$trPath=<>)) {
		L:	if (/^\s*[a-z]\w*+(?:\h*[\/>]\h*[a-z]\w*+(?:\[\d+\]|,\d+)?)*\/*\s*$/i){
			$validP[$i++]=$_=~s/\s//gr=~s/\/$//r;
		}else{
			print "'$_' is invalid path form\nEdit it or skip it? (E/Enter: Edit  S: Skip.  Else: Abort) ";my $y=<>;
			if ($y=~/^e?$/i){	print "Edit: ";$_=<>; goto L;
			}elsif ($y=~/^s/i) {	next;}
			else {	die "Aborted by user\n";}
	}}
}
PREP:{
{	undef local $/;$whole=<R>;close R;}
my (@e,@miss,$miss1);	$e[1]=1;
for(@validP){
	unless ($e[1]) {
		print "\nSkip the missing '$miss1'\nto process the next path? (Y/Enter: yes. Else: aborting) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborted by user\n";}
	@e=getElePath($_);
	if ($e[1]) {
		push(@opath,$_);
		push(@path, [s/\//>/gr=~s/\[(\d+)\]/,$1/gr,@e]);	# AoA: uniformed path form, offset, element
	}else{
		push(@miss,$miss1=$_);}
}
if ($e[1]) {	for(@miss) {print "Skipping non existant '$_'\n";}
}else{
	print "\nCouldn't find ";
	if (@opath){
		print "the last path\n'$miss1'\nKeep working on previous path(s) found? (Y/Enter: yes. Else: aborting) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborted by user\n";
	}else{	die "'$miss1'\nNothing was done";}
}}
unless	(@ARGV){
	print "\nWhich operation will be done :\n- Remove\n- Get\n(R: remove. Else: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name to save the result: (hit Enter to standard output) ';
	my $of=<>=~s/^\h+//r=~ s/\s+$//r;
	open W,">","$of" or die "Cannot open '$of'\n" if($of);
}}
SWC:
for ($O){
	if (/^r/i){			# filter out path whose head is as the shorter one
		@path = sort {length $a->[0] cmp length $b->[0] } @path;		# first sort their lengths
		OUT:
		for(my $i=1;$i<=$#path;) {
			for(my $j=0;$j<=$i-1;) {
				if ($path[$i][0]=~/^$path[$j++][0]/){
					splice(@path,$i,1);
					next OUT;}
			}
			$i++;
		}
		@path = sort {reverse length $a->[1] cmp $b->[1] } @path;		# sort from longest path offsets
		for(@path){
			$whole=~ s/\A(\Q$_->[1]\E)\Q$_->[2]\E\s?(.*)\Z/$1$2/s;}
		fileno W? print W $whole:print $whole;
		last SWC;
	}
	my ($res,$i);
	for (@path){	$res.="\n$opath[$i++]:\n$_->[2]\n";}
	fileno W? print W $res:print $res;
}
close W;
