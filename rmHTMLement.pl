#!/usr/bin/perl -w
use strict;

my $whole;
sub getElem{	my $NTH=$_[1];	#=nth-1  $_[0]= searched element tag $_[2]= string under to search
	 $_[2]=~ /\A(<\w+[^>]*+>(?:(?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base|!DOCTYPE)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?(?'tnode'(?=<$_[0]\b[^>]*+>)(?&node))){$NTH}(?>(?&at)|(?&node))*?)((?&tnode))/s;
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

my (@path, @offE, @offElem, $O);
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
	print 'Path of the element: ';
	my $i;for (split(/;/,$trPath=<>)) {
		if (/^\s*[a-z]\w*+(?:\h*[\/>]\h*[a-z]\w*+(?:\[\d+\]|,\d+)?)*\/*\s*$/i){
			$validP[$i++]=$_=~s/\s//gr=~s/\/$//r;
		}else{	print "'$_' is invalid path form\nEdit it or skip it? (E/Enter: Edit  S: Skip  other: Abort) ";
			my $y=<>;
			if ($y=~/^e?$/i){	print "Edit: ";$validP[$i++]=<>;
			}elsif ($y=~/^s/i) {	next;}
			else {	die "Aborted by user\n";}
		}
	}
}
INI:{
{	undef local $/;$whole=<R>;close R;}
my (@e,@miss,$miss1);	$e[1]=1;
for(@validP){
	unless ($e[1]) {
		print "\nSkip the missing '$miss1'\nto process the next path? (Y/Enter: yes. Else: aborting) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborted by user\n";
	}
	@e=getElePath($_);
	if ($e[1]) {
		push(@path,$_);
		push(@offElem,[@e]);
	}else{
		push(@miss,$miss1=$_);}
}
if ($e[1]) {
	for(@miss) {print "Skipping non existant '$_'\n";}
}else{
	print "\nCouldn't find ";
	if (@path){
		print "the last path\n'$miss1'\nContinue doing on other previous path? (Y/Enter: yes. Else: aborting) ";
		<>=~s/^\h+//r =~/^y?$/i or die "Aborted by user\n";
	}else{	die "'$miss1'\nNothing was done";}
}}
unless	(@ARGV){
	print "\nWhich operation will be done :\n- Remove\n- Get\n(R/r: remove. Else: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name of the result: (hit Enter to standard output) ';
	my $of=<>=~s/^\h+//r=~ s/\s+$//r;
	open W,">","$of" or die "Cannot open '$of'\n" if($of);
}}

SWC:
for ($O){
	if (/^r/i) {
		my (@ufpath,@fpath,$F);
		@path=sort{length($a)<=>length($b)} @path;		# filter out path whose head is as the shorter one
		for(@ufpath=@path){
			s/\//>/g; s/\[(\d+)\]/,$1/g;}
		$offE[my $k=$#path]=$offElem[0];
		OUT:
		for my $i (1..$#path) {
			for my $j (0..$i-1) {	$ufpath[$i]=~ /^$ufpath[$j]/ && next OUT;	}
			$offE[--$k]=$offElem[$i];
		}
		for(@offE){
			$whole=~ s/\A(\Q$_->[0]\E)\Q$_->[1]\E\s?(.*+)\Z/$1$2/s;}
		if (fileno W){ print W $whole;}	else { print $whole;}
		last SWC;
	}
	my ($i,$res);
	for (@path){
		$res.="\n$_:\n$offElem[$i++]->[1]\n";}
	if (fileno W){ print W $res;}	else { print $res;}
}
close W;
