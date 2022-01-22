#!/usr/bin/perl -w
use strict;

sub getNthElem {			# $_[0] searched el  $_[1]=nth  $_[2] whole el under which to search
 return not $_[2] =~/^(<[a-z]\w*[^>]*+>(?:(?'cnt'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|(?'node'<(\w++)[^>]*+>(?&cnt)*+<\/\g-1>)))*?(?=<$_[0]\b)((?&node))){$_[1]})(?{ @{$_[3]}=[substr($1,0,-length($5)), $5] })/
}

sub getAllNthE {			# $_[0] searched el  $_[1] el under which to search  $_[2] prev offset
	my $pre;
	return not $_[1] =~/^(<[a-z]\w*[^>]*+>)(?:((?'cnt'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|(?'node'<(\w++)[^>]*+>(?&cnt)*+<\/\g-1>)))*?)(?=<$_[0]\b)((?&node))(?{push (@{$_[3]}, [$_[2].$1.($pre.=$2), $6]); $pre.=$6}))*/
}

sub getAllDepth {		# $_[0] searched el  $_[1] nth  $_[2] el under which to search  $_[4] its offset  $_[5] path depth
	my ($nth, $ret, $min, $max, $E, $onref, $d, @nd,$offset,$offs) = ( $_[1], $_[3], $_[5] );
	my @curNode=[$_[4], $_[2]];
	if ($_[1]){
	while (@curNode) {
		for $onref (@curNode) {
			$onref->[1]=~
			/^(<[a-z]\w*[^>]*+>)(?{$offset=$1})
			(?'cnt'(?:
			((?'at'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>))*+)
			(?{$offs=$offset.=$3})
			(?:(?!<$_[0]\b)
			(?'inod' (?{ $max=0 })
			(?'node'<(\w++)[^>]*+>
			(?{$max=$d if ++$d>$max})
			(?>(?&at)|(?&node))*+<\/\g-1>(?{--$d})))
			(?{if ($max>$min) {
				push (@nd, [$onref->[0].$offset, $+{node}]);
				$offset.=$+{node}}})
			)?)*+
			(?=<$_[0]\b)(?'tnd'(?&inod))
			(?{ push (@nd, [$onref->[0].$offs, $+{tnd}]) if $max>$min;
			$offset=$offs.$+{tnd} })
			){$_[1]}
			(?{ push (@$ret, [$onref->[0].$offs, $+{tnd}]) if $max>=$min })
			(?&cnt)*/x
		}
		@curNode=@nd; @nd=();
	}}else {
	while (@curNode) {
		for $onref (@curNode) {
			$onref->[1]=~
			/^(<[a-z]\w*[^>]*+>)(?{$offset=$1}) (?:
			((?'at'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>))*+)
			(?{$offset.=$2})
			(?:(?=<$_[0]\b (?{$E=1}) |)
			(?'inod' (?{ $max=0 })
			(?'node'<(\w++)[^>]*+>
			(?{$max=$d if ++$d>$max})
			(?>(?&at)|(?&node))*+<\/\g-1>(?{--$d})))
			(?{if ($max>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{node}]) if $E;
				push (@nd, [$onref->[0].$offset, $+{node}]) if $max>$min;
				$E=0}
			$offset.=$+{node} })
			)?)* /x
		}
		@curNode=@nd; @nd=();
	}}
	return !@$ret
}

sub getAllDepthEatt {	# $_[0] searched el  $_[1] attribute  $_[2] el under which to search  $_[4] its offset  $_[5] depth
	my ($att, $ret, $min, $max, $E, $onref, $d, @nd,$offset) = ( $_[1], $_[3], $_[5] );
	my @curNode=[$_[4], $_[2]];
	while (@curNode) {
		for $onref (@curNode) {
			$onref->[1]=~
			/^(<[a-z]\w*[^>]*+>)(?{$offset=$1}) (?:
			((?'at'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>))*+)
			(?{$offset.=$2})
			(?:(?=(<$_[0]\s+$att) (?{$E=1}) |)
			(?'inod' (?{ $max=0 })
			(?'node'<(\w++)[^>]*+>
			(?{$max=$d if ++$d>$max})
			(?>(?&at)|(?&node))*+<\/\g-1>(?{--$d})))
			(?{if ($max>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{node}]) if $E;
				push (@nd, [$onref->[0].$offset, $+{node}]) if $max>$min;
				$E=0}
			$offset.=$+{node} })
			)?)* /x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}

sub getAllDepthAatt {	# $_[0] attribute  $_[1] el under which to search  $_[2] its offset  $_[4] depth
	my ($att, $ret, $min, $max, $E, $onref, $d, @nd,$offset) = ( $_[0], $_[3], $_[4] );
	my @curNode=[$_[2], $_[1]];
	while (@curNode) {
		for $onref (@curNode) {
			$onref->[1]=~
			/^(<[a-z]\w*[^>]*+>)(?{$offset=$1}) (?:
			((?'at'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>))*+)
			(?{$offset.=$2})
			(?:(?=(<\S+\s+$att) (?{$E=1}) |)
			(?'inod' (?{ $max=0 })
			(?'node'<(\w++)[^>]*+>
			(?{$max=$d if ++$d>$max})
			(?>(?&at)|(?&node))*+<\/\g-1>(?{--$d})))
			(?{if ($max>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{node}]) if $E;
				push (@nd, [$onref->[0].$offset, $+{node}]) if $max>$min;
				$E=0}
			$offset.=$+{node} })
			)?)* /x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}
# These subs return 1 on failure to find. Else 0 and offset & node pairs in the 4rd arg, $_[3]

my (@res, $MUL);
sub getE_Path_Rec { my $iOffNode = $_[1];
	my ($ADepth, $tag, $nth, $att, $alla, $path) =
	$_[0]=~ m{ ^(/)?/ (?> ([^/@[]+) (?:\[ (?>([1-9]+)|@([^]]+))? \])? | @([a-z]\w*) ) (.*) }x;
	$MUL |= !$nth;
	for (@$iOffNode) {
		my @OffNode;
		if ($ADepth) {
			my $depth=++(()=$path=~/\//g);
			if ( $tag? $att?
					getAllDepthEatt ($tag, $att, $_->[1], \@OffNode, $_->[0], $depth) :			# offset-node pair return is in @OffNode
					getAllDepth ($tag, $nth, $_->[1], \@OffNode, $_->[0], $depth) :
				getAllDepthAatt ($alla, $_->[1], $_->[0], \@OffNode, $depth)) {
				next if @res or $MUL;
				return 1}
		}elsif ($nth) {
			if (getNthElem ($tag, $nth, $_->[1], \@OffNode)) {	
				next if @res or $MUL;
				return 1}
			${$OffNode[0]}[0]=$_->[0].${$OffNode[0]}[0];
		}else {
			if (getAllNthE ($tag, $_->[1], $_->[0], \@OffNode )) {
				next if @res or $MUL;
				return 1}
		}
		if ($path) {	return getE_Path_Rec ($path, \@OffNode)			# to be propagated to the next depth
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
	print "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere lives (by breadth or depth) under the first div element, under any\nbody element, under any html element.\n\nTo put multiply at once, put one after another delimited by ;\nPut element path: ";
	die "No any Xpath given\n" if ($trPath=<>)=~/^\s*$/;
	for (split /;/,$trPath) {
		my $xpath=qr{^\h*(?:(/?/[a-z]\w*+(?:\[(?>[1-9]+|@[a-z]+(?:=\w+)?)\])?|/?/@[a-z]\w*)|\.\.?)(?1)*+[/\h]*$}i;
		if (/$xpath/) {
			s#\h|/+$##g;
			if (/^[^\/]/) {
				if(!$CP){
					print "\n'$_'\nis relative to base/current path and it's empty, so now give one:\n";
					print "\n'$CP' is not a valid xpath" while (($CP=<>=~s#\s|/$##gr) !~ $xpath);
					$CP=~s#\h|/+$##g
				}
				s#^\./##;
				if (/^\.\./) {	$CP=~s#/?[^/]+$##;	s#^../##	}
				$_="$CP/$_"
			}
			push (@valid, $_);
		}else {
			print "'$_' is invalid Xpath\nSkip or abort it? (S: skip.  any else: abort) ";my $y=<>;
			if ($y=~/^s/i) { next
			}else{	die "Aborting\n"}
	}}
	print "\nHTML/XML file name to process: ";
	my $file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "\n'$file' not exist\n";
	$!=2;open R,"$file" or die "\nCannot open '$file'\n";
	undef local $/;$whole=<R>;close R;
	print "\nProcessing HTML document on '$file'...\n"
}

die "\nCan't parse the HTML with ill form\nBecause likely of unbalanced tag pair\n" unless
$whole=~/^(<(?:!DOCTYPE|xml)[^>]*+>[^<]*)(<([a-z]\w*)[^>]*+>(?'cnt'(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>|<(\w++)[^>]*+>(?&cnt)*+<\/\g-1>))*?<\/\g3>).*/s;
my @in=[$1,$2]; my $wTAG=$3;

my ($E, @path, @fpath, @miss, @short);
for (sort{length $a cmp length $b} @valid) {
	if ($E) {
		print "\nSkip it to process the next path? (Y/Enter: yes. any else: Abort) ";
		<>=~/^(?:\h*y)?$/i or die "Aborting\n"}
	@res=();
	m{^/([a-z]\w*)(/.*)};
	if ($wTAG ne $1 or $E=&getE_Path_Rec ($2, \@in) or not @res and $E=1) {
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
	print "\nWhich operation will be done :\n- Remove\n- Get\n(R: remove   Else key: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name to save the result: (hit Enter to standard output) ';
	my $of=<>=~s/^\h+//r=~ s/\s+$//r;
	open W,">","$of" or die "Cannot open '$of'\n" if $of
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
