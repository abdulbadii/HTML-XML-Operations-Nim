#!/usr/bin/perl -w
use strict;

# Node regexes
my $HEAD= qr/<[a-z]\w*[^>]*+>/;
my $A=qr/(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)/;			# Asymetric tag/text content. Atomic repetition is:
my $AT= qr/$A*+/;
my $CT= qr/((?>$A|<(\w++)[^>]*+>(?-2)*+<\/\g-1>))/;			# Content of node. Its lazy repetition is:
my $C= qr/$CT*?/;
my $ND= qr/<(?'t'\w++)[^>]*+>$CT*+<\/\g{t}>/;			# The node having the content

sub getNthEAtt {		# $_[0] searched el  $_[1]=nth/nth backw  $_[2] el under which to search  $_[4] nth backw $_[5] attr
	# obtain max nth +1 to solve backward nth
	if ($_[4]) {
		my $c=1; $_[2]=~/^$HEAD(?:$C(?=<$_[0]\b)$ND(?{ ++$c }))+/;	$_[1]=$c-$_[4]}
	return not $_[2]=~ /^($HEAD(?:$C(?=<$_[0]\b$_[5])($ND)){$_[1]})(?{ @{$_[3]}=[substr($1,0,-length($4)), $4] })/
}

sub getAllNthEAtt {		# $_[1] el under which to search  $_[2] its offset  $_[4]) nth range pos $_[5] attr $_[6] all nodes 
	my ($att, $all, $a,$b,$i, $OFF,$off)= ($_[5],$_[6], 1, '+');
	if ($_[4]) {
		my ($lt,$e,$n)= $_[4]=~ /(?>(<)|>)(=)?(\d+)/;
		$b = $lt?	$e? "{$n}" : '{'.--$n.'}'	: ($a=$e? $n : $n+1, $b)
	}
	return not $_[1] =~/^($HEAD) (?{ $OFF=$_[2].$1 })
	(?: ($C)
	(?(?{$all}) | (?=<$_[0]\b$att)) ($ND)
	(?{ if (++$i>=$a) {
		push (@{$_[3]}, [$OFF.($off.=$2), $5]); $off.=$5}
	})) $b /x
}

{# Node regex tp count depth
my ($max,$dth);
my $N= qr/(?{ $max=0 })(<(\w++)[^>]*+>(?{ $max=$dth if ++$dth>$max })(?>$A|(?-2))*+<\/\g-1>(?{--$dth}))/;

sub getAllDepth {		# $_[1] nth/nth bckwrd  $_[2] search space ele $_[4] its offset $_[5] depth  $_[6] nth bckwrd
	my ($ret, $min, $E, $onref, @nd,$offset,$offs) = ($_[3], $_[5]);
	my @curNode=[$_[4], $_[2]];
	while (@curNode) {
		for $onref (@curNode) {
			if ($_[6]) {
				my $c=1; $onref->[1]=~ /^$HEAD(?:$C(?=<$_[0]\b)$ND(?{ ++$c }))+/;	$_[1]=$c-$_[6]}
			$onref->[1]=~
			/^($HEAD) (?{ $offset=$1 }) (?:
			(?: ($AT) (?{$offs=$offset.=$2 })
			(?: (?!<$_[0]\b) (?'n'$N)
			(?{ push (@nd, [$onref->[0].$offset, $+{n}]) if $max>$min;
			$offset.=$+{n} })
			)? )*+
			(?=<$_[0]\b) (?'nd'$N)
			(?{ push (@nd, [$onref->[0].$offs, $+{nd}]) if $max>$min;
			$offset=$offs.$+{nd} })
			) {$_[1]}
			(?{ push (@$ret, [$onref->[0].$offs, $+{nd}]) if $max>=$min })
			(?: (?'a'$AT) (?{ $offset.=$+{a} })
			(?'z'$N) (?{ push (@nd, [$onref->[0].$offset, $+{z}]) if $max>$min; $offset.=$+{z} })
			)*/x
		}
		@curNode=@nd; @nd=();
	}
}
sub getAllDepNthRnAtt	{				# in every nth or positon within range 
	my @curNode=[$_[2], $_[1]];
	my ($ret, $min, $att, $all, $M, $onref, @nd,$offset) = ($_[3], $_[4], $_[6], $_[7]);
	while (@curNode) {
		for $onref (@curNode) {
			my ($a,$b,$i)= (1, '+');
			if ($_[5]) {
				my ($lt,$e,$n)= $_[5]=~ /(?>(<)|>)(=)?(\d+)/;
				$b = $lt?	$e? "{$n}" : '{'.--$n.'}': ($a=$e? $n : $n+1, $b);
			}
			$onref->[1]=~
			/^($HEAD)(?{$offset=$1}) (?:
			($AT)
			(?{$offset.=$2})
			(?:(?=<$_[0]\b$att (?{$M=1}))?
			(?'n'$N) (?{ if ($max>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{n}]) if $M and ++$i>=$a;
				push (@nd, [$onref->[0].$offset, $+{n}]) if $max>$min;
				$M=0}
			$offset.=$+{n} })
			)?) $b /x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}

sub getAllDepthAatt {	# $_[0] attribute  $_[1] el under which to search  $_[2] its offset  $_[4] depth
	my ($att, $ret, $min, $M, $onref, @nd,$offset)= ($_[0], $_[3], $_[4]);
	my @curNode=[$_[2], $_[1]];
	while (@curNode) {
		for $onref (@curNode) {
			$onref->[1]=~
			/^($HEAD)(?{$offset=$1}) (?:
			($AT)
			(?{$offset.=$2})
			(?: (?=<\w+$att (?{$M=1}) )?
			(?'n'$N)
			(?{ if ($max>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{n}]) if $M;
				push (@nd, [$onref->[0].$offset, $+{n}]) if $max>$min;
				$M=0}
			$offset.=$+{n} })
			)?)* /x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}
}# Above subs' $_[0] : searched el tag or att. Returns 1 on failure finding, else 0 and offset & node pairs in the 4rd arg, $_[3]

my @res;
sub getE_Path_Rec {			# path,  offset - node pair
	my ($AllDepth, $tag, $nth,$nrev,$range, $attg, $aatt, $allnode, $path, $R)=$_[0]=~
	m{ ^(/)?/ (?> ([^/@*[]+) (?> \[ (?>([1-9]+ | last\(\)-([1-9]+)) | position\(\)(?!<1)([<>]=?\d+) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*) }x;
	$attg=$attg? '\s+'.($attg eq '*'? '\S' : $attg) :'';
	$aatt=$aatt? '\s+'.($aatt eq '*'? '\S' : $aatt) :'';
	for (@{$_[1]}) {
		my @OffNode;
		if ($AllDepth) {
			my $depth=1+(()=$path=~/\//g);					# offset-node pair return is in @OffNode..
			if ($tag?	$nth?
					getAllDepth ($tag, $nth, $_->[1], \@OffNode, $_->[0], $depth, $nrev)
				: getAllDepNthRnAtt ($tag, $_->[1], $_->[0], \@OffNode, $depth, $range, $attg)
				: getAllDepthAatt ($aatt, $_->[1], $_->[0], \@OffNode, $depth) ){
				next if \$_->[0] != \${$_[1]}[-1];			# no error return yet if there's the next node, not equal to last reference
				return !@res}
		}elsif ($nth) {
			if (getNthEAtt ($tag, $nth, $_->[1], \@OffNode, $nrev, $attg)) {	
				next if \$_->[0] != \${$_[1]}[-1];
				return !@res}
			${$OffNode[0]}[0]=$_->[0].${$OffNode[0]}[0];
		}else {
			if (getAllNthEAtt ($tag, $_->[1], $_->[0], \@OffNode, $range, $attg, $allnode)) {
				next if \$_->[0] != \${$_[1]}[-1];
				return !@res}
		}
		if ($path)	{		$R=getE_Path_Rec ($path, \@OffNode)					# ..to always propagate to the next
		}	else {					push (@res, @OffNode)	}
	}
	return $R
}

my ($whole, $trPath, @valid, $O, $CP);
if (@ARGV) {
	$trPath=shift;	$O=shift;
	undef local $/;$whole=<>
}else {
	print "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere under the first div element, under any\nbody element, under any html element.\n\nTo put multiply at once, put one after another delimited by ; or |\nPut element path: ";
	die "No any Xpath given\n" if chomp($trPath=<>)=~/^\s*$/;
	for (split /[|;]/,$trPath) {
		my $xpath=qr{^ \h* (?:
		(//? ([a-z]\w*+) (?:\[ (?> [1-9]+ | last\(\)-[1-9]+ | position\(\)(?!<1)[<>]=?[1-9]+ | @(?'a'(?>(?2)(?:=(?2))? |\*)) ) \])? | //?@(?&a) |//?\*) | \.\.? ) (?1)*+ [/\h]*$ }x;
		if (/$xpath/) {
			s#\h|/+$##g;
			if (/^[^\/]/) {
				if(!$CP){
					print "\n'$_'\nis relative to base/current path which is now empty, specify one:\n";
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
	print "\nProcessing HTML document '$file'...\n"
}

die "\nCan't parse the ill formed HTML because likely of unbalanced tag pair\n" unless
$whole=~m{^(<(?>!DOCTYPE|xml)[^>]*+>[^<]*)(<([a-z]\w*)[^>]*+>$C</\g3>)$C};
my @in=[$1,$2]; my $firsTAG=$3;

my ($ER, @path, @fpath, @miss, @short);
for (sort{length $a cmp length $b} @valid) {
	if ($ER) {
		print "\nSkip it to process the next path? (Y/Enter: yes. any else: Abort) ";
		<>=~/^(?:\h*y)?$/i or die "Aborting\n"}
	@res=();
	m{^/([a-z]\w*)(/.*)};
	if ($firsTAG ne $1 or $ER=getE_Path_Rec ($2, \@in)) {
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
if($#path) {print "\nProcessing the path:";print "\n$_->[0]" for(@path)}

for ($O){
if (! /^r/i) {
	my $o;for (@path) {
		$o.="\n$_->[0]:";
		$o.="\n-------------\n$_->[1]\n" for @{$_->[1]}
	}
	fileno W? print W $o:print $o;
	last
}

@fpath=sort {length $b->[0] <=> length $a->[0]} @fpath;
$whole=~ s/\A(\Q$_->[0]\E)\Q$_->[1]\E(.*)\Z/$1$2/s	for (@fpath);
fileno W? print W $whole:print "\n\nRemoval result:\n$whole"
}
close W;
