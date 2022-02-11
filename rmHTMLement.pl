#!/usr/bin/perl -w
use strict;

# HTML regexes
my $HEAD= qr/^<(?>[a-z]\w*+|!DOCTYPE)[^>]*+>/;
my $A=qr{(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)};			# Asymetric tag/text content
my $C= qr{((?>$A|<(\w++)[^>]*+>(?-2)*+</\g-1>))};			# Content of node in lazy mode repetition or none
my $ND= qr{(<([a-z]\w*+)[^>]*+>(?>$A|[^/>]*+/>|(?-2))*+</\g-1>)}; # Node that may have the content or head closing 

sub getNthEAtt {		# $_[0] searched el  $_[1]=nth/nth backw  $_[2] el under which to search  $_[4] nth backw $_[5] attr
	# obtain max nth +1 to solve backward nth
	if ($_[4]) {
		my $i=1; $_[2]=~/$HEAD(?:$C(?=<$_[0]\b)$ND(?{ ++$i }))+/;	$_[1]=$i-$_[4]}
	return not $_[2]=~ /($HEAD(?:$C*?(?=<$_[0]\b$_[5])($ND)){$_[1]})(?{ @{$_[3]}=[substr($1,0,-length($4)), $4] })/
}

sub getENthAllAtt {		# $_[1] el under which to search  $_[2] its offset  $_[4]) nth range pos $_[5] attr $_[6] all nodes 
	my ($att, $all, $a,$b,$i, $OFF,$off)= ($_[5],$_[6], 1, '+');
	if ($_[4]) {
		my ($lt,$e,$n)= $_[4]=~ /(?>(<)|>)(=)?(\d+)/;
		$b = $lt?	$e? "{$n}" : '{'.--$n.'}'	: ($a=$e? $n : $n+1, $b) }
	return not $_[1]=~ /($HEAD)
	(?{ $OFF=$_[2].$1 })
	(?: ($C*?) (?(?{$all}) | (?=<$_[0]\b$att)) ($ND)
	(?{ if (++$i>=$a) {	push (@{$_[3]}, [$OFF.($off.=$2), $5]); $off.=$5 } }) )$b /x
}

# Node regex to count depth
my ($MAX,$DTH);
my $N= qr{(?{ $MAX=0 })(<(\w++)[^>]*+>(?{ $MAX=$DTH if ++$DTH>$MAX })(?>$A|[^/>]*+/>|(?-2))*+</\g-1>(?{--$DTH}))};

sub getAllDepth {		# $_[1] nth/nth bckwrd  $_[2] search space element
															# $_[4] its offset  $_[5] depth  $_[6] nth bckwrd
	my ($ret, $min, $onref, @nd,$offset,$offs) = ($_[3], $_[5]);
	my @curNode=[$_[4], $_[2]];
	while (@curNode) {
		for $onref (@curNode) {
			if ($_[6]) {
				my $i=1; $onref->[1]=~ /$HEAD(?:$C*?(?=<$_[0]\b)$ND(?{ ++$i }))+/;	$_[1]=$i-$_[6]}
			$onref->[1]=~
			/($HEAD) (?{ $offset=$1 }) (?:
			(?: ($A*+) (?{$offs=$offset.=$2 })
			(?: (?!<$_[0]\b) (?'n'$N)
			(?{ push (@nd, [$onref->[0].$offset, $+{n}]) if $MAX>$min;
			$offset.=$+{n} })
			)? )*+
			(?=<$_[0]\b) (?'nd'$N)
			(?{ push (@nd, [$onref->[0].$offs, $+{nd}]) if $MAX>$min;
			$offset=$offs.$+{nd} })
			) {$_[1]}
			(?{ push (@$ret, [$onref->[0].$offs, $+{nd}]) if $MAX>=$min })
			(?: (?'a'$A*+) (?{ $offset.=$+{a} })
			(?'z'$N) (?{ push (@nd, [$onref->[0].$offset, $+{z}]) if $MAX>$min; $offset.=$+{z} })
			)*/x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}
sub getAllDepNthRnAtt	{				# in every nth or positon within range 
	my @curNode=[$_[2], $_[1]];
	my ($ret, $min, $att, $all, $M, $onref, @nd, $offset) = ($_[3], $_[4], $_[6], $_[7]);
	while (@curNode) {
		for $onref (@curNode) {
			my ($a,$b,$i)= (1, '+');
			if ($_[5]) {
				my ($lt,$e,$n)= $_[5]=~ /(?>(<)|>)(=)?(\d+)/;
				$b = $lt?	$e? "{$n}" : '{'.--$n.'}': ($a=$e? $n : $n+1, $b)	}
			$onref->[1]=~
			/($HEAD)(?{$offset=$1}) (?:
			($A*+)
			(?{$offset.=$2})
			(?:(?=<$_[0]\b$att (?{$M=1}))?
			(?'n'$N) (?{ if ($MAX>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{n}]) if $M and ++$i>=$a;
				push (@nd, [$onref->[0].$offset, $+{n}]) if $MAX>$min;
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
			/($HEAD)(?{$offset=$1}) (?:
			($A*+)
			(?{$offset.=$2})
			(?: (?=<\w+$att (?{$M=1}) )?
			(?'n'$N)
			(?{ if ($MAX>=$min) {
				push (@$ret, [$onref->[0].$offset, $+{n}]) if $M;
				push (@nd, [$onref->[0].$offset, $+{n}]) if $MAX>$min;
				$M=0}
			$offset.=$+{n} })
			)?)* /x
		}
		@curNode=@nd; @nd=();
	}
	return !@$ret
}
# Above subs' $_[0] : searched el tag or att. Returns 1 on failure finding, else 0 and offset & node pairs in the 4rd arg, $_[3]

my @res;
sub getE_Path_Rec {			# path,  offset - node pair
	my ($AllDepth, $tag, $nth,$nrev,$range, $attg, $aatt, $allnode, $path) = $_[0] =~
	m{ ^/ (/)? (?> ([^/@*[]+) (?> \[ (?>([1-9]+ | last\(\)-([1-9]+)) | position\(\)(?!<1)([<>]=?\d+) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*) }x;
	$attg=$attg? '\s+'.($attg eq '*'? '\S' : $attg) :'';
	$aatt=$aatt? '\s+'.($aatt eq '*'? '\S' : $aatt) :'';
	for (@{$_[1]}) {
		my @OffNode;
		if ($AllDepth) {
			my $depth=split /\//, $path=~ s/^\/\h*//r;
			if ($tag?	$nth?
					getAllDepth ($tag, $nth, $_->[1], \@OffNode, $_->[0], $depth, $nrev)					# offset-node pair return is in @OffNode..
					: getAllDepNthRnAtt ($tag, $_->[1], $_->[0], \@OffNode, $depth, $range, $attg)
				: getAllDepthAatt ($aatt, $_->[1], $_->[0], \@OffNode, $depth) ){
				next if \$_ != \${$_[1]}[-1];											# no error return yet check if there's next must-search
				return !@res}															# (if current reference not yet equal to the last's) so loop in the next
		}elsif ($nth) {
			if (getNthEAtt ($tag, $nth, $_->[1], \@OffNode, $nrev, $attg)) {	
				next if \$_ != \${$_[1]}[-1];
				return !@res}
			${$OffNode[0]}[0]=$_->[0].${$OffNode[0]}[0];
		}else {
			if (getENthAllAtt ($tag, $_->[1], $_->[0], \@OffNode, $range, $attg, $allnode)) {
				next if \$_ != \${$_[1]}[-1];
				return !@res}
		}
		if ($path)	{
				my $R=getE_Path_Rec ($path, \@OffNode);					# ..to always propagate to the next
				return $R if \$_ == \${$_[1]}[-1]
		}	else {
				push (@res, @OffNode)}
	}
	return
}

my ($whole, $trPath, @valid, $O, $CP);
if (@ARGV) {
	$trPath=shift;	$O=shift;
	undef local $/;$whole=<>
}else {
	print "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere under the first div element, under any body element,\nunder any html element.\n\nTo put multiply at once, put one after another delimited by ; or |\nPut element path: ";
	die "No any Xpath given\n" if chomp($trPath=<>)=~/^\s*$/;
	my $xpath=qr{ ^ \h* (?:
	(//? ([a-z]\w*+) (?:\[ (?> [1-9]+ | last\(\)-[1-9]+ | position\(\)(?!<1)[<>]=?[1-9]+ | @(?'a'(?>(?2)(?:=(?2))? |\*)) ) \])? | //?@(?&a) |//?\*) | \.\.? ) (?1)*+ [/\h]*$ }x;
	for (split /[|;]/,$trPath) {
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
			die "Aborting\n" unless $y=~/^s/i
	}}
	print "\nHTML/XML file name to process: ";
	my $file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "\n'$file' not exist\n";
	$!=2;open R,"$file" or die "\nCannot open '$file'\n";
	undef local $/;$whole=<R>;close R;
	$|=1; print "\nChecking HTML document '$file'... "
}
die "can't parse it due to its ill-formed of HTML unbalanced tag pair\n" unless $whole=~ /^
(\s* (?:<\?xml\b[^>]*+>\s*)?) (<(!DOCTYPE)[^>]*+>[^<]* $ND $C*$ ) /x;
my @in=[  $1, $2."</$3>" ];
my ($ER, @path, @fpath, @miss, @short);
for (sort{length $a cmp length $b} @valid) {
	if ($ER) {
		print "\nSkip it to process the next path? (Y/Enter: yes. any else: Abort) ";
		<>=~/^(?:\h*y)?$/i or die "Aborting\n"}
	@res=();
	if ( $ER = getE_Path_Rec ($_, \@in)) {
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
	}else{	print "\n\nNothing was done";exit
}}
unless	(@ARGV){
	print "\n\nWhich operation will be done :\n- Remove\n- Get\n(R: remove   Else key: just get it) ";
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
#$r=qr/ fo(?:o|k|l)\h*bar/ ; $s = 'fo(?:o|k|l)\h*bar'  ;cmpthese( 0, {
  #re , sub { 'foobar'=~$r },
  #str , sub { 'foobar'=~/$r/ },
#})
