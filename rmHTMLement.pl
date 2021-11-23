#!/usr/bin/perl -w

sub getElem{my ($ELEM, $NTH)=@_;	--$NTH;
	 return ('',undef) unless $_[2]=~ /\A(<\w+[^>]*+>(?:(?>(?'at'[^<>]|<(?>meta|link|input|img|hr|base|!DOCTYPE)\b[^>]*+>)|(?'node'<(\w++)[^>]*+>(?>(?&at)|(?&node))*+<\/\g-1>))*?(?'tnode'(?=<$ELEM\b[^>]*+>)(?&node))){$NTH}(?>(?&at)|(?&node))*?)((?&tnode))/s;
	return ($1,$6);
}

sub getElePath{
	my $offset='', $under=$_[1];
	for (split(/\/|->|>/,$_[0])) {
		/^(\w++)(?|\[(\d+)\]|,(\d+))?/;
		($off,$under)=getElem($1, $2?$2:1, $under);
		die "$_[0] is not found in $file\n" unless $under;
		$offset.=$off;
	}
	return ($offset,$under);
}
sub getElePathWDoc{
	($off,$el) = getElePath($_[0],"<d>$whole");	# global, with dummy prefix
	return ($off=~s/\A<d>//r,$el);
}

if (@ARGV) {
	$paths=shift;
	$O=shift;
	undef local $/;$whole=<>;}
else {
	print "Element path is form of Xpath e.g:\n\nhtml/body/div/div[2]/div[3]\n\n/ may be replaced with > or -> and [2] with ,2 e.g:\nhtml>body>div>div,2>div,3\n\nIt may be put multiply, delimited by ;\n\n";
	print "File name to work on: ";
	$file=<>=~s/^\h+//r=~ s/\s+$//r;
	$!=1;-e $file or die "'$file' not exist\n";
	$!=2;open R,"$file" or die "Cannot open '$file'\n";
	print 'Path of the element: ';
	$paths=<>=~s/^\h*\/?//r=~ s/\/?\s*$//r;
	print "\nWhich operation will be done :\n- Remove\n- Get\n(r: remove, else: just get it) ";
	$O=<>=~s/^\h+//r=~ s/\s+$//r;
	print 'File name of the result: (hit Enter to standard output) ';
	$fn=<>=~s/^\h+//r=~ s/\s+$//r;
	undef local $/;$whole=<R>;close R;
	open W,">","$fn" or die "Cannot open '$fn'\n" if($fn);
}
@path=split(/;/,$paths);
SWC:
for ($O){
	if (/^r/) {
		@path=sort{length($a)<=>length($b)} @path;		# filter out any path having the exact same head to the shorter
		my $i=0,$k=$#path, $fpath[$k]=$path[0];
		MAIN:
		for $i (1..$#path) {
			for $j (0..$i-1) {
				$path[$i]=~ /^$path[$j]/ and next MAIN;}
			$fpath[--$k]=$path[$i];
		}
		for(@fpath){
			@e=getElePathWDoc($_);
			$whole=~ s/\A(\Q$e[0]\E)\Q$e[1]\E(.*+)\Z/$1$2/s;
		}
		if ($fn) { print W $whole;}
		else { print $whole;}
		last SWC;
	}
	$res='';
	for(@path){
		@e=getElePathWDoc($_);
		$res.="\n$_:\n$e[1]";}
	if ($fn) { print W $res;}
	else { print $res;}
}
close W;

