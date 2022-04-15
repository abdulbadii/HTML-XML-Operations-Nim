import std/[os, terminal, nre, math, algorithm, macros]

template  strUint( s :string) :uint=
 var res :uint
 for i, c in s:
  if c in '0'..'9': res += ((ord(c) - 48) * 10^(s.high-i)).uint
  else: raise newException(ValueError, "Non-number in string")
 res
template numChr( c :char, s: string) :uint=
 var res :uint
 for i in s:
  if i==c: res.inc
 res
    # ML regexes       text node & comment/asymetric tag as content of text node:
let
 txNodeR= r"(?:[^<>]++(?><!--[^->]*-->|<(?>meta|link|input|img|hr|base)\b[^>]*+>)*)+"
 ctntR= r"(?:[^<>]*+(?><!--[^->]*-->|<(?>meta|link|input|img|hr|base)\b[^>]*+>)*)*+" # as of element node
 ctnt= re("(?s)^(" & ctntR & ")(.+)")
 nodeR= r"(<([a-z]\w*+)(?>[^/>]*+/>|[^>]*+>(?:" & ctntR & r"(?-2)*+)*+</\g-1>))"
 headR= r"(<(?>[a-z]\w*+|!DOCTYPE)[^>]*+>)"
 head= re("(?s)^" & headR & "(.+)")
var
 whole, offset, res, remain :string
 totN, maxND :uint

template nodeB( tot, max, d :uint)=
 if m.isNone: return false
 let g=m.get.captures.toSeq
 tot.inc; d.inc; if d>max: max=d
 var
   tagname= g[0].get
   off = "<" & tagname
   str = g[3].get
 if g[1].isSome:
   res= off & g[1].get
   dec(d)
   if d==0:
    remain= str
    totN=tot; maxND=max
   return true
 off &= g[2].get
 while true:
   let m = str.find ctnt
   off &= m.get.captures[0]
   str= m.get.captures[1]
   if str[0..1] == "</" :
     tagname &= ">"
     if str[ 2..<2+tagname.len] == tagname:
       res= off & "</" & tagname
       dec(d)
       if d==0:
        remain= str[ 2+tagname.len..^1]
        totN=tot; maxND=max
       return true
     else: break
   elif nodeRec( str):
     off &= res
     str= str[ res.len..^1]
   else: break
 return false

# 2 node functions to find direct closed tag or nested node content, it'd count the max depth, nodes etc
# they're just header; with and without lookahead tag fed to the recursive node function inside
proc node( str= remain) :bool=
 var tot, max, d :uint
 proc nodeRec( str :string) :bool=
  let m= str.find re"(?xs) ^<([a-z]\w*+) (?> ([^/>]*+/>) | ([^>]*+>) ) (.+)"
  nodeB tot, max, d
 nodeRec str

proc node( str, tag :string) :bool=
 var tot, max, d :uint
 proc nodeRec( str :string; tag="") :bool=
  let m= str.find re("(?xs) ^<(" & tag & r"[a-z]\w*+) (?> ([^/>]*+/>) | ([^>]*+>) ) (.+)" )
  nodeB tot, max, d
 nodeRec str, tag

template getOffUp2Tag( tag="") :bool=
 let m= remain.find re(
  "(?s)^((?:" & ctntR & "(?:(?!<" & tag & r"\b)" & nodeR & ")?)*+)(?=<" & tag & r"\b)" & nodeR & "(.+)" )
 if m.isNone: false
 else:
  offset &= m.get.captures[0]
  res= m.get.captures[3]
  remain= m.get.captures[5]
  true

type
 Pair = enum
  Offset, Node
 StrPair = array[ Pair,string]
 OffNodArray = seq[ StrPair]

template ctnPrevTag( nd :OffNodArray; isTheTagNode) =
 while true:
  let m= remain.find ctnt
  offset &= m.get.captures[0]
  remain = m.get.captures[1]
  if isTheTagNode:
   if maxND > minD: nd.add [offset, res]
   offset &= res
  else: break

template ctnoTagUp2Tag( nd :OffNodArray; notTag, tag :string; xSpeCmd) =
 ctnPrevTag nd:
  node remain, notTag
 if node( remain, tag):
  if maxND > minD: nd.add [offset, res]
  xSpeCmd
  offset &= res
 else: break

template headeRemain( nd :string, prevOff="")=
 var m = nd.find head
 offset= prevOff & m.get.captures[0]
 remain= m.get.captures[1]

const
 isERROR  = true
 isSUCCEED= false

template posiN( posn :string)=
 var
  a {.inject.}, b {.inject.}:uint
  g = posn.find(re"(?>(<)|>)(=)?(\d+)").get.captures.toSeq
  eq= g[1].isSome
  n = g[2].get.strUint       # Get a-b as lower-upper bound
 if g[0].isSome:
       b= if eq: n else: n-1
 else: a= if eq: n-1 else: n

proc getxNRev( n :var string) :bool=
 var
  i :uint
  n_uint= n.strUint
 while true:
  inc(i)
  let m= remain.find re( r"^(?:" & nodeR & "*" & txNodeR & "){" & $i & "}" )
  if m.isNone: break
 if i<=n_uint: return false
 n= $(i-n_uint)         # i is the max nth from which subtract the specified nth
 true

proc getNthRev( tag = ""; ntha :var uint) :bool=
 var i :uint
 while true:
  i.inc
  let m= remain.find re(
   "^((?:(?:" & ctntR & "(?:(?!<" & tag & r"\b)" & nodeR & ")?)*+(?=<" & tag & r"\b)" & nodeR & "){" & $i & "})" )
  if m.isNone: break
 if i<=ntha: return false
 ntha = i-ntha
 true

template getTextNth( ret :OffNodArray; off, nod, n :string; txNRev:bool) :bool=
 nod.headeRemain off
 if txNRev and not getxNRev( n): isERROR
 else:
  let m= remain.find re( r"^((?:" & nodeR & "*(" & txNodeR & ")){" & n & "})" )
  if m.isNone: isERROR
  else:
   var r= m.get.captures[3]
   ret.add [offset & m.get.captures[0][0..^r.len+1], r]
   isSUCCEED

macro getTextMulN( ret :var OffNodArray; off, nod, posn :string) :bool=
 result = quote do:
  `nod`.headeRemain `off`
  var a,b,i :uint
  if `posn` != "": `posn`.posiN
  while true:
   inc(i)
   let m= remain.find re( r"^((?:" & nodeR & "*(" & txNodeR & ")){" & $i & "})" )
   if m.isNone: break
   if i>a and (b==0 or i<=b):
    var r= m.get.captures[3]
    `ret`.add [offset & m.get.captures[0][0..^r.len+1], r]
  `ret`.len==0

template aDInit {.dirty.}=
 var curNode, nd= newSeqOfCap[ array[ 2, string]](avgOffNode_Ply)
 for _ in 0..avgOffNode_Ply:
  nd.add [newStringOfCap(maxw), newStringOfCap(maxw)]
  curNode.add [newStringOfCap(maxw), newStringOfCap(maxw)]
 curNode.reset
 curNode.add [this[Offset], this[Node]]

template getAllDepthTxNth( ret :var OffNodArray; off, nod :string; nth :uint; txNRev:bool) :bool=
 aDInit
 while curNode.len>0:
  nd.reset
  for o_n in curNode:
   getTextNth ret, o_n[0], o_n[1], nth, txNRev
   while true:
    if getOffUp2Tag:
     nd.add [offset, res]
    else:
     break

 ret.len==0
 
template getE_Nth( ret :OffNodArray; off, nod, tag :string; n:uint; nthRev:bool) :bool=
 nod.headeRemain off
 if nthRev and not getNthRev( tag, n): isERROR
 else:
  let m= remain.find re(
    r"^((?:(?:" & ctntR & "(?:(?!<" & tag & r"\b)" & nodeR & ")?)*+(?=<" & tag & r"\b)" & nodeR & "){" & $n & "})")
  if m.isNone: isERROR
  else:
   var r= m.get.captures[3]
   ret.add [offset & m.get.captures[0][0..^r.len+1], r]
   isSUCCEED

macro getE_MultiN( ret :var OffNodArray; off, nod, tag, posn, att :string) :bool=
 result = quote do:
  `nod`.headeRemain `off`
  var
   a,b,i :uint
   tag = `tag`
  if `posn` != "": `posn`.posiN
  elif `att` != "": tag &= r"\s+" & `att`
  while true:
   if getOffUp2Tag( `tag`):
    i.inc
    if i>a and (b==0 or i<=b):
     `ret`.add [offset, res]
    offset &= res
   else: break
  `ret`.len==0

template getE_MultiN( ret :var OffNodArray; off, nod, aatt :string) :bool=
 nod.headeRemain off
 while true:
  if getOffUp2Tag( r"\S+\s+" & aatt):
   ret.add [offset, res]
   offset &= res
  else: break
 ret.len==0

template getE_MultiN( ret :var OffNodArray; off, nod :string) :bool=
 nod.headeRemain off
 while true:
  if getOffUp2Tag:
    ret.add [offset, res]
    offset &= res
  else: break
 ret.len==0

template getAllDepthNth( ret :var OffNodArray; off, nod, tag :string; minD, nth :uint; nthRev:bool) :bool=
 var
  notTag = "(?!" & tag & r"\b)"
  ttag = "(?=" & tag & r"\b)"
 aDInit
 template ePssblNodeLoop( nthRevCondition)=
  while curNode.len>0:
   nd.reset
   for o_n in curNode:
    o_n[1].headeRemain o_n[0]
    nthRevCondition
   curNode= nd
 template loopB(n:uint)=
  var i:uint
  while true:
   inc(i)
   ctnoTagUp2Tag nd, notTag, ttag:
    if i==n and maxND >= minD: ret.add [offset, res]
 if nthRev:
  ePssblNodeLoop:
   var n=nth
   if getNthRev( tag, n): n.loopB
 else:
  ePssblNodeLoop: nth.loopB
 ret.len==0

template loop( foundCmd)=
 aDInit
 while curNode.len>0:
  nd.reset
  for o_n in curNode:
   var i {.inject.} :uint
   o_n[1].headeRemain o_n[0]
   while true:
    ctnoTagUp2Tag nd, notTag, theTag:
     foundCmd
  curNode=nd
template getAllDepthMultiN( ret :var OffNodArray; o, n :string; minD:uint; tag:string; posn, att="") :bool=
 var
  a,b:uint
  notTag {.inject.} = "(?!" & tag & r"\b"
  theTag {.inject.} = "(?=" & tag & r"\b"
 if posn != "": posn.posiN
 elif att != "":
  notTag &= r"\s+" & att; theTag &= r"\s+" & att
 notTag &= ")"; theTag &= ")"
 loop:
  i.inc
  if i>a and (b==0 or i<=b) and maxND >= minD:
   ret.add [offset, res]
 ret.len==0
template getAllDepthMultiN( ret :var OffNodArray; o, n :string; minD :uint; aatt :string) :bool=
 var
  notTag {.inject.}= r"(?!\S+\s+" & aatt & ")"
  theTag {.inject.}= r"(?=\S+\s+" & aatt & ")"
 loop:
  if maxND >= minD: ret.add [offset, res]
 ret.len==0
template getAllDepthMultiN( ret :var OffNodArray; o, n :string; minD :uint) :bool=
 var notTag{.inject.},theTag{.inject.}=""
 loop:
  if maxND >= minD: ret.add [offset, res]
 ret.len==0

template offsetNodeLoop( xPathPat) =
 for i, this {.inject.} in offsetNode:
  retOffNode.reset
  if xPathPat:
   if i<offsetNode.high: continue     # if it's not the last in loop, go on iterating, otherwise
   return resultArr.len==0              # return true (1) if finding none or false if finding any
  if remPath.len>0:
   let e= getE_Path_R( remPath, retOffNode)   #...is propagating to the next depth and returning boolean
   if i==offsetNode.high: return e            # value which'll be returned if this's the last iteration 
  else:
   resultArr.add retOffNode

var
 avgOffNode_Ply, maxw :uint
 resultArr :OffNodArray

proc getE_Path_R( path :string; offsetNode :OffNodArray) :bool=
 var
  g= path.find(re"(?x)^/ (/)? (?> (text\(\)) (?: \[ (?> (last\(\)-)? ([1-9]\d*+) | position\(\) (?!<1) ([<>]=? [1-9]\d*+) ) \] )? | ([^/@*[]+) (?: \[ (?> (?>(last\(\)-)|position\(\)=)? ([1-9]\d*+) | position\(\) (?!<1) ( [<>]=? [1-9]\d*+ ) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*)" ).get.captures.toSeq
  nth :uint
  txNth, txPos, tag, posn, attg, aatt :string
  remPath= g[12].get
  minD= 1+numChr( '/', remPath)
  retOffNode= newSeqOfCap[ StrPair]( avgOffNode_Ply) # retOffNode is offset-node found which...
 for _ in 0..avgOffNode_Ply:
  retOffNode.add [newStringOfCap(maxw),newStringOfCap(maxw)]
 template isAllDepths:bool= g[0].isSome
 template isTxNode   :bool= g[1].isSome
 template txNRev     :bool= g[2].isSome
 template isTxNth:bool= g[3].isSome
 template isTxPos:bool= g[4].isSome
 template isTag  :bool= g[5].isSome
 template nthRev :bool= g[6].isSome
 template isNth  :bool= g[7].isSome
 template isPosn :bool= g[8].isSome
 template isAttg :bool= g[9].isSome
 template isAatt :bool= g[10].isSome
 if isTxNode:
  if isTxNth:
   txNth= g[3].get
  elif isTxPos:
   txPos= g[4].get
 elif isTag:
  tag= g[5].get
  if isNth:
   nth= g[7].get.strUint
  elif isPosn:
   posn= g[8].get
  elif isAttg:
   attg= g[9].get
 elif isAatt:
  aatt= g[10].get
 if isAllDepths:              # all depths under current //
  if isTag:
   if isNth:
    offsetNodeLoop: getAllDepthNth retOffNode, this[Offset], this[Node], tag, minD, nth, nthRev
   else:
    offsetNodeLoop: getAllDepthMultiN retOffNode, this[Offset], this[Node], minD, tag, posn, attg
  elif isAatt:
   offsetNodeLoop: getAllDepthMultiN retOffNode, this[Offset], this[Node], minD, aatt=aatt
  else:
   offsetNodeLoop: getAllDepthMultiN retOffNode, this[Offset], this[Node], minD
 elif isTag:
  if isNth:
   offsetNodeLoop: getE_Nth retOffNode, this[Offset], this[Node], tag, nth, nthRev
  else:
   offsetNodeLoop: getE_MultiN retOffNode, this[Offset], this[Node], tag, posn, attg
 elif isAatt:
  offsetNodeLoop: getE_MultiN retOffNode, this[Offset], this[Node], aatt
 elif isTxNode:
  if isTxNth:
   offsetNodeLoop: getTextNth retOffNode, this[Offset], this[Node], txNth, txNRev
  else:
   offsetNodeLoop: getTextMulN retOffNode, this[Offset], this[Node], txPos
 else:
  offsetNodeLoop: getE_MultiN retOffNode, this[Offset], this[Node]
         # be any of these true, it failed finding, so see if
 isSUCCEED

var
 aCP, outf :string
 paths :seq[ string]
template xPathsCheck( path:string; hasTarget="")=
 paths.reset
 for p in path.split re"[|;]":
  if p.contains xpath:
    if p.contains re"^[^/]":
      if aCP.len == 0:
       echo "\n'",p,"' is relative to base/current path which is empty"
       while true:
        echo "\nPut a valid one: "
        aCP = readLine(stdin).replace( re"\h+", "")
        if aCP.contains xpath: break
        echo "\n'",aCP,"' is not a valid Xpath"
      aCP &= "/"
      var pr= p.replace( re"^\./", "")
      while pr.contains(re"^\.\.") :
        if aCP == "/" : echo "\n'", pr,"' upward node ..\nran over '",aCP,"' current path";quit(0)
        aCP = aCP.replace( re"[^/]+/$", "")
        pr = pr.replace( re"^../?", "")
      paths.add( aCP & pr)
    else: paths.add( p)
  else:
   echo "\n'",p,"' is invalid Xpath\nSkip? (s: skip. else: abort): "
   if getch() != 's': echo "\nAborting";quit(1)
 let totPaths {.inject.}= paths.len.uint
 if totPaths==0: quit("\nNo valid xpath " & hasTarget,0)

template getDocFile( f :string)=
 f.getDocFile(whole)
template getDocFile( f, w :string)=
 while true:
  if fileExists(f):
   try: w = readFile f
   except IOError as e:
    echo "\nCannot read '",f,"': ",e.msg
    continue
   except OverflowDefect as e:
    echo e.msg
    continue
   except:
    echo "\nFile '",outf,"': critical error"
    continue
   finally:break
  else:
   echo "'",f,"' doesn't exist"
  while true:
   stdout.write "Try another file name:"
   f=readLine(stdin).replace(re"^\h+|\s+$", "")
   if f.len>0: break

template validatingML=
 validatingML( whole)
template validatingML( w :string)=
 let m= w.find re(
  r"(?xs)^(\s* (?: <\?xml\b [^>]*+> \s* )?) (< (!DOCTYPE) [^>]*+> [^<]* (.+))" )
 if m.isNone or
  not m.get.captures[3].node or (let r=remain.replace(re"^\s+|\s+$",""); r).len>0 and
  not r.contains(re( r"\s*(?:" & nodeR & ")*")):
   echo "\nCan't parse it due to mark-up language's ill-form or unbalanced tag pair\nAborting"
   quit(0)
 iniNode= @[[m.get.captures[0], m.get.captures[1] & "</" & m.get.captures[2] & ">"]]

template unsortRes( fnd)=
 foundd=""
 for i in pathResult:
  foundd &= "\n" & i[0] & ":"
  for j{.inject.} in i[1]:
   foundd &= "\n--------\n" & j[Node];fnd

template path_search_H=
 avgOffNode_Ply= (totN.float / maxND.float * 1.5 ).uint
 miss = newSeqOfCap[ string ](totPaths)
 let maxFouND = (totN.float * 3 / 4).uint
 fpath= newSeqOfCap[ array[ 2, string] ](maxFouND)
 maxw= (whole.len-17).uint
 offset= newStringOfCap(maxw)
 res   = newStringOfCap(maxw)
 remain= newStringOfCap(maxw)
 for _ in 0..maxFouND:
  resultArr.add [newStringOfCap(maxw), newStringOfCap(maxw)]
  fpath.add [newStringOfCap(maxw), newStringOfCap(maxw)]
 for _ in 0..totPaths:
  pathResult.add (newStringOfCap(71), fpath)
template path_search_B( asTarget="")=
 fpath.reset
 pathResult.reset
 var fail :bool
 paths.sort( proc( a,b :string) :int= cmp(a.len, b.len) )
 for aPath in paths:
  if fail:
   echo "Skipping it, searching for the next path"
  resultArr.reset
  fail= getE_Path_R( aPath, iniNode)
  if fail:
   miss.add aPath; echo "Can't find:\n",aPath
  else:
   pathResult.add (aPath, resultArr)       # tuple of each path's array of offset-node found
   block F:
    for s in short:         # filter out duplicate path or path whose head as the same as shorter one's
     if aPath.contains(re(r"^\Q" & s & r"\E")): break F
    fpath.add(resultArr)
   short.add aPath
 if miss.len>0:
  if pathResult.len>0:
    echo "\nSkip every unfound path and keep going for the found ones? (y: Yes. else key: Abort) "
    if getch()=='y':
     echo "To process every path"
     for p in pathResult:
      echo "\n",p[0]
    else: quit("\nAborting",0)
  else: quit("\nNothing was done" & asTarget,0)
 else: stdout.write "Every given path was "
 if asTarget=="":
      unsortRes: founds &= j[Node]
 else:unsortRes: discard
template each_path_search( file :string; asTarget="")=
 file.getDocFile
 echo "Checking document '",file,"'... "
 validatingML()
 path_search_H
 path_search_B( asTarget)
 echo "found on document '",file,"'",asTarget,"\nEvery element of it:\n",foundd

######   main   ######
let
 xpath= re"(?x) ^(?> /?/? ( text\(\) (?: \[ (?> (?:last\(\)-)? [1-9]\d*+ | position\(\) (?!<1) [<>]=? [1-9]\d*+) \] )? | ([a-z]\w*+) (?:\[ (?> (?:last\(\)-)? [0-9]\d*+ | position\(\) (?!<1)[<>]=? [0-9]\d*+ | @((?>(?2)(?:=(?2))? | \*)) ) \])? | @(?-1) | \*) | \.\.?) (?://?(?1))*+ $"
var
 cmdLine= commandLineParams()
 (pathStr, srcFile)= if cmdLine.len>0:    # This block expectedly error and need a knowledgable one's
  echo "\nTry to accomplish:"             # colloboration to correct it
  for i,l in cmdLine:
   echo i,". ",l
  var
   op= cmdLine[1]
   l = cmdLine[0]
  (cmdLine[2], cmdLine[3])
 else:
   echo "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere under the first div element, under any body element,\nunder any html element.\n\nTo put multiply at once, put one after another delimited by ; or |. Put in two data,\nFirst, the element path. Copy operation may be as source then target delimited by '+>'\nSecond, the HTML/XML file name :\n"
   (readLine(stdin), readLine(stdin)) 
if pathStr.len==0: quit("\nNo Xpath given",0)

let srdPaths= pathStr.replace(re"\h+", "").split(re"\+>")
srdPaths[0].xPathsCheck
var
 pathResult = newSeqOfCap[ (string, OffNodArray) ](totPaths)        # Preallocation
 miss :seq[ string ]
 short :seq[ string ]
 fpath, iniNode :OffNodArray
 opt :char
 founds, foundd :string
block:                      # scope to get around equivalent C++ delete command with a hope
 srcFile.each_path_search   # once exiting it, any allocation inside gets freed by Nim GC
if cmdLine.len==0:
 opt= if srdPaths.len>1:'c'
 else:
  echo "\nWhich operation would be done:\n- Remove\n- Copy\n- Save to file or quit\n( r: Remove. c: Copy. Else key: Save or quit )"
  getch()

case opt
of 'c','C':
 var dstPath= if srdPaths.len==1:
  echo "\nPut target element, in xpath form:"; readLine(stdin)
 else: srdPaths[1]
 dstPath.xPathsCheck : "of copy target"
 echo "\nSpecify the copy target file (Enter: as the same as the source):"
 var dstFile=readLine(stdin)
 if dstFile != "":
  dstFile.each_path_search: " to copying"
 else:
  path_search_B: " to copying"
  echo "found as copy target in the same document '",srcFile,"'\nEvery element of it:\n",foundd
 fpath.sort( proc( a,b :StrPair) :int=cmp( b[Offset].len, a[Offset].len) )
 echo "Should source element be under target element, replacing it, preceding it, or following it?\n(u: Under it. r: Replacing it. p: Preceding it. else key: Following it)"
 case getch()
 of 'u','U':
  for on in fpath:
   whole= whole.replace(re(
    r"^\Q" & on[Offset] & r"\E" & headR), "$0" & founds)
 of 'r','R':
  for on in fpath:
   whole= whole.replace(re(
    r"^(\Q" & on[Offset] & r"\E)" & nodeR), "$1" & founds)
 of 'p','P':
  for on in fpath:
   whole= whole.replace(re(
    r"^\Q" & on[Offset] & r"\E"), "$0" & founds)
 else:
  for on in fpath:
   whole= whole.replace(re(
    r"^\Q" & on[Offset] & on[Node] & r"\E"), "$0" & founds)
 echo "\nCopying result:\n",whole
of 'r','R':
 fpath.sort( proc( a,b :StrPair) :int=cmp( b[Offset].len, a[Offset].len) )
 for on in fpath:
  whole= whole.replace(re(r"(?s)^(\Q" & on[Offset] & r"\E)\Q" & on[Node] & r"\E(.*)"), "$1$2")
 echo "\nRemoval result:\n",whole
else: whole=founds
echo "Save to a file? (y: Yes, save. else key: Quit)"
if getch()=='y':
 while true:
  echo "File name to save:"
  outf=readLine(stdin).replace(re"^\h+|\s+$", "")
  if outf.len>0:
   if fileExists(outf):
    echo "There is file name '",outf,"'\nOverwrite it (y: Yes. Else key: No) ?"
    if getch() != 'y':
     echo "\nNot overwrite it"
   else:
    try: writeFile(outf,whole)
    except IOError as e:
     echo "\nCannot write to '",outf,"': ",e.msg
     continue
    except:
     echo "\nFile '",outf,"': critical error"
     continue
    finally:break
 echo "Successfully saved to '",outf,"'"
