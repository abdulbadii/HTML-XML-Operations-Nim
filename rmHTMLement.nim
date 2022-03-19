import std/[os, terminal, nre, math, algorithm, macros]

macro strUint( s :string) :uint=
 result = quote do:
  var res :uint
  for i, c in `s`:
   if c in '0'..'9': res += ((ord(c) - 48) * 10^(`s`.high-i)).uint
   else: raise newException(ValueError, "Non-number in string")
  res
template numChr( c :char, s: string) :uint=
 var res :uint
 for i in s:
  if i==c: res.inc
 res

let             # ML regexes
 nond= r"(?:[^<>]*+(?:<(?>meta|link|input|img|hr|base)\b[^>]*+>)?)*+"  # asymetric tag & no node content
 aCtn= re("(?s)^(" & nond & ")(.+)")
 nodRE= r"(<([a-z]\w*+)(?>[^/>]*+/>|[^>]*+>(?:" & nond & r"(?-2)?)*+</\g-1>))"
 head=re"(?s)^(<(?>[a-z]\w*+|!DOCTYPE)[^>]*+>)(.+)"
var
 offset, res, remain :string
 totN, maxND :uint

# a node function for finding direct closed tag node or nested node content
# it's just a header having a literal tag fed into the recursive node search function inside
proc node( str:string; res :var string; tag="") :bool=
  var tot, max, d :uint
  proc nodeNoT( str :string; res :var string; tag="") :bool=
    let m= str.find( re(
       "(?xs) ^<(" & tag & r"[a-z]\w*+) (?> ([^/>]*+/>) | ([^>]*+>) ) (.+)" ))
    if m.isNone: return false
    let g=m.get.captures.toSeq
    tot.inc; d.inc; if d>max: max=d
    var
      tagname= g[0].get()
      off = "<" & tagname
      str = g[3].get()
    if g[1].isSome:
      res= off & g[1].get()
      if tag != "":
       totN=tot; maxND=max
       remain= str
      return true
    off &= g[2].get()
    while true:
      let m = str.find( aCtn)
      off &= m.get.captures[0]
      str= m.get.captures[1]
      if str[0..1] == "</" :
        tagname &= ">"
        if str[ 2..<2+tagname.len] == tagname:
          res= off & "</" & tagname
          dec(d)
          if d==0:
            totN=tot; maxND=max
            remain= str[ 2+tagname.len..^1]
          return true
        else: break
      elif nodeNoT( str, res):
        off &= res
        str= str[ res.len..^1]
      else: break
    false
  nodeNoT str, res, tag

template ctntNode( remainstr, tag, resNoffset :untyped) :bool=
 let m {.inject.}= remainstr.find(re(
  "(?s)^((?:" & nond & "(?:(?!<" & tag & r"\b)" & nodRE & ")?)*+)(?=<" & tag & r"\b)" & nodRE & "(.+)" ))
 if m.isNone: false
 else:
  resNoffset
  remainstr = m.get.captures[5]
  true

template ctnoTagNode( remains, tag :string) :bool= ctntNode( remains, tag) :discard
template ctnoTagNode( remains, tag, res :string) :bool=
 ctntNode( remains, tag) :
  offset= m.get.captures[0]
  res= m.get.captures[3]

macro ctnPrevTag( nd :seq[array[2,string]]; res :string; nodeExists:untyped) =
 result = quote do:
  while true:
   let m = remain.find( aCtn)
   offset &= m.get.captures[0]
   remain = m.get.captures[1]
   if `nodeExists` :
    if maxND > mindepth: `nd`.add( [offset, `res`])
    offset &= `res`
   else: break

template ctnUp2Tag( nd :seq[ array[2,string]]; res, notTag, tag :string; xPathSpecmd:untyped) =
 ctnPrevTag nd, res:
  node remain, res, notTag
 if node( remain, res, tag):
  if maxND > mindepth: nd.add( [offset, res])
  xPathSpecmd
  offset &= res
 else: break

template headeRemain( nd :string; prevOff="")=
 var m=nd.find(head)
 offset= prevOff & m.get.captures[0]
 remain= m.get.captures[1]

const
 isERROR = true
 isSUCCED = false

template abPosN( posn :string) =
 var
  g= posn.find(re"(?>(<)|>)(=)?(\d+)").get.captures.toSeq
  eq= g[1].isSome
  n= g[2].get().strUint       # Get a b as lower-upper bound number
 if g[0].isSome:
  b= if eq: n else: n-1
 else:   a = if eq: n-1 else: n

macro getNthRev( tag :string; n :uint) :bool=
 result = quote do:
  var
   i :uint
   remn= remain
  while ctnoTagNode( remn, `tag`): i.inc
  if i < `n`: false
  else:
   `n` = 1+i-`n`              # i as max from which nth is subtracted
   true

template getE_Nth( ret :seq[array[2,string]]; nodOffset, nod, tag :string; nth:uint; nthRev:bool) :bool=
 var n=nth
 nod.headeRemain
 if nthRev and not getNthRev( tag, n): isERROR
 else:
  let m = remain.find(re(
    r"^((?:(?:" & nond & "(?:(?!<" & tag & r"\b)" & nodRE & ")?)*+(?=<" & tag & r"\b)" & nodRE & "){" & $n & "})"))
  if m.isNone: isERROR
  else:
   var r = m.get.captures[3]
   ret.add( [nodOffset & offset & m.get.captures[0][0..^r.len+1], r])
   isSUCCED

proc getE_MultiN( ret :var seq[array[2,string]]; nodOffset, nod :string; tag, posn, att, aatt :string="") :bool=
  var res :string
  nod.headeRemain(nodOffset)
  if tag != "":
   var
    a, b, i :uint
    tag = tag
   if posn != "": abPosN posn
   elif att != "":
    tag &= r"\s+" & att
   while true:
    if ctnoTagNode( remain, tag, res):
     i.inc
     if i > a and (b==0 or i <= b):
      ret.add( [offset, res])
     offset &= res
    else: break
  elif aatt != "":
   var tag= r"\S+\s+" & aatt
   while true:
    if ctnoTagNode( remain, tag, res):
     ret.add( [offset, res])
     offset &= res
    else: break
  else:
   while true:
    if ctnoTagNode( remain, tag, res):
      ret.add( [offset, res])
      offset &= res
    else: break
  ret.len==0

var avgNumNdPly :uint
proc getAllDepthMultiN( ret :var seq[array[2,string]]; nodOffset, nod, tag :string; mindepth, nth :var uint; nthRev:bool) :bool=
 var curNode, nd = newSeqOfCap[ array[ 2, string]](avgNumNdPly)
 curNode.add( [nodOffset, nod] )
 template loopCondPar( cond :untyped )=
  while curNode.len > 0:
   for o_n in curNode:
    var res :string
    o_n[1].headeRemain(o_n[0])
    cond
    block :
     let
      notTag = "(?!" & tag & r"\b)"
      tag = "(?=" & tag & r"\b)"
     for i in 1..nth:
      ctnUp2Tag nd, res, notTag, tag :
       if i == nth:
        if maxND >= mindepth: ret.add( [offset, res])
    ctnPrevTag nd, res:
     node remain, res
   curNode = nd; nd.reset
 if nthRev:
  loopCondPar :
   if getNthRev( tag, nth) :
    discard
 else:
  loopCondPar: discard
 ret.len==0

proc getAllDepthMultiN( ret :var seq[array[2,string]]; nodOffset, nod :string; mindepth :uint; tag, posn, att, aatt ="") :bool=
 var nd, curNode = newSeqOfCap[ array[ 2, string]](avgNumNdPly)
 curNode.add( [nodOffset, nod])

 template loop( foundCmd)=
  while curNode.len > 0:
   for o_n in curNode:
    o_n[1].headeRemain(o_n[0])
    while true:
     ctnUp2Tag nd, res, notTag, tag :
      foundCmd
   curNode = nd; nd.reset
 if tag != "":
  var
   a, b, i :uint
   notTag = "(?!" & tag & r"\b"
   tag = "(?=" & tag & r"\b"
  if posn != "":
   abPosN posn
  elif att != "":
   notTag &= r"\s+" & att
   tag &= r"\s+" & att
  notTag &= ")"; tag &= ")"
  loop:
   i.inc
   if i>a and (b==0 or i<=b) and maxND >= mindepth:
    ret.add( [offset, res])
 elif aatt != "":
  var
   notTag= r"(?!\S+\s+" & aatt & ")"
   tag= r"(?=\S+\s+" & aatt & ")"
  loop:
   if maxND >= mindepth: ret.add( [offset, res])

 ret.len==0



var resultArr :seq[ array[ 2,string]]
proc getE_Path_R( path :string, offsetNode :seq[ array[2,string]]) :bool=
  var
    g= path.find(re"(?x)^/ (/)? (?> ([^/@*[]+) (?> \[ (?> (last\(\)-)? ([1-9]\d*) | position\(\)(?!<1)([<>]=? [1-9]\d*) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*)" ).get.captures.toSeq
    nth :uint
    tag, posn, attg, aatt :string
    anyNode :bool
    remPath = g[8].get()
    isAllDepths = g[0].isSome
    isTag = g[1].isSome
    nthRev = g[2].isSome
    isNth = g[3].isSome
    isPosn = g[4].isSome
    isAttg = g[5].isSome
    isAatt = g[6].isSome
  if isTag:
    tag = g[1].get()
    if isNth:
      nth = g[3].get().strUint()
    elif isPosn:
      posn = g[4].get()
    elif isAttg:
      attg = g[5].get()
  elif isAatt:
    aatt = g[6].get()
  else:
    anyNode = true       # * for any tag name
  var
   remDepth = 1 + numChr('/', remPath)
   retOffNode= newSeqOfCap[ array[ 2,string]](avgNumNdPly)       # the will-be offset-node result
  for i, u in offsetNode:
   if
    if isAllDepths:                     # all depths under current //
     if isTag:
      if isNth:
       getAllDepthMultiN retOffNode, u[0], u[1], tag, remDepth, nth, nthRev   # retOffNode is offset-node found..
      else:
       getAllDepthMultiN retOffNode, u[0], u[1], remDepth, tag, posn, attg
     elif isAatt:
      false#getAllDepthMultiN retOffNode, u[0], u[1], remDepth, tag, att=attg
     else:
      false #getAllDepthMultiN retOffNode, u[0], u[1], aatt, remDepth
    elif isTag:
     if isNth:
      getE_Nth( retOffNode, u[0], u[1], tag, nth, nthRev)
     else:
      getE_MultiN( retOffNode, u[0], u[1], tag, posn, att= attg)
    elif isAatt:
     getE_MultiN( retOffNode, u[0], u[1], aatt= aatt)
    else:       # allNode:
     getE_MultiN( retOffNode, u[0], u[1])             # be any of these true, it failed finding, so
    :
     if i < offsetNode.high: continue        # see, if it's not the last in loop, go on iterating
     return resultArr.len==0                 # otherwise return true (1) if finding none or 0 if finding any
   if remPath.len > 0:
    let e = getE_Path_R( remPath, retOffNode)       #...which will always propagate to the next whose
    if i==offsetNode.high : return e              # boolean result is return if this is the last iteration
   else:
    resultArr.add( retOffNode)
   retOffNode.reset
  isSUCCED

var             ###   main   ##
 valPaths :seq[ string]
 cmdLine = commandLineParams()
 y :char
 whole, p, aCP, restr, outf :string

let (pathStr, file) = if cmdLine.len > 0:
  for i,l in cmdLine:
    echo i," _ ",l
  quit(0)
  var
   op = cmdLine[1]
   l = cmdLine[0]
   whole = readFile(l)
  (cmdLine[2], cmdLine[3])
else:
  echo "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere under the first div element, under any body element,\nunder any html element.\n\nTo put multiply at once, put one after another delimited by ; or |\nPut in two data, Element path and HTML/XML file name:\n"
   (readLine(stdin), readLine(stdin)) 
if pathStr.len==0: echo "\nNo Xpath given";quit(0)

let xpath=
 re"(?x) ^(?> /?/? (([a-z]\w*+) (?:\[ (?> (?:last\(\)-)? [0-9]\d* | position\(\)(?!<1)[<>]=? [0-9]\d* | @((?>(?2)(?:=(?2))? | \*)) ) \])? | @(?-1) | \*) | \.\.?) (?://?(?1))*+ $"
for p in pathStr.replace( re"\h+", "").split(re"[|;]"):
  if p.contains(xpath):
    if p.contains( re"^[^/]") :
      if aCP.len == 0:
        echo "\n'",p,"' is relative to base/current path which is empty"
        while true:
          echo "\nPut a valid one: "
          aCP = readLine(stdin).replace( re"\h+", "")
          if aCP.contains(xpath): break
          echo "\n'",aCP,"' is not a valid Xpath"
      aCP &= "/"
      var pr= p.replace( re"^\./", "")
      while pr.contains(re"^\.\.") :
        if aCP == "/" : echo "\n'", pr,"' upward node ..\nran over '",aCP,"' current path";quit(0)
        aCP = aCP.replace( re"[^/]+/$", "")
        pr = pr.replace( re"^../?", "")
      valPaths.add( aCP & pr)
    else: valPaths.add( p)
  else:
    echo "\n'",p,"' is invalid Xpath\nSkip? (s: skip. Key else: abort): "; y=getch()
    if y != 's': echo "\nAborting";quit(1)

if fileExists(file):
  try:
    whole = readFile(file)
  except IOError as e:
    echo "\nCannot open '",file,"': ",e.msg
else:
  echo "\n'",file,"' doesn't exist\n";quit(0)
echo "\nChecking HTML document '",file,"'... "

let                  # Validating ML format:
 m= whole.find(re(
   r"(?xs)^(\s* (?: <\?xml\b [^>]*+> \s* )?) (< (!DOCTYPE) [^>]*+> [^<]* (.+))" ))

if m.isNone or not node( m.get.captures[3], restr) or
 (let r=remain.replace(re"^\s|\s$",""); r).len > 0 and
  not r.contains(re("(" & nodRE & r"\s*)*")):
   echo "\ncan't parse it due to ill-form or unbalanced tag pair mark-up language\nAborting";quit(0)

let maxFouND = (totN.float * 2 / 3).uint
var
 innd= @[ [m.get.captures[0], m.get.captures[1] & "</" & m.get.captures[2] & ">"] ]
 path = newSeqOfCap[ ( string, seq[ array[ 2, string] ]) ](valPaths.len)
 miss, short = newSeqOfCap[ string ](valPaths.len)
 fpath = newSeqOfCap[ array[ 2, string] ](maxFouND)
 fail :bool
 op :char
offset = newStringOfCap(whole.len-17)
res = newStringOfCap(whole.len-17)
remain = newStringOfCap(whole.len-17)
resultArr = newSeqOfCap[ array[ 2,string] ](maxFouND)
avgNumNdPly = (totN.float / maxND.float * 1.3).uint

valPaths.sort( proc( a,b:string) :int =cmp(a.len, b.len) )
for u in valPaths:
  if fail:
   echo "\nSkip it to process the next path? (Y/Enter: yes. any else: Abort) "
   if getch() == 'y': echo "Aborting\n";quit(0)
  resultArr = @[]
  fail = getE_Path_R( u, innd)
  if fail:
   miss.add(u); echo "\nCan't find: ",u
  else:
   path.add( (u, resultArr) )
   block F:
    for s in short:   # filter out duplicate path or path whose head as the same as shorter one's
     if u.contains(re(r"^\Q" & s & r"\E")) : break F
    fpath.add(resultArr)
   short.add(u)

if miss.len > 0:
  if path.len > 0:
    echo "\nKeep processing ones found? (Enter/y: yes. Any else: Abort) "
    y=getch();if y != 'y': echo "Aborting"
  else: echo "\nNothing was done";quit(0)

if cmdLine.len==0:
  echo "\n\nWhich operation will be done :\n- Remove\n- Extract\n(R: remove. Else key: extract) "
  op=getch()
  echo "File name to save the result: (hit Enter to standard output) "
  outf=readLine(stdin).replace(re"^\h+|\s+$", "")

if outf.len > 0:
  if fileExists(outf):
    echo "There exists file name '",outf,"'\nOverwrite it (y: Yes Else key: Abort)?"
    y=getch();if y != 'y':
      echo "\nWill not overwrite existing file, aborting";quit(0)
if path.len > 1:
  echo "\nProcessing the paths:"
  for p in path:
   echo "\n",p[0]

case op:
 of 'r':
  fpath.sort( proc( a,b :array[2,string]) :int=cmp( b[0].len, a[0].len) )
  for on in fpath:
    whole= whole.replace(re(r"\A(\Q" & on[0] & r"\E)\Q" & on[1] & r"\E(.*)\Z"), "$1$2")
  echo "\nRemoval result:\n", whole

 else:
  whole = ""
  for i in path :
    whole &= "\n" & i[0] & ":"
    for k in i[1] :
      whole &= "\n--------\n" & k[1]

if outf.len > 0:
  try: writeFile(whole, outf)
  except:
    echo "\nCannot open '",outf,"'\n"
else:
  echo whole
