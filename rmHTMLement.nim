import std/[os, terminal, nre, math, algorithm]

template strUint( str :string) :uint=
 var res :uint
 for i, c in str:
  if c in '0'..'9': res += ((ord(c) - 48) * 10^(str.high-i)).uint
  else: raise newException(ValueError, "Non-number in string")
 res

template numChr( c :char, s: string) :uint=
 var res :uint
 for i in s:
  if i==c: inc(res)
 res

let             # ML regexes
  nond= r"(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)++"   # no/asymetric tag: no node content
  at= re("^(" & nond & ")(?s)(.+)")
  nodRE= r"(<([a-z]\w*+)(?>[^/>]*+/>|[^>]*+>(?>" & nond & r"|(?-2))*+</\g-1>))"
  ct= "(?>" & nond & "|" & nodRE & ")"                    # node content

var
 remain :string
 totN, maxND :uint
# this node function is direct closed tag node or nested node content
# it's just the header/wrapper for real recursive node search function inside

proc node( str:string; res :var string; tag :string= r"[a-z]\w*+"; ftag:bool=true, att:string="") :bool=
  var
   tag = tag & r"\b"
   tot, max, d :uint
  proc nodeR( str :string; res :var string; tag :string=r"[a-z]\w*+"; nodeName :string="") :bool=
    var m= str.find( re(
         "(?xs) ^<(" & tag & ") (?> ([^/>]*+/>) | ([^>]*+>) ) (.+)" ))
    if m.isNone: return false
    inc(tot);inc(d); if d > max: max = d
    var
      g= m.get.captures.toSeq
      tagname= g[0].get()
      off = "<" & tagname
      clsdNod = g[1]
      str = g[3].get()
    if clsdNod.isSome:
      res= off & clsdNod.get(); remain= str; return true
    off &= g[2].get()
    while true:
      m = str.find( at)
      if m.isSome:
        off &= m.get.captures[0]
        str= m.get.captures[1]
      elif str[0..1] == "</" :
        tagname &= ">"
        if str[ 2..<2+tagname.len] == tagname :
          res= off & "</" & tagname
          dec(d)
          if nodeName != "" :
            remain= str[ 2+tagname.len..^1]
            totN=tot; maxND=max
          return true
        else: break
      elif nodeR( str, res):
        off &= res
        str= str[ res.len..^1]
      else: break
    false
  if not ftag:
   tag= "(?!" & tag & r")[a-z]\w*+"
  if att != "":
   tag &= r"\s+" & att
  let m= str.find( re("(?<=^<)" & tag))
  if m.isNone:
   return false
  nodeR str, res, tag, m.get.match

proc nodeCtn( str :string, tag="", ftag=true, att="") :string=
 var tag=tag
 if tag != "":
  tag &= r"\b"
  if att != "": tag &= r"\s+" & att
  tag= if ftag: "(?=<" & tag & ")" else: "(?!<" & tag & r")"
 let m = str.find(re( "(?s)^((?:" & nond & "|" & tag & nodRE & ")*+)(.+)" ))
 remain = m.get.captures[3]
 m.get.captures[0]
 #remain = str
 #var off, restr :string
 #while true:
   #var m = remain.find(at)
   #if m.isSome:
     #off &= m.get.captures[0]
     #remain = m.get.captures[1]
   #if node( remain, restr, tag, ftag, att):
     #off &= restr
   #else: break
 #off

const
 isERROR = true
 isSUCCED = false

proc head( n :string; off :var string) :bool {.inline.}=
  var m= n.match(re"(?s)^(<(?>[a-z]\w*+|!DOCTYPE)[^>]*+>)(.+)")
  if m.isSome:
    off= m.get.captures[0]
    remain= m.get.captures[1]
    return isSUCCED
  isERROR

proc getNthRev( tag :string; nth :uint; n :var uint) :bool=
  var
   off, res :string
   remainB = remain
   i :uint
  while true:
    off &= nodeCtn( remain, tag, false)
    if node( remain, res, tag):
      off &= res
      inc(i)
    else: break
  if i < nth: return false
  n = 1+i-nth              # i as max from which nth is subtracted
  remain = remainB
  true

proc getNthE( ret :var seq[array[2,string]]; nod, nodOff, tag :string; nth:uint; nthRev:bool) :bool =
  var
   offset, res :string
   n = nth
  if nod.head( offset): return isERROR
  if nthRev and not getNthRev( tag, nth, n) : return isERROR
  for i in 1..n:
    offset &= nodeCtn( remain, tag, false)
    if node( remain, res, tag):
     if i < n: offset &= res
    else: return isERROR
  ret.add( [nodOff & offset, res])
  isSUCCED

template abPosN( posn :string) =
 var
  i {.inject.}, a {.inject.}, b {.inject.}:uint
  g= posn.find(re"(?>(<)|>)(=)?(\d+)").get.captures.toSeq
  eq= g[1].isSome
  n= g[2].get().strUint       # Get a b as lower/upper bound number
 if g[0].isSome:
    b= if eq: n else: n-1
 else:   a = if eq: n else: n+1

proc getE_AllN( ret :var seq[array[2,string]]; nod, nodOff:string; tag, posn, att, aatt :string="") :bool=
  var off, res :string
  if nod.head(off): return isERROR
  if tag != "":
   if posn != "":
    abPosN posn
    while true:
     off &= nodeCtn( remain, tag, false)
     if node( remain, res, tag, true):
      inc(i)
      if i >= a and (b==0 or i <= b):
       ret.add( [nodOff & off, res])
      off &= res
     else: break
   else:
    while true:
     off &= nodeCtn( remain, tag, false, att=att)
     if node( remain, res, tag, att=att) :
      ret.add( [ nodOff & off, res])
      off &= res
     else: break
  elif aatt != "":
   while true:
    off &= nodeCtn( remain, ftag=false, att=aatt)
    if node( remain, res, att= aatt) :
     ret.add( [ nodOff & off, res])
     off &= res
    else: break
  else:
   while true:
    var m = remain.find(at)
    if m.isSome:
     off &= m.get.captures[0]
     remain = m.get.captures[1]
    if node( remain, res) :
     ret.add( [ nodOff & off, res])
     off &= res
    else: break
  ret.len==0

var
 resultArr :seq[ array[ 2,string]]
 avgNumNdPly :uint

proc getAllDNth( ret :var seq[array[2,string]]; nod, nodOff :string; mindepth :uint; tag :string; nth:uint; nthRev:bool) :bool=
 var
  curNode, nd = newSeqOfCap[ array[ 2, string]](avgNumNdPly)
  n = nth
  nthFind = true
 curNode.add( [nodOff, nod] )
 while curNode.len > 0:
  for onref in curNode:
   var offset, res :string
   if onref[1].head(offset) : return isERROR
   if nthRev: nthFind= getNthRev( tag, nth, n)
   block N:
    if nthFind:
     for i in 1..n:
      while true:
       var m = remain.find( at)
       if m.isSome:
         offset &= m.get.captures[0]
         remain = m.get.captures[1]
       if node( remain, res, tag, false) :
        if maxND > mindepth: nd.add( [onref[0] & offset, res] )
        offset &= res
       else: break
      if node( remain, res, tag) :
       if maxND > mindepth: nd.add( [onref[0] & offset, res] )
       if i < n: offset &= res
      else: break N
     if maxND >= mindepth: ret.add( [onref[0] & offset, res] )
   while true:
     var m = remain.find( at)
     if m.isSome:
      offset &= m.get.captures[0]
      remain = m.get.captures[1]
     if node( remain, res) :
       if maxND > mindepth: nd.add( [onref[0] & offset, res] )
       offset &= res
     else: break
  curNode = nd; nd.setlen(0)
 ret.len==0

proc getAllDPosn( ret :var seq[array[2,string]]; nod, nodOff :string; mindepth :uint; tag, posn, attg :string) :bool=
 var nd, curNode = newSeqOfCap[ array[ 2, string]](avgNumNdPly)
 curNode.add( [nodOff, nod])
 while curNode.len > 0:
  for onref in curNode:
   var
    off, offset, res :string
    i, a, b :uint
   if posn != "":          # Get lower/upper bound number for >/<
    let
     g= posn.find(re"(?>(<)|>)(=)?(\d+)").get.captures.toSeq
     eq= g[1].isSome
     n= g[2].get().strUint
    if g[0].isSome:
      b= if eq: n else: n-1
    else:
      a = if eq: n else: n+1
   if onref[1].head( offset) : return isERROR
   while true:
    var m = remain.find( at)
    if m.isSome:
     offset &= m.get.captures[0]; off= offset
     remain = m.get.captures[1]
    if node( remain, res, tag) :
     inc(i)
     if i >= a and (b==0 or i <= b) and maxND >= mindepth:
      ret.add( [onref[0] & offset, res])
    elif node( remain, res) :
     if maxND > mindepth: nd.add( [onref[0] & offset, res])
     offset &= res
  curNode = nd; nd.reset
 ret.len==0

proc getE_Path_R( path :string, offsetNode :seq[ array[2,string]]) :bool=
  var
    g=(path.find(re"(?x)^/ (/)? (?> ([^/@*[]+) (?> \[ (?> (last\(\)-)? ([1-9]\d*) | position\(\)(?!<1)([<>]=? [1-9]\d*) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*)" )).get.captures.toSeq
    nth :uint
    tag, posn, attg, aatt :string
    allNode :bool
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
    allNode = true       # * for any node
  var
   remDepth = 1 + numChr('/', remPath)
   retOffNode= newSeqOfCap[ array[ 2,string]](avgNumNdPly)       # the will-be offset-node result
  for i, u in offsetNode:
   if
    if isAllDepths:                     # all depths under current //
     if isTag:
      if isNth:
       getAllDNth retOffNode, u[1], u[0], remDepth, tag, nth, nthRev   # retOffNode is offset-node found..
      elif isPosn:
       getAllDPosn retOffNode, u[1], u[0], remDepth, tag, posn, attg
      else:
        false#getAllDAtt retOffNode, u[1], u[0], remDepth, tag, att=attg
     else:
      false #getAllDepthAatt retOffNode, u[1], u[0], aatt, remDepth
    elif isTag:
     if isNth:
      getNthE( retOffNode, u[1], u[0], tag, nth, nthRev)
     else:
      getE_AllN( retOffNode, u[1], u[0], tag, posn, att= attg)
    elif isAatt:
     getE_AllN( retOffNode, u[1], u[0], aatt= aatt)
    elif allNode:
     getE_AllN( retOffNode, u[1], u[0])             # be any of these true, it has failed finding
    else: quit("BUG IN XPATH VALIDATION RE",0)
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

var             ###   MAIN   ##
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
 g = m.get.captures.toSeq

if m.isNone or not node( g[3].get(), restr) or
 (let r=remain.replace(re"^\s|\s$",""); r).len > 0 and
  not r.contains(re("(" & nodRE & r"\s*)*")):
   echo "\ncan't parse it due to ill-form or unbalanced tag pair mark-up language\nAborting";quit(0)

let maxFouND = (totN.float * 2 / 3).uint
var
 innd= @[ [g[0].get(), g[1].get() & "</" & g[2].get() & ">"] ]
 path = newSeqOfCap[ ( string, seq[ array[ 2, string] ]) ](valPaths.len)
 miss, short = newSeqOfCap[ string ](valPaths.len)
 fpath = newSeqOfCap[ array[ 2, string] ](maxFouND)
 fail = false
 op :char

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
     for s in short:   # filter out duplicate path or path with the same head as shorter one
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
