import std/[os, terminal, nre, options, math, algorithm]

func strUint(str: string) :uint=
  for i, c in str:
    if c in '0'..'9':
      result += cast[uint]((ord(c) - ord('0')) * 10^(str.high-i))
    else:
      raise newException(ValueError, "Non-number in string")
func numChr(s: string, c :char) :uint {.inline.}=
  for u in s:
    if u==c : inc(result)

let             # ML regexes
  nond= r"(?>[^<>]|<(?>meta|link|input|img|hr|base)\b[^>]*+>)++"   # no/asymetric tag: no node content
  at= re("^(" & nond & ")(?s)(.+)")
  nodRE= r"(<([a-z]\w*+)(?>[^/>]*+/>|[^>]*+>(?>" & nond & r"|(?-2))*+</\g-1>))"
  ct= "(?>" & nond & "|" & nodRE & ")"                    # node content

var
 remain :string
 totN, maxND :uint
# this node function is direct closed tag node or nested node content
# it's just the header/wrapper for the recursive node search function inside

proc node( str:string; restr :var string; tag :string= r"[a-z]\w*+"; ftag:bool=true, att:string="") :bool=
  var
   tot, max, d :uint
  proc nodeR( str :string; restr :var string; tag :string=r"[a-z]\w*+"; litag :string="") :bool=
    var m= str.find( re(
         "(?xs) ^<(" & tag & r") (?> ([^/>]*+/>) | ([^>]*+>) ) (.+)" ))
    if m.isNone: return false
    inc(tot);inc(d); if d > max: max = d
    var
      g= m.get.captures.toSeq
      curlitag= g[0].get()
      off = "<" & curlitag
      clsdNod = g[1]
      str = g[3].get()
    if clsdNod.isSome:
      restr= off & clsdNod.get(); remain= str; return true
    off &= g[2].get()
    while true:
      m = str.find( at)
      if m.isSome:
        off &= m.get.captures[0]
        str= m.get.captures[1]
      elif str[0..1] == "</" :
        curlitag &= ">"
        if str[ 2..<2+curlitag.len] == curlitag :
          restr= off & "</" & curlitag
          dec(d)
          if litag != "" :
            remain= str[ 2+curlitag.len..^1]
            totN=tot; maxND=max
          return true
        else: break
      elif nodeR( str, restr):
        off &= restr
        str= str[ restr.len..^1 ]
      else: break
    false
  var
   tag= if att != "" : tag & r"\b\s+" & att else: tag & r"\b"
   m= str.find( re("(?<=^<)" & tag) )
  if m.isNone: return false
  nodeR str, restr, if ftag: tag else:("(?!" & tag & r")[a-z]\w*+"), m.get.match

proc nodeCtn( str:string; tag:string) :string =
  var
   off, restr :string
   str = str
  while true:
    var m = str.find( at)
    if m.isSome:
      off &= m.get.captures[0]
      str = m.get.captures[1]
    elif node( str, restr, tag, false):
      off &= restr
      str= remain
    else: break
  remain= str
  off

const
  isERROR = true
  isSUCCED = false

proc head( nd :string; off :var string) :bool {.inline.}=
  var m= nd.match(re"(?s)^(<(?>[a-z]\w*+|!DOCTYPE)[^>]*+>)(.+)")
  if m.isSome:
    off= m.get.captures[0]
    remain= m.get.captures[1]
    return isSUCCED
  isERROR

proc getNthEAtt( offnode :var seq[array[2,string]]; nod, nodOff, tag :string; nth:uint; nthRev:bool) :bool =
  var
   nth = nth
   off, offset, res :string
   i :uint
  if nod.head( off):
   return isERROR
  if nthRev:             # Get max nth +1 #var i=1
    var remainB = remain
    while true:
      off &= nodeCtn( remain, tag)
      if node( remain, res, tag):
        offset = off
        off &= res
        inc(i)
      else: break
    if i < nth : return isERROR
    nth = i-nth
    remain = remainB
    off = ""
  for _ in 1..nth:
    off &= nodeCtn( remain, tag)
    if node( remain, res, tag):
      offset = off
      off &= res
    else: return isERROR
  offnode.add( [nodOff & offset, res])
  return isSUCCED

proc getAllEAtt( offnode :var seq[array[2,string]]; nod, nodOff, tag, posn :string; attg, aatt :string; allnode :char) :bool=
  var
    off, offset, res :string
    i, a, b :uint
  if posn != "":          # Get lower/upper bound number for >/<
    let
     g= posn.find(re"(?>(<)|>)(=)?(\d+)").get.captures.toSeq
     eq= g[1].isSome
     n=g[2].get().strUint
    if g[0].isSome:
      b= if eq: n else: n-1
    else:
      a = if eq: n else: n+1
  if nod.head(off):
   return isERROR
  while true:
    off &= nodeCtn( remain, tag)
    if node( remain, res, tag) :
      offset = off
      off &= res
      inc(i)
      if i > a:
       if b < 1 or i <= b:
        offnode.add( [ nodOff & offset, res ] )
    else: break
  if offnode.len > 0: return isSUCCED
  isERROR

var
 resultArr :seq[ array[ 2,string] ]
 numOffNode :uint
proc getE_Path_R( path :string, offsetNode :seq[ array[2,string]]) :bool=
  var
    g=(path.find(re"(?x)^/ (/)? (?> ([^/@*[]+) (?> \[ (?> (last\(\)-)? ([1-9]\d*) | position\(\)(?!<1)([<>]=? [1-9]\d*) | @(\*| [^]]+) ) \] )? | @([a-z]\w*[^/]* |\*) | (\*) ) (.*)" )).get.captures.toSeq
    nth :uint
    tag, posn, attg, aatt :string
    allnode :char
    remPath = g[8].get()
    isDepths = g[0].isSome
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
    allnode = '*'       # any node

  var newOffNode= newSeqOfCap[ array[2,string] ](numOffNode)       # the will-be-returned offset-node
  for i, u in offsetNode:
    #if isDepths:                     # entire depths under current //
     #var remDepth = 1+ numChr( remPath,'/')
     #if
      #if isTag:
       #if isNth:
        #getAllDepth newOffNode, u[1], u[0], tag, nth, remDepth, nthRev # offset-node is returned to newOffNode..
       #else:
        #getAllDepNthRnAtt newOffNode, u[1], u[0], tag, remDepth, posn, attg
      #else:
       #getAllDepthAatt newOffNode, u[1], u[0], aatt, remDepth
    #:
      #if i < offsetNode.high : continue           # if fail finding, if not the last in loop, go on iterating
      #return resultArr.len==0                    # else (if the last) return 1/true if as overall failing
    #el
    if isNth:
      if getNthEAtt( newOffNode, u[1], u[0], tag, nth, nthRev):
        if i < offsetNode.high : continue
        return resultArr.len==0
    else:
      if getAllEAtt( newOffNode, u[1], u[0], tag, posn, attg, aatt, allnode):
        if i < offsetNode.high : continue
        return resultArr.len==0
    if remPath.len > 0:
      let e = getE_Path_R( remPath, newOffNode)    # ..which will always propagate to the next whose...
      if i==offsetNode.high : return e           # boolean result is return if this point is the last iteration
    else:
      resultArr.add(newOffNode)
    newOffNode.setlen(0)
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
  echo "Element path is of Xpath form e.g:\n\thtml/body/div[1]//div[1]/div[2]\nmeans find in a given HTML or XML file, the second div tag element that is under the first\ndiv element anywhere under the first div element, under any body element,\nunder any html element.\n\nTo put multiply at once, put one after another delimited by ; or |. Put two info below\nElement path:\nHTML/XML file name: " 
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

let maxFouND = (totN.float* 2/ 3).uint
var
 innd= @[ [g[0].get(), g[1].get() & "</" & g[2].get() & ">"] ]
 path = newSeqOfCap[ ( string, seq[ array[ 2, string] ]) ](valPaths.len)
 miss, short = newSeqOfCap[ string ](valPaths.len)
 fpath = newSeqOfCap[ array[ 2, string] ](maxFouND)
 fail = false
 op :char

resultArr = newSeqOfCap[ array[ 2,string] ](maxFouND)
numOffNode = (totN.float / maxND.float).uint
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
    block RMV:
      for s in short:   # filter out duplicate path or path with the same head as shorter one
       if u.contains(re(r"^\Q" & s & r"\E")) : break RMV
      fpath.add(resultArr)
    short.add(u)


if miss.len > 0:
  if path.len > 0:
    echo "\nKeep processing ones found? (Enter/y: yes. Any else: Abort) "
    y=getch();if y != 'y': echo "Aborting"
  else: echo "\nNothing was done";quit(0)

if cmdLine.len==0:
  echo "\n\nWhich operation will be done :\n- Remove\n- Get\n(R: remove. Else key: extract) "
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

var o :string
case op:
 of 'r':
  fpath.sort( proc( a,b :array[2,string]) :int=cmp( b[0].len, a[0].len) )
  for on in fpath:
    whole= whole.replace(re(r"\A(\Q" & on[0] & r"\E)\Q" & on[1] & r"\E(.*)\Z"), "$1$2")
  echo "\nRemoval result:\n", whole

 else:
  for i in path :
    o &= "\n" & i[0] & ":"
    for k in i[1] :
      o &= "\n--------\n" & k[1]

if outf.len > 0:
  try: writeFile(whole, outf)
  except:
    echo "\nCannot open '",outf,"'\n"
else:
  echo o
