reuse last settings (tlm none)? [y]/n rm -rf head_sf.* head.* driver* params.conf all_globals_* ad_template.f *mod-whirl tmpOutput data.tmp
** running call *************************************************
cat all_globals_mod.f head.f > head_sf.f
./mfef90 -F -N132 head_sf.f
./whirl2xaif -o head_sf.xaif head_sf.B
Iter(1): DGraphIterativeDFP::solve(2): changed atDGraphEdge
iterCnt ----------------------------------> 1
iterCnt ----------------------------------> 2
iterCnt ----------------------------------> 1
iterCnt ----------------------------------> 1
ManagerICFGActive::performAnalysis: Calling usefulman->performAnalysis() ...
--------------- ICFGdumpBegin --------------
12->13;
11->12;
10->11;
10->8 [label="CALL", style=dashed];
7->10;
9->7 [label="RETURN", style=dashed];
9->11 [label="RETURN", style=dashed];
14->9;
8->14;
6->7;
6->8 [label="CALL", style=dashed];
5->6;
4->5;
2->3;
1->2;
--------------- ICFGdumpEnd --------------
:Node:13
13 ---------- after transfer
Y, 
Iter(1): DGraphIterativeDFP::solve(13): changed atDGraphNode
:Node:12
12 ---------- from Node(13 CFLOW_EDGE)
Y, 
:Node:11
11 ---------- from Node(12 CFLOW_EDGE)
Y, 
:Node:10
10 ---------- from Node(11 CFLOW_EDGE)
Y, 
:Node:7
7 ---------- from Node(10 CFLOW_EDGE)
Y, 
:Node:9
Y --> G
9 ---------- from Node(11 RETURN_EDGE)
G, 
:Node:14
14 ---------- from Node(9 CFLOW_EDGE)
G, 
G -> F 
14 ---------- after transfer
F, 
:Node:8
8 ---------- from Node(14 CFLOW_EDGE)
F, 
:Node:6
6 ---------- from Node(7 CFLOW_EDGE)
Y, 
F ->> X
6 ---------- from Node(8 CALL_EDGE)
X, 
:Node:5
5 ---------- from Node(6 CFLOW_EDGE)
X, Y, 
:Node:4
4 ---------- from Node(5 CFLOW_EDGE)
X, Y, 
:Node:3
:Node:2
:Node:1
iterCnt ----------------------------------> 1
:Node:13
:Node:12
:Node:11
:Node:10
F ->> A
10 ---------- from Node(8 CALL_EDGE)
A, 
Iter(2): DGraphIterativeDFP::solve(10): changed atDGraphNode
:Node:7
7 ---------- from Node(10 CFLOW_EDGE)
A, 
:Node:9
Y --> G
:Node:14
:Node:8
:Node:6
6 ---------- from Node(7 CFLOW_EDGE)
A, 
F ->> X
:Node:5
5 ---------- from Node(6 CFLOW_EDGE)
A, 
:Node:4
4 ---------- from Node(5 CFLOW_EDGE)
A, 
:Node:3
:Node:2
:Node:1
iterCnt ----------------------------------> 2
:Node:13
:Node:12
:Node:11
:Node:10
F ->> A
:Node:7
:Node:9
Y --> G
:Node:14
:Node:8
:Node:6
F ->> X
:Node:5
:Node:4
:Node:3
:Node:2
:Node:1
iterCnt ----------------------------------> 3
ManagerICFGActive::performAnalysis: Calling varyman->performAnalysis() ...
--------------- ICFGdumpBegin --------------
4->5;
5->6;
6->7;
6->8 [label="CALL", style=dashed];
7->10;
10->11;
10->8 [label="CALL", style=dashed];
8->14;
14->9;
9->7 [label="RETURN", style=dashed];
9->11 [label="RETURN", style=dashed];
11->12;
12->13;
1->2;
2->3;
--------------- ICFGdumpEnd --------------
:Node:4
Iter(1): DGraphIterativeDFP::solve(4): changed atDGraphNode
:Node:5
5 ---------- from Node(4 CFLOW_EDGE)
X, 
:Node:6
6 ---------- from Node(5 CFLOW_EDGE)
X, 
:Node:7
7 ---------- from Node(6 CFLOW_EDGE)
X, 
:Node:10
10 ---------- from Node(7 CFLOW_EDGE)
X, 
:Node:8
X --> F
8 ---------- from Node(6 CALL_EDGE)
F, 
:Node:14
14 ---------- from Node(8 CFLOW_EDGE)
F, 
:Node:9
9 ---------- from Node(14 CFLOW_EDGE)
F, G, 
:Node:11
11 ---------- from Node(10 CFLOW_EDGE)
X, 
F ->> A
G ->> Y
11 ---------- from Node(9 RETURN_EDGE)
Y, A, 
:Node:12
12 ---------- from Node(11 CFLOW_EDGE)
X, Y, A, 
:Node:13
13 ---------- from Node(12 CFLOW_EDGE)
X, Y, A, 
:Node:1
:Node:2
:Node:3
iterCnt ----------------------------------> 1
:Node:4
:Node:5
:Node:6
:Node:7
F ->> X
G ->> B
7 ---------- from Node(9 RETURN_EDGE)
B, 
Iter(2): DGraphIterativeDFP::solve(7): changed atDGraphNode
:Node:10
10 ---------- from Node(7 CFLOW_EDGE)
B, 
:Node:8
X --> F
:Node:14
:Node:9
:Node:11
11 ---------- from Node(10 CFLOW_EDGE)
B, 
F ->> A
G ->> Y
:Node:12
12 ---------- from Node(11 CFLOW_EDGE)
B, 
:Node:13
13 ---------- from Node(12 CFLOW_EDGE)
B, 
:Node:1
:Node:2
:Node:3
iterCnt ----------------------------------> 2
:Node:4
:Node:5
:Node:6
:Node:7
F ->> X
G ->> B
:Node:10
:Node:8
X --> F
:Node:14
:Node:9
:Node:11
F ->> A
G ->> Y
:Node:12
:Node:13
:Node:1
:Node:2
:Node:3
iterCnt ----------------------------------> 3
ManagerICFGActive::performAnalysis: Ending varyman->performAnalysis()
Iter(1): DGraphIterativeDFP::solve(12): changed atDGraphNode
Y --> G
F ->> X
iterCnt ----------------------------------> 1
F ->> A
Y --> G
F ->> X
iterCnt ----------------------------------> 2
iterCnt ----------------------------------> 1
iterCnt ----------------------------------> 1
iterCnt ----------------------------------> 1
./xaifBooster -i head_sf.xaif -c /home/jaewook/work/OpenAD/xaif/schema/examples/inlinable_intrinsics.xaif -o head_sf.xb.xaif 
========================================== reset log
0:DuUdMapEntry.cpp:123:ERROR:DuUdMapEntry::use: an empty StatementIdList implies dead code, the subsequent transformations may fail
./xaif2whirl --structured head_sf.B head_sf.xb.xaif
./whirl2f -openad head_sf.xb.x2w.B
perl multi-pp.pl -f head_sf.xb.x2w.w2f.f
cat head_sf.xb.x2w.w2f.pp.f | sed '/MODULE all_globals_mod/,/END MODULE/d' > head.xb.x2w.w2f.pp.f
cat head_sf.xb.x2w.w2f.pp.f | sed -n '/MODULE all_globals_mod/,/END MODULE/p' > all_globals_mod.xb.x2w.w2f.pp.f
ifort -g -fpe0 -O0   -fixed -c all_globals_mod.xb.x2w.w2f.pp.f
ifort -g -fpe0 -O0   -fixed -c head.xb.x2w.w2f.pp.f
ifort -g -fpe0 -O0   -free -c driver.f90
ifort -g -fpe0 -O0   w2f__types.o active_module.o all_globals_mod.xb.x2w.w2f.pp.o head.xb.x2w.w2f.pp.o driver.o -o driver 
examples/call/refOutput/tlmnonehead_sf.xaif not available, copy and hg add it? [y]/n  cannot verify head_sf.xaif
