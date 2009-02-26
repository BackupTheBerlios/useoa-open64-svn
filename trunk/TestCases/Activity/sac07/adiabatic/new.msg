markVaried: Independent(PIPE
-v->PIPE(12)
PIPE(12:F:aifl_)-v->WEN(13)
WEN(13:F:aifl_)-v->WTEN(14)
WTEN(14:F:aifl_)-v->EN(1)
EN(1:F:aifl_)-v->NIN(2)
NIN(2:F:aifl_)-v->NUM(3)
NUM(3:F:aifl_)-v->t__10(7)
<-
NUM(3:F:aifl_)-v->t__12(15)
<-
NUM(3:F:aifl_)-v->t__14(19)
<-
NUM(3:F:aifl_)-v->t__16(23)
<-
<-
<-
EN(1:F:aifl_)-v->PTPEN(17)
<-
<-
WTEN(14:F:aifl_)-v->PTPEN(17)
PTPEN(17:F:aifl_)-v->EN(1)
<-
<-
<-
WEN(13:F:aifl_)-v->VAPOR(41)
<-
WEN(13:F:aifl_)-v->LIQUID(42)
<-
<-
<-
markUseful: Dependent(VAPOR
-u->VAPOR(41)
VAPOR(41:F:0)VAPOR(41:F:0)-u->WEN(13)
WEN(13:F:0)-u->PIPE(12)
<-
WEN(13:F:0)WEN(13:F:0)WEN(13:F:0)WEN(13:F:0)WEN(13:F:0)WEN(13:F:0)WEN(13:F:0)<-
<-
markUseful: Dependent(LIQUID
-u->LIQUID(42)
LIQUID(42:F:0)LIQUID(42:F:0)-u->WEN(13)
<-
<-
VDG - Run time is 0.100000
VDG - ActiveSizeInBytes: 264
VDG - numGlobalEdges(0) vs. maxLocalAccVars(0)
