adifor2/c2> whirl2xaif -o aaa c2.B                                              digraph OA_DUG {
node [shape=rectangle];
N_1->t__1_2[label="F_fcn_"];
Y_4->YTEMP_5[label="F_fcn_"];
I_3->YTEMP_5[label="F_fcn_"];
W_6->Y_4[label="F_fcn_"];
I_3->Y_4[label="F_fcn_"];
ID_7->IID_8[label="F_fcn_"];
Y_4->YP_9[label="F_fcn_"];
Y_4->SUM_10[label="F_fcn_"];
N_1->t__2_11[label="F_fcn_"];
CPARM_12->YP_9[label="F_fcn_"];
I_3->YP_9[label="F_fcn_"];
SUM_10->YP_9[label="F_fcn_"];
I_3->SUM_10[label="F_fcn_"];
N_1->t__3_13[label="F_fcn_"];
W_6->YP_9[label="F_fcn_"];
YTEMP_5->Y_4[label="F_fcn_"];
}
markVaried: Independent(Y
-v->Y(4)
Y(4:F:fcn_)-v->YTEMP(5)
<-
Y(4:F:fcn_)-v->YP(9)
<-
Y(4:F:fcn_)-v->SUM(10)
SUM(10:F:fcn_)-v->YP(9)
<-
<-
<-
markUseful: Dependent(YP
-u->YP(9)
YP(9:F:0)-u->Y(4)
Y(4:F:0)Y(4:F:0)Y(4:F:0)-u->YTEMP(5)
YTEMP(5:F:0)<-
<-
YP(9:F:0)YP(9:F:0)YP(9:F:0)-u->SUM(10)
SUM(10:F:0)-u->Y(4)
<-
SUM(10:F:0)<-
YP(9:F:0)<-
VDG - Run time is 0.050000
VDG - NumActiveSyms: 4
VDG - ActiveSizeInBytes: 184
