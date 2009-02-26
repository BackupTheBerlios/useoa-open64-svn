digraph OA_DUG {
node [shape=rectangle];
X_1->Y_2[label="F_func_"];
}
markVaried: Independent(X)
-v->X(1)
X(1:F:func_)-v->Y(2)
<-
<-
markUseful: Dependent(Y)
-u->Y(2)
Y(2:F:0)-u->X(1)
<-
<-
VDG - Run time is 0.120000
VDG - NumActiveSyms: 2
VDG - ActiveSizeInBytes: 16
