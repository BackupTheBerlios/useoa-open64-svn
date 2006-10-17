struct anotherStruct {
  int d;
};

struct myStruct { 
  int b; 
  struct anotherStruct *a; 
}; 

struct myStruct *gp; 
struct myStruct gy; 
struct myStruct gb[10][20]; 

int (*fp)(); 
int bar(); 
int* hello(); 

int
bar() 
{ 
  int x,y; 
  struct myStruct *p; 
  int **q; 
  int *r; 
  struct myStruct sta[40][10][30]; 
  struct anotherStruct stb; 
  
  p = 0;
  *(sta[3][4][2].a) = stb;
  r = &x;
  /* ... = (...)malloc(...) */
  /* foo(&p); */
  /* *(hello()) = ...  */
  **q = x;
  gp->a->d = 5;
  /* ... = (*p).a */
  p->a = 0;
  /* ... = &(p->a) */
  /* ... = &(sta[3][4]) */
  y = *(r<*q ? r : *q);
  *(r + 1) = 6;
}

int
main() 
{ 
  fp = bar;
  fp();
}
