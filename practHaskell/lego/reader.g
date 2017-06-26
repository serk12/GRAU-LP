#header
<<
#include <vector>
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  attr->text = "";
  if (type == Assign) attr->kind = "Assign";
  else if (type == Input) attr->kind = "Input";
  else if (type == Print) attr->kind = "Print";
  else if (type == Empty) attr->kind = "Empty";
  else if (type == Push) attr->kind = "Push";
  else if (type == Pop) attr->kind = "Pop";
  else if (type == Size) attr->kind = "Size";
  else if (type == Cond) attr->kind = "Cond";
  else if (type == Loop) attr->kind = "Loop";
  else if (type == Gt) attr->kind = "Gt";
  else if (type == Eq) attr->kind = "Eq";
  else if (type == Plus) attr->kind = "Plus";
  else if (type == Minus) attr->kind = "Minus";
  else if (type == Times) attr->kind = "Times";
  else if (type == Const or type == ConstF) {
    attr->kind = "Const";
    attr->text = text;
  }
  else if (type == Var) {
    attr->kind = "Var";
    char* com = "\"";
    char aux[30];
    strcpy(aux,com);
    strcat(aux, text);
    strcat(aux, com);
    attr->text = aux;
  }
  else attr->kind = text;
  
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "Seq" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="Seq";
 as->right=NULL;
 as->down=child;
 return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}


void ASTPrintIndentL(AST *a, int &prof, int &next, int& needsCom, bool &lastCom, int needsComUp);

int calcNumNext(AST *a) {
    AST *i = a -> down;
    int prof = 0;
    while (i != NULL) {
        ++prof;
        i = i -> right;
    }
    return prof - 1;
}

                                                 //1 === print(,) 0 === nothing, 2 >== print for Cond
void ASTPrintIndent(AST *a,int &prof, int &next, int &needsCom, bool &lastCom) {
  if (a==NULL) return;
  int profUp, nextUp, needsComUp;
  if (a->kind == "Seq") {
    needsComUp = needsCom;
    profUp = prof;
    prof = 0;
    nextUp = next;
  }
  if (a-> kind == "Seq" and needsCom == 2) {
      cout << ",(";
      needsCom = 1;
  }
  cout << " " << a->kind;
  lastCom = false;
  if (a->text!="") cout << " " << a->text;
  if (a->down != NULL) {
    if (a->kind == "Seq") {
        cout<<"[";
        lastCom = true;
        next = calcNumNext(a);
    }
    else {
        cout << "(";
        lastCom = true;
        ++prof;
    }
  if (a->kind != "Seq"  and a->kind != "Input" and a->kind != "Empty" and 
      a->kind != "Print" and a->kind != "NOT") { //next of Expresion (command, BExpr or NExpr)
      needsComUp = needsCom;
      needsCom = 1;
      if (a->kind == "Cond") needsCom = 2;
    }
  }
  
  ASTPrintIndentL(a,prof, next, needsCom, lastCom, needsComUp);
  
  if (a->down != NULL) {
    if (a->kind == "Seq") {
        cout<<"]";
        lastCom = false;
    }
    else {
      cout << ")";
      if (a-> kind == "Cond") cout << ")";
      lastCom = false;
      --prof;
      if (prof == 0 and next > 0) {
        cout << ",";
        lastCom = true;
        --next;
      }
    }
  }
  
  if (a->kind == "Seq") {
      prof = profUp;
      next = nextUp;
  }
}

void ASTPrintIndentL(AST *a, int &prof, int &next, int &needsCom, bool &lastCom, int needsComUp)
{
  
  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    ASTPrintIndent(i, prof, next, needsCom, lastCom);
    i=i->right;
  }
  
  if (needsCom >= 1 and i != NULL) {
    if (not lastCom) {
        cout << "," ;
        lastCom = true;
        --needsCom;
        needsCom = needsComUp;
    }
  }
  
  if (i!=NULL) {
      ASTPrintIndent(i,prof, next, needsCom, lastCom);
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  int prof = 0;
  int needsCom = 0;
  bool lastCom = false;
  while (a!=NULL) {
    int next = calcNumNext(a);
    ASTPrintIndent(a,prof, next, needsCom, lastCom);
    a=a->right;
  }
  cout << endl;
}

int main() {
  root = NULL;
  ANTLR(start(&root), stdin);
  ASTPrint(root);
}


>>

#lexclass START
//...
#token SPACE "[\ \n]" << zzskip();>>

#token Assign   "\:\="
#token Input    "INPUT"
#token Print    "PRINT"
#token Empty    "EMPTY"
#token Push     "PUSH"
#token Pop      "POP"
#token Size     "SIZE"
#token Cond     "IF"
#token THEN     "THEN"
#token Loop     "WHILE"
#token DO       "DO"
#token END      "END"
#token ELSE     "ELSE"

#token AND      "AND"
#token OR       "OR"
#token NOT      "NOT"
#token Gt       "\>"
#token Eq       "\="

#token Plus     "\+"
#token Minus    "\-"
#token Times    "\*"

#token Const    "[0-9]+"
#token ConstF   "[0-9]+\.[0-9]+"
#token Var      "[a-zA-Z0-9]+"


bole:  (Var | Const | ConstF) (Gt^ | Eq^) (Var | Const | ConstF);
boole: (NOT^ | ) bole;
bexpr: boole ((AND^ | OR^)  boole)*;
nexpr: (Var | Const | ConstF) ((Plus^ | Minus^ | Times^) (Var | Const | ConstF))*;

push:  Push^ Var nexpr;
twoOp: (Size^ | Pop^) Var Var;
oneOp: (Input^ | Print^ | Empty^) Var;
whils: Loop^ bexpr DO! start END!;
elses: ELSE! start END!;
cond:  Cond^ bexpr THEN! start (END!| elses);
decl:  Var Assign^ nexpr;
op: decl | oneOp | twoOp | push | whils | cond;
start: (op)* <<#0=createASTlist(_sibling);>>;
//....






