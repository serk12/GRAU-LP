#header
<<
#include <string>
#include <iostream>

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
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>
#include <vector>
#include <map>
#include <stack>
#include <utility>

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    attr->kind = text;
   	attr->text = "";
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

/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
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

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }

}
//////////////////////////////////////
/*                                  */
/*                                  */
/*ALL MY CODE NAINO NAINO NAAAA~~~~~*/
/*                                  */
/*                                  */
//////////////////////////////////////

int main() {
  AST *root = NULL;
  ANTLR(plumber(&root), stdin);
  ASTPrint(root);
}
>>
#lexclass START
#token SPACE "[\ \n]" << zzskip();>>
#token DIRFILES "\-\>|\<\-"
#token OPCONCURRENCE2 "\||\;"
#token OPCONCURRENCE1 "\#"
#token OPCONCURRENCE "\+"
#token OPPAR "\("
#token CLPAR "\)"
#token F "file"
#token INIT "start"
#token END "end"
#token CONNECTION "connection"
#token QUERIES "QUERIES"
#token QUERIESOP2 "difference|correctfile"
#token QUERIESOP "critical"
#token ID "[a-zA-Z][a-zA-Z0-9]*"


subOperation: ID | OPPAR! concurrenciOps CLPAR!;
concurrenciOps2:   subOperation (OPCONCURRENCE2^ subOperation)*;
concurrenciOps1:concurrenciOps2 (OPCONCURRENCE1^ concurrenciOps2)*;
concurrenciOps: concurrenciOps1 (OPCONCURRENCE^ concurrenciOps1)*;

dirf: ID DIRFILES^ ID;

def: INIT! concurrenciOps END! ID^;
connect: CONNECTION^ ID ID;
filesistem: F^ dirf;

dosOp: QUERIESOP2^ ID ID;
unOp:  QUERIESOP^ ID;

querries: dosOp | unOp;

ops: def| connect | filesistem;
querri: (QUERIES^ (querries)*) | ;
opsAux: (ops)* <<#0=createASTlist(_sibling);>>;
querriAux: querri <<#0=createASTlist(_sibling);>>;
plumber: opsAux querriAux <<#0=createASTlist(_sibling);>>;
//...
//...