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
  ANTLR(roomba(&root), stdin);
  ASTPrint(root);
}
>>
#lexclass START
#token SPACE "[\ \n]" << zzskip();>>
#token ENDC "endcleaning"
#token STARTC "startcleaning"
#token POSITION "position"
#token TASK "TASK"
#token ENDTASK "ENDTASK"
#token MOVE "move"
#token DIR "right|left|up|down"
#token FLUSH "flush"
#token BOOLFUN "sensorprox"
#token INTFUN  "sensorlight"
#token EXEC "exec"
#token OPS "ops"
#token OPCOR "\["
#token CLCOR "\]"
#token INTCOM "\>"
#token BOOLCOM "\=\="
#token COM "\,"
#token IF "if"
#token THEN "then"
#token BOOLOP "OR|AND"
#token OFFON "OFF|ON"
#token NUM "[0-9]*"
#token ID "[a-zA-Z][a-zA-Z0-9]*"

boolComponent: ((INTFUN | NUM) (BOOLCOM^|INTCOM^) (INTFUN | NUM)) | ((BOOLFUN|OFFON) BOOLCOM^ (BOOLFUN|OFFON));
boolSentence: boolComponent (BOOLOP^ boolComponent)*;

condition: IF^ boolSentence THEN! ops;
opsArray: OPS^ OPCOR! ops (COM! ops)* CLCOR!;
exec: EXEC^ ID;
flush: FLUSH^ NUM;
move: MOVE^ DIR NUM;

ops: move | flush | exec | opsArray | condition;

task: TASK^ ID linstr ENDTASK!;

linstr: (ops)* <<#0=createASTlist(_sibling);>>;
tasks: (task)* <<#0=createASTlist(_sibling);>>;
position: POSITION^ NUM NUM; 
roomba: position STARTC! linstr ENDC! tasks <<#0=createASTlist(_sibling);>>;
