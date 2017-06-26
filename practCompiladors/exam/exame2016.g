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
#token GRID "Grid"
#token PLACE "PLACE"
#token MOVE "MOVE"
#token NOT "NOT"
#token DIR "NORTH|SOUTH|EAST|WEAST"
#token BOOLOP "AND|OR"
#token BOOLFUN "FITS"
#token FUNCTION "HEIGHT"
#token COMOP "\<|\>"
#token AT "AT"
#token WHILE "WHILE"
#token DEF "DEF"
#token ENDEF "ENDEF"
#token COM "\,"
#token EQ "\="
#token OPAR "\("
#token CPAR "\)"
#token OCLAU "\["
#token CCLAU "\]"
#token NUM "[0-9]*"
#token ID "[a-zA-Z][a-zA-Z0-9]*"


grid: GRID^ NUM NUM;
par:  NUM (COM! NUM)* <<#0=createASTlist(_sibling);>>;
dec: ID EQ! PLACE^ (OPAR! par CPAR!) AT! ((OPAR! par CPAR!) | ID);
move: MOVE^ ID DIR NUM;
comp: num COMOP^ num;
num: NUM | (FUNCTION^ OPAR! ID CPAR!);
exp: BOOLFUN^ OPAR! ID COM! par CPAR!;
subbool: exp | (OPAR! booleans CPAR!) | comp | (NOT^ subbool);
booleans: subbool (BOOLOP^ subbool)*;
whiles: WHILE^ OPAR! booleans CPAR! OCLAU! ins CCLAU!;
definitions: DEF^ ID ins ENDEF;

instructions: dec | move | whiles;
ins: (instructions)* <<#0=createASTlist(_sibling);>>;
def: (definitions)* <<#0=createASTlist(_sibling);>>;
plumber: grid ins def <<#0=createASTlist(_sibling);>>;
//...