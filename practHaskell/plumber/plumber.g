#header
<<
#include <string>
#include <iostream>
#include <map>
#include <vector>
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

struct tube {
    int len;
    int diam;
};

struct vtube {
    vector<tube> elems;
    int msize;
};

map<string,tube> mtubes;
map<string, vtube> mvectors;
map<string,int> mconnectors;
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    if (type==ID) {
        attr->kind = "id";
        attr->text = text;
    } else if (type==NUM | type==DNUM ) {
        attr->kind = "num";
        attr->text = text;
    } else {
        attr->kind = text;
        attr->text = "";
    }
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


string translate (string in) {
 if (in=="NOT") return "Not";
 if (in=="AND") return "And";
 if (in=="OR") return "Or";
 if (in=="+") return "Plus";
 if (in=="-") return "Minus";
 if (in=="*") return "Times";
 if (in=="==") return "Eq";
 if (in=="<") return "Lt";
 if (in==">") return "Gt";
 return in;
}

void ASTPrintInst(AST *a);


void ASTPrintExpression(AST *a) {
  if (a==NULL) return;
  if (a->kind=="id") {
      cout << "(Var \"" << a->text << "\")";
      return;
  }
  if (a->kind=="num") {
      cout << "(Const " << a->text << ")";
      return;
  }
  if (a->kind=="LENGTH") {
      cout << "(Length \"" << child(a,0)->text << "\")";
      return;
  }
  if (a->kind=="DIAMETER") {
      cout << "(Diameter \"" << child(a,0)->text << "\")";
      return;
  }
  if (a->kind=="FULL") {
      cout << "(Full \"" << child(a,0)->text << "\")";
      return;
  }
  if (a->kind=="EMPTY") {
      cout << "(Empty \"" << child(a,0)->text << "\")";
      return;
  }
  if (a->down==NULL) {
      cout << translate(a->kind);
      return;   
  }
  
  cout<< "(" << translate(a->kind) << " ";
  AST *i = a->down;
  ASTPrintExpression(i);
  i=i->right;

  while (i!=NULL) {
    cout << " ";
    ASTPrintExpression(i);
    i=i->right;
  }
  cout << ")";
}

void ASTPrintConExpr(AST *a){
  if (a->kind=="id"){
      cout << "(CVar \"" << a->text << "\")";
      return;
  }  
  if (a->kind=="CONNECTOR"){
      cout << "(Connector ";
      ASTPrintExpression(child(a,0));
      cout << ")";
      return;
  }
}

void ASTPrintTubeExpr(AST *a){
  if (a->kind=="MERGE") {
      cout << "(Merge ";
      ASTPrintTubeExpr(child(a,0));
      cout << " ";
      ASTPrintConExpr(child(a,1));
      cout << " ";
      ASTPrintTubeExpr(child(a,2));
      cout << ")";
      return;
  }      
  if (a->kind=="TUBE") {
      cout << "(Tube ";
      ASTPrintExpression(child(a,0));
      cout << " ";
      ASTPrintExpression(child(a,1));
      cout << ")";
      return;
  }
  if (a->kind=="id"){
      cout << "(TVar \"" << a->text << "\")";
      return;
  }  
}

void ASTPrintList(AST *a) {
  if (a==NULL) return;
  if (a->kind!="list") return;
  if (a->down==NULL) {
      cout << "(Seq [])";
      return;
  }

  cout << "(Seq [";
  AST *i = a->down;
  ASTPrintInst(i);
  i=i->right;

  while (i!=NULL) {
    cout << ",";
    ASTPrintInst(i);
    i=i->right;
  }
  cout << "])";
}

void ASTPrintInst(AST *a)
{
  if (a==NULL) return;
  if (a->kind=="list") ASTPrintList(a);
  if (a->kind=="INPUT") {
      cout << "(Input \"" << child(a,0)->text << "\")";
      return;
  }
  if (a->kind=="PUSH") {
      cout << "(Push \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\")";
      return;
  }
  if (a->kind=="POP") {
      cout << "(Pop \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\")";
      return;
  }
  if (a->kind=="DRAW") {
      cout << "(Draw ";
      ASTPrintTubeExpr(child(a,0));
      cout << ")";
      return;
  }
  if (a->kind=="PRINT") {
      cout << "(Print ";
      ASTPrintExpression(child(a,0));
      cout << ")";
      return;
  }
  if (a->kind=="=") {
      if (child(a,1)->kind == "id"){
          if (child(a,2) == NULL){
              cout << "(Copy \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\")";
          } else {
              cout << "(Split \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\" \"" << child(a,2)->down->text << "\")";
          }
      } else {
          if (child(a,1)->kind == "TUBEVECTOR"){
              cout << "(DeclareVector \"" << child(a,0)->text << "\" ";
              ASTPrintExpression(child(a,1)->down);
              cout << ")";
          }
          else if (child(a,1)->kind == "CONNECTOR"){
              cout << "(CAssign \"" << child(a,0)->text << "\" ";
              ASTPrintConExpr(child(a,1));
              cout << ")";
          }
          else {
              cout << "(TAssign \"" << child(a,0)->text << "\" ";              
              ASTPrintTubeExpr(child(a,1));
              cout << ")";
          }
      }
      return;
  }
  if (a->kind=="WHILE") {
      cout << "(Loop ";
      ASTPrintExpression(child(a,0));
      cout << " ";
      ASTPrintList(child(a,1));
      cout << ")";
      return;
  }      
  if (a->kind=="IF"){
      cout << "(Cond ";
      ASTPrintExpression(child(a,0));
      cout << " ";
      ASTPrintList(child(a,1));
      cout << " ";
      ASTPrintList(child(a,2));
      cout << ")";
      return;
  }
}

int main() {
  root = NULL;
  ANTLR(plumber(&root), stdin);
  ASTPrintInst(root);
  cout << endl;
}
>>

#lexclass START
#token INPUT "INPUT"
#token PRINT "PRINT"
#token DRAW "DRAW"
#token OPENP "\("
#token CLOSEP "\)"
#token WHILE "WHILE"
#token IF "IF"
#token ELSE "ELSE"
#token MERGE "MERGE"
#token SPLIT "SPLIT"
#token TUBE "TUBE"
#token PUSH "PUSH"
#token POP "POP"
#token FULL "FULL"
#token EMPTY "EMPTY"
#token TUBEVECT "TUBEVECTOR"
#token OF "OF"
#token CONN "CONNECTOR"
#token LENGTH "LENGTH"
#token DIAMETER "DIAMETER"
#token NUM "[0-9]+"
#token DNUM "[0-9]+\.[0-9]+"
#token LESS "<"
#token GREAT ">"
#token ASSIG "="
#token EQUAL "=="
#token AND "AND"
#token OR "OR"
#token NOT "NOT"
#token PLUS "\+"
#token MINUS "\-"
#token TIMES "\*"
#token SC "\,"
#token ENDWHILE "ENDWHILE"
#token ENDIF "ENDIF"
#token ID "[a-zA-Z][a-zA-Z0-9]*"
#token SPACE "[\ \t \n]" << zzskip();>>

plumber: (ops)* <<#0=createASTlist(_sibling);>>;

ops: input | draw | print | iter | conditionals | assign | pushpop;

input: INPUT^ ID;

print: PRINT^ expr;

draw: DRAW^ (merge | literal | ID );

length: LENGTH^ OPENP! ID CLOSEP!;

diameter: DIAMETER^ OPENP! ID CLOSEP!;

conditionals: IF^ OPENP! cond CLOSEP! plumber ELSE! plumber ENDIF!;
iter: WHILE^ OPENP! cond CLOSEP! plumber ENDWHILE!;

assign: (ID | (OPENP! ID SC! ID CLOSEP!)) ASSIG^ (merge | connect | split | 
literal | ID | vect);

connect: CONN^ expr;

merge: MERGE^ base;

base: (tatom|merge) catom (tatom|merge); 

catom: ID | connect;

tatom: ID | literal;

split: SPLIT^ ID;

literal: TUBE^  expr expr;

vect: TUBEVECT^ OF! expr;

pushpop: (PUSH^|POP^) ID ID;

cond: cond1 (OR^ cond1)* ;
cond1: cond2 (AND^ cond2)* ;
cond2: (NOT^|) cond3 ;
cond3: boolFunc | expr (LESS^|GREAT^|EQUAL^) expr | OPENP! cond CLOSEP!;

boolFunc: FULL^ OPENP! ID CLOSEP! | EMPTY^ OPENP! ID CLOSEP!;

expr: term ((PLUS^ | MINUS^) term)* ;
term: val (TIMES^ val)* ;

val: ID | NUM | length | diameter;

