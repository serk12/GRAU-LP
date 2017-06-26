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
/*  if (type == ID) {
    attr->kind = "id";
    attr->text = text;
  }
  else {*/
    attr->kind = text;
    attr->text = "";
//  }
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

/// exercici 2 MY CODE

const int UNDF = -1;
struct mypair {
    int x, y;
    mypair() {}
    mypair(int x, int y) {
        this->x = x; this->y = y;
    }
    mypair(AST* a) {
        x = stoi(a->kind);
        y = stoi(a->right->kind);
    }
};

struct tblock {
    int x, y;
    int h, w;
    tblock() {x = y = h = w = UNDF;}
    tblock(int x, int y, int h, int w) {
        this->x = x; this->y = y; this->h = h; this->w = w;
    }
    tblock(mypair pos, mypair size) {
        x = pos.x; y = pos.y; h = size.x; w = size.y;
    }
};

struct Graella{
    int n, m;
    vector<vector<int> > height;
    map<string, tblock> blocks;
    Graella() {}
    Graella(int x, int y) {
        n = x; m = y;
        height = vector<vector<int> > (x, vector<int> (y, 0));    
    }
};

Graella g;
map<string, AST*> definitions;

void ops(AST *a);

vector<AST* > getAll(AST *a) {
    vector<AST* > allChild;
    a = a -> down;
    while (a != NULL) {
        allChild.push_back(a);
        a = a -> right;
    }
    return allChild;
}

tblock getVar(string name) {
    map<string, tblock>::iterator it = g.blocks.find(name);
    if (it == g.blocks.end()) {
        std::cerr << "VAR " << name << " DOSEN'T EXISTS" << endl;
        return tblock(UNDF,UNDF,UNDF,UNDF);
    }
    else return it -> second;
}

int heightCall(AST *a) {
    tblock block = getVar(a -> kind);
    if (block.x != UNDF)    return g.height[block.x][block.y];
    else                    return UNDF;
}

inline int evaluate(AST *a) {
    if (a -> kind == "HEIGHT") return heightCall(a -> down);
    else return stoi(a -> kind);
}


bool fits(tblock block, tblock block2, int z, int &retX, int & retY) {
    if (block.h <= block2.h and block.w <= block2.w) {
        int x = block2.x;   int y = block2.y; 
        int h = block2.h;   int w = block2.w;
        int area = 0;       int height;
        bool in = true;
        for (int i = 0; i < h and (x + i - 1) < g.height.size(); ++i) {
            for (int j = 0; j < w and (y + j - 1) < g.height[0].size(); ++j) { 
                int height = g.height[x + i - 1][y + j - 1];
                retX = x; retY = y;
                for (int k = 0; k < block.h and (x + i + k - 1) < g.height.size(); ++k) {
                    for (int t = 0; t < block.w and (y + j + t - 1) < g.height[0].size(); ++t) {
                        in = (height == g.height[x + k + i - 1][y + t + j - 1]);  
                    }
                }
                if (in) break;
            }
            if (in) break;
        }
        return in and (z == UNDF or z <= height);
    }
    return false;
}

bool fits(AST *a) {
    tblock block = getVar(a -> kind);
    if (block.x != UNDF) {
        tblock block2(stoi(a -> right -> kind), stoi(a -> right -> right -> kind), block.h, block.w);
        int z =  stoi(a -> right -> right -> right -> kind); 
        int x, y; 
        return fits(block, block2, z, x, y);
    }
    else return false;
}

bool confirmBoolF(AST *a) {
    string typeOp = a -> kind;
    vector<AST *> childrens = getAll(a);
    if      (typeOp == "OR")    return (confirmBoolF(childrens[0]) or  confirmBoolF(childrens[1]));
    else if (typeOp == "AND")   return (confirmBoolF(childrens[0]) and confirmBoolF(childrens[1]));
    else if (typeOp == "<>")    return evaluate(childrens[0]) != evaluate(childrens[1]);
    else if (typeOp == "==")    return evaluate(childrens[0]) == evaluate(childrens[1]);
    else if (typeOp == "<")     return evaluate(childrens[0]) <  evaluate(childrens[1]);
    else if (typeOp == ">")     return evaluate(childrens[0]) >  evaluate(childrens[1]);
    else if (typeOp == "FITS")  return fits(childrens[0]);
    else {
        std::cerr << "ERROR CALCULING BOOL OP CAN'T IDENTIFY WHY" << std::endl;
        return false;
    }
}


void modGrid(tblock block, bool sum) {
    for (int i = 0; i < block.h  and (block.x + i-1) < g.height.size(); ++i) {
        for (int j = 0; j < block.w and (block.y + j - 1) < g.height[0].size(); ++j) {
            if (sum) ++g.height[block.x + i -1][block.y + j-1];
            else     --g.height[block.x + i -1][block.y + j-1];
        }
    }
}

void cleanPos(tblock block) {modGrid(block, false);}

void addBlock(tblock block) {modGrid(block, true );}

void addVar(tblock block, string name) {
    std::pair<std::map<string, tblock>::iterator,bool> exist = g.blocks.insert(std::pair<string, tblock>(name, block));
    if (!exist.second) exist.first->second = block;   
}

bool addToGrid(tblock block, string name) {
    if (block.x != UNDF) {
        if (((block.x + block.h -1) <= g.height.size()) and ((block.y + block.w -1) <= g.height[0].size())) {
            int height = g.height[block.x -1][block.y -1];
            bool inside = true;
            for (int i = 0; i < block.h and inside; ++i)
                for (int j = 0; j < block.w and inside; ++j) 
                    inside = (g.height[block.x + i -1][block.y + j -1] == height);
            if (inside) {
                addVar(block, name);
                addBlock(block);
                return true;
            }
        }
    }
    return false;
}

inline tblock place(AST *a) {
    a = a->down;
    mypair size(a->down);
    mypair pos (a->right->down);
    return tblock(pos, size);
}
//DONT SAVE THE CHANGES IN PUSH AND POPS ON THE TBLOCK INFO
tblock opPoPush(tblock block1, tblock block2, string type) { 
    if (type == "POP") {
        if (((block1.x <= (block2.h + block2.x)) and (block1.y <= (block2.w + block2.y)) and block1.x >= block2.x and block1.y >= block2.y) 
            or (block1.x == UNDF and block1.y == UNDF and block1.w != UNDF and block1.h != UNDF)) {
            cleanPos(block1);
            return block2;
        }
        else std::cerr << "ERROR CAN'T POP" << std::endl; //tachnically it must not pass
    }
    else if (type == "PUSH") {
        int x, y;
        if (fits(block1, block2, UNDF, x, y)) {
            block1.x = x; block1.y = y;
            addBlock(block1);
            return block1;
        }
        else std::cerr << "ERROR CAN'T PUSH " << std::endl;
    }
    else std::cerr << "ERROR" << std::endl; //error impossible
    return tblock();
}


tblock getHW(AST *a) {
    tblock block;
    if (a -> kind == "list") {
        block.h = stoi(a -> down -> kind);
        block.w = stoi(a -> down -> right -> kind);
    }
    else block = getVar(a -> kind);
    return block;
}

tblock popPushI(AST *a) {
    string type = a -> kind;
    if (type != "POP" and type != "PUSH") return getVar(type);
    else {
        AST *var  = a -> down -> right;
        AST *next = a -> down;
        tblock block  = opPoPush(popPushI(next), getHW(var), type);
        if (type == "PUSH") addVar(block, var -> kind);
        return block;
    }
}

tblock popPush(AST *a) {
    string type = a -> kind;
    AST *var  = a -> down;
    AST *next = a -> down -> right;
    return opPoPush(getHW(var), popPushI(next), type);
}

void newVar(AST *a) {
    string varName = a -> kind;
    tblock block;
    a = a -> right;
    if (a -> kind == "PLACE") {
        block = place(a);
        if(addToGrid(block, varName)) std::cout << "NICE NEW BLOCK!" << std::endl;
        else std::cout << "UFF NOT A GOOD NEW ONE" << std::endl;
    }
    else {
        block = popPush(a);
        addVar(block, varName);
        if (block.h != UNDF) std::cout << "NICE NEW PUSH/POP!" << std::endl;
        else std::cout << "UFF NOT A GOOD PUSH/POP" << std::endl;
    }
}

void movesCall(AST *a) {
    vector<AST* > childrens = getAll(a);
    string name = childrens[0]->kind;
    tblock block = getVar(name);
    if (block.x != UNDF) {
        string dir = childrens[1] -> kind;
        int units = stoi(childrens[2] -> kind);
        tblock del = block;
        if (dir == "WEST") block.x -= units;
        else if (dir == "EAST") block.x += units;
        else if (dir == "NORTH") block.y -= units;
        else if (dir == "SOUTH") block.y += units;
        if(addToGrid(block, name)) {
            cleanPos(del);
            std::cout << "NICE MOVE!" << std::endl;
        }
        else std::cout << "UFF NOT A GOOD MOVE" << std::endl;
    }
}

void whileCall(AST *a) {
    vector<AST* > childrens = getAll(a);
    while (confirmBoolF(childrens[0])) ops(childrens[1]);
}

void functionsCall(string def, AST* param) {
    if (def == "HEIGHT") cout << heightCall(param) << endl;
    else {
        map<string, AST*>::iterator it = definitions.find(def);
        if (it == definitions.end()) std::cerr << "ERROR DEFINITION " << def << " DON'T EXIST" << std::endl;
        else ops(it->second);
    }
}


bool start(AST *a) {
    if (a -> kind != "Grid") std::cerr << "ERROR MUST START WHIT Grid x y" << std::endl;
    else {
        if (( a -> down) == NULL ||  (a -> down -> right)  == NULL) std::cerr << "Grid have to had two nums" << std::endl;
        else  {
            int x = stoi(a -> down -> kind); int y = stoi(a -> down -> right -> kind);
            if (x > 0 and y > 0) {
                g = Graella(x, y);
                return true;
            }
            else {
                std::cerr << " SIZE OF GRID NOT VALID" << std::endl;
                return false;
            }
        }
    }
    return false;
}

void ops(AST *a) {
    vector <AST* > childrens = getAll(a);
    for (int i = 0; i < childrens.size(); ++i) {
        string operant = childrens[i] -> kind;
        if (operant == "=") newVar(childrens[i] -> down);
        else if (operant == "WHILE") whileCall(childrens[i]);
        else if (operant == "MOVE") movesCall(childrens[i]);
        else functionsCall(operant, childrens[i] -> down);
    }
}


void defs(AST *a) {
    a = a -> down;
    while (a != NULL) {
        definitions.insert(std::pair<string, AST*> (a -> down -> kind, a -> down -> right));
        a = a -> right;
    }
}

void executeListInstrucctions(AST *a) {
    if (a != NULL) {
        vector<AST* > childrens = getAll(a);
        if (childrens.size() >= 1 and start(childrens[0])) {
            if (childrens.size() == 3 and childrens[2] != NULL) defs(childrens[2]);
            if (childrens.size() >= 2) ops(childrens[1]);
        }
        else std::cerr << "FATAL ERROR THERE IS NO CODE OR CAN'T OMIT THE ERROR AND CONTINUE" << std::endl;
    }
}


/// END MY CODE
int main() {
  root = NULL;
  ANTLR(lego(&root), stdin);
  ASTPrint(root);
  executeListInstrucctions(root);   //execute my code
}
>>

#lexclass START
//...
#token SPACE "[\ \n]" << zzskip();>>
#token COM "\," 
#token GRID "Grid"
#token OPP "PUSH | POP"
#token EQUAL "\="
#token BOOLOP  "\<|\>|\==|\<>" 
#token BOOLOPP "AND | OR"
#token PARL "\("
#token PARR "\)"
#token CORL "\["
#token CORR "\]"
#token DIR "WEST | EAST| NORTH | SOUTH"
#token DEF "DEF"
#token ENDEF "ENDEF"
#token WHILE "WHILE"
#token PLACE "PLACE"
#token AT "AT"
#token MOVE "MOVE"
#token NUM "[0-9]+"
#token ID "[a-zA-Z0-9]+"

grid:   GRID^ NUM NUM;
par:    PARL! NUM COM! NUM PARR! <<#0=createASTlist(_sibling);>>;
place:  PLACE^ par AT! par;
moreP:  ID (OPP^ ID)*;
push:   (par | ID) OPP^ moreP;
info:   (ID^ (param | )) | NUM;
boolFP: info (BOOLOP^ info)*;
boolF:  boolFP (BOOLOPP^ boolFP)*;
whilee: WHILE^ PARL! boolF PARR! CORL! code CORR!;
move:   MOVE^ ID DIR NUM;
param:  PARL!  (ID | NUM | par) (COM! (ID | NUM | par))* PARR!;
decOp:  ID^ ((EQUAL^ (place | push)) | (param | ));
ops:    whilee | decOp | move ;
defs:   DEF^ ID code ENDEF!;
def:    (defs)* <<#0=createASTlist(_sibling);>>;
code:   (ops)* <<#0=createASTlist(_sibling);>>;
lego:   grid code def <<#0=createASTlist(_sibling);>>;
//....









