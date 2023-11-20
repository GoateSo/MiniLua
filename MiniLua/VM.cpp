#include <fstream>
#include <iostream>
#include <stdlib.h>
#include <string>
#include <vector>

/*
** basic types
*/
enum types {
  LUA_TNONE = -1,
  LUA_TNIL = 0,
  LUA_TBOOLEAN = 1,
  // LUA_TLIGHTUSERDATA = 2,
  LUA_TNUMBER = 3,
  LUA_TSTRING = 4,
  LUA_TTABLE = 5,
  LUA_TFUNCTION = 6,
  // LUA_TUSERDATA = 7,
  // LUA_TTHREAD = 8
};

enum opcodes {
  OP_MOVE,      // a, b
  OP_LOADK,     // a, bx
  OP_LOADBOOL,  // a, b, c
  OP_LOADNIL,   // a, b
  OP_GETUPVAL,  // a, b
  OP_GETGLOBAL, // a, bx
  OP_GETTABLE,  // a, b, c
  OP_SETGLOBAL, // a, bx
  OP_SETUPVAL,  // a, b
  OP_SETTABLE,  // a, b, c
  OP_NEWTABLE,  // a, b, c
  OP_SELF,      // unused
  OP_ADD,       // a, b, c
  OP_SUB,       // a, b, c
  OP_MUL,       // a, b, c
  OP_DIV,       // a, b, c
  OP_MOD,       // a, b, c
  OP_POW,       // a, b, c
  OP_UNM,       // a, b
  OP_NOT,       // a, b
  OP_LEN,       // a, b
  OP_CONCAT,    // a, b, c
  OP_JMP,       // a, sBx
  OP_EQ,        // a, b, c
  OP_LT,        // a, b, c
  OP_LE,        // a, b, c
  OP_TEST,      // a, c
  OP_TESTSET,   // a, b, c
  OP_CALL,      // a, b, c
  OP_TAILCALL,  // unused
  OP_RETURN,    // a, b
  OP_FORLOOP,   // a, sBx
  OP_FORPREP,   // a, sBx
  OP_TFORLOOP,  // unused
  OP_SETLIST,   // a, b, c
  OP_CLOSE,     // a
  OP_CLOSURE,   // a, bx
};
typedef struct callinfo {
  int base, func, top; // base, function index, top for this fn
  const int *savedpc;  // `savedpc' of current function
  int nresults;        // expected number of results from this call
} call_info;

// unioned lua value
typedef union Value {
  int i;
  double n;
  void *p;
} Value;

// tagged value, stored in stack
typedef struct {
  int type;
  Value value;
} t_value;

#define isnil(o) ((o).type == 0)
#define isboolean(o) ((o).type == 1)
#define isnumber(o) ((o).type == 2)
#define isstring(o) ((o).type == 3)
#define istable(o) ((o).type == 4)

// store current state of VM
typedef struct {
  int top, base;       // 1st free slot in stk, base for current function
  call_info *ci;       // call info for current function
  const int *savedpc;  // savedpc of current function
  int stk_last, stack; // last free slot in stack, stack base
} l_state;

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <file>" << std::endl;
    exit(1);
  }

  std::ifstream file(argv[1]);

  if (!file.is_open()) {
    std::cout << "Could not open file: " << argv[1] << std::endl;
    exit(1);
  }

  // read all bytes from file into vector
  std::vector<char> bytes((std::istreambuf_iterator<char>(file)),
                          std::istreambuf_iterator<char>());

  return 0;
}