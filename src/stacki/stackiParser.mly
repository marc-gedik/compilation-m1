%{

  open StackiAST

%}

%token REMEMBER DEFINE UNDEFINE ADD MUL DIV SUB GT GTE LT LTE EQ EXIT
%token COLON EOF
%token<int> INT
%token<string> ID COMMENT

%start<StackiAST.t> program

%%

program: p=labelled_instruction* EOF
{
  p
}

labelled_instruction: l=label? i=located(instruction) {
  (l, i)
}

label: l=ID COLON {
  Label l
}

instruction:
  REMEMBER i=INT { Remember i    }
| DEFINE x=ID    { Define (Id x) }
| UNDEFINE       { Undefine      }
| EXIT           { Exit          }
| ADD            { Binop Add     }
| SUB            { Binop Sub     }
| MUL            { Binop Mul     }
| DIV            { Binop Div     }
| GT             { Binop GT      }
| GTE            { Binop GTE     }
| LT             { Binop LT      }
| LTE            { Binop LTE     }
| EQ             { Binop EQ      }
| x=COMMENT      { Comment x     }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
