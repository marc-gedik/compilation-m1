%{

  open StackixAST

%}

%token BLOCKCREATE BLOCKGET BLOCKSET
%token UJUMP JUMP SWAP CJUMP OR GETVARIABLE
%token REMEMBER DEFINE UNDEFINE ADD MUL DIV SUB GT GTE LT LTE EQ EXIT
%token COLON EOF
%token<int> INT
%token<string> ID COMMENT LABEL

%start<StackixAST.t> program

%%

program: p=labelled_instruction* EOF
{
  p
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}


labelled_instruction: l=label? i=located(instruction) {
  (l, i)
}

label: l=ID COLON {
  Label l
}

instruction:
  REMEMBER i=INT       { Remember i                             }
| REMEMBER i=LABEL     { RememberLabel (Label i)                }
| DEFINE x=ID          { Define (Id x)                          }
| UNDEFINE             { Undefine                               }
| EXIT                 { Exit                                   }
| ADD                  { Binop Add                              }
| SUB                  { Binop Sub                              }
| MUL                  { Binop Mul                              }
| DIV                  { Binop Div                              }
| GT                   { Binop GT                               }
| GTE                  { Binop GTE                              }
| LT                   { Binop LT                               }
| LTE                  { Binop LTE                              }
| EQ                   { Binop EQ                               }
| BLOCKCREATE          { BlockCreate                            }
| BLOCKGET             { BlockGet                               }
| BLOCKSET             { BlockSet                               }
| GETVARIABLE i=INT    { GetVariable i                          }
| JUMP l=ID            { Jump (Label l)                         }
| UJUMP                { UJump                                  }
| SWAP                 { Swap                                   }
| x=COMMENT            { Comment x                              }
| CJUMP l1=ID OR l2=ID { ConditionalJump (Label l1, Label l2)   }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
