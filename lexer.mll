{
    open Parser;;
    exception Eof;;
}

rule token = parse
    | [' ' '\t']            {   token lexbuf            }
    | '\n'                  {   EOL                     }
    | "<=>"                 {   EQUIVALENT              }
    | "=>"                  {   IMPLIES                 }
    | 'X'                   {   XOR                     }
    | "\\/"                 {   OR                      }
    | "/\\"                 {   AND                     }
    | '~'                   {   NOT                     }
    | '('                   {   LPARENT                 }
    | ')'                   {   RPARENT                 }
    | '-'?['0'-'9']+ as s   {   INT (int_of_string s)   }
    | "True"                {   TRUE                    }
    | "False"               {   FALSE                   }
    | eof                   {   raise Eof               }
