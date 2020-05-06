Goose — Parsing Grammar
=======================

**module** := *item*\*

**item** := *definition*

**definition** := `def` IDENT ( ( `[` *type_params* `]` ) `(` *params* `)` )? `=` *body* (`;`)?

**body** := *expression_0* ( `;` expression_0 )*

**expression_0** :=\
| *let_expr*\
| *assign_expr*\
| *expression_1*

**let_expr** := `let` *pattern* `=` *expression_1*

**assign_expr** := *lvalue* ( `=` | `+=` | `-=` | `*=` | `/=` ) *expression_1*

**expression_1** := *expression_2* ( `||` *expression_2* )*

**expression_2** := *expression_3* ( `&&` *expression_3* )*

**expression_3** := *expression_4* ( ( `==` | `!=` | `>` | `<` | `<=` | `>=` ) *expression_4* )?

**expression_4** := *expression_5* ( ( `+` | `-` ) *expression_5* )*

**expression_5** := *expression_6* ( ( `*` | `/` | `%` ) *expression_6* )*

**expression_6** :=\
| ( `-` | `!` ) *expression_6*\
| *expression_7*

**expression_7** :=\
| *expression_8*
(`?`)* *app_clause*\* (`?`)* ( *method_or_field_access* | *pipeline* )\*\
| *for_expr*\
| *while_expr*\
| *if_expr*\
| *match_expr*

**method_or_field_access** := `.` IDENT (`?`)* *app_clause*\* (`?`)*

**pipeline** := `|>` *expression_8* (`?`)* *app_clause*\* (`?`)*

**app_clause** := ( `[` *type_args* `]` )? ( `(` *args* `)` | `{` *struct_initializer* `}` )

**expression_8** :=\
| *unit_expr* \
| `(` *body* `)`\
| `(` *tuple_expr* `)`\
| *none_expr* | *some_expr*\
| *var_expr*\
| *placeholder_expr*\
| STRING_LITERAL \
| INT_LITERAL\
| FLOAT_LITERAL

**tuple_expr** := *expression_1* ( `,` *expression_1* )*

**unit_expr** := `(` `)`

**none_expr** := `<` `>`

**some_expr** := `<` tuple_expr `>`

**var_expr** := IDENT

**placeholder_expr** := `_`

**for_expr** := `for` *pattern* `in` *expression_1* `:` *expression_0*

**while_expr** := `while` *expression_1* `:` *expression_0*

**if_expr** := `if` *expression_1* `:` *expression_0* ( `else` ( `:` *expression_0* | *if_expr* ) )?

**match_expr** := `match` *expression_1* `:` ( *match_arm*\* | `(` *match_arm*\* `)` )

**match_arm** := `|` *pattern* `=>` *body*

**params** := ( *param* ( `,` *param* )* )?

**param** := IDENT ( `:` *type* )?

**args** := *expression_1* ( `,` *expression_1* )

**type_args** := *type*\*

**struct_initializer** :=  (IDENT `=` *expression_1* ( `,` IDENT `=` *expression_1*)* )?

**pattern** :=\
| *unit_pattern*\
| *dot_pattern*\
| *appl_pattern*\
| `(` *tuple_pattern* `)`\
| `{` *struct_pattern* `}`\
| *none_pattern*\
| *some_pattern*\
| INT_LITERAL\
| STRING_LITERAL

**unit_pattern** := `(` `)`

**dot_pattern** := (`.`)  IDENT ( `(` *tuple_pattern* `)` )?

**appl_pattern** := IDENT (`.` IDENT)* ( `(` *tuple_pattern* `)` | `{` *struct_pattern* `}` )?

**tuple_pattern** := *pattern* ( `,` *pattern* )*

**none_pattern** := `<` `>`

**some_pattern** := `<` *tuple_pattern* `>`

**struct_pattern** := (IDENT `=` *pattern* ( `,` IDENT `=` *pattern*)* )?