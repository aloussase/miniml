#ifndef syntax_h__
#define syntax_h__

typedef struct Ty {
    enum {
        TInt,
        TBool,
        TArrow,
    } type;
    struct {
        struct Ty* from;
        struct Ty* to;
    } arrow;
} Ty;

typedef enum {
    EInt,
    EBool,
    EIdent,
    EApp,
    EPlus,
    ETimes,
    ELess,
    EMinus,
    EEqual,
    EIf,
    EFun,
} ExprType;

typedef struct Expr Expr;

typedef struct {
    Expr* lhs;
    Expr* rhs;
} BinOp;

typedef struct {
    Expr* condition;
    Expr* then_branch;
    Expr* else_branch;
} IfExpr;

typedef struct {
    char* name;
    char* param_name;
    Ty* param_type;
    Ty* return_type;
    Expr* body;
} FunExpr;

typedef struct {
    ExprType type;
    union {
        int number;
        char* string;
        BinOp binop;
        IfExpr if_;
        FunExpr fun;
    } as;
} Expr;

#endif
