module Parser
    (
        -- * AST data types
        Pre, PreExpr,
        Type,Ident,Package,
        Constant,Binop,UnOp,UnOpFlag,
        Access,Expr,
        Catch,Case,VarExpr,FuncExpr,Param, 
        ClassTrait,Import,FileTrait,File,

        -- * Functions
        parseFile -- :: String -> Either ParseError File

    ) where

import System.Environment
import Text.Parsec hiding (string,chainl1,chainr1)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Monad.State hiding (join)

----------------------------------------------------------------------------------------------

operators = ["+","-","*","/","%",
             "|","&","^","<<",">>",">>>",
             "||","&&",
             "==","!=",">","<",">=","<=",
             "=","+=","-=","*=","/=","%=","|=","&=","^=","<<=",">>=",">>>=",
             "!","~","++","--","?",":","..."]

langDef :: LanguageDef st
langDef = emptyDef {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.opStart = oneOf "!=<>?~-+*/^|&:%",
    P.opLetter = oneOf "!=<>?~-+*/^|&:%",
    P.reservedNames = ["if","else","try","catch","throw","for","while","in","new",
                     "public","private","inline","override","static","do","cast",
                     "switch","case","var","function","class","extends","return",
                     "#if","#elseif","#else","#end"],
    P.reservedOpNames = operators,
    P.caseSensitive = True
}

lexer = P.makeTokenParser langDef

parens p      = lexeme $ do { char '('; whiteSpace; res <- lexeme p; char ')'; return res; }
brackets p    = lexeme $ do { char '['; whiteSpace; res <- lexeme p; char ']'; return res; }
angles p      = lexeme $ do { char '<'; whiteSpace; res <- lexeme p; char '>'; return res; }
braces p      = lexeme $ do { char '{'; whiteSpace; res <- lexeme p; char '}'; return res; }
integer       = do { x <- lexeme $ (P.integer lexer) <|> (P.hexadecimal lexer); return $ fromIntegral x }

ident  = lexeme $ P.identifier lexer
float  = lexeme $ P.float      lexer
semi   = lexeme $ P.semi       lexer
comma  = lexeme $ P.comma      lexer
dot    = lexeme $ P.dot        lexer
colon  = lexeme $ P.colon      lexer

reserved s = lexeme $ (P.reserved lexer) s
operator s = lexeme $ (P.reservedOp lexer) s
symbol   s = lexeme $ (P.symbol   lexer) s

lexeme p      = do { res <- p; whiteSpace; return res }
stringLiteral = P.stringLiteral lexer

-- ignore whitespace and metadata for lexemes
whiteSpace    = let ws = P.whiteSpace lexer in ws>>many(metadata>>ws)
metadata      = ((char '@')>>(optional $ char ':')>>(many1 $ alphaNum <|> char '_')>>(return ()))
               <?> "Metadata"
           
----------------------------------------------------------------------------------------------

-- #if expr a [#elseif expr a] [#else a] #end
type Pre a = (PreExpr, a, [(PreExpr, a)], Maybe a)

data PreExpr = PreIdent Ident
             | PreAnd PreExpr PreExpr
             | PreOr PreExpr PreExpr
             | PreNot PreExpr
             deriving(Show)

----------------------------------------------------------------------------------------------

type Type    = String -- name of Type
type Ident   = String -- name of Ident
type Package = String -- name of Package

data Constant = CInt Int
              | CFloat Double
              | CString String
              | CIdent Ident
              deriving (Show)

data Binop = OpAdd | OpMul | OpDiv | OpSub | OpMod | OpAssign | OpEq | OpNeq | OpGt | OpLt
           | OpGeq | OpLeq | OpOr | OpAnd | OpXor | OpBoolAnd | OpBoolOr | OpShl | OpShr
           | OpUShr | OpAssignOp Binop | OpInterval
           deriving (Show)
data UnOp = OpInc | OpDec | OpNot | OpNeg | OpNegBits deriving (Show)
data UnOpFlag = FlagPre | FlagPost deriving (Show)

data Access = APublic | APrivate | AStatic | AOverride | AInline
            | APre (Pre [Access])
            deriving (Show)

data Expr = EConst Constant                     -- literal/ident
          | EArray [Expr]                       -- [Expr*]
          | EArrayAccess Expr Expr              -- Expr[Expr]
          | EBlock [Expr]                       -- {(Expr;?)*}
          | EUnOp UnOp UnOpFlag Expr            
          | EBinop Binop Expr Expr
          | ETernary Expr Expr Expr             -- Expr ? Expr : Expr
          | EWhile Bool Expr Expr               -- while (Expr) Expr (true) do Expr while(Expr) (false)
          | EFor Expr Expr                      -- for (Expr) Expr
          | EReturn (Maybe Expr)                -- return Expr?
          | EIn Ident Expr                      -- Ident in Expr
          | EIf Expr Expr (Maybe Expr)          -- If (Expr) Expr [else Expr]?
          | EField Expr Ident                   -- Expr.Ident
          | EContinue
          | EBreak
          | ECall Expr [Expr]                   -- Expr(Expr*)
          | EThrow Expr                         -- throw Expr
          | ETry Expr [Catch]                   -- try Expr [Catch*]
          | ENew Type [Expr]                    -- new Type(Expr*)
          | ESwitch Expr [Case] (Maybe Expr)    -- switch(Expr) { [Case*] [Def]? }
          | EVars [VarExpr]                     -- var VarExpr+
          | EFunction FuncExpr                  -- function FuncExpr
          | ECast Expr (Maybe Type)             -- cast expr or cast(expr,Type)
          | EPre (Pre [Expr])
          deriving (Show)

type Catch    = (Ident,Type,Expr)                  -- (Ident : Type) Expr
type Case     = (Expr,Expr)                        -- Expr: Expr ;
type VarExpr  = (Ident,Maybe Type,Maybe Expr)      -- Ident [:Type]? [=Expr]?
type FuncExpr = ([Param],Maybe Type,Expr)          -- (Params*) [:Type]? Expr
type Param    = (Bool,VarExpr)                     -- ?? VarExpr

data TraitInfo = Member VarExpr
               | Method Ident FuncExpr
               | Property Ident Ident Ident Type
               deriving (Show)
type CTrait = ([Access],TraitInfo)

data ClassTrait = CTrait [Access] TraitInfo
                | CPre (Pre [ClassTrait])
                deriving (Show)

data Import    = SImport Package | PreImport (Pre [Import]) deriving (Show)

data FileTrait = FImport Import
               | FTypeDef Type (Either Type (Pre Type))
               | FClass Ident (Maybe Type) [ClassTrait]
               | FPre (Pre [FileTrait])
               deriving (Show)

type File      = (Package,[FileTrait])

----------------------------------------------------------------------------------------------

join sep [] = ""
join sep (x:[]) = x
join sep xs = foldl1 (\x y -> x ++ sep ++ y) xs

-- allow things like !!!a which buildExpressionParser does not permit
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))

----------------------------------------------------------------------------------------------

pre_expr = chainl1 and_expr (do { symbol "||"; return PreOr })
    where
        and_expr = chainl1 not_expr (do { symbol "&&"; return PreAnd })
        not_expr= try (do { symbol "!"; e <- pre_expr; return $ PreNot e })
                  <|> base_expr
                  <?> "Preprocessor expression"
        base_expr = (parens pre_expr) <|> (fmap PreIdent ident) <?> "Preprocessor base expression"

-- parse instances of 'a' wrapped in pre-processor condition statement
-- eg: pre ident would parse things like #if 'condition' <<ident>>* #end
pre a = (do { reserved "#if"; cond <- pre_expr
           ; x <- a
           ; elses <- many $ do { reserved "#elseif"; cond <- pre_expr
                                ; x <- a
                                ; return (cond,x) }
           ; elsec <- optionMaybe $ (reserved "#else") >> a
           ; reserved "#end"
           ; return $ (cond,x,elses,elsec) })
        <?> "Preprocessor statement"

----------------------------------------------------------------------------------------------

constant =      do { x <- stringLiteral; return $ CString x }
       <|>      do { x <- ident;         return $ CIdent  x }
       <|> try (do { x <- float;         return $ CFloat  x })
       <|>      do { x <- integer;       return $ CInt    x }
       <?> "constant"

typep = chainl1 (do { t <- ident
                    ; params <- option "" $ fmap ((\x->"<"++x++">") . (join ",")) (angles (sepBy1 typep comma))
                    ; return (t ++ params)
                    })
                (do { sep <- (symbol ".")<|>(symbol "->"); return (\a b -> a ++ sep ++ b) })
        <?> "type"

----------------------------------------------------------------------------------------------

chainl1 p op = do { x <- p; rest x}
    where rest x = (try $ do { f <- op; y <- p; rest (f x y) }) <|> (return x)
chainr1 p op = scan
    where scan = do { x <- p; rest x }
          rest x = (try $ do { f <- op; y <- scan; return (f x y) }) <|> (return x)

-- all expressions
expr = (chainr1 tur_expr $ choice [
         binop "=" OpAssign,
         eqop "+="  OpAdd, eqop "-="  OpSub, eqop "/="   OpDiv, eqop "*=" OpMul, eqop "%=" OpMod,
         eqop "<<=" OpShl, eqop ">>=" OpShr, eqop ">>>=" OpUShr,
         eqop "&="  OpAnd, eqop "|="  OpOr,  eqop "^="   OpXor
       ])
      <?> "expression"
    where
        binop s x = do { symbol s; return $ EBinop x }
        eqop s x = do { symbol s; return $ EBinop (OpAssignOp x) }

        tur_expr = do { x <- or_expr; rest x } <?> "ternary expression"
          where rest x = (do { symbol "?"; y <- expr; colon; z <- expr; rest $ ETernary x y z })
                         <|> (return x)
        or_expr  = chainl1 and_expr $ binop "||" OpBoolOr
        and_expr = chainl1 int_expr $ binop "&&" OpBoolAnd
        int_expr = chainl1 cmp_expr $ binop "..." OpInterval
        cmp_expr = chainl1 bin_expr $ choice [
                binop "==" OpEq, binop "!=" OpNeq, binop "<=" OpLeq,
                binop ">=" OpGeq, binop "<=" OpLeq, binop "<" OpLt, binop ">" OpGt
            ]
        bin_expr = chainl1 sft_expr $ choice [binop "|" OpOr, binop "&" OpAnd, binop "^" OpXor]
        sft_expr = chainl1 add_expr $ choice [binop ">>" OpShr, binop "<<" OpShl, binop ">>>" OpUShr]
        add_expr = chainl1 mul_expr $ choice [binop "+" OpAdd, binop "-" OpSub]
        mul_expr = chainl1 mod_expr $ choice [binop "*" OpMul, binop "/" OpDiv]
        mod_expr = chainl1 una_expr $ binop "%" OpMod
        una_expr = try (do { symbol "-"; e <- una_expr; return $ EUnOp OpNeg FlagPre e })
                <|>try (do { symbol "~"; e <- una_expr; return $ EUnOp OpNegBits FlagPre e })
                <|>try (do { symbol "!"; e <- una_expr; return $ EUnOp OpNot FlagPre e })
                <|>try (do { symbol "++"; e <- una_expr; return $ EUnOp OpInc FlagPre e })
                <|>try (do { symbol "--"; e <- una_expr; return $ EUnOp OpDec FlagPre e })
                <|>try (do { e <- base_expr; symbol "++"; return $ EUnOp OpInc FlagPost e })
                <|>try (do { e <- base_expr; symbol "--"; return $ EUnOp OpDec FlagPost e })
                   <|> base_expr
                   <?> "unary expression"

{-
-- expressions for precedences up to the ?: ternary operator.
low_expr = buildExpressionParser table base_expr <?> "small expression"
    where
        table = [[unop "-" Prefix FlagPre OpNeg, unop "~" Prefix FlagPre OpNegBits, unop "!" Prefix FlagPre OpNot,
                    unop "++" Prefix FlagPre OpInc, unop "--" Prefix FlagPre OpDec,
                    unop "++" Postfix FlagPost OpInc, unop "--" Postfix FlagPost OpDec],
                 [binop "%"  OpMod],
                 [binop "*"  OpMul, binop "/"  OpDiv],
                 [binop "+"  OpAdd, binop "-"  OpSub],
                 [binop ">>" OpShr, binop "<<" OpShl, binop ">>>" OpUShr],
                 [binop "|"  OpOr , binop "&"  OpAnd, binop "^"   OpXor ],
                 [binop "==" OpEq , binop "!=" OpNeq, binop "<="  OpLeq,
                     binop ">=" OpGeq, binop "<" OpLt, binop ">" OpGt],
                 [binop "..." OpInterval],
                 [binop "&&"  OpBoolAnd ],
                 [binop "||"  OpBoolOr  ]]
        unop  s pre fix op = pre (do { operator s; return $ EUnOp op fix })
        binop s op = Infix (do { operator s; return $ EBinop op }) AssocLeft
-}

-- const, bracketed expression, array literal
factor =  (fmap EConst constant)
      <|> (parens expr)
      <|> (fmap EArray $ brackets (sepBy expr comma))
      <|> (fmap EPre $ pre (many expr))
      <?> "factor expression"

-- field access, function call, array access
factoring = do { x <- factor; rest x } <?> "factored expression"
    where rest x = (do { y <- try (fmap (EField x) $ dot >> ident)
                           <|> (fmap (ECall x) $ parens (sepBy expr comma))
                           <|> (fmap (EArrayAccess x) $ brackets expr)
                      ; rest(y)
                      })
                   <|> return x

-- expression, or #if'ed expression ; #end style
statement = try ((fmap EPre) $ pre (many statement))
            <|> (do { e <- expr; optional semi; return e })
            <?> "statement"

-- basic expressions
base_expr =  try (do { x <- ident; reserved "in"; e <- expr; return $ EIn x e })
         <|> try factoring
         <|> (fmap (EBlock) $ braces (many statement)) 
         <|> (do { reserved "while"; cond <- parens expr; e <- expr; return $ EWhile True cond e })
         <|> (do { reserved "do"; e <- expr; reserved "while"; cond <- parens expr; return $ EWhile False cond e })
         <|> ((reserved "continue")>>(return EContinue))
         <|> ((reserved "break"   )>>(return EBreak   ))
         <|> (do { reserved "for"; it <- parens expr; e <- expr; return $ EFor it e })
         <|> (fmap (EReturn) $ (reserved "return") >> (optionMaybe expr))
         <|> (do { reserved "if"; cond <- parens expr; e <- expr; optional semi;
                 ; elsee <- optionMaybe $ (reserved "else") >> expr
                 ; return $ EIf cond e elsee })
         <|> (fmap (EThrow) $ (reserved "throw") >> expr)
         <|> (do { reserved "try"; test <- expr;
                 ; catches <- many $ do { reserved "catch"
                                        ; symbol "("; name <- ident; colon; typee <- typep; symbol ")"
                                        ; e <- expr
                                        ; return $ (name,typee,e)
                                        }
                 ; return $ ETry test catches })
         <|> (do { reserved "new"; t <- typep; args <- parens $ sepBy expr comma; return $ ENew t args })
         <|> (fmap (EFunction) $ (reserved "function") >> func_expr)
         <|> (do { reserved "switch"; test <- expr; symbol "{"
                 ; cases0 <- many $ do { reserved "case"; m <- expr; colon; e <- expr; semi; return $ (m,e) }
                 ; def <- optionMaybe $ do { reserved "default"; colon; e <- expr; semi; return e }
                 ; cases1 <- many $ do { reserved "case"; m <- expr; colon; e <- expr; semi; return $ (m,e) }
                 ; symbol "}"
                 ; return $ ESwitch test (cases0++cases1) def })
         <|> (fmap (EVars) $ (reserved "var") >> (many1 var_expr))
         <|> ((reserved "cast") >> (try (do { symbol "("; e <- expr; comma; t <- typep; symbol ")"; return $ ECast e (Just t) })
                                    <|> (do { e <- expr; return $ ECast e Nothing })))
         <?> "base expression"

-- variable expression
var_expr = do { name <- ident
              ; typee <- optionMaybe $ colon >> typep
              ; value <- optionMaybe $ (symbol "=") >> expr
              ; return $ (name,typee,value)
              }

-- function expression
func_expr = do { params <- parens $ sepBy param comma;
               ; typee <- optionMaybe $ colon >> typep
               ; e <- expr
               ; return $ (params,typee,e)
               }
    where param = do { opt <- option False $ (symbol "?")>>(return True)
                     ; vare <- var_expr
                     ; return $ (opt,vare)
                     }

----------------------------------------------------------------------------------------------

accessor = (foldl1 (<|>) $ map (\(x,y) -> (reserved x) >> (return y)) $ [
                ("public",APublic), ("private",APrivate), ("inline",AInline),
                ("override",AOverride), ("static",AStatic)
           ])
           <|> (fmap APre $ (pre $ many accessor))
           <?> "access modifier"

typedef = (do { reserved "typedef"
              ;  a <- typep; symbol "="; t <- (fmap Left typep) <|> (fmap Right $ pre typep)
              ;  semi
              ; return $ FTypeDef a t })
         <?> "typedef"

package = (do { reserved "package";
              ; name <- (fmap (join ".")) (sepBy ident dot);
              ; semi
              ; return name })
          <?> "package declaration"

importp = (do { reserved "import";
              ; name <- (fmap (join ".")) (sepBy ident dot);
              ; semi
              ; return $ SImport name })
          <|> (fmap PreImport $ (pre $ many importp))
          <?> "import declaration"

ctrait = (do { as <- many accessor
             ; trait <- try (do { reserved "var"; v <- var_expr; semi; return $ Member v })
                        <|> (do { reserved "function"; name <- ident; f <-func_expr; return $ Method name f })
                        <|> (do { reserved "var"; name <-ident
                                ; symbol "("; get <- ident; comma; set <- ident; symbol ")"
                                ; colon ; t <- typep
                                ; return $ Property name get set t })
             ; return $ CTrait as trait })
         <?> "class trait"

class_trait = (try ctrait) <|> (fmap CPre $ pre (many class_trait))

classp = (do { reserved "class";
             ; name <- ident
             ; super <- optionMaybe $ (reserved "extends") >> typep
             ; symbol "{"
             ; traits <- many class_trait
             ; symbol "}"
             ; return $ FClass name super traits })
          <?> "class declaration"

file_trait = (foldl1 (<|>) [fmap FImport importp, typedef, classp, fmap FPre $ pre (many file_trait)])
            <?> "file trait"

file = (do { p <- option "" package
           ; traits <- many file_trait
           ; return (p,traits)
           })
      <?> "file"

----------------------------------------------------------------------------------------------

mainParser = do { whiteSpace; res <- file; eof; return res }

parseFile fname = do { input <- readFile fname
                     ; return (runParser mainParser () fname input)
                     }
