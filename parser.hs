module Parser
    (
        -- * AST data types
		Pre,
		PreExpr(PreIdent,PreAnd,PreOr,PreNot),
		Type(BasicType,ParamType,PreType,FuncType),
		Ident,
		Package,
		Constant(CInt,CFloat,CString,CIdent),
		Binop(OpAdd,OpMul,OpDiv,OpSub,OpMod,OpAssign,OpEq,OpNeq,OpGt,OpLt,OpGeq,OpLeq,OpOr,OpAnd,OpXor,OpBoolAnd,OpBoolOr,OpShl,OpShr,OpUShr,OpAssignOp,OpInterval),
		Unop(OpInc,OpDec,OpNot,OpNeg,OpNegBits),
		UnopFlag(FlagPre,FlagPost),
		Expr(EConst,EArray,EArrayAccess,EBlock,EUnop,EBinop,ETernary,EWhile,EFor,EReturn,EIn,EIf,EField,EContinue,EBreak,ECall,EThrow,ETry,ENew,ESwitch,EVars,EFunction,ECast,EPre1,EPreN,EAnon,EUntyped,EUnchecked,EStdFor),
		Catch,
		Case,
		VarExpr,
		FuncExpr,
		Param,
		TraitInfo(Member,Method,Property),
		CTrait,
		ClassTrait(CTrait,CPre),
		Access(APublic,APrivate,AStatic,AOverride,AInline,APre),
		Import,
		FileTrait(FImport,FTypeDef,FClass,FPre),
		File,

        -- * Functions
        parseFile -- :: String -> Either ParseError File

    ) where

import System.Environment
import Text.Parsec
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
             "!","~","++","--","?",":","...","->"]

opletter = "!=<>?~-+*/^|&:%"

langDef :: LanguageDef st
langDef = emptyDef {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.opStart = oneOf opletter,
    P.opLetter = oneOf opletter,
    P.reservedNames = ["if","else","try","catch","throw","for","while","in","new",
                     "public","private","inline","override","static","do","cast",
                     "switch","case","var","function","class","extends","return",
                     "#if","#elseif","#else","#end","untyped","default"],
    P.reservedOpNames = operators,
    P.caseSensitive = True
}

lexer = P.makeTokenParser langDef

parens p      = lexeme $ do { char '('; whiteSpace; res <- lexeme p; char ')'; return res; }
brackets p    = lexeme $ do { char '['; whiteSpace; res <- lexeme p; char ']'; return res; }
angles p      = lexeme $ do { char '<'; whiteSpace; res <- lexeme p; char '>'; return res; }
braces p      = lexeme $ do { char '{'; whiteSpace; res <- lexeme p; char '}'; return res; }

integer = liftM fromIntegral (lexeme (P.integer lexer) <|> P.hexadecimal lexer)

ident  = lexeme $ P.identifier lexer
float  = lexeme $ P.float      lexer
semi   = lexeme $ P.semi       lexer
comma  = lexeme $ P.comma      lexer
dot    = lexeme $ P.dot        lexer
colon  = lexeme $ P.colon      lexer

reserved = lexeme . P.reserved lexer
symbol   = lexeme . P.symbol   lexer

lexeme p      = do { res <- p; whiteSpace; return res }

-- not as nice as the inbuilt one with escape chars. but need to allow newlines in strings!
-- aswell as strings formed with '' instead of ""
--stringLiteral = P.stringLiteral lexer
stringLiteral
	= lexeme $ do { strch <- oneOf "'\""
				  ; dat <- many $ (do { x <- noneOf (strch : "\\"); return [x] })
                              <|> (do { char '\\'; x <- noneOf ""; return ['\\',x] })
				  ; char strch
			      ; return $ (strch : concat dat) ++ [strch]
				  }

-- match a reserved operator s, and only if matched string does not make a prefix of a longer operator
--operator s = lexeme $ (P.reservedOp lexer) s
operator s
    = lexeme $ try $ do { string s
                  ; notFollowedBy prefixer }
    where prefixer
            = do { rest <- many1 $ oneOf opletter
                 ; unless (any (`elem` operators) $ fixes s rest) parserZero
--                 ; if any (flip elem operators) $ fixes s rest then return () else parserZero
                 }
          fixes xs [] = [xs]
          fixes xs (y:ys) = let xsy = xs++[y] in xsy : fixes xsy ys

-- ignore whitespace and metadata for lexemes
whiteSpace    = let ws = P.whiteSpace lexer in ws>>many(metadata>>ws)
metadata = (do { char '@'; optional (char ':')
               ; many1 (alphaNum <|> char '_')
               ; optional(do { char '('
                             ; many (noneOf ")")
                             ; char ')' })
               ; return () }) 
           <?> ""
 
----------------------------------------------------------------------------------------------

-- #if expr a [#elseif expr a] [#else a] #end
type Pre a = (PreExpr, a, [(PreExpr, a)], Maybe a)

data PreExpr = PreIdent Ident
             | PreAnd PreExpr PreExpr
             | PreOr PreExpr PreExpr
             | PreNot PreExpr
             deriving(Show)

----------------------------------------------------------------------------------------------

data Type = BasicType String      -- [Package.]?Ident
          | ParamType Type [Type] -- Type<Types...>
          | PreType (Pre Type)    -- #if Type #else ... #end
          | FuncType Type Type    -- Type -> Type
          deriving (Show)

instance Eq Type where
    (BasicType x) == (BasicType y) = x == y
    (ParamType t ts) == (ParamType s ss) = t == s && all (uncurry (==)) (zip ts ss) && length ts == length ss 
    (FuncType a b) == (FuncType x y) = a == x && b == y
    x == y = False

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
data Unop = OpInc | OpDec | OpNot | OpNeg | OpNegBits deriving (Show)
data UnopFlag = FlagPre | FlagPost deriving (Show)

data Access = APublic | APrivate | AStatic | AOverride | AInline
            | APre (Pre [Access])
            deriving (Show)

data Expr = EConst Constant                     -- literal/ident
          | EArray [Expr]                       -- [Expr*]
          | EArrayAccess Expr Expr              -- Expr[Expr]
          | EBlock [Expr]                       -- {(Expr;?)*}
          | EUnop Unop UnopFlag Expr            
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
          | ESwitch Expr [Case] (Maybe [Expr])    -- switch(Expr) { [Case*] [Def]? }
          | EVars [VarExpr]                     -- var VarExpr+
          | EFunction FuncExpr                  -- function FuncExpr
          | ECast Expr (Maybe Type)             -- cast expr or cast(expr,Type)
          | EPre1 (Pre Expr)   -- expression
          | EPreN (Pre [Expr]) -- statement
          | EAnon [(Ident,Expr)]                -- { name : value, name : value ... }
          | EUntyped Expr                       -- untyped expr

-- additional AST for c#/js
		  | EUnchecked Expr -- c# to wrap functions and avoid overflow checks
		  | EStdFor Expr Expr Expr Expr -- c#/js standard for loop
          deriving (Show)

type Catch    = (Ident,Type,Expr)                  -- (Ident : Type) Expr
type Case     = (Expr,[Expr])  	                   -- Expr: [Expr ;]
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

type Import = String

data FileTrait = FImport Import
               | FTypeDef Type Type 
               | FClass Type (Maybe Type) [ClassTrait]
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

preExpr = buildExpressionParser table baseExpr <?> "preprocessor expression"
    where 
        table = [[prefix $ operator "!" >> return PreNot],
                 [Infix (operator "&&" >> return PreAnd) AssocLeft],
                 [Infix (operator "||" >> return PreOr) AssocLeft]]
        baseExpr = parens preExpr <|> liftM PreIdent ident <?> "Preprocessor base expression"

-- parse instances of 'a' wrapped in pre-processor condition statement
-- eg: pre ident would parse things like #if 'condition' <<ident>>* #end
pre a = (do { reserved "#if"; cond <- preExpr
           ; x <- a
           ; elses <- many $ do { reserved "#elseif"; cond <- preExpr
                                ; x <- a
                                ; return (cond,x) }
           ; elsec <- optionMaybe $ reserved "#else" >> a
           ; reserved "#end"
           ; return (cond,x,elses,elsec) })
        <?> "Preprocessor statement"

----------------------------------------------------------------------------------------------

constant =      do { x <- stringLiteral; return $ CString x }
       <|>      do { x <- ident;         return $ CIdent  x }
       <|> try (do { x <- float;         return $ CFloat  x })
       <|>      do { x <- integer;       return $ CInt    x }
       <?> "constant"

typep = chainr1 baseType (do { operator "->"; return FuncType })
    where
        factorType = liftM (BasicType . join ".") (sepBy1 ident dot)
                   <|> parens typep
                   <|> liftM PreType (pre typep)
                   <?> "factor type"
        baseType = factorType >>= rest <?> "factored type"
            where rest x = (do { y <- liftM (ParamType x) $ angles (sepBy1 typep comma)
                               ; rest y })
                        <|>  return x

----------------------------------------------------------------------------------------------

-- all expressions
expr = chainr1 turExpr (choice [
         do { operator "="; return $ EBinop OpAssign },
         op "+="  OpAdd, op "-="  OpSub, op "/="   OpDiv, op "*=" OpMul, op "%=" OpMod,
         op "<<=" OpShl, op ">>=" OpShr, op ">>>=" OpUShr,
         op "&="  OpAnd, op "|="  OpOr,  op "^="   OpXor
       ])
      <?> "expression"
    where op s x = do { operator s; return $ EBinop (OpAssignOp x) }

-- expressions up to and including ?: ternary operator
turExpr = lowExpr >>= rest <?> "ternary expression"
    where rest x = (do { symbol "?"; y <- turExpr; colon; z <- turExpr; rest $ ETernary x y z })
                   <|> return x

-- expressions for precedences up to the ?: ternary operator.
lowExpr = buildExpressionParser table baseExpr <?> "small expression"
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
        unop  s pre fix op = pre (do { operator s; return $ EUnop op fix })
        binop s op = Infix (do { operator s; return $ EBinop op }) AssocLeft

-- const, bracketed expression, array literal
factor =  liftM EConst constant
      <|> parens expr
      <|> liftM EArray (brackets (sepBy expr comma))
      <|> liftM EPre1 (pre expr)
      <|> try (do { reserved "cast"; symbol "("; e <- expr; comma; t <- typep; symbol ")"; return $ ECast e (Just t) })
      <|> (do { reserved "new"; t <- typep; args <- parens $ sepBy expr comma; return $ ENew t args })
      <|> try (liftM EAnon $ (braces . flip sepBy1 comma) $ do { name <- ident; colon; e <- expr; return (name,e) })
      <?> "factor expression"

-- field access, function call, array access
factoring = factor >>= rest <?> "factored expression"
    where rest x = (do { y <- try (liftM (EField x) $ dot >> ident)
                           <|> liftM (ECall x) (parens (sepBy expr comma))
                           <|> liftM (EArrayAccess x) (brackets expr)
                      ; rest y })
                   <|> return x

-- expression, or #if'ed expression ; #end style
--statement = try ((liftM EPre) $ pre (many statement))
statement = try (do { es <- pre (many statement); optional semi; return $ EPreN es })
            <|> (do { e <- expr; optional semi; return e })
            <?> "statement"

-- basic expressions
baseExpr =  try (do { x <- ident; reserved "in"; e <- expr; return $ EIn x e })
         <|> try factoring
         <|> liftM EBlock (braces (many statement))
         <|> (do { reserved "while"; cond <- parens expr; e <- expr; return $ EWhile True cond e })
         <|> (do { reserved "do"; e <- expr; reserved "while"; cond <- parens expr; return $ EWhile False cond e })
         <|> (reserved "continue" >> return EContinue)
         <|> (reserved "break"    >> return EBreak   )
         <|> (do { reserved "for"; it <- parens expr; e <- expr; return $ EFor it e })
         <|> liftM EReturn (reserved "return" >> optionMaybe expr)
         <|> (do { reserved "if"; cond <- parens expr; e <- expr;
				 ; elsee <- try (do { optional semi; reserved "else"; e <- expr; return $ Just e }) <|> return Nothing
                 ; return $ EIf cond e elsee })
         <|> liftM EThrow (reserved "throw" >> expr)
         <|> (do { reserved "try"; test <- expr;
                 ; catches <- many $ do { reserved "catch"
                                        ; symbol "("; name <- ident; colon; typee <- typep; symbol ")"
                                        ; e <- expr
                                        ; return (name,typee,e)
                                        }
                 ; return $ ETry test catches })
         <|> liftM EFunction (reserved "function" >> funcExpr)
         <|> (do { reserved "switch"; test <- parens expr; symbol "{"
                 ; cases0 <- many $ do { reserved "case"; m <- expr; colon; e <- many statement; return (m,e) }
                 ; def <- optionMaybe $ do { reserved "default";     colon; many statement }
                 ; cases1 <- many $ do { reserved "case"; m <- expr; colon; e <- many statement; return (m,e) }
                 ; symbol "}"
                 ; return $ ESwitch test (cases0++cases1) def })
         <|> liftM EVars (reserved "var" >> sepBy1 varExpr comma)
         <|> (do { reserved "cast"; e <- expr; return $ ECast e Nothing })
		 <|> (do { reserved "untyped"; e <- expr; return $ EUntyped e })
         <?> "base expression"

-- variable expression
varExpr = do { name <- ident
              ; typee <- optionMaybe $ colon >> typep
              ; value <- optionMaybe $ symbol "=" >> expr
              ; return (name,typee,value)
              }

-- function expression
funcExpr = do { params <- parens (sepBy param comma);
               ; typee <- optionMaybe $ colon >> typep
               ; e <- expr
               ; return (params,typee,e)
               }
    where param = do { opt <- option False $ symbol "?">>return True
                     ; vare <- varExpr
                     ; return (opt,vare)
                     }

----------------------------------------------------------------------------------------------

accessor = choice (map (\(x,y) -> reserved x >> return y) [
                ("public",APublic), ("private",APrivate), ("inline",AInline),
                ("override",AOverride), ("static",AStatic)
           ])
           <|> liftM APre (pre (many accessor))
           <?> "access modifier"

typedef = (do { reserved "typedef"
              ;  a <- typep; symbol "="; t <- typep
              ;  semi
              ; return $ FTypeDef a t })
         <?> "typedef"

package = (do { reserved "package";
              ; name <- liftM (join ".") (sepBy ident dot);
              ; semi
              ; return name })
          <?> "package declaration"

importp = (do { reserved "import";
              ; name <- liftM (join ".") (sepBy ident dot);
              ; semi
              ; return name })
          <?> "import declaration"

ctrait = (do { as <- many accessor
             ; trait <- try (do { reserved "var"; v <- varExpr; semi; return $ Member v })
                        <|> (do { reserved "function"; name <- fname; f <-funcExpr; return $ Method name f })
                        <|> (do { reserved "var"; name <-ident
                                ; symbol "("; get <- ident; comma; set <- ident; symbol ")"
                                ; colon ; t <- typep; semi
                                ; return $ Property name get set t })
             ; return $ CTrait as trait })
         <?> "class trait"
    where fname = try ident <|> (reserved "new" >> return "new")
 

classTrait = try ctrait <|> liftM CPre (pre (many classTrait))

classp = (do { reserved "class";
             ; name <- typep
             ; super <- optionMaybe $ reserved "extends" >> typep
             ; symbol "{"
             ; traits <- many classTrait
             ; symbol "}"
             ; return $ FClass name super traits })
          <?> "class declaration"

fileTrait =  classp
          <|> typedef
          <|> try (liftM FImport importp)
          <|> liftM FPre (pre (many fileTrait))
          <?> "file trait"

file = (do { p <- option "" package
           ; traits <- many fileTrait
           ; return (p,traits)
           })
      <?> "file"

----------------------------------------------------------------------------------------------

mainParser = do { whiteSpace; res <- file; eof; return res }

parseFile fname = do { input <- readFile fname
                     ; return (runParser mainParser () fname input)
                     }
