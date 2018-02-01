module MiniLogo where
import Prelude hiding (Num)

-- Task 1
type Num   = Int
type Var   = String
type Macro = String
type Prog  = [Cmd]

data Mode = Down | Up
  deriving (Eq)

data Expr = VarEx Var
          | NumEx Num
          | Add Expr Expr
  deriving (Eq)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq)

-- Task 2
-- define line (x1,y1,x2,y2) {
--   pen up;
--   move (x1,y1);
--   pen down;
--   move (x2,y2);
-- }

line = Define
       "line"
       ["x1","y1","x2","y2"]
       [
           (Pen Up),
           (Move (VarEx "x1") (VarEx "y1")),
           (Pen Down),
           (Move (VarEx "x2") (VarEx "y2"))
       ]

-- Task 3
-- define nix (x,y,w,h) {
--   call line (x,y,x+w,y+h);
--   call line (x,y+h,x+w,y);
-- }

nix = Define 
      "nix"
      ["x","y","w","h"]
      [
          (Call "line"
              [
                  (VarEx "x"),
                  (VarEx "y"),
                  (Add (VarEx "x") (VarEx "w")),
                  (Add (VarEx "y") (VarEx "h"))
              ]
          ),
          (Call "line"
              [
                  (VarEx "x"),
                  (Add (VarEx "y") (VarEx "h")),
                  (Add (VarEx "x") (VarEx "w")),
                  (VarEx "y")
              ]
          )
      ]

-- Task 4
steps :: Int -> Prog
steps 0 = []
steps n = [
              (Call "line"
                  [
                      (NumEx n),
                      (NumEx n),
                      (NumEx (n-1)),
                      (NumEx n)
                  ]
              ),
              (Call "line"
                  [
                      (NumEx (n-1)),
                      (NumEx n),
                      (NumEx (n-1)),
                      (NumEx (n-1))
                  ]
              )
          ]
          ++ (steps (n-1))

-- Task 5
macros :: Prog -> [Macro]
macros []                         = []
macros ((Define name _ _) : tail) = name : (macros tail)
macros (_ : tail)                 = macros tail

-- Task 6
instance Show Mode where
  show (Down) = "down"
  show (Up)   = "up"

instance Show Expr where
  show (VarEx v) = v
  show (NumEx n) = show n
  show (Add a b) = show a ++ "+" ++ show b

showVarList :: [Var] -> String
showVarList []            = []
showVarList (head : [])   = head
showVarList (head : tail) = head ++ "," ++ showVarList tail

showExprList :: [Expr] -> String
showExprList []            = []
showExprList (head : [])   = show head
showExprList (head : tail) = show head ++ "," ++ showExprList tail

showCmdHelper :: Prog -> String
showCmdHelper [] = []
showCmdHelper (head : tail) = "  " ++ show head ++ showCmdHelper tail
  
instance Show Cmd where
  show (Pen Up)       = "pen up;\n"
  show (Pen Down)     = "pen down;\n"
  show (Move a b)     = "move (" ++ show a ++ "," ++ show b ++ ");\n"
  show (Define a b c) = "define " ++ a ++ " (" ++ showVarList b ++ ") {\n"  ++ showCmdHelper c ++ "}\n"
  show (Call a b)     = "call " ++ a ++ " (" ++ showExprList b ++ ");\n"

pretty :: Prog -> String
pretty []            = []
pretty (head : tail) = show head ++ pretty tail
