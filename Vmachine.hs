import           Control.Monad
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString.Lazy (ByteString, length, readFile)
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V
import           Data.Word
import           Foreign.Storable


data Operation =
    Halt                      | --  0        = stop execution and terminate the program
    Set  Word16 Word16        | --  1 a b    = set register <a> to the value of <b>
    Push Word16               | --  2 a      = push <a> onto the stack
    Pop  Word16               | --  3 a      = remove the top element from the stack and write it into <a>; empty stack = error
    Eq   Word16 Word16 Word16 | --  4 a b c  = set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
    Gt   Word16 Word16 Word16 | --  5 a b c  = set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
    Jmp  Word16               | --  6 a      = jump to <a>
    Jt   Word16 Word16        | --  7 a b    = if <a> is nonzero, jump to <b>
    Jf   Word16 Word16        | --  8 a b    = if <a> is zero, jump to <b>
    Add  Word16 Word16 Word16 | --  9 a b c  = assign into <a> the sum of <b> and <c> (modulo 32768)
    Mult Word16 Word16 Word16 | --  10 a b c = store into <a> the product of <b> and <c> (modulo 32768)
    Mod  Word16 Word16 Word16 | --  11 a b c = store into <a> the remainder of <b> divided by <c>
    And  Word16 Word16 Word16 | --  12 a b c = stores into <a> the bitwise and of <b> and <c>
    Or   Word16 Word16 Word16 | --  13 a b c = stores into <a> the bitwise or of <b> and <c>
    Not  Word16 Word16        | --  14 a b   = stores 15-bit bitwise inverse of <b> in <a>
    Rmem Word16 Word16        | --  15 a b   = read memory at address <b> and write it to <a>
    Wmem Word16 Word16        | --  16 a b   = write the value from <b> into memory at address <a>
    Call Word16               | --  17 a     = write the address of the next instruction to the stack and jump to <a>
    Ret                       | --  18       = remove the top element from the stack and jump to it; empty stack = halt
    Out  Word16               | --  19 a     = write the character represented by ascii code <a> to the terminal
    In   Word16               | --  20 a     = read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
    Noop                       --  21      = no operation
     deriving (Show, Eq)

-- ------------------------------------------------
parseOperation :: [Word16] -> (Operation, Int)
parseOperation ( 0:xs      ) = (Halt      , 0)
parseOperation ( 1:a:b:xs  ) = (Set    a b, 2)
parseOperation ( 2:a:xs    ) = (Push     a, 1)
parseOperation ( 3:a:xs    ) = (Pop      a, 1)
parseOperation ( 4:a:b:c:xs) = (Eq   a b c, 3)
parseOperation ( 5:a:b:c:xs) = (Gt   a b c, 3)
parseOperation ( 6:a:xs    ) = (Jmp      a, 1)
parseOperation ( 7:a:b:xs  ) = (Jt     a b, 2)
parseOperation ( 8:a:b:xs  ) = (Jf     a b, 2)
parseOperation ( 9:a:b:c:xs) = (Add  a b c, 3)
parseOperation (10:a:b:c:xs) = (Mult a b c, 3)
parseOperation (11:a:b:c:xs) = (Mod  a b c, 3)
parseOperation (12:a:b:c:xs) = (And  a b c, 3)
parseOperation (13:a:b:c:xs) = (Or   a b c, 3)
parseOperation (14:a:b:xs  ) = (Not    a b, 2)
parseOperation (15:a:b:xs  ) = (Rmem   a b, 2)
parseOperation (16:a:b:xs  ) = (Wmem   a b, 2)
parseOperation (17:a:xs    ) = (Call     a, 1)
parseOperation (18:xs      ) = (Ret       , 0)
parseOperation (19:a:xs    ) = (Out      a, 1)
parseOperation (20:a:xs    ) = (In       a, 1)
parseOperation (21:xs      ) = (Noop      , 0)

-- - numbers 0..32767 mean a literal value
-- - numbers 32768..32775 instead mean registers 0..7
-- - numbers 32776..65535 are invalid
-- ------------------------------------------------

type Memory = M.Map Word16 Word16
data CPU = CPU {
            mem   :: Memory,
            pc    :: Int,
        currentOp :: Operation,
            stack :: [Word16] } deriving Show

maxAddress :: Word16
maxAddress = 32767
-- ------------------------------------------------
rawContent :: ByteString -> [Word16]
rawContent contents = runGet (Prelude.map fromIntegral `fmap` replicateM count getInt16le) contents where
    count = fromIntegral (BSL.length contents) `div` sizeOf (1 :: Word16)


readWord :: Memory -> Word16 -> Word16
readWord mem ix
    | ix <= maxAddress = ix     -- literal
    | otherwise =  (M.!) mem ix -- register

writeWord ::  Word16 -> Word16 -> Memory -> Memory
writeWord = M.insert

printChar :: Word16 -> IO ()
printChar n = putChar (toEnum' n') where
    n' = fromIntegral n::Int
    toEnum' n = toEnum n :: Char

-- 32768..32775 - regs
initMem :: [Word16] -> Memory
initMem ints = M.union m1 m2 where
   m1 = M.fromList . zip [0..] $ ints
   m2 = M.fromList . zip [32768..32775] $ repeat 0

toInt :: Word16 -> Int
toInt w = fromIntegral  w :: Int

execute :: CPU -> IO CPU
execute cpu@(CPU _ _ Halt _)    = return cpu
execute cpu@(CPU mem pc op stk) = executeNextOp cpu >>= execute

executeNextOp cpu@(CPU mem pc op stk) =
    case op'  of
        Halt  -> return $ CPU mem (pc + pc' + 1) Halt stk

        Noop  -> return $ CPU mem (pc + pc' + 1) Noop stk
        -- set register <a> to the value of <b>
        Set a b ->  return $ CPU mem' (pc + pc' + 1) op' stk where
                        mem' = writeWord a (readWord mem b) mem

        -- push <a> onto the stack
        Push a  ->  return $ CPU mem (pc + pc' + 1) op' stk' where
                        stk' = readWord mem a : stk

        -- remove the top element from the stack and write it into <a>; empty stack = error
        Pop a   -> return $ CPU mem' (pc + pc' + 1) op' stk' where
                        (val:stk')  = stk
                        mem' = writeWord a val mem

        -- set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        Eq a b c -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = if readWord mem b == readWord mem c
                                then 1
                                else 0
                        mem' = writeWord a val mem


        -- set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        Gt a b c -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = if readWord mem b > readWord mem c
                            then 1
                            else 0
                        mem' = writeWord a val mem

        -- jump to <a>
        Jmp a    -> return $ CPU mem jmp op' stk where -- maybe use Word16 as program counter
                        jmp = fromIntegral (readWord mem a)::Int


        -- if <a> is nonzero, jump to <b>
        Jt a b   ->  return $ CPU mem jmp op' stk where
                        jmp = if toInt (readWord mem a) /= 0
                                    then toInt (readWord mem b)
                                    else pc + pc' + 1


        -- if <a> is zero, jump to <b>
        Jf a b   -> return $ CPU mem jmp op' stk where
                        jmp = if toInt (readWord mem a) == 0
                            then toInt (readWord mem b)
                            else pc + pc' + 1


        -- assign into <a> the sum of <b> and <c> (modulo 32768)
        Add a b c -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (readWord mem b + readWord mem c) `mod` (32768::Word16)
                        mem' = writeWord a val mem


        -- store into <a> the product of <b> and <c> (modulo 32768)
        Mult a b c -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (readWord mem b * readWord mem c) `mod` (32768::Word16)
                        mem' = writeWord a val mem


        -- a b c = store into <a> the remainder of <b> divided by <c>
        Mod a b c  -> return $ CPU mem' (pc + pc' + 1) op' stk where
                         val = readWord mem b `rem` readWord mem c
                         mem' = writeWord a val mem

        -- a b c = stores into <a> the bitwise and of <b> and <c>
        And a b c  -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (.&.) (readWord mem b)  (readWord mem c)
                        mem' = writeWord a val mem


        -- stores into <a> the bitwise or of <b> and <c>
        Or a b c  -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (.|.) (readWord mem b)  (readWord mem c)
                        mem' = writeWord a val mem

        -- stores 15-bit bitwise inverse of <b> in <a>
        Not a b   -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = complement (readWord mem b) `xor` (32768 :: Word16)
                        mem' = writeWord a val mem

        -- read memory at address <b> and write it to <a>
        Rmem a b  ->  return $ CPU mem' (pc + pc' + 1) op' stk where
                        isReg = maxAddress < b
                        ix = if isReg then readWord mem b else b
                        val = (M.!) mem ix
                        mem' = writeWord a val mem

        -- write the value from <b> into memory at address <a>
        Wmem a b  -> return $ CPU mem' (pc + pc' + 1) op' stk where
                        isReg = maxAddress < a
                        a' = if isReg then readWord mem a  else a
                        b' = readWord mem b
                        mem' = writeWord a' b' mem

        -- write the address of the next instruction to the stack and jump to <a>
        Call a    -> return $ CPU mem (toInt newPc) op' stk' where
                            stk' = (fromIntegral (pc + pc' + 1)::Word16):stk
                            newPc = readWord mem a

        -- remove the top element from the stack and jump to it; empty stack = halt
        Ret         ->  if stk == []
                            then
                                return $ CPU mem 0 Halt stk
                            else do
                                let (val:stk') = stk
                                return $ CPU mem (toInt val) op' stk'

        -- write the character represented by ascii code <a> to the terminal
        Out a       -> printChar (readWord mem a) >> return (CPU mem (pc + pc' + 1) op' stk)


        -- read a character from the terminal and write its ascii code to <a>;
        -- it can be assumed that once input starts,
        -- it will continue until a newline is encountered;
        -- this means that you can safely read whole lines from the
        -- keyboard and trust that they will be fully read
        In a        -> do
                        c <- getChar
                        let mem' = writeWord a (fromIntegral (fromEnum c)::Word16) mem
                        return $ CPU mem' (pc + pc' + 1) op' stk
        where
            (op', pc') = nextOp pc mem
            nextOp :: Int -> Memory -> (Operation, Int)
            nextOp ix = parseOperation . map snd . drop ix . M.toList

main = do
    contents <- BSL.readFile "challenge.bin"
    let mem  = initMem . rawContent $ contents
    let cpu = CPU mem 0 Noop []
    execute cpu
    print "Done."
