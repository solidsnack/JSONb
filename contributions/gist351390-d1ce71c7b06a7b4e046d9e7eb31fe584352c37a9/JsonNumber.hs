module JsonNumber
where

import Data.Attoparsec
import Control.Applicative ( (<|>), (<$>), (*>), (<*), pure )
import Data.Word
import Data.Ratio ( (%) )
import Data.ByteString.Nums.Careless.Int (positive)
import Data.List (foldl')


number :: Parser Rational
number = negativeNumber <|> positiveNumber
    where
      negativeNumber = negate <$> (minus *> positiveNumber)

      positiveNumber = do
        i <- integral
        f <- option 0 (try fractional)
        e <- option 1 (try scientific)
        return $ (i + f) * e

      integral = ((zero *> pure 0) <|> positiveIntegral)

      positiveIntegral = do
        first <- positive 0 <$> positiveDigit
        foldl' positive first <$> many digit

      fractional = do
        period
        words <- many1 digit
        return ((foldl' positive 0 words) % (10 ^ length words))

      scientific = do
        choice [word8 69, word8 101] -- e or E
        choice [negativeSci, positiveSci]

      positiveSci = ((10 ^) . foldl' positive 0) <$> choice [plus *> many1 digit, many1 digit]

      negativeSci = minus *> (((1 %) . (10 ^) . foldl' positive 0) <$> many1 digit)

      digit :: Parser Word8
      digit = satisfy isDigit
          where isDigit w = w >= 48 && w <= 57

      positiveDigit :: Parser Word8
      positiveDigit = satisfy (\w -> w >= 49 && w <= 57) <?> "positiveDigit"

      plus = word8 43
      minus = word8 45
      zero = word8 48
      period = word8 46
