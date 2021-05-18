module RealWorldHaskell.CH03.BookStore () where

-- Book ISBN 书名 作者
data BookInfo = Book Int String [String] deriving (Show)

data MagzineInfo = Magzine Int String [String] deriving (Show)

type CustomerID = Int

type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

--
data Customer = Customer
  { customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
  }
  deriving (Show)