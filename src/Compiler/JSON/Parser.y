{
module Compiler.JSON.Parser

import Compiler.JSON.Token
import qualified Compiler.JSON.AbSyn as A
}

%name parser
%tokentype { Token }
%error { parseError }