{-# LANGUAGE QuasiQuotes #-}

import System.Process
import Text.Nowdoc

main :: IO ()
main = () <$ rawSystem "perl" ["-e", [nowdoc|
use strict;
use warnings;

my $hello = "Hello, world!\n";
print $hello;
|]]
