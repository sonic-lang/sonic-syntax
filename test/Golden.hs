module Golden
  ( test_golden
  )
where

import           System.Directory               ( listDirectory )
import           System.FilePath.Posix          ( isExtensionOf )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Text.Pretty.Simple             ( pShowNoColor )

import           Test.Tasty
import           Test.Tasty.Golden              ( goldenVsStringDiff )

import           Language.Sonic.Parser          ( parseModuleWithComment )

goldenDir :: FilePath
goldenDir = "test/golden/"

test_golden :: IO [TestTree]
test_golden = do
  files <- listDirectory goldenDir
  let sources = filter (isExtensionOf ".sonic") files
  pure $ map makeGoldenTest sources

makeGoldenTest :: FilePath -> TestTree
makeGoldenTest file = goldenVsStringDiff file diff goldenPath $ do
  source <- readFile path
  let result = pShowNoColor $ parseModuleWithComment source
  pure $ encodeUtf8 result
 where
  diff ref new = ["diff", "-y", ref, new]
  goldenPath = path ++ ".golden"
  path       = goldenDir ++ file
