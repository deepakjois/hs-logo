{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import Control.Arrow (arr, (>>>), (>>^), (&&&))
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Monoid (mempty)
import System.FilePath (takeFileName, takeDirectory, takeBaseName, replaceBaseName, replaceExtension, (</>))
import qualified Control.Category as C
import Hakyll

main :: IO ()
main = hakyllWith config $ do

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- copy static assets like CSS, JS and images (including logo images)
  match "static/**" $ do
    route idRoute
    compile copyFileCompiler

  -- Read the logo example source files
  match "examples/sources/*.logo" $ compile (readPageCompiler >>> addLogoExampleFields)

  -- Read the logo example image files
  match "examples/svgs/*.svg" $ do
    route idRoute
    compile copyFileCompiler

  -- Render index page with select examples
  match "intro.markdown" $ compile pageCompiler

  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> setFieldPage "intro" "intro.markdown"
    >>> setFieldPageList sortByIndex "templates/example.html" "examples" frontPageExamples
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/site.html"

  -- Render examples page
  match "examples.html" $ route idRoute
  create "examples.html" $ constA mempty
    >>> arr (setField "title" "Examples")
    >>> setFieldPageList id "templates/example.html" "examples" "examples/sources/*.logo"
    >>> applyTemplateCompiler "templates/examples.html"
    >>> applyTemplateCompiler "templates/site.html"

  -- Render about page
  match (list ["about.markdown", "installation.markdown"] ) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/site.html"
 where
  sortByIndex = sortBy $ comparing $
    fromJust .
    (flip elemIndex $ frontPageExamplesList ) .
    getField "title"


-- *****************
-- Compilers
-- *****************

addLogoExampleFields :: Compiler (Page a) (Page a)
addLogoExampleFields = (getIdentifier &&& C.id >>^ uncurry addFields)
 where
  addFields i = trySetField "title"      (takeFileName p)
              . trySetField "image"      (outputFileName p)
              . trySetField "exampleid" (takeBaseName p)
   where
    p = toFilePath i
    outputFileName = uncurry (</>) . (replaceDirectoryName &&& replaceFileName)
     where
      replaceDirectoryName = (flip replaceBaseName  $ "svgs") . takeDirectory
      replaceFileName      = (flip replaceExtension $ "svg")    . takeBaseName


-- *****************
-- Configuration
-- *****************

frontPageExamples :: forall a. Pattern a
frontPageExamples = list $
  map (parseIdentifier . ("examples/sources" </>)) $ frontPageExamplesList

frontPageExamplesList :: [FilePath]
frontPageExamplesList = ["sun.logo", "moire.logo", "spiral.logo", "rotating_circle.logo", "snowflake.logo", "brownian_motion.logo"]

config :: HakyllConfiguration
config = defaultHakyllConfiguration  {
           deployCommand = "rsync  -avz --delete --exclude \".git\"  --progress _site/  ../../hs-logo-site/"
         }