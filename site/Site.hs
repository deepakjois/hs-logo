{-# LANGUAGE OverloadedStrings, RankNTypes #-}
import Control.Arrow (arr, (>>>), (>>^), (&&&))
import Data.Monoid (mempty, mconcat)
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
  match "examples/images/*.png" $ do
    route idRoute
    compile copyFileCompiler

  -- Render index page with select examples
  match "intro.markdown" $ compile pageCompiler

  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> setFieldPage "intro" "intro.markdown"
    >>> requireAllA frontPageExamples addLogoExamplesList
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/site.html"

  -- Render examples page
  match "examples.html" $ route idRoute
  create "examples.html" $ constA mempty
      >>> arr (setField "title" "Examples")
      >>> requireAllA "examples/sources/*.logo" addLogoExamplesList
      >>> applyTemplateCompiler "templates/examples.html"
      >>> applyTemplateCompiler "templates/site.html"

  -- Render about page
  match "about.markdown" $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/site.html"

-- *****************
-- Compilers
-- *****************

addLogoExampleFields :: Compiler (Page a) (Page a)
addLogoExampleFields = (getIdentifier &&& C.id >>^ uncurry addFields)
 where
  addFields i = trySetField "title" (takeFileName p)
              . trySetField "image" (outputFileName p)
   where
    p = toFilePath i
    outputFileName = uncurry (</>) . (replaceDirectoryName &&& replaceFileName)
     where
      replaceDirectoryName = (flip replaceBaseName  $ "images") . takeDirectory
      replaceFileName      = (flip replaceExtension $ "png")    . takeBaseName

addLogoExamplesList :: Compiler (Page String, [Page String]) (Page String)
addLogoExamplesList = setFieldA "examples" $
  require "templates/example.html" (\p t -> map (applyTemplate t) p)
  >>> arr mconcat
  >>> arr pageBody


-- *****************
-- Configuration
-- *****************

frontPageExamples :: forall a. Pattern a
frontPageExamples = list $
  map (parseIdentifier . ("examples/sources" </>)) $
  ["moire.logo", "sun.logo", "spiral.logo", "rotating_circle.logo", "snowflake.logo", "brownian_motion.logo"]

config :: HakyllConfiguration
config = defaultHakyllConfiguration  {
           deployCommand = "rsync  -avz --delete --exclude \".git\"  --progress _site/  ../../hs-logo-site/"
         }