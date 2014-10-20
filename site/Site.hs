{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
import           Control.Arrow    ((&&&))
import           Data.Monoid      (mappend)
import           Hakyll
import           System.FilePath  (replaceBaseName, replaceExtension,
                                   takeBaseName, takeDirectory, takeFileName,
                                   (</>))

main :: IO ()
main = hakyllWith config $ do

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- Read the logo example source files
  match "examples/sources/*.logo" $ compile getResourceBody

  -- Read the logo example image files
  match "examples/svgs/*.svg" $ do
    route idRoute
    compile copyFileCompiler

  -- Intro text on index page
  match "intro.markdown" $ compile pandocCompiler

  -- Render index page with select examples
  create [fromFilePath "index.html"] $ do
    route idRoute
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexPageCtx
        >>= loadAndApplyTemplate "templates/site.html" defaultContext

  -- Render examples page
  create [fromFilePath "examples.html"] $ do
    route idRoute
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/examples.html" examplesCtx
        >>= loadAndApplyTemplate "templates/site.html" defaultContext

  -- Render about page
  match (fromList ["about.markdown", "installation.markdown"]) $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate "templates/site.html" defaultContext


--- ****************
--  Contexts
--- ****************

-- Index page context containing some examples
indexPageCtx :: Context String
indexPageCtx = listField "examples" exampleCtx (loadAll frontPageExamples) `mappend`
               field "intro" (\_ -> loadBody "intro.markdown") `mappend`
               defaultContext

-- Examples page context containing all examples
examplesCtx :: Context String
examplesCtx =  listField "examples" exampleCtx (loadAll "examples/sources/*.logo") `mappend`
               defaultContext

-- Context for a single example
exampleCtx :: Context String
exampleCtx = field "title" getTitle `mappend`
             field "exampleid" getExampleId `mappend`
             field "image" getImage `mappend`
             defaultContext
 where
  getTitle item = do
    let i = toFilePath . itemIdentifier $ item
    return . takeFileName $ i

  getExampleId item = do
    let i = toFilePath . itemIdentifier $ item
    return . takeBaseName $ i

  getImage item = do
    let i = toFilePath . itemIdentifier $ item
    return . outputFileName $ i
   where
    outputFileName = uncurry (</>) . (replaceDirectoryName &&& replaceFileName)
    replaceDirectoryName = (flip replaceBaseName  $ "svgs") . takeDirectory
    replaceFileName      = (flip replaceExtension $ "svg") . takeBaseName


-- *****************
-- Configuration
-- *****************

frontPageExamples :: Pattern
frontPageExamples = fromList $ map (fromFilePath . ("examples/sources" </>)) $ frontPageExamplesList

frontPageExamplesList :: [FilePath]
frontPageExamplesList = ["sun.logo", "moire.logo", "spiral.logo", "rotating_circle.logo", "snowflake.logo", "brownian_motion.logo"]

config :: Configuration
config = defaultConfiguration  {
           deployCommand = "rsync  -avz --delete --exclude \".git\"  --progress _site/  ../../hs-logo-site/"
         }
