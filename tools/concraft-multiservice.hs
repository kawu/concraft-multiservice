{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Options.Applicative
import System.CPUTime (getCPUTime)
import Control.Arrow (second)
import Control.Monad (void, forM)
import Data.Int (Int32)
import Data.List (intercalate)
import qualified System.IO as IO
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.HashMap.Strict as H
import qualified Control.Monad.State.Strict as S
import           Control.Monad.State.Strict (lift)

-- Thrift library
import Thrift.Server

-- Thrift generated modules
import qualified Types_Types as TT
import qualified AnnotatingService as Ann
import qualified AnnotatingService_Iface as Iface

import qualified NLP.Concraft as Core
import qualified NLP.Concraft.Polish as P
import qualified NLP.Concraft.Polish.Morphosyntax as X
import qualified NLP.Concraft.Polish.Maca as P

---------------------------
-- Core data type
---------------------------

data ConPL = ConPL
    { macaPool  :: P.MacaPool
    , concraft  :: P.Concraft }

---------------------------
-- Interface implementation
---------------------------

-- | Implementation of the annotating service.
instance Iface.AnnotatingService_Iface ConPL where
    annotate conPL (Just ttext) _ =
        case V.toList <$> TT.f_TText_paragraphs ttext of
            Nothing -> return ttext
            Just ps -> do
                beg <- getCPUTime
                ps' <- evalCM $ mapM (annPar conPL) ps
                end <- getCPUTime
                let coef = 10 ^ (9 :: Integer)
                let diff = (end - beg) `div` coef
                return $ ttext
                    { TT.f_TText_paragraphs             = Just (V.fromList ps')
                    , TT.f_TText_annotationHeaders      = Just $
                        addHeaders diff (getHeaders ttext)
                    , TT.f_TText_textHeader             = Just textHeader
                    , TT.f_TText_annotationDetails      = Just annDetails
                    , TT.f_TText_summary                = Just ""
                    , TT.f_TText_coreferences           = Just V.empty }
      where
        getHeaders = maybe H.empty id . TT.f_TText_annotationHeaders
        textHeader = TT.THeader
            { f_THeader_id                  = Just ""
            , f_THeader_title               = Just ""
            , f_THeader_distributor         = Just ""
            , f_THeader_publicationTime     = Just 0
            , f_THeader_processingDuration  = Just 0
            , f_THeader_sourceDescText      = Just "" 
            , f_THeader_retrievedFrom       = Just "" }
        annDetails = TT.AnnotationDetails
            { f_AnnotationDetails_hasSegmentsDisambiguated      = Just True
            , f_AnnotationDetails_hasMorphosyntaxDisambiguated  = Just True
            , f_AnnotationDetails_hasMorphosyntaxPartiallyDisambiguated =
                 Just False }

-- | A record of current identifier values.
data IdS = IdS
    { currParId     :: !Int
    , currSentId    :: !Int
    , currTokId     :: !Int }

-- | Initial identifier values.
initIdS :: IdS
initIdS = IdS 0 0 0

-- | A Concraft monad.
type CM = S.StateT IdS IO

-- | Evalueate CM computation.
evalCM :: CM a -> IO a
evalCM cm = S.evalStateT cm initIdS

-- | Build textual identifier value.
buildId :: String -> [Int] -> L.Text
buildId pref ks =
    L.pack $ pref ++ intercalate "." (map show ks)

-- | Get new paragraph ID.
newParId :: CM L.Text
newParId = do
    S.modify $ \idS -> idS {currParId = currParId idS + 1}
    buildId "p-" <$> sequence [S.gets currParId]

-- | Get new sentence ID.
newSentId :: CM L.Text
newSentId = do
    S.modify $ \idS -> idS {currSentId = currSentId idS + 1}
    buildId "s-" <$> sequence
        [ S.gets currParId
        , S.gets currSentId ]

-- | Get new token ID.
newTokId :: CM L.Text
newTokId = do
    S.modify $ \idS -> idS {currTokId = currTokId idS + 1}
    buildId "seg-" <$> sequence
        [ S.gets currParId
        , S.gets currSentId
        , S.gets currTokId ]

-- | Add appropriate annotation headers.
addHeaders
    :: Integer
    -> H.HashMap TT.TAnnotationLayer TT.THeader
    -> H.HashMap TT.TAnnotationLayer TT.THeader
addHeaders procTime
    = H.insert TT.SEGMENTATION entry
    . H.insert TT.MORPHOSYNTAX entry
  where
    entry = TT.THeader
        { f_THeader_id = Just ""
        , f_THeader_title = Just ""
        , f_THeader_distributor = Just "Concraft"
        , f_THeader_publicationTime = Just 0
        , f_THeader_processingDuration = Just (fromIntegral procTime)
        , f_THeader_sourceDescText = Just ""
        , f_THeader_retrievedFrom = Just "" }

-- | Annotate paragraph.
annPar :: ConPL -> TT.TParagraph -> CM TT.TParagraph
annPar ConPL{..} tpar = case TT.f_TParagraph_text tpar of
    Nothing -> return tpar
    Just tx -> do
        parId <- newParId
        lift $ L.putStr "> " >> L.putStrLn tx
        par <- lift (P.tag macaPool concraft (L.toStrict tx)) >>= convertPar tx
        return $ tpar
            { TT.f_TParagraph_id        = Just parId
            , TT.f_TParagraph_sentences = Just (V.fromList par) }

-- | Convert the paragraph to the list of thrift sentences.
convertPar :: L.Text -> [X.Sent X.Tag] -> CM [TT.TSentence]
convertPar tx toks =
    let tokOffs = computeOffsets tx toks
    in  mapM convertSent tokOffs 

-- | Convert the sentence to the thrift form.
convertSent :: [(X.Seg X.Tag, Int32)] -> CM TT.TSentence
convertSent tokOffs = do
    sentId <- newSentId
    toks   <- sequence
        [ convertTok tok off
        | (tok, off) <- tokOffs ]
    return $ TT.TSentence
        { f_TSentence_id                = Just sentId
        , f_TSentence_tokens            = Just (V.fromList toks)
        , f_TSentence_rejectedTokens    = Just V.empty
        , f_TSentence_words             = Just V.empty
        , f_TSentence_groups            = Just V.empty
        , f_TSentence_names             = Just V.empty
        , f_TSentence_dependencyParse   = Just V.empty
        , f_TSentence_mentions          = Just V.empty }

-- | Convert the token to the thrift form.
convertTok :: X.Seg X.Tag -> Int32 -> CM TT.TToken
convertTok X.Seg{..} offset = newTokId >>= \tokId -> return $ TT.TToken
    { f_TToken_id                       = Just tokId
    , f_TToken_orth                     = (Just . L.fromStrict) orth
    , f_TToken_offset                   = Just offset
    , f_TToken_noPrecedingSpace         = Just (space == X.None)
    , f_TToken_interpretations          = Just (V.fromList interps')
    , f_TToken_chosenInterpretation     = chosen
    , f_TToken_candidateInterpretations = Just V.empty }
  where
    X.Word{..} = word
    interps' = map (convertInterp . fst) (M.toList interps)
    chosen   = maybeHead
        [ convertInterp interp
        | (interp, True) <- M.toList interps ]
    maybeHead []    = Nothing
    maybeHead (x:_) = Just x

-- | Convert interpretation to the thrift form.
convertInterp :: X.Interp X.Tag -> TT.TInterpretation
convertInterp X.Interp{..} = TT.TInterpretation
    { f_TInterpretation_base    = Just $ maybe "" L.fromStrict base
    , f_TInterpretation_ctag    = Just ctag
    , f_TInterpretation_msd     = Just msd }
  where
    (ctag, msd) = splitTag tag

-- | Split tag into the (class, msd) pair.
splitTag :: X.Tag -> (L.Text, L.Text)
splitTag = second (L.drop 1) . L.break (==':') . L.fromStrict

----------------------------------
-- Offset computation
----------------------------------

-- | TODO: Use more idiomatic code instead of the state monad
-- in order to make the function work lazily.

-- | Compute token offsets.
computeOffsets :: L.Text -> [[X.Seg a]] -> [[(X.Seg a, Int32)]]
computeOffsets txt xss =
    flip S.evalState (txt, 0) $ forM xss $ mapM $ \seg -> do
        (x, i) <- S.get
        let (y, j) = eat seg (x, i)
        S.put (y, j)
        return (seg, i)

-- | "Eat" word from the beginning of the given text
-- and increase the offset counter.
eat :: X.Seg a -> (L.Text, Int32) -> (L.Text, Int32)
eat seg (tt, i) = case L.stripPrefix orth t1 of
    Nothing -> error "Error during computation of offsets"
    Just t2 -> (t2, i + fromIntegral (L.length t0 + L.length orth))
  where
    (t0, t1) = L.span C.isSpace tt
    orth = L.fromStrict $ X.orth $ X.word seg

----------------------------------
-- Command-line program definition
----------------------------------
        
data Service = Service
    { modelPath :: FilePath
    , port      :: Int }

service :: Parser Service
service = Service
    <$> argument str (metavar "MODEL")
    <*> option
         ( long "port"
        <> short 'p'
        <> help "Port number"
        <> value 10018 )

runService :: Service -> IO ()
runService Service{..} = do
    putStrLn "Starting 1 maca instance..."
    macaPool <- P.newMacaPool 1
    putStr "Reading model..." >> IO.hFlush IO.stdout
    concraft <- P.loadModel modelPath
    -- Check if `disamb` component has been loaded.
    Core.disamb concraft `seq` putStrLn " done"
    putStrLn "Start server"
    let conPL = ConPL macaPool concraft
    void $ runBasicServer conPL Ann.process (fromIntegral port)
    putStrLn "Server stopped"

main :: IO ()
main =
    execParser opts >>= runService
  where
    opts = info (helper <*> service)
      ( fullDesc
     <> progDesc "Run Concraft multiservice component"
     <> header "concraft-multiservice" )
