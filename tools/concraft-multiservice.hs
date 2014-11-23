{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Options.Applicative
import System.CPUTime (getCPUTime)
import Control.Arrow (second)
import Control.Monad (void)
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
import           NLP.Concraft.Morphosyntax (unWMap)
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

---------------------------
-- Application state
---------------------------

-- | A record with an application state.
data CMState = CMState
    { currParId     :: !Int
    -- ^ Paragraph ID.
    , currSentId    :: !Int
    -- ^ Sentence ID.
    , currTokId     :: !Int
    -- ^ Token ID.
    , parLeft       :: !L.Text
    -- ^ Paragraph text left to process.  Useful for offset computation.
    -- Has to be updated when a new paragraph is supplied.
    , currOffset    :: !Int
    -- ^ Offset of the current word.
    }

-- | Initial identifier values.
initCMState :: CMState
initCMState = CMState 0 0 (-1) "" 0

-- | A Concraft monad.
type CM = S.StateT CMState IO

-- | Evalueate CM computation.
evalCM :: CM a -> IO a
evalCM cm = S.evalStateT cm initCMState

-- | Build textual identifier value.
buildId :: String -> [Int] -> L.Text
buildId pref ks =
    L.pack $ pref ++ intercalate "." (map show ks)

-- | Get new paragraph ID.  The function takes a paragraph text.
newPar :: L.Text -> CM L.Text
newPar tx = do
    S.modify $ \cms -> cms
        { currParId  = currParId cms + 1
        , currSentId = 0   
        , currTokId  = 0
        , parLeft    = tx
        , currOffset = 0 }
    buildId "p-" <$> sequence [S.gets currParId]

-- | Get new sentence ID.
newSent :: CM L.Text
newSent = do
    S.modify $ \cms -> cms
        { currSentId = currSentId cms + 1
        , currTokId  = (-1) }
    buildId "s-" <$> sequence
        [ S.gets currParId
        , S.gets currSentId ]

-- | Get new token ID.
newTok :: CM L.Text
newTok = do
    S.modify $ \cms -> cms {currTokId = currTokId cms + 1}
    buildId "seg-" <$> sequence
        [ S.gets currParId
        , S.gets currSentId
        , S.gets currTokId ]

-- | "Eat" word from the beginning of the given text
-- and increase the offset counter.
consumeOrth :: L.Text -> CM Int
consumeOrth orth = do
    (tt, i) <- S.gets $ (,) <$> parLeft <*> currOffset
    let (t0, t1) = L.span C.isSpace tt
    case L.stripPrefix orth t1 of
        Nothing -> error "Error during computation of offsets"
        Just t2 -> S.modify $ \cms -> cms
            { parLeft       = t2
            , currOffset    = i + fromIntegral
                (L.length t0 + L.length orth) }
    return i

----------------------------
-- Conversion and annotation
----------------------------

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
        lift $ L.putStr "> " >> L.putStrLn tx
        parId <- newPar tx
        let tag x = map (P.tag concraft) <$> P.macaPar macaPool x
        -- par   <- lift (P.tag macaPool concraft (L.toStrict tx))
        par   <- lift (tag $ L.toStrict tx)
             >>= fmap V.fromList . convertPar
        return $ tpar
            { TT.f_TParagraph_id        = Just parId
            , TT.f_TParagraph_sentences = Just par }

-- | Convert the paragraph to the list of thrift sentences.
convertPar :: [X.Sent X.Tag] -> CM [TT.TSentence]
convertPar = mapM convertSent

-- | Convert the sentence to the thrift form.
convertSent :: X.Sent X.Tag -> CM TT.TSentence
convertSent sent = do
    sentId <- newSent
    toks   <- V.fromList <$> mapM convertTok sent
    return $ TT.TSentence
        { f_TSentence_id                = Just sentId
        , f_TSentence_tokens            = Just toks
        , f_TSentence_rejectedTokens    = Just V.empty
        , f_TSentence_words             = Just V.empty
        , f_TSentence_groups            = Just V.empty
        , f_TSentence_names             = Just V.empty
        , f_TSentence_dependencyParse   = Just V.empty
        , f_TSentence_mentions          = Just V.empty }

-- | Convert the token to the thrift form.
convertTok :: X.Seg X.Tag -> CM TT.TToken
convertTok seg@X.Seg{..} = do
    tokId  <- newTok
    offset <- consumeOrth $ L.fromStrict $ X.orth $ X.word seg
    return $ TT.TToken
        { f_TToken_id                       = Just tokId
        , f_TToken_orth                     = Just lorth
        , f_TToken_offset                   = Just (fromIntegral offset)
        , f_TToken_noPrecedingSpace         = Just (space == X.None)
        , f_TToken_interpretations          = Just (V.fromList interps')
        , f_TToken_chosenInterpretation     = chosen
        , f_TToken_candidateInterpretations = Just V.empty }
  where
    lorth = L.fromStrict orth
    X.Word{..} = word
    interps' = map (convertInterp lorth . fst)
                   (M.toList $ unWMap interps)
    chosen   = maybeHead
        [ convertInterp lorth interp
        -- | (interp, True) <- M.toList interps ]
        | (interp, weight) <- M.toList $ unWMap interps
        , weight > 0 ]
    maybeHead []    = Nothing
    maybeHead (x:_) = Just x

-- | Convert interpretation to the thrift form.  We use `X.orth`
-- (should be supplied as the first argument) as a default base form.
convertInterp :: L.Text -> X.Interp X.Tag -> TT.TInterpretation
convertInterp _orth X.Interp{..} = TT.TInterpretation
    -- { f_TInterpretation_base    = Just $ maybe orth L.fromStrict base
    { f_TInterpretation_base    = Just $ L.fromStrict base
    , f_TInterpretation_ctag    = Just ctag
    , f_TInterpretation_msd     = Just msd }
  where
    (ctag, msd) = splitTag tag

-- | Split tag into the (class, msd) pair.
splitTag :: X.Tag -> (L.Text, L.Text)
splitTag = second (L.drop 1) . L.break (==':') . L.fromStrict

----------------------------------
-- Command-line program definition
----------------------------------
        
data Service = Service
    { modelPath :: FilePath
    , port      :: Int }

service :: Parser Service
service = Service
    <$> strArgument (metavar "MODEL")
    <*> option auto
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
