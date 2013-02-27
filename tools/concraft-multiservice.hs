{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Options.Applicative
import System.CPUTime (getCPUTime)
import Data.Int (Int32)
import Data.List (intercalate)
import qualified System.Process.Text.Lazy as Proc
import qualified Data.Binary as Binary
import qualified System.IO as IO
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.HashMap.Strict as H
import qualified Control.Monad.State.Strict as State

-- Thrift library
import Thrift.Server

-- Thrift generated modules
import qualified Types_Types as TT
import qualified AnnotatingService as Ann
import qualified AnnotatingService_Iface as Iface

import qualified NLP.Concraft as C
import qualified NLP.Concraft.Format as F
import qualified NLP.Concraft.Format.Plain as P

---------------------------
-- Interface implementation
---------------------------

-- | Implementation of the annotating service.
instance Iface.AnnotatingService_Iface C.Concraft where
    annotate concraft (Just ttext) _ =
        case V.toList <$> TT.f_TText_paragraphs ttext of
            Nothing -> return ttext
            Just ps -> do
                beg <- getCPUTime
                ps' <- evalCM $ mapM (annPar concraft) ps
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

-- A record of current identifier values.
data IdS = IdS
    { currParId     :: !Int
    , currSentId    :: !Int
    , currTokId     :: !Int }

-- | Initial identifier values.
initIdS :: IdS
initIdS = IdS 0 0 0

-- | A Concraft monad.
type CM = State.StateT IdS IO

-- | Evalueate CM computation.
evalCM :: CM a -> IO a
evalCM cm = State.evalStateT cm initIdS

-- | Build textual identifier value.
buildId :: String -> [Int] -> L.Text
buildId pref ks =
    L.pack $ pref ++ intercalate "." (map show ks)

-- | Get new paragraph ID.
newParId :: CM L.Text
newParId = do
    State.modify $ \idS -> idS {currParId = currParId idS + 1}
    buildId "p-" <$> sequence [State.gets currParId]

-- | Get new sentence ID.
newSentId :: CM L.Text
newSentId = do
    State.modify $ \idS -> idS {currSentId = currSentId idS + 1}
    buildId "s-" <$> sequence
        [ State.gets currParId
        , State.gets currSentId ]

-- | Get new token ID.
newTokId :: CM L.Text
newTokId = do
    State.modify $ \idS -> idS {currTokId = currTokId idS + 1}
    buildId "seg-" <$> sequence
        [ State.gets currParId
        , State.gets currSentId
        , State.gets currTokId ]

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
annPar :: C.Concraft -> TT.TParagraph -> CM TT.TParagraph
annPar concraft tpar = case TT.f_TParagraph_text tpar of
    Nothing -> return tpar
    Just tx -> do
        parId <- newParId
        State.lift $ L.putStr "> "
        State.lift $ L.putStrLn tx
        par <- map (disamb concraft) <$> State.lift (macalyse tx)
        sentences <- V.fromList <$> convertPar tx par
        return $ tpar
            { TT.f_TParagraph_id        = Just parId
            , TT.f_TParagraph_sentences = Just sentences }

-- | Use Maca tool to analyse the input text.
macalyse :: L.Text -> IO [[P.Token]]
macalyse inp = do
    let args = ["-q", "morfeusz-nkjp-official", "-o", "plain"]
    (_exitCode, out, _) <- Proc.readProcessWithExitCode "maca-analyse" args inp
    return $ P.parsePlain "ign" out

-- | Disambiguate sentence in the plain format.
disamb :: C.Concraft -> [P.Token] -> [P.Token]
disamb concraft toks =
    let sentH = F.sentHandler (P.plainFormat "ign")
    in  C.tagSent sentH concraft $ toks

-- | Convert the paragraph to the list of thrift sentences.
convertPar :: L.Text -> [[P.Token]] -> CM [TT.TSentence]
convertPar tx toks =
    let tokOffs = computeOffsets tx toks
    in  mapM convertSent tokOffs 

-- | Convert the sentence to the thrift form.
convertSent :: [(P.Token, Int32)] -> CM TT.TSentence
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
convertTok :: P.Token -> Int32 -> CM TT.TToken
convertTok tok offset = newTokId >>= \tokId -> return $ TT.TToken
    { f_TToken_id                       = Just tokId
    , f_TToken_orth                     = (Just . L.fromStrict) (P.orth tok)
    , f_TToken_offset                   = Just offset
    , f_TToken_noPrecedingSpace         = Just (P.space tok == P.None)
    , f_TToken_interpretations          = Just (V.fromList interps)
    , f_TToken_chosenInterpretation     = chosen
    , f_TToken_candidateInterpretations = Just V.empty }
  where
    interps = map (convertInterp . fst) $ M.toList (P.interps tok)
    chosen  = maybeHead
        [ convertInterp interp
        | (interp, True) <- M.toList (P.interps tok) ]
    maybeHead []    = Nothing
    maybeHead (x:_) = Just x

-- | Convert interpretation to the thrift form.
convertInterp :: P.Interp -> TT.TInterpretation
convertInterp P.Interp{..} = TT.TInterpretation
    { f_TInterpretation_base    = Just $ maybe "" L.fromStrict base
    , f_TInterpretation_ctag    = Just ctag
    , f_TInterpretation_msd     = Just msd }
  where
    (ctag, msd) = splitTag tag

-- | Split tag into the (class, msd) pair.
splitTag :: F.Tag -> (L.Text, L.Text)
splitTag = L.break (==':') . L.fromStrict

-- | Compute token offsets.
-- TODO: This is a stub, implement real offsets computation.
computeOffsets :: L.Text -> [[P.Token]] -> [[(P.Token, Int32)]]
computeOffsets _ tokss =
    [ map (,0) toks
    | toks <- tokss ]

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
        <> value 10008 )

decodeModel :: FilePath -> IO C.Concraft
decodeModel = Binary.decodeFile

runService :: Service -> IO ()
runService Service{..} = do
    putStr "Reading model..." >> IO.hFlush IO.stdout
    concraft <- decodeModel modelPath
    C.disamb concraft `seq` putStrLn " done"
    putStrLn "Start server"
    _ <- runBasicServer concraft Ann.process (fromIntegral port)
    putStrLn "Server stopped"

main :: IO ()
main =
    execParser opts >>= runService
  where
    opts = info (helper <*> service)
      ( fullDesc
     <> progDesc "Run Concraft multiservice component"
     <> header "nerf-concraft" )
