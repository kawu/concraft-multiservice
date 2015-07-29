namespace cpp multiservice
namespace java pl.waw.ipipan.zil.multiservice.thrift.types
namespace py multiservice.types

enum TAnnotationLayer {
   SEGMENTATION,
   MORPHOSYNTAX,
   WORDS,
   GROUPS,
   NAMES,
   SUMMARY,
   DEPENDENCY_PARSE,
   MENTIONS,
   COREFERENCE,
   SENTIMENT
}

exception MultiserviceException {
    1: string message
}

struct TInterpretation {
    1: required string base,
    2: required string ctag,
    3: string msd
}

struct TToken {
    1: string id,
    2: string orth,
    3: i32 offset,
    4: bool noPrecedingSpace,
    5: list<TInterpretation> interpretations,
    6: TInterpretation chosenInterpretation,
    7: list<TInterpretation> candidateInterpretations
}

struct TSyntacticWord {
    1: string id,
    2: string orth,
    3: TInterpretation chosenInterpretation,
    4: list<TInterpretation> candidateInterpretations,
    5: list<string> childIds,
    6: string rule
}

struct TSyntacticGroup {
    1: string id,
    2: string orth,
    3: string semanticHeadId,
    4: string syntacticHeadId,
    5: list<string> childIds,
    6: string type,
    7: string rule
}

struct TSentimentTag {
    1: string id,
    2: string orth,
    3: double value, // -1 means "extremely negative", 1 means "extremely positive"
    4: list<string> childIds, // contains ids of morphosyntactic entities
    5: string rule
}

struct TNamedEntity {
    1: string id,
    2: string orth,
    3: string base,
    4: string type,
    5: string subtype,
    6: list<string> childIds
}

struct DependencyParseNode {
    1: string startTokenId,
    2: string endTokenId,
    3: string label
}

struct TMention {
    1: string id,
    2: list<string> headIds,
    3: list<string> childIds,
    4: bool zeroSubject
}

struct TCoreference {
    1: string id,
    2: string type,
    3: string dominant,
    4: list<string> mentionIds,
    5: string sourceMentionId
}

struct TSentence {
    1: string id,
    2: list<TToken> tokens,
    3: list<TToken> rejectedTokens, // tokens from rejected segmentation variants
    4: list<TSyntacticWord> words,
    5: list<TSyntacticGroup> groups,
    6: list<TNamedEntity> names,
    7: list<DependencyParseNode> dependencyParse,
    8: list<TMention> mentions,
    9: list<TSentimentTag> sentimentTags
}

struct TParagraph {
    1: string id,
    2: string text,
    3: list<TSentence> sentences
}

struct THeader {
    1: string id,
    2: string title,		// title of the text (if exists)
    3: string distributor,	// distributor of the text or name of the annotating tool
    4: i64 publicationTime,	// date of publication in POSIX format
    5: i64 processingDuration, 	// time spent on annotation, in milliseconds
    6: string sourceDescText,
    7: string retrievedFrom
}

struct AnnotationDetails {
    1: bool hasSegmentsDisambiguated = false,
    2: bool hasMorphosyntaxDisambiguated = false,
    3: bool hasMorphosyntaxPartiallyDisambiguated = false
}

struct TText {
    1: THeader textHeader,
    2: required list<TParagraph> paragraphs,
    3: required map<TAnnotationLayer, THeader> annotationHeaders = {},
    4: required AnnotationDetails annotationDetails = {},
    5: string summary,
    6: list<TCoreference> coreferences
}
