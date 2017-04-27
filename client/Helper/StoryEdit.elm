module Helper.StoryEdit exposing (..)

import Model exposing (..)
import Language
import Api
import Dict
import Parser
import Helper


storyFromDraft : StoryDraft -> Maybe Api.Story
storyFromDraft draft =
    Maybe.map2 (,) draft.source draft.target
        |> Maybe.andThen
            (\( source, target ) ->
                if
                    (draft.sentences
                        |> Dict.values
                        |> List.any
                            (.text
                                >> Parser.parseTranslatedText target
                                >> Helper.isErr
                            )
                    )
                        || Dict.isEmpty draft.sentences
                then
                    Nothing
                else
                    Just
                        { title = draft.title
                        , sentences =
                            List.map
                                (\{ text, audioUrl } ->
                                    { text = text, audioUrl = audioUrl }
                                )
                                (Dict.values draft.sentences)
                        , sourceLanguage = Language.encodeLanguage source
                        , targetLanguage = Language.encodeLanguage target
                        }
            )
