module Helper.StoryEdit exposing (..)

import Model exposing (..)
import Api
import Dict
import Parser
import Helper


storyFromDraft : StoryDraft -> Maybe Api.Story
storyFromDraft draft =
    if
        (draft.sentences
            |> Dict.values
            |> List.any
                (.text
                    >> Parser.parseTranslatedText
                    >> Helper.isErr
                )
        )
            || Dict.isEmpty draft.sentences
    then
        Nothing
    else
        Maybe.map2
            (\source target ->
                { title = draft.title
                , sentences =
                    List.map
                        (\{ text, audioUrl } ->
                            { text = text, audioUrl = audioUrl }
                        )
                        (Dict.values draft.sentences)
                , sourceLanguage = encodeLanguage source
                , targetLanguage = encodeLanguage target
                }
            )
            draft.source
            draft.target
