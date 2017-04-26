module Helper.Auth exposing (..)

import Model exposing (..)


user : AppModel -> Maybe User
user app =
    case app of
        NotReady nr ->
            nr.user

        Ready app ->
            case app.page of
                LandingPage page ->
                    page.user

                LoginPage page ->
                    page.user

                SignupPage page ->
                    page.user

                Dashboard page ->
                    Just page.user

                StoryPage page ->
                    Just page.user

                StoryEditPage page ->
                    Just page.user

                LoggedoutPage ->
                    Nothing
